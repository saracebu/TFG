#CREACI?N DE LA NUEVA BASE DE DATOS

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                           SET PATHS                                    ####
path_data <- 'C:/Users/Tester/Desktop/TFG'
#path_src <- 'N:/UDMTD16/code/src'
#path_data <- 'N:/UDMTD16/data'
#path_data <- 'N:/UDMTD/UDMTD16/data'
#path_src  <- 'N:/UDMTD/UDMTD16/code/src'
#path_data <- 'C:/Users/David/Documents/Cursos.Seminarios/UCM/TFG/Sara Cebula - Depuración/data'
#path_src  <- 'C:/Users/David/Documents/Cursos.Seminarios/UCM/TFG/Sara Cebula - Depuración/code/src'
####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  LOAD PACKAGES AND FUNCTIONS                           ####
library(data.table)
library(pROC)
library(ggplot2)
#library(viridis) #customize ggplots
#library(gridExtra) #customize ggroc
library(latex2exp)
#library(ROCR)
library(matrixStats)
library(randomForest)
library(ranger)


source(file.path(path_data, 'area_curva.R'))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  LOAD PACKAGES AND FUNCTIONS                           ####

target_vars <- c("CLASE_AS", "CNAE_Div_AS")


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  READ RDS                                              ####

tru.dt <- readRDS(file = file.path(path_data, 'FF_2011.dt.rds'))
raw.dt <- readRDS(file = file.path(path_data, 'FG_2011.dt.rds'))

raw.dt[, cod := paste(CODSEC, VIV, HOGAR, NORDEN_ID, sep = "")]
tru.dt[, cod := paste(CODSEC, VIV, HOGAR, NORDEN_ID, sep = "")]

varNames_all    <- names(raw.dt)
varNames_ID     <- c('cod', 'CCAA', 'ESTRATO', "SEXOa", "EDADa", "PROXY_0", 'FACTORADULTO')
varNames_target <- c( "F9", "F18", "F8_2", "F17a_2", "F17m_2", "F7_2",
                      "F16a_2", "F16m_2", "A10_i",
                      "CNO_Sub_AS", "CNO_SPl_AS", "CNO_GPl_AS",
                      "CNAE_Gru_AS", "CNAE_Div_AS", "CNAE_Sec_AS",
                      "SitProf_AS", "CLASE_AS", "A7_2a", "D28", "ACTIVa")

raw.dt <- raw.dt[, c(varNames_ID, varNames_target), with = FALSE]
setnames(raw.dt, varNames_target, paste0(varNames_target, '_raw'))

tru.dt <- tru.dt[, c('cod', varNames_target), with = FALSE]
setnames(tru.dt, varNames_target, paste0(varNames_target, '_true'))


dat.dt <- merge(raw.dt, tru.dt, by = 'cod')


regressors_vars <- c(setdiff(varNames_ID, 'cod'), paste0(varNames_target, '_raw'))


for (i in target_vars){

  r <- paste(i, "_raw", sep = "")
  t <- paste(i, "_true", sep = "")
  targetName <- paste0('target_', i)
  set(dat.dt, which(is.na(dat.dt[[r]])), r, '*')
  set(dat.dt, which(is.na(dat.dt[[t]])), t, '*')
  dat.dt[, (targetName) := ifelse(get(r) == get(t), 0, 1)]

}

dat.dt[
  , target := (rowSums2(as.matrix(.SD), na.rm = TRUE) > 0) * 1L, .SDcols = paste0('target_', target_vars)]

data_rf.dt <- copy(raw.dt)[
  dat.dt[, c('cod', 'target')], on = 'cod'][
  , c('target', 'cod', regressors_vars), with = FALSE]

data_rf.dt[is.na(data_rf.dt)] <- '*'

train_index <- sample(1:nrow(data_rf.dt), nrow(data_rf.dt) * 0.8)
train_rf.dt <- data_rf.dt[train_index, ]
test_rf.dt  <- data_rf.dt[-train_index, ]


fit <- ranger(formula = target ~ ., data = train_rf.dt, num.trees = 1000, importance = "impurity")
fit$variable.importance
#ggplot2
importance.dt <- data.table(importance = fit$variable.importance, variable = names(fit$variable.importance))
ggplot(importance.dt, aes(x = reorder(variable, importance, sum), y = importance),color = "green4") +
  geom_col(fill = "green4")+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title   = element_text(hjust = 0.5, size = 22),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "darkgreen"),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3"))+
  labs(x = paste0("Variable"))


values_mtry  <- seq.int(from = 1, to = length(regressors_vars), length.out = 4)
values_trees <- c(500, 750, 1000, 1250, 1500, 1750)
error        <- matrix(nrow = length(values_mtry), ncol = length(values_trees))

for(i in 1:length(values_mtry)){
  for(j in 1:length(values_trees)){
    pred     <- ranger(formula = target~.,
                       data = train_rf.dt,
                       num.trees = values_trees[j],
                       mtry = values_mtry[i])
    error[i,j] <- pred$r.squared
  }
}

max_col <- max.col(error)
max_row <- which.max(c(error[1,max_col[1]], error[2,max_col[2]], error[3,max_col[3]], error[4,max_col[4]]))
t <- proc.time()
model_rf <- ranger(formula = target ~ .,
                   data = train_rf.dt,
                   num.trees = values_trees[max_col[max_row]],
                   mtry = values_mtry[max_row])
model_rf <- ranger(formula = target ~ .,
                   data = train_rf.dt,num.trees = 1500, mtry =17)

pred_rf_train <- model_rf$predictions
result_pred   <- predict(fit, test_rf.dt)
pred_rf_test  <- result_pred$predictions
tim <- proc.time() - t

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                          RESULTADOS                                    ####

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS")
#1500 ARBOLES
#MTRY = 9
#TIEMPO system = 0.25
#R squared (OOB): 0.2490863
#ROC = 0.72
#AUCC = 0.62
#sd = 0.084

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "A10_i"):
#1250 ARBOLES
#MTRY = 9
#TIEMPO system = 0.16
#R squared (OOB): 0.2855142
#ROC = 0.7
#AUCC = 0.6
#sd = 0.081

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "D28")
#1500 ARBOLES
#MTRY = 17
#TIEMPO system = 0.20
#R squared (OOB): 0.8173601
#ROC = 0.99
#AUCC = 0.65
#sd = 0.088

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F16a_2"):
#1500 ARBOLES
#MTRY = 9
#TIEMPO system = 0.29
#R squared (OOB): 0.226799
#ROC = 0.68
#AUCC = 0.62
#sd = 0.089

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "A7_2a"):
#750 ARBOLES
#MTRY = 9
#TIEMPO system = 0.06
#R squared (OOB): 0.2412784
#ROC = 0.68
#AUCC = 0.59
#sd = 0.086

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F16m_2"):
#1750 ARBOLES
#MTRY = 9
#TIEMPO system = 0.61
#R squared (OOB): 0.2349662
#ROC = 0.7
#AUCC = 0.64
#sd = 0.071

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F17m_2):
#1750 ARBOLES
#MTRY = 9
#TIEMPO system = 0.31
#R squared (OOB): 0.2057714
#ROC = 0.68
#AUCC = 0.61
#sd = 0.089

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F18"):
#1750 ARBOLES
#MTRY = 9
#TIEMPO system = 0.75
#R squared (OOB): 0.0579404
#ROC = 0.72
#AUCC = 0.64
#sd = 0.084

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F7_2"):
#1250 ARBOLES
#MTRY = 9
#TIEMPO system = 0.47
#R squared (OOB): 0.2502167
#ROC = 0.74
#AUCC = 0.59
#sd = 0.097

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F8_2"):
#1000 ARBOLES
#MTRY = 9
#TIEMPO system = 0.19
#R squared (OOB): 0.2451784
#ROC = 0.73
#AUCC = 0.60
#sd = 0.077

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F9"):
#1500 ARBOLES
#MTRY = 9
#TIEMPO system = 1.71
#R squared (OOB): 0.2543974
#ROC = 0.71
#AUCC = 0.59
#sd = 0.082

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "F17a_2"):
#1500 ARBOLES
#MTRY = 9
#TIEMPO system = 0.37
#R squared (OOB): 0.2002753
#ROC = 0.71
#AUCC = 0.62
#sd = 0.084

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "A10_i", "D28"):
#1750 ARBOLES
#MTRY = 17
#TIEMPO system = 0.56
#R squared (OOB): 0.8269563
#ROC = 0.99
#AUCC = 0.63
#sd = 0.108

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "A10_i", "F9")
#1750 ARBOLES
#MTRY = 9
#TIEMPO system = 0.77
#R squared (OOB): 0.2835906
#ROC = 0.7
#AUCC = 0.62
#sd = 0.085

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                       CURVAS ROC                                       ####


curvaROC1 <- roc(response  = test_rf.dt[, target],
                 predictor =   pred_rf_test)
AUC1 <- auc(curvaROC1)

ggroc(curvaROC1, size = 1.6, color = "gray22") +
  geom_abline(slope = 1, intercept = 1, linetype = 'dotted', size = 1.1, color = "gray22") +
  annotate("text", x = 0.25, y = 0.05, label = as.character(round(AUC1,2))) +
  labs(title = paste0('ROC curves for CLASE_AS, CNAE_Div_AS and D28'),
       color = 'Variable', linetype = 'Variable') +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 15, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 12, color = "green4", face = "bold"))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                        LOCAL SCORE

id.vars     <- c("CCAA", "ESTRATO", "SEXOa", "EDADa", "PROXY_0")

test_rf.dt[, score := pred_rf_test*FACTORADULTO]
test_rf.dt[, score_factor := FACTORADULTO]
test_rf.dt[, score_rnd := runif(1), by = "cod"]
test_rf.dt[, set := "test"]
test_rf.dt <- test_rf.dt[order(-score)]
test_rf.dt[, edPriority := .I]
test_factor.dt <- copy(test_rf.dt)[order(-score_factor)]
test_factor.dt[, edPriority := .I]
test_rnd.dt <- copy(test_rf.dt)[order(-score_rnd)]
test_rnd.dt[, edPriority := .I]

train_rf.dt[, score := pred_rf_train*FACTORADULTO]
train_rf.dt[, score_factor := FACTORADULTO]
train_rf.dt[, score_rnd := runif(1), by = 'cod']
train_rf.dt[, set:= "train"]
train_rf.dt <- train_rf.dt[order(-score)]
train_rf.dt[, edPriority := .I]
train_factor.dt <- copy(train_rf.dt)[order(-score_factor)]
train_factor.dt[, edPriority := .I]
train_rnd.dt <- copy(train_rf.dt)[order(-score_rnd)]
train_rnd.dt[, edPriority := .I]

tt.dt  <- rbindlist(list(test_rf.dt, train_rf.dt))
tt.dt  <- tt.dt[order(CCAA, ESTRATO, SEXOa, EDADa, PROXY_0, FACTORADULTO)]
dat.dt <- dat.dt[order(CCAA, ESTRATO, SEXOa, EDADa, PROXY_0, FACTORADULTO)]
tt.dt[, CLASE_AS_true := dat.dt$CLASE_AS_true]
tt.dt <- tt.dt[order(set,-score)]

clase_plots <- levels(as.factor(unique(tt.dt$CLASE_AS_true)))
tt.dt       <- tt.dt[CLASE_AS_true %in% clase_plots, ]

workingDT <- copy(tt.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_raw == val})]
DT        <- melt(workingDT,
           id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "FACTORADULTO", "score", "set", "edPriority"),
           measure.vars = clase_plots, variable.name = 'value_bin',
           value.name = 'bin')

workingDT_ed <- copy(tt.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_true == val})]
DT_ed        <- melt(workingDT_ed,
              id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "FACTORADULTO", "score", "set", "edPriority"),
              measure.vars = clase_plots, variable.name = 'value_bin_ed',
              value.name = 'bin_ed')

DT[, value_bin_ed := DT_ed[['value_bin_ed']]][
  , bin_ed := DT_ed[['bin_ed']]][
    bin == TRUE]

output_train <- lapply(tt.dt[set == 'train']$edPriority, function(edPrior){

  tempDT <- DT[set == 'train']
  tempDT[, bin_run := bin][
    edPriority <= edPrior, bin_run := bin_ed]
  localOutput <- tempDT[, list(estim = sum(FACTORADULTO * bin_run, na.rm = TRUE)), by = c('value_bin')]
  setnames(localOutput, 'value_bin', "CLASE_AS")[, edPriority := edPrior]
  return(localOutput)

})
output_train.dt <- rbindlist(output_train)[
  , set := 'train']


output_test <- lapply(tt.dt[set == 'test']$edPriority, function(edPrior){

  tempDT <- DT[set == 'test']
  tempDT[, bin_run := bin][
    edPriority <= edPrior, bin_run := bin_ed]
  localOutput <- tempDT[, list(estim = sum(FACTORADULTO * bin_run, na.rm = TRUE)), by = c('value_bin')]
  setnames(localOutput, 'value_bin', "CLASE_AS")[, edPriority := edPrior]
  return(localOutput)

})
output_test.dt <- rbindlist(output_test)[
  , set := 'test']


output.dt <- rbindlist(list(output_train.dt, output_test.dt))

estim0.dt <- tt.dt[, list(estim0 = sum(FACTORADULTO, na.rm = TRUE)), by = c('set', "CLASE_AS_true")]

estim.dt <- merge(output.dt, estim0.dt, by.x = c('set', "CLASE_AS"), by.y = c('set', "CLASE_AS_true"))


pbias.dt <- estim.dt[
  , ARB      := abs((estim - estim0) / estim0)][
  , totalARB := sum(ARB), by = c("CLASE_AS", "set")][
  , maxN     := max(edPriority), by = list(CLASE_AS, set)][
  , ARBrel   := ifelse(abs(totalARB) < .Machine$double.eps, 0, ARB / totalARB)][
  , Nrel     := edPriority / maxN][
  , cumPbias := cumsum(ARBrel), by = c("CLASE_AS", 'set')]


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####            Calculo del Area 'under the cumulative curve'               ####
AUCC.dt <- pbias.dt[
  , .(AUCC = 1 - area(cumPbias, Nrel)), by = c('set', 'CLASE_AS')][
  , label := paste0('AUCC= ', round(AUCC, 2))]

dcast(AUCC.dt, formula = CLASE_AS ~ set, value.var = 'AUCC')

mean(AUCC.dt$AUCC)
sd(AUCC.dt$AUCC)

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                 Gráfico del AUCC                                      ####
ggplot(pbias.dt, aes(x = Nrel, y = cumPbias)) +
  geom_line(color = "gray16") +
  facet_grid(CLASE_AS ~set) +
  geom_text(data = AUCC.dt, mapping = aes(x = 0.78, y = 0.10, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title   = element_text(hjust = 0.5, size = 22, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "green4"),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(title = paste0("Area under the cumulative curve using AUCC function"))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                    Score = Random                                      ####
####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                       CURVAS ROC (random)                              ####


curvaROC2 <- roc(response  = test_rnd.dt$target,
                 predictor =   test_rnd.dt$score_rnd)
AUC1 <- auc(curvaROC2)

ggroc(curvaROC2, size = 1.6, color = "gray22") +
  geom_abline(slope = 1, intercept = 1, linetype = 'dotted', size = 1.1, color = "gray22") +
  annotate("text", x = 0.25, y = 0.05, label = as.character(round(AUC1,2))) +
  labs(title = paste0('ROC curves for CLASE_AS, CNAE_Div_AS and D28'),
       color = 'Variable', linetype = 'Variable') +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 15, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 12, color = "green4", face = "bold"))

tt_rnd.dt  <- rbindlist(list(test_rnd.dt, train_rnd.dt))
tt_rnd.dt  <- tt_rnd.dt[order(CCAA, ESTRATO, SEXOa, EDADa, PROXY_0, FACTORADULTO)]
dat.dt <- dat.dt[order(CCAA, ESTRATO, SEXOa, EDADa, PROXY_0, FACTORADULTO)]
tt_rnd.dt[, CLASE_AS_true := dat.dt$CLASE_AS_true]
tt_rnd.dt <- tt_rnd.dt[order(set,-score_rnd)]

clase_plots <- levels(as.factor(unique(tt_rnd.dt$CLASE_AS_true)))
tt_rnd.dt       <- tt_rnd.dt[CLASE_AS_true %in% clase_plots, ]

workingDT_rnd <- copy(tt_rnd.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_raw == val})]
DT_rnd        <- melt(workingDT_rnd,
                  id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "FACTORADULTO", "score_rnd", "set", "edPriority"),
                  measure.vars = clase_plots, variable.name = 'value_bin',
                  value.name = 'bin')

workingDT_ed_rnd <- copy(tt_rnd.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_true == val})]
DT_ed_rnd        <- melt(workingDT_ed_rnd,
                     id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "FACTORADULTO", "score_rnd", "set", "edPriority"),
                     measure.vars = clase_plots, variable.name = 'value_bin_ed',
                     value.name = 'bin_ed')

DT_rnd[, value_bin_ed := DT_ed_rnd[['value_bin_ed']]][
  , bin_ed := DT_ed_rnd[['bin_ed']]][
    bin == TRUE]

output_train_rnd <- lapply(tt_rnd.dt[set == 'train']$edPriority, function(edPrior){
  
  tempDT <- DT_rnd[set == 'train']
  tempDT[, bin_run := bin][
    edPriority <= edPrior, bin_run := bin_ed]
  localOutput <- tempDT[, list(estim = sum(FACTORADULTO * bin_run, na.rm = TRUE)), by = c('value_bin')]
  setnames(localOutput, 'value_bin', "CLASE_AS")[, edPriority := edPrior]
  return(localOutput)
  
})
output_train_rnd.dt <- rbindlist(output_train_rnd)[
  , set := 'train']


output_test_rnd <- lapply(tt_rnd.dt[set == 'test']$edPriority, function(edPrior){
  
  tempDT <- DT_rnd[set == 'test']
  tempDT[, bin_run := bin][
    edPriority <= edPrior, bin_run := bin_ed]
  localOutput <- tempDT[, list(estim = sum(FACTORADULTO * bin_run, na.rm = TRUE)), by = c('value_bin')]
  setnames(localOutput, 'value_bin', "CLASE_AS")[, edPriority := edPrior]
  return(localOutput)
  
})
output_test_rnd.dt <- rbindlist(output_test)[
  , set := 'test']


output_rnd.dt <- rbindlist(list(output_train_rnd.dt, output_test_rnd.dt))

estim0_rnd.dt <- tt_rnd.dt[, list(estim0 = sum(FACTORADULTO, na.rm = TRUE)), by = c('set', "CLASE_AS_true")]

estim_rnd.dt <- merge(output_rnd.dt, estim0_rnd.dt, by.x = c('set', "CLASE_AS"), by.y = c('set', "CLASE_AS_true"))


pbias_rnd.dt <- estim_rnd.dt[
  , ARB      := abs((estim - estim0) / estim0)][
  , totalARB := sum(ARB), by = c("CLASE_AS", "set")][
  , maxN     := max(edPriority), by = list(CLASE_AS, set)][
  , ARBrel   := ifelse(abs(totalARB) < .Machine$double.eps, 0, ARB / totalARB)][
  , Nrel     := edPriority / maxN][
  , cumPbias := cumsum(ARBrel), by = c("CLASE_AS", 'set')]


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####            Calculo del Area 'under the cumulative curve'               ####
AUCC_rnd.dt <- pbias_rnd.dt[
  , .(AUCC = 1 - area(cumPbias, Nrel)), by = c('set', 'CLASE_AS')][
    , label := paste0('AUCC= ', round(AUCC, 2))]

dcast(AUCC_rnd.dt, formula = CLASE_AS ~ set, value.var = 'AUCC')

mean(AUCC_rnd.dt$AUCC)
sd(AUCC_rnd.dt$AUCC)

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                 Gráfico del AUCC                                      ####
ggplot(pbias_rnd.dt, aes(x = Nrel, y = cumPbias)) +
  geom_line(color = "gray16") +
  facet_grid(CLASE_AS ~set) +
  geom_text(data = AUCC_rnd.dt, mapping = aes(x = 0.78, y = 0.10, label = label)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title   = element_text(hjust = 0.5, size = 22, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "green4"),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(title = paste0("Area under the cumulative curve using AUCC function with random score"))
