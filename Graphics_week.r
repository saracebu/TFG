####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                           SET PATHS                                    ####
path_data <- 'C:/Users/Tester/Desktop/TFG'
#path_src <- 'N:/UDMTD16/code/src'
#path_data <- 'N:/UDMTD16/data'
#path_data <- 'N:/UDMTD/UDMTD16/data'
#path_src  <- 'N:/UDMTD/UDMTD16/code/src'

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  LOAD PACKAGES AND FUNCTIONS                           ####
library(data.table)
#library(pROC)
library(ggplot2)
#library(viridis) #customize ggplots
#library(gridExtra) #customize ggroc
library(latex2exp)
#library(ROCR)
source(file.path(path_data, "area_curva.R"))
source(file.path(path_data, "Calculate_PBIAS.R"))


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  READ RDS                                              ####

data <- readRDS(file = file.path(path_data, 'data.rds'))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  CALCULO NUEVAS VARIABLES                              ####

pbias.dt <- calculate_PBIAS(data)

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####            Calculo del Area 'under the cumulative curve'               ####
AUCC1.dt <- pbias.dt[
  , .(AUCC = area(Nrel, ARBrel)), by = c('week', 'CLASE_AS')][
    , week  := as.integer(as.character(week))][
    , label := paste0('AUCC= ', round(AUCC, 4))]
AUCC2.dt <- pbias.dt[
  , .(AUCC = area(edPriority, ARB)), by = c('week', 'CLASE_AS')][
  , week  := as.integer(as.character(week))][
  , label := paste0('AUCC= ', round(AUCC, 2))]

AUCC3.dt <- pbias.dt[
  , .(AUCC = area(Nrel, cumPbias)), by = c('week', 'CLASE_AS')][
  , week  := as.integer(as.character(week))][
  , label := paste0('AUCC= ', round(AUCC, 2))]


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                         PLOT AUCC TIME SERIES                           ####
clase_plots       <- c("1", "2", "3", "4", "5", "6", "NC")
clase_labs        <- paste0('social class ', clase_plots)
names(clase_labs) <- clase_plots

ggplot(AUCC3.dt, aes(x = week, y = AUCC)) +
  ylim(0,1) +
  geom_line(size = 1.25, color = "gray26") +
  geom_point(size = 1.75, color = "gray18") +
  facet_grid(CLASE_AS ~ .,
             labeller = labeller(CLASE_AS = clase_labs)) +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 16, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 14, color = "green4"),
        axis.text.x  = element_text(angle = 0, size = 6),
        axis.text.y  = element_text(size = 8),
        strip.text.y = element_text(size = 7, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(x = paste0("Week"),
       y = 'Area under the cumulative curve',
       title = paste0("Area under the ARB Cumulative Curve per CLASE value and week\n"))


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                PLOT PSEUDOSESGO RELATIVO VS PRIORITY                   ####
week_plots        <- 1:6
week_labs         <- paste0('week ', week_plots)
names(week_labs)  <- week_plots

ggplot(pbias.dt[week %in% week_plots], aes(x = edPriority, y = ARB)) +
  geom_point(size=0.8, color = "gray12") +
  geom_line(color = "gray16") +
  geom_text(AUCC2.dt[week %in% week_plots], mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust   = -0.6, vjust   = -5.5) +
  facet_grid(week ~ CLASE_AS, scales = 'free',
             labeller = labeller(week = week_labs, CLASE_AS = clase_labs)) +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 22, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "green4"),
        axis.text.x  = element_text(angle = 90, size = 8),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(x = paste0("Editing Priority"),
       y = TeX("$|\\hat{Y}^{ed} - \\hat{Y}^{(0)}| / \\hat{Y}^{(0)}$"),
       title = paste0("Relative PseudoBias per CLASE value and week\n"))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####            Plot fraction of ARB vs fraction of Edited Units            ####
ggplot(pbias.dt[week %in% week_plots], aes(x = Nrel, y = ARBrel)) +
  geom_point(size=0.8, color = "gray12") +
  geom_line(color = "gray16") +
  geom_text(AUCC1.dt[week %in% week_plots], mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust   = -0.6, vjust   = -5.5) +
  facet_grid(week ~ CLASE_AS, scales = 'free',
             labeller = labeller(week = week_labs, CLASE_AS = clase_labs)) +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 22, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "green4"),
        axis.text.x  = element_text(angle = 90, size = 8),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(x = paste0("Proportion of Editing Priority"),
       y = TeX("$B_j = b_j / \\sum_{i=1}^N b_i$ where $b_i=|\\hat{Y}_i^{ed} - \\hat{Y}^{(0)}| / \\hat{Y}^{(0)}$"),
       title = paste0("Relative PseudoBias per CLASE value and week\n"))

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####        Plot fraction of CUMULATIVE ARB vs fraction of Edited Units     ####

ggplot(pbias.dt[week %in% week_plots], aes(x = Nrel, y = cumPbias)) +
  geom_point(size=0.8, color = "gray12") +
  geom_line(color = "gray16") +
  geom_text(AUCC3.dt[week %in% week_plots], mapping = aes(x = -Inf, y = -Inf, label = label),
            hjust   = -1, vjust   = -2.5) +
  facet_grid(week ~ CLASE_AS, scales = 'free',
             labeller = labeller(week = week_labs, CLASE_AS = clase_labs)) +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 22, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 16, color = "green4"),
        axis.text.x  = element_text(angle = 90, size = 8),
        axis.text.y  = element_text(size = 8),
        strip.text.x = element_text(size = 8, face = "bold"),
        strip.text.y = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "chartreuse3")) +
  labs(x = paste0("Proportion of Editing Priority"),
       y = TeX("$A_l=\\sum_{j=1}^l B_j$"),
       title = paste0("Accumulative Relative PseudoBias per CLASE value and week\n"))




