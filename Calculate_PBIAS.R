#' @title  Calculate the relative pseudo bias
#'
#' @description A large list is transformed into a data table. With these 
#' variables, we are going to build the edited database. We will
#' eliminate the NA values of CLASE_AS_true, since they have no importance 
#' in the study. We will calculate the variables "estim" and "estim0". "estim0" 
#' is the sum of the "PesoDisAdulto" respect to the variable CLASE_AS_true and
#' "Week". To "estim" we focus on the weeks separately, and we order the values by 
#' "Priority_CNOAS_FT". Next for each possible value of "CLASE_AS", we are going
#' to add the first "PesoDisAdulto" whose "CLASE_AS" corresponds to 
#' "CLASE_AS_true" up to the one that matches its "Prioridad_CNOAS_FT", to add 
#' it to the value of "PesoDisAdulto" other whose "CLASE_AS" corresponds 
#' with "CLASE_AS_raw". With the values of "estim" and "estim0" we calculate 
#' "ARB", as the relative error of "estim" and "estim0", taking "estim0" as the
#' exact value and "estim" as the approximate value. We will calculate the 
#' variable "totalARB" as the sum of all "ARB" values, according to "CLASE_AS_true"
#' and "Week". The variable "maxN", which will return the highest value of 
#' "Priority_CNOAS_FT" according to "Week" and "CLASE_AS_true". "ARBrel" is the 
#' relative frequency of "ARB", but, when "ARB" is 0 then "ARBrel" is 0. "Nrel" 
#' is the ratio between "Priority_CNOAS_FT" and  "maxN". "cumPbias" is the 
#' accumulated sum of "ARBrel"according to "CLASE_AS_true" and "Week"
#'
#' @param data Large list with elements which represent weeks.In each week we
#' must have a data table with the columns "CODSEC", "NVIV", "HOGAR", "NORDEN",
#' "Prioridad_CNOAS_FT", "CLASE_AS_raw", "PesoDisAdluto", "CLASE_AS_true". 
#' 
#' 
#' @return Data table with the variables "Week", "CLASE_AS" (which will have 
#' the values from "CLASE_AS_true"), "estim", "estim0", "edPriority" (which 
#' will have the values from "Priority_CNOAS_FT"), "totalARB", "maxN", 
#' "ARBrel", "Nrel" and "cumPbias".
#' 


calculate_PBIAS <- function(data){
  
  num_weeks <- length(data)
  newpbias  <- data.table("CLASE_AS" = NULL, "estim" = NULL, "edPriority" = NULL, "estim0" = NULL, "ARB" = NULL, "week" = NULL)
  ####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
  ####                  CALCULO PSEUDOSESGO RELATIVO                        ####


  for (wk in 1:36){
  
    print(wk)
    lote.dt <- data[[wk]]
    clase_plots <- levels(as.factor(unique(lote.dt$CLASE_AS_true)))
    id.vars <- c("CODSEC", "NVIV", "HOGAR", "NORDEN")
    lote.dt <- lote.dt[lote.dt$CLASE_AS_true %in% clase_plots, ]
 
    lote.dt[, estim0 := sum(PesoDisAdulto, na.rm = TRUE), by = "CLASE_AS_true"]
  
    ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
    ####                       CALCULAMOS ESTIM0                         #### 
  
    workingDT <- copy(lote.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_raw == val})]
  
    DT <- melt(workingDT, 
               id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "PesoDisAdulto", "Prioridad_CNOAS_FT"), 
               measure.vars = clase_plots, variable.name = 'value_bin',
               value.name = 'bin')
    workingDT_ed <- copy(lote.dt)[, (clase_plots) := lapply(clase_plots, function(val){CLASE_AS_true == val})]
    DT_ed <- melt(workingDT_ed, 
                  id.vars = c(id.vars, "CLASE_AS_raw", "CLASE_AS_true", "PesoDisAdulto", "Prioridad_CNOAS_FT"), 
                  measure.vars = clase_plots, variable.name = 'value_bin_ed',
                  value.name = 'bin_ed')
  
    DT[, value_bin_ed := DT_ed[['value_bin_ed']]][
      , bin_ed := DT_ed[['bin_ed']]][
        bin == TRUE]
  
    output <- lapply(lote.dt$Prioridad_CNOAS_FT, function(edPrior){
    
      DT[, bin_run := bin][
        Prioridad_CNOAS_FT <= edPrior, bin_run := bin_ed]
      localOutput <- DT[, list(estim = sum(PesoDisAdulto * bin_run, na.rm = TRUE)), by = c('value_bin')]
      setnames(localOutput, 'value_bin', "CLASE_AS")[, edPriority := edPrior]
      return(localOutput)
    
    })
    output <- rbindlist(output) 
  
    estim0.dt <- lote.dt[, list(estim0 = sum(PesoDisAdulto, na.rm = TRUE)), by = "CLASE_AS_true"]
  
    estim.dt <- merge(output, estim0.dt, by.x = "CLASE_AS", by.y = "CLASE_AS_true")
    estim.dt[, week := wk]
    
    newpbias <- rbind(newpbias, estim.dt)
  }

  newpbias <- newpbias[order(week, CLASE_AS)]
  
  newpbias[, ARB := abs((estim - estim0) / estim0)]
  newpbias[, totalARB := sum(ARB), by = list(CLASE_AS, week)]
  newpbias[, maxN     := max(edPriority), by = list(CLASE_AS, week)]
  newpbias[, ARBrel   := ifelse(abs(totalARB) < .Machine$double.eps, 0, ARB / totalARB)]
  newpbias[, Nrel     := edPriority / maxN]
  newpbias <- newpbias[order(Nrel)]
  newpbias[, cumPbias := cumsum(ARBrel), by = list(CLASE_AS, week)]
  
  return(newpbias)
}

