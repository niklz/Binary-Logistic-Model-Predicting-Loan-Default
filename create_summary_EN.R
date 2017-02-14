# ----------------------------------------------------------------
# Function generates data summary
# INPUT
# df -- data frame with the data
# OUTPUT
# data_summary_All -- table summarising the explanatory variables:
#    Var -- variable's name
#    Class -- variable's class
#    NumLev -- numbers of variable's observed values
#    NumNA -- number of NA's occurencies
#    ProcNA -- fraction of NA's occurencies
#    ProcNum -- fraction of the mode's occurencies 
#    GINI -- variable's absolute gini (after NA's deletions)
#    GINI_S -- variable's gini (after NA's deletions)
#    AFTER DATA PROCESSING:
#    NumLevCat -- numbers of categorized variable's observed values
#    NumLevCont -- numbers of regularized variable's observed values
#    GINI_CAT -- variable's absolute gini (after categorization)
#    GINI_CAT_S -- variable's gini (after categorization)
#    GINI_CONT --  variable's absolute gini (after regularization)
#    GINI_CONT_S -- variable's gini (after regularization)            
# --------------------------------------------------------------------
create_summary <- function(df, target_variable){
    # Preliminary summary
    data_summary <- makeDataSummary(df)
    
    # Variables for which we calculate gini
    include_var_gini = names(df)[data_summary$Class %in% 
                                     c("numeric", "integer")]
    # Exclude certain variables
    include_var_gini = setdiff(include_var_gini, exclude_var)
    
    # Init gini column 
    data_summary$GINI_S = NA
    y = df[ , target_variable]
    
    # Loop over chosen variables
    if (length(include_var_gini) > 0){
        for (v in 1:length(include_var_gini)){
            x = df[, include_var_gini[v]]
            name_v = include_var_gini[v]
            # Delete NA's
            xx = x[!is.na(x)]
            yy = y[!is.na(x)]
            if(length(xx) > 0 & length(unique(yy)) > 1 & 
               length(unique(xx)) > 1){
                gini_signed = round(somersd(predit = xx, target = yy), 3)
                data_summary[data_summary$Var == name_v, "GINI_S"] = gini_signed
                data_summary[data_summary$Var == name_v, "GINI_S"] = gini_signed
                # auc_i = colAUC(xx, yy, plotROC=FALSE, alg="ROC")
                # gini_coef = round(2 * as.numeric(auc_i) - 1, 3)
                # data_summary[data_summary$Var == name_v, "GINI_S"] = gini_coef
                
            } else {
                warning(paste("After NA's deletion it is impossible to 
                              calculate gini for variable: ",
                              name_v))
            }
        }
    }
    
    return(data_summary)
}




# ---------------------------------------------------------------------
# Function generates a preliminary data summary
# INPUT
# df -- data frame
# predVarName -- "y" variable name
# ALL -- flag: obsolete!
# OUTPUT
# data_summary -- summary table:
#   Var -- variable's name
#   Class -- variable's class
#   NumLevCat -- numbers of categorized variable's observed values
#   NumLevCont -- numbers of regularized variable's observed values
#   ProcNA -- fraction of NA
#   ProcNum -- fraction of the most frequent level
# ---------------------------------------------------------------------
makeDataSummary <- function(df, predVarName = NULL, ALL = TRUE){
    
    data_summary <- as.data.frame(names(df), stringsAsFactors=FALSE)
    names(data_summary) = "Var"
    data_summary$Class = lapply(df, function(x) class(x)[1]) %>% unlist %>% unname
    
    if(ALL){
        data_summary$NumLev = lapply(df, function(x) length(unique(x))) %>% unlist %>% unname
        data_summary$NumNA = lapply(df, function(x) sum(is.na(x))) %>% unlist %>% unname
        data_summary$ProcNA = round(data_summary$NumNA / nrow(df), 4)
        NumMode = lapply(df, function(x) mode(x)) %>% unlist %>% unname
        data_summary$ProcNum = round(NumMode / nrow(df), 2)
    }
    
    return(data_summary)   
}


# ---------------------------------------------------------------
# Number of occurencies of the most frequent level of vector x
# INPUT
# x -- a vector
# ---------------------------------------------------------------
mode <- function(x) {
    ux <- unique(x)
    # ux[which.max(tabulate(match(x, ux)))]
    return(max(tabulate(match(x, ux))))
}

