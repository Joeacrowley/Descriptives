vars_exist <- 
function (variable_list, data) 
{
    if (any(grepl("survey", class(data)))) {
        df_to_check <- data[["variables"]]
    }
    else {
        df_to_check <- data
    }
    predictors <- variable_list %>% unlist %>% unique
    variables_in_df <- names(df_to_check)
    variables_in_both <- intersect(predictors, variables_in_df)
    length(predictors) == length(variables_in_both)
}
