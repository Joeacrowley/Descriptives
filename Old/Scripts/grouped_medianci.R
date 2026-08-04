grouped_medianci <- 
function (data, outcome, predictors) 
{
    data_with_required_predictors <- data %>% select(all_of(predictors))
    predictors_as_list <- map(names(data_with_required_predictors), 
        ~data %>% pull(.x) %>% fct_drop)
    split_data <- split(data, predictors_as_list, drop = TRUE)
    results <- map(1:length(split_data), function(number) {
        outcome_data <- split_data[[number]] %>% pull(outcome)
        if (length(outcome_data) > 5) {
            cil <- wrappedtools::median_cl_boot(outcome_data)
            levels_in_use <- split_data[[number]] %>% select(all_of(predictors)) %>% 
                unique() %>% mutate(estimate_low = cil$CIlow, 
                estimate_upp = cil$CIhigh)
            return(levels_in_use)
        }
    }) %>% bind_rows()
    return(results)
}
