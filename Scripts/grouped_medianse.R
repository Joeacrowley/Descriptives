grouped_medianse <- 
function (data, outcome, predictors) 
{
    data_with_required_predictors <- data %>% select(all_of(predictors))
    predictors_as_list <- map(names(data_with_required_predictors), 
        ~data %>% pull(.x) %>% fct_drop)
    split_data <- split(data, predictors_as_list, drop = TRUE)
    results <- map(1:length(split_data), function(number) {
        outcome_data <- split_data[[number]] %>% pull(outcome)
        median <- wrappedtools::medianse(outcome_data)
        levels_in_use <- split_data[[number]] %>% select(all_of(predictors)) %>% 
            unique() %>% mutate(estimate_se = median)
    }) %>% bind_rows()
    return(results)
}
