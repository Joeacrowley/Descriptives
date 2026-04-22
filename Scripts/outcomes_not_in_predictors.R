outcomes_not_in_predictors <- 
function (outcomes, predictors) 
{
    predictor_vector <- unlist(predictors) %>% unique
    overlap_of_predictors_and_outcomes <- intersect(outcomes, 
        predictor_vector)
    length(overlap_of_predictors_and_outcomes) == 0
}
