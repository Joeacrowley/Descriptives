return_pvalues <- 
function (data, outcome, predictor) 
{
    if (any(str_detect(class(data), "survey"))) {
        data_2 <- data[["variables"]]
        weights <- "weights"
    }
    else {
        data_2 <- data
        weights <- "no weights"
    }
    outcome_class_factor <- class(data_2 %>% pull(outcome)) %>% 
        str_detect(., "factor") %>% any()
    if (weights == "no weights") {
        if (outcome_class_factor == TRUE) {
            result <- unweighted_test_cat_by_cat(data = data, 
                outcome = outcome, predictor = predictor)
        }
        else {
            result <- unweighted_test_numeric_by_cat(data = data, 
                outcome = outcome, predictor = predictor)
        }
    }
    else {
        if (outcome_class_factor == TRUE) {
            result <- weighted_test_cat_by_cat(data = data, outcome = outcome, 
                predictor = predictor)
        }
        else {
            result <- weighted_test_numeric_by_cat(data = data, 
                outcome = outcome, predictor = predictor)
        }
    }
    return(result)
}
