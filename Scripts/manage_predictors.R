manage_predictors <- 
function (predictors, outcomes) 
{
    if (is.null(predictors)) {
        pred <- rep(list(predictors), length(outcomes))
    }
    else if (is.list(predictors)) {
        ldepth <- list_depth(predictors)
        if (ldepth > 3) {
            pred <- NULL
        }
        else {
            pred <- predictors
        }
    }
    else {
        pred <- list(predictors)
    }
    if (!is.null(predictors)) {
        if (list_depth(pred) == 1) {
            pred <- pred %>% list
        }
        pred_lenth <- length(pred)
        if (pred_lenth != 1 & pred_lenth != length(outcomes)) {
            pred <- pred[1]
        }
        pred_lenth <- length(pred)
        if (pred_lenth == 1) {
            pred <- rep(list(pred), length(outcomes)) %>% flatten
        }
    }
    return(pred)
}
