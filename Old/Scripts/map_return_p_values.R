map_return_p_values <- 
function (outcomes, predictors, data) 
{
    map_df(predictors, function(pred) {
        map_df(outcomes, function(out) {
            return_pvalues(data = data, outcome = out, predictor = pred)
        })
    })
}
