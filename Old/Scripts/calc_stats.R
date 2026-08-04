calc_stats <- 
function (data, outcomes, predictors = NULL, statistics = c("count", 
    "mean"), conf = NULL, base = NULL, pval = NULL, multicode = TRUE) 
{
    if (class(predictors) == "character") {
        predictors <- list(predictors)
    }
    vector_of_variables_concerned <- c(outcomes, unlist(unique(predictors)))
    test <- vars_exist(data = data, variable_list = vector_of_variables_concerned)
    if (test != TRUE) 
        stop("Some variables you want to use do not exist in the data frame.")
    if (!is.null(predictors)) {
        test <- outcomes_not_in_predictors(outcomes = outcomes, 
            predictors = predictors)
        if (test != TRUE) 
            stop("Outcome appears in predictor list.")
        test <- check_all_factors(data = data, variable_list = predictors)
        if (test != TRUE) 
            stop("Not all predictors are factor variables. Make them factors.")
        test <- list_depth(predictors) == 1
        if (test != TRUE) 
            stop("Predictor list should not contained further lists.")
    }
    suppressMessages({
        suppressWarnings({
            if (any(str_detect(class(data), "survey"))) {
                unweighted_data <- data[["variables"]]
                weighted_data <- data
            }
            else {
                unweighted_data <- data
                weighted_data <- NULL
            }
            individual_tables <- pmap(list(outcomes), function(cur_outcome) {
                single_outcome <- unweighted_data %>% pull(cur_outcome)
                if (is.factor(single_outcome)) {
                  statistic <- str_remove_all(statistics, "mean|median|sum")
                }
                if (is.numeric(single_outcome)) {
                  statistic <- str_remove_all(statistics, "perc|count")
                }
                results <- list()
                if ("mean" %in% statistic) {
                  results <- append(results, list(unweighted_mean(unweighted_data, 
                    outcomes = cur_outcome, predictors = predictors, 
                    conf = conf, base = base)))
                }
                if ("median" %in% statistic) {
                  results <- append(results, list(unweighted_median(unweighted_data, 
                    outcomes = cur_outcome, predictors = predictors, 
                    conf = conf, base = base)))
                }
                if ("sum" %in% statistic) {
                  results <- append(results, list(unweighted_sum(unweighted_data, 
                    outcomes = cur_outcome, predictors = predictors, 
                    conf = conf, base = base)))
                }
                if ("perc" %in% statistic) {
                  results <- append(results, list(unweighted_perc(unweighted_data, 
                    outcomes = cur_outcome, predictors = predictors, 
                    conf = conf, base = base)))
                }
                if ("count" %in% statistic) {
                  results <- append(results, list(unweighted_count(unweighted_data, 
                    outcomes = cur_outcome, predictors = predictors, 
                    conf = conf, base = base)))
                }
                if (!is.null(weighted_data)) {
                  if ("w_mean" %in% statistic) {
                    results <- append(results, list(weighted_mean(weighted_data, 
                      outcomes = cur_outcome, predictors = predictors, 
                      conf = conf, base = base)))
                  }
                  if ("w_median" %in% statistic) {
                    results <- append(results, list(weighted_median(weighted_data, 
                      outcomes = cur_outcome, predictors = predictors, 
                      conf = conf, base = base)))
                  }
                  if ("w_sum" %in% statistic) {
                    results <- append(results, list(weighted_sum(weighted_data, 
                      outcomes = cur_outcome, predictors = predictors, 
                      conf = conf, base = base)))
                  }
                  if ("w_perc" %in% statistic) {
                    results <- append(results, list(weighted_perc(weighted_data, 
                      outcomes = cur_outcome, predictors = predictors, 
                      conf = conf, base = base)))
                  }
                  if ("w_count" %in% statistic) {
                    results <- append(results, list(weighted_count(weighted_data, 
                      outcomes = cur_outcome, predictors = predictors, 
                      conf = conf, base = base)))
                  }
                }
                if (length(results) > 0) {
                  results <- results %>% reduce(bind_rows)
                }
                return(results)
            })
            merged_tables <- individual_tables %>% bind_rows
            if (!is.null(pval)) {
                pvalue_preds <- predictors[predictors %>% map_lgl(., 
                  ~length(.x) == 1)]
                pvalue_preds <- pvalue_preds %>% unlist %>% unique
                pvalues <- list()
                if (any(statistics %in% c("mean", "median", "sum", 
                  "perc", "count"))) {
                  unw_pvalues <- map_return_p_values(data = unweighted_data, 
                    outcomes = outcomes, predictors = pvalue_preds)
                  pvalues <- append(pvalues, list(unw_pvalues))
                }
                if (!is.null(weighted_data)) {
                  if (any(statistics %in% c("w_mean", "w_median", 
                    "w_sum", "w_perc", "w_count"))) {
                    wgt_pvalues <- map_return_p_values(data = data, 
                      outcomes = outcomes, predictors = pvalue_preds)
                    pvalues <- append(pvalues, list(wgt_pvalues))
                  }
                }
                pvalues <- pvalues %>% bind_rows()
                merged_tables <- merged_tables %>% left_join(pvalues)
            }
            else {
                merged_tables <- merged_tables %>% mutate(p_method = NA, 
                  p_value = NA)
            }
            if (is.null(predictors)) {
                merged_tables <- merged_tables %>% mutate(cross_break = "Total")
            }
            if (multicode == T) {
                merged_tables <- convert_multicodes(data = merged_tables, 
                  base_info = base, keep = "Yes")
            }
            return(merged_tables)
        })
    })
}
