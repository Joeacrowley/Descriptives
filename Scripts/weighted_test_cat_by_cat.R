weighted_test_cat_by_cat <- 
function (data, outcome, predictor) 
{
    x_lab <- data[["variables"]] %>% select(all_of(predictor)) %>% 
        var_label(unlist = TRUE, null_action = "fill")
    y_lab <- data[["variables"]] %>% select(all_of(outcome)) %>% 
        var_label(unlist = TRUE, null_action = "fill")
    filtered_data <- data %>% filter(if_all(c(outcome, predictor), 
        ~!is.na(.x)))
    frmla <- formula(paste0("~", outcome, " + ", predictor))
    result <- tryCatch({
        method_used <- "Chi-Square test"
        p_val <- svychisq(frmla, design = filtered_data, statistic = "adjWald")$p.value
        p_val <- p_val %>% as.vector()
        list(method = method_used, p = p_val)
    }, error = function(e) {
        list(method = "Significance test returned an error.", 
            p = NA)
    })
    result <- tibble(cross_break = predictor, predictor1 = predictor, 
        p_lab1 = x_lab, outcome = outcome, o_lab = y_lab, p_method = result$method, 
        p_value = result$p)
    result <- map_df(c("w_perc", "w_count"), ~result %>% mutate(stat = .x))
    return(result)
}
