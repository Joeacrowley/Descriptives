weighted_test_numeric_by_cat <- 
function (data, outcome, predictor) 
{
    suppressMessages({
        x_lab <- data[["variables"]] %>% select(all_of(predictor)) %>% 
            var_label(unlist = TRUE, null_action = "fill")
        y_lab <- data[["variables"]] %>% select(all_of(outcome)) %>% 
            var_label(unlist = TRUE, null_action = "fill")
        filtered_data <- data %>% filter(if_all(c(outcome, predictor), 
            ~!is.na(.x)))
        frmla <- paste0(outcome, " ~ ", predictor)
        result <- tryCatch({
            model <- survey::svyglm(frmla, design = filtered_data)
            shapiro_p <- shapiro.test(residuals(model))$p.value
            if (shapiro_p < 0.05) {
                method_used <- "Kruskal–Wallis"
                p_val <- survey::svyranktest(frmla, design = filtered_data)$p.value
            }
            else {
                method_used <- "Wald Test"
                anova_result <- survey::regTermTest(model, predictor)
                p_val <- anova_result[["p"]] %>% as.vector()
            }
            list(method = method_used, p = p_val)
        }, error = function(e) {
            list(method = "Significance test returned an error.", 
                p = NA)
        })
        result <- tibble(cross_break = predictor, predictor1 = predictor, 
            p_lab1 = x_lab, outcome = outcome, o_lab = y_lab, 
            p_method = result$method, p_value = result$p)
        result <- map_df(c("w_mean", "w_median", "w_sum"), ~result %>% 
            mutate(stat = .x))
        return(result)
    })
}
