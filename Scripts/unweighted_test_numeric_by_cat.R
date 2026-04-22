unweighted_test_numeric_by_cat <- 
function (data, outcome, predictor) 
{
    x <- data %>% pull(predictor)
    y <- data %>% pull(outcome)
    x_lab <- data %>% select(all_of(predictor)) %>% var_label(unlist = TRUE, 
        null_action = "fill")
    y_lab <- data %>% select(all_of(outcome)) %>% var_label(unlist = TRUE, 
        null_action = "fill")
    complete_idx <- complete.cases(y, x)
    y <- y[complete_idx]
    x <- x[complete_idx]
    result <- tryCatch({
        model <- lm(y ~ x)
        shapiro_p <- shapiro.test(residuals(model))$p.value
        if (shapiro_p < 0.05) {
            method_used <- "Kruskal–Wallis"
            p_val <- kruskal.test(y ~ x)$p.value
        }
        else {
            method_used <- "Welch's ANOVA"
            p_val <- oneway.test(y ~ x, var.equal = FALSE)$p.value
        }
        list(method = method_used, p = p_val)
    }, error = function(e) {
        list(method = "Significance test returned an error.", 
            p = NA)
    })
    result <- tibble(cross_break = predictor, predictor1 = predictor, 
        p_lab1 = x_lab, outcome = outcome, o_lab = y_lab, p_method = result$method, 
        p_value = result$p)
    result <- map_df(c("mean", "median", "sum"), ~result %>% 
        mutate(stat = .x))
    return(result)
}
