unweighted_count <- 
function (data, outcomes, predictors = NULL, conf = NULL, base = NULL) 
{
    suppressMessages({
        suppressWarnings({
            output <- map(outcomes, function(cur_outcome) {
                base_defintion <- ifelse(is.null(base), NA, create_bases(base_info = base, 
                  variables = cur_outcome))
                one_total_table <- data %>% filter(!is.na(.data[[cur_outcome]])) %>% 
                  count(.data[[cur_outcome]], name = "estimate") %>% 
                  mutate(base = sum(estimate, na.rm = T), prop = estimate/base, 
                    estimate_se = sqrt(base * prop * (1 - prop)), 
                    estimate_low = (estimate - 1.96 * estimate_se), 
                    estimate_upp = (estimate + 1.96 * estimate_se), 
                    ) %>% select(-prop) %>% mutate(outcome = cur_outcome) %>% 
                  rename(any_of(c(o_cat = cur_outcome))) %>% 
                  mutate(o_lab = attributes(data[[cur_outcome]])$label) %>% 
                  mutate(base_description = base_defintion)
                if (!is.null(predictors)) {
                  predictor_tables <- map(predictors, function(cur_predictor) {
                    if (length(cur_predictor) > 0) {
                      predictor_and_outcome <- c(cur_predictor, 
                        cur_outcome)
                      base_defintion <- ifelse(is.null(base), 
                        NA, create_bases(base_info = base, variables = predictor_and_outcome))
                      one_predictor_table <- data %>% filter(rowSums(across(all_of(predictor_and_outcome), 
                        ~is.na(.))) == 0) %>% group_by(across(all_of(predictor_and_outcome))) %>% 
                        summarise(estimate = n()) %>% group_by(across(all_of(cur_predictor))) %>% 
                        mutate(base = sum(estimate, na.rm = T), 
                          prop = estimate/sum(base), estimate_se = sqrt(base * 
                            prop * (1 - prop)), estimate_low = (estimate - 
                            1.96 * estimate_se), estimate_upp = (estimate + 
                            1.96 * estimate_se)) %>% select(-prop)
                      one_predictor_table <- one_predictor_table %>% 
                        standardise_names(data = data, preds = cur_predictor, 
                          out_var = cur_outcome) %>% mutate(cross_break = paste0(cur_predictor, 
                        collapse = "_X_")) %>% mutate(base_description = base_defintion)
                    }
                    return(one_predictor_table)
                  })
                  predictor_tables <- bind_rows(predictor_tables)
                }
                if (!is.null(predictors)) {
                  table_for_one_outcome <- bind_rows(one_total_table, 
                    predictor_tables)
                }
                else {
                  table_for_one_outcome <- one_total_table
                }
                table_for_one_outcome <- table_for_one_outcome %>% 
                  mutate(stat = "count", unweighted_n = estimate) %>% 
                  mutate(across(contains(c("cross_break", "predictor", 
                    "p_lab", "p_cat")), ~case_when(is.na(.x) ~ 
                    "Total", TRUE ~ as.character(.x)))) %>% select(contains(c("cross_break", 
                  "predictor", "p_lab", "p_cat")), outcome, o_lab, 
                  o_cat, stat, contains("estimate"), unweighted_n, 
                  base, base_description)
                if (!is.null(conf)) {
                  if (conf == "ci") {
                    table_for_one_outcome <- table_for_one_outcome %>% 
                      mutate(estimate_ci = paste0(as.character(estimate_low), 
                        " - ", as.character(estimate_upp)))
                  }
                }
                else {
                  table_for_one_outcome <- table_for_one_outcome %>% 
                    mutate(estimate_se = "-") %>% select(-estimate_upp, 
                    -estimate_low)
                }
                return(table_for_one_outcome)
            })
            output <- output %>% bind_rows()
            if (!is.null(conf)) {
                if (conf == "se") {
                  keep <- c("upp", "low")
                }
                else {
                  keep <- "_se"
                }
                output <- output %>% select(!ends_with(keep))
            }
            return(output)
        })
    })
}
