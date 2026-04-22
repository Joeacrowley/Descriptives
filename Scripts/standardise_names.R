standardise_names <- 
function (data, out_var, preds, table) 
{
    if (any(str_detect(class(data), "survey"))) {
        data_to_look_in <- data[["variables"]]
    }
    else {
        data_to_look_in <- data
    }
    standardised_variable_names <- c(paste0("p_cat", seq_along(preds)), 
        "o_cat")
    renamed_variables <- table %>% rename(any_of(setNames(c(preds, 
        out_var), standardised_variable_names))) %>% mutate(o_lab = attributes(data_to_look_in[[out_var]])$label) %>% 
        mutate(outcome = out_var)
    all_renamed <- map(seq_len(length(preds)), function(number) {
        standardised_predictor_names <- paste0(c("predictor", 
            "p_lab"), number)
        p_label <- attributes(data_to_look_in[[preds[number]]])$label
        if (is.null(p_label)) {
            p_label <- preds[number]
        }
        ctab_plus_new_labels <- renamed_variables %>% mutate(predictor = preds[number], 
            p_lab = p_label) %>% rename(any_of(setNames(c("predictor", 
            "p_lab"), standardised_predictor_names)))
        return(ctab_plus_new_labels)
    }) %>% reduce(., full_join)
    return(all_renamed)
}
