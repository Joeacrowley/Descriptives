base_information <- 
function (data, general_base, specific_bases) 
{
    everything_else <- general_base
    var_descriptions <- specific_bases
    variable_labels <- data %>% select(names(var_descriptions)) %>% 
        var_label(unlist = T, null_action = "fill")
    all_variable_labels <- data %>% var_label(unlist = T, null_action = "fill")
    base_descriptions <- list(everything_else, var_descriptions, 
        variable_labels, all_variable_labels)
    return(base_descriptions)
}
