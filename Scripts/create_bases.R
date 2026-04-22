create_bases <- 
function (base_info, variables) 
{
    names_of_variables_with_specific_bases <- base_info[[2]] %>% 
        names
    bases_to_use <- names_of_variables_with_specific_bases %in% 
        variables
    variable_labels_to_use <- base_info[[3]][bases_to_use]
    bases_to_use <- base_info[[2]][bases_to_use]
    if (length(bases_to_use) > 0) {
        list_of_bases <- paste0(bases_to_use, ":-  ", variable_labels_to_use)
    }
    else {
        list_of_bases <- c()
    }
    variables_using_default_description <- variables[!variables %in% 
        names_of_variables_with_specific_bases]
    if (length(variables_using_default_description) > 0) {
        labels_of_variables_using_default_description <- base_info[[4]][variables_using_default_description]
        default_base_description <- paste0(base_info[[1]], ":-  ", 
            labels_of_variables_using_default_description)
        list_of_bases <- c(list_of_bases, default_base_description)
    }
    list_of_bases <- list_of_bases %>% paste0(., collapse = " X ")
    return(list_of_bases)
}
