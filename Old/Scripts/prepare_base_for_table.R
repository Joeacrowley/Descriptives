prepare_base_for_table <- 
function (bases_for_table) 
{
    base_results <- list()
    bases <- bases_for_table %>% str_split_1(., " X ") %>% unique
    base_descriptions <- bases %>% str_split_i(., ":-  ", 1)
    variables <- bases %>% str_split_i(., ":-  ", 2)
    unique_base_descriptions <- unique(base_descriptions)
    if (length(unique_base_descriptions) == 1) {
        base_results <- append(base_results, list(unique_base_descriptions))
    }
    else {
        most_common_base_description <- data.frame(base_descriptions = base_descriptions) %>% 
            count(base_descriptions) %>% mutate(max_n = max(n)) %>% 
            filter(n == max_n) %>% pull(base_descriptions) %>% 
            unique
        if (length(most_common_base_description) > 1) {
            most_common_base_description <- most_common_base_description[1]
        }
        base_results[[1]] <- most_common_base_description %>% 
            paste("All other variables:- ", ., collapse = "") %>% 
            str_to_sentence()
        unique_base_descriptions <- unique_base_descriptions[!unique_base_descriptions %in% 
            most_common_base_description]
        for (xxx in unique_base_descriptions) {
            index <- which(base_descriptions %in% xxx)
            variable_list <- paste0(variables[index], collapse = ", ")
            variable_description <- paste0(xxx, ":- ", variable_list)
            base_results <- append(base_results, variable_description)
        }
    }
    base_results <- rev(base_results)
    return(base_results)
}
