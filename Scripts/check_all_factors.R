check_all_factors <- 
function (data, variable_list) 
{
    if (any(grepl("survey", class(data)))) {
        df_to_check <- data[["variables"]]
    }
    else {
        df_to_check <- data
    }
    df_to_check %>% select(all_of(unlist(variable_list))) %>% 
        map_chr(., ~paste0(class(.x), collapse = ", ")) %>% unique == 
        "factor"
}
