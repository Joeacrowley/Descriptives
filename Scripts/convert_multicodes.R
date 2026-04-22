convert_multicodes <- 
function (data, base_info = NULL, keep = "Yes") 
{
    if (!is.null(base_info)) {
        base_info_name_as_string <- deparse(substitute(base_info))
    }
    multis <- data %>% filter(stat == "perc" | stat == "count" | 
        stat == "w_count" | stat == "w_perc") %>% filter(grepl(": ", 
        o_lab)) %>% group_by(across(contains(c("cross_break", 
        "p_cat"))), outcome) %>% mutate(lev_num = max(row_number())) %>% 
        group_by(outcome) %>% mutate(lev_relevant = case_when(any(o_cat == 
        "Yes") ~ 1, TRUE ~ 0)) %>% filter(lev_num <= 2 & lev_relevant == 
        1) %>% mutate(left_stem = str_split_i(o_lab, ": ", 1)) %>% 
        group_by(across(contains(c("crossbreak", "p_cat"))), 
            left_stem) %>% mutate(stem_count = n_distinct(outcome), 
        base_count = n_distinct(base)) %>% filter(stem_count > 
        1) %>% filter(base_count == 1)
    if (nrow(multis) > 0) {
        multicode_row_identifiers <- multis %>% unite(ident, 
            contains(c("cross_break", "outcome"))) %>% pull(ident) %>% 
            unique
        multicode_row_identifiers
        single_codes <- data %>% unite(ident, contains(c("cross_break", 
            "outcome")), remove = F) %>% filter(!ident %in% multicode_row_identifiers) %>% 
            select(-ident)
        if ("cross_break" %in% names(multis)) {
            multis <- multis %>% mutate(left_stem2 = paste0(left_stem, 
                " - ", cross_break))
        }
        else {
            multis <- multis %>% mutate(left_stem2 = left_stem)
        }
        unique_stems <- multis$left_stem2 %>% unique
        result <- map(unique_stems, function(stem) {
            multis2 <- multis %>% filter(left_stem2 == stem)
            old_variable_label <- multis2$o_lab[1]
            multis2 <- multis2 %>% ungroup() %>% mutate(right_stem = str_split_i(o_lab, 
                ": ", 2), o_lab = left_stem) %>% filter(o_cat == 
                "Yes") %>% mutate(o_cat = right_stem, outcome = common_prefix(outcome))
            if (!is.null(base_info)) {
                new_variable_name <- multis2$outcome %>% unique()
                new_variable_label <- multis2$o_lab %>% unique
                names(new_variable_label) <- new_variable_name
                existing_base_description <- multis2$base_description[1]
                if (str_detect(existing_base_description, " X ")) {
                  existing_base_description_2 <- str_split(existing_base_description, 
                    " X ") %>% unlist
                }
                else {
                  existing_base_description_2 <- existing_base_description
                }
                index <- str_detect(existing_base_description_2, 
                  old_variable_label)
                reverse_index <- !str_detect(existing_base_description_2, 
                  old_variable_label)
                existing_base_description_3 <- existing_base_description_2[index] %>% 
                  str_split_i(., ":- ", 1)
                revised_base_label <- paste0(existing_base_description_3, 
                  ":- ", new_variable_label)
                names(revised_base_label) <- new_variable_name
                if (str_detect(existing_base_description, " X ")) {
                  remainder_of_base_description <- existing_base_description_2[reverse_index]
                  revised_base_description <- paste0(c(revised_base_label, 
                    remainder_of_base_description), collapse = " X ")
                }
                else {
                  revised_base_description <- revised_base_label
                }
                multis2 <- multis2 %>% mutate(base_description = revised_base_description)
                if (new_variable_name %in% names(base_info[[3]])) {
                  keep_item_2 <- which(!(base_info[[2]] %>% names()) %in% 
                    new_variable_name)
                  keep_item_3 <- which(!(base_info[[3]] %>% names()) %in% 
                    new_variable_name)
                  keep_item_4 <- which(!(base_info[[4]] %>% names()) %in% 
                    new_variable_name)
                  base_info[[2]] <- base_info[[2]][keep_item_2]
                  base_info[[3]] <- base_info[[3]][keep_item_3]
                  base_info[[4]] <- base_info[[4]][keep_item_4]
                }
                return(list(multis2, revised_base_label, new_variable_label))
            }
            else {
                return(list(multis2))
            }
        })
        tables <- bind_rows(map(result, ~(.x[[1]])))
        if (!is.null(base_info)) {
            new_base_descriptors <- map(result, ~(.x[[2]])) %>% 
                unlist
            new_base_descriptors <- new_base_descriptors[!duplicated(new_base_descriptors)]
            new_variable_labels <- map(result, ~(.x[[3]])) %>% 
                unlist
            new_variable_labels <- new_variable_labels[!duplicated(new_variable_labels)]
            example_of_base_info2_temp <- base_info
            base_info[[2]] <- base_info[[2]][which(!base_info[[2]] %in% 
                new_base_descriptors)]
            base_info[[3]] <- base_info[[3]][which(!base_info[[3]] %in% 
                new_variable_labels)]
            base_info[[4]] <- base_info[[4]][which(!base_info[[4]] %in% 
                new_variable_labels)]
            example_of_base_info2_temp[[2]] <- c(base_info[[2]], 
                new_base_descriptors)
            example_of_base_info2_temp[[3]] <- c(base_info[[3]], 
                new_variable_labels)
            example_of_base_info2_temp[[4]] <- c(base_info[[4]], 
                new_variable_labels)
            assign(base_info_name_as_string, example_of_base_info2_temp, 
                envir = .GlobalEnv)
        }
        multis2 <- tables %>% ungroup() %>% select(-lev_num, 
            -lev_relevant, -left_stem, -stem_count, -base_count, 
            -left_stem2, -right_stem)
        multicoded_and_single_code_data <- bind_rows(single_codes, 
            multis2)
        return(multicoded_and_single_code_data)
    }
    else {
        return(data)
    }
}
