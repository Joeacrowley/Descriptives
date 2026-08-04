# Load the voting.dta Stata dataset and assign it to the object mydata
library(foreign)
library(tidyverse)
df <- read.dta("Y:/P18592/voting.dta")
head(df)

# I will use Bush voter as stand in for NEET
df %>% count(bush)

# Data suppression rules

#  -  anything with a base size less than 10.
#  -  anything with a cell size less than 3.
#  -  anything where cell size is less than 3 and value can be inferred from other data present.

# For the final point above, we must suppress one other cell where any other 'single' cell is suppressed. 

# Number and percentage of people in each level of each variable

overview <- function(
    data, # data frame
    base_suppression_threshold = 10, 
    cell_suppression_threshold = 3
    ) { 
  
  output <-  map(names(data), ~data %>% count(.data[[.x]]) %>% 
        mutate(p = n / sum(n), 
               base = sum(n)) %>% 
        rename(levels = .x) %>% 
        mutate(var = .x, 
               levels = as.character(levels)) %>% 
        relocate(var) %>% 
        
        # Suppress cells
        mutate(base_suppress = case_when(base < base_suppression_threshold ~ 1, TRUE ~ 0),
               cell_suppress = case_when(n < cell_suppression_threshold ~ 1, TRUE ~ 0),
               n_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ n), 
               p_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ p), 
               base_sprs = case_when(base_suppress == 1 ~ NA, TRUE ~ base)) %>% 
        
        # Code to identify cases where we have suppressed one cell only and need to suppress another to avoid disclosure. 
        mutate(
          
          # Concern is when we have 1 suppressed cell in a group, need to suppress 1 more. 
          mis_vals = sum(is.na(n_sprs)), 
          suppress_id = case_when(mis_vals == 1 ~ 1, TRUE ~ 0), 
          
          # Identify the non-missing values in that group we might suppress 
          # Must be a character or sample will behave oddly
          # e.g. if you sample from 9 it will automatically sample from a vector of 1:9
          choices = list(as.character(unique(na.omit(n_sprs)))), 
          
          # Select one non missing value to suppress
          choice = case_when(suppress_id == 1 ~ sample(choices[[1]], 1)), 
          
          # Suppress that value
          row_to_suppress = case_when(suppress_id == 1 & n_sprs == as.numeric(choice) ~ 1, TRUE ~ 0),
          n_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ n_sprs), 
          p_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ p_sprs), 
          any_suppress = case_when(row_to_suppress == 1 ~ 3, 
                                   base_suppress == 1 ~ 2,
                                   cell_suppress == 1 ~ 1,
                                   TRUE ~ 0)
        ) %>% 
        select(-suppress_id,-row_to_suppress,-choices, -choice, -mis_vals, -suppress_id)
  )
  
  return(output)
  
  }

overview_tbl <- 
  df %>% 
  select(bush, income, region, statename) %>% 
  overview(cell_suppression_threshold = 100) %>% 
  bind_rows()

overview_tbl

# Number and percentage NEET (Here, bush voter) in each variable
# This will also work for topic two - risk factor prevalence by other variables.
vars <- c("region", "income","statename")
crossbreak <- function(data,
                       base_suppression_threshold = 10, 
                       cell_suppression_threshold = 3,
                       breaks, 
                       outvar
                       ) {
  

  results <-
    map(breaks, function(xxx){
    
    interim_tbl <- 
      data %>% count(.data[[xxx]], .data[[outvar]]) %>% 
        group_by(.data[[xxx]]) %>% 
        mutate(p = n / sum(n), base = sum(n)) %>% 
        rename(levels = xxx) %>% 
        mutate(var = xxx, 
               levels = as.character(levels)) %>% 
        relocate(var) %>% 
        
        mutate(base_suppress = case_when(base < base_suppression_threshold ~ 1, TRUE ~ 0),
               cell_suppress = case_when(n < cell_suppression_threshold ~ 1, TRUE ~ 0),
               n_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ n), 
               p_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ p), 
               base_sprs = case_when(base_suppress == 1 ~ NA, TRUE ~ base)) %>%
        
        group_by(levels) %>%

        # Cell suppress..
        mutate(

          # Concern is when we have 1 suppressed cell in a group, need to suppress 1 more.
          mis_vals = sum(is.na(n_sprs)),
          suppress_id = case_when(mis_vals == 1 ~ 1, TRUE ~ 0),

          # Identify the non-missing values in that group we might suppress
          # Must be a character or sample will behave oddly
          # e.g. if you sample from 9 it will automatically sample from a vector of 1:9
          choices = list(as.character(unique(na.omit(n_sprs)))), 
          choices = case_when(all(is.na(n_sprs)) ~ list("missing"), TRUE ~ choices),

          # Select one non missing value to suppress
          choice = case_when(
            suppress_id == 1 ~ sample(choices[[1]], 1)),
          
        # Suppress that value
          row_to_suppress = case_when(suppress_id == 1 & n_sprs == as.numeric(choice) ~ 1, TRUE ~ 0),
          n_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ n_sprs),
          p_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ p_sprs)
        ) %>% ungroup
    
    levels_to_use <-
      interim_tbl %>%
      group_by(levels) %>%
      summarise(mis_vals = sum(is.na(base_sprs)))     %>%
      mutate(mis_vals = case_when(mis_vals > 1 ~ 1, TRUE ~ 0)) %>%
      filter(mis_vals == 1) %>%
      select(levels) %>% unlist() %>% length

    if(levels_to_use == 1){

    interim_tbl <- interim_tbl %>%
        # Base suppress...
        mutate(

          # Concern is when we have 1 suppressed cell in a group, need to suppress 1 more.
          suppress_id = 1,

          # Identify the non-missing values in that group we might suppress
          # Must be a character or sample will behave oddly
          # e.g. if you sample from 9 it will automatically sample from a vector of 1:9
          choices = list(as.character(unique(na.omit(base_sprs)))),

          # Select one non missing value to suppress
          choice = case_when(suppress_id == 1 ~ sample(choices[[1]], 1)),

          # Suppress that value
          row_to_suppress = case_when(suppress_id == 1 & base_sprs == as.numeric(choice) ~ 1, TRUE ~ 0),
          base_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ base_sprs),
          n_sprs = case_when(is.na(base_sprs) ~ NA, TRUE ~ n_sprs),
          p_sprs = case_when(is.na(base_sprs) ~ NA, TRUE ~ p_sprs)
        )
    }

    interim_tbl <- interim_tbl %>%
      mutate(any_suppress = case_when(row_to_suppress == 1 ~ 3,
                               base_suppress == 1 ~ 2,
                               cell_suppress == 1 ~ 1,
                               TRUE ~ 0)) %>%
      select(-suppress_id,-row_to_suppress,-choices, -choice, -mis_vals, -suppress_id) %>%
      ungroup()

  return(interim_tbl)
  
  })  %>%
    bind_rows() %>%
    print(n=1000)
  
  return(results)
  
}

df %>% crossbreak(breaks = vars, outvar = "bush", 
                  cell_suppression_threshold = 50, 
                  base_suppression_threshold = 90) %>%
  bind_rows() %>%
  print(n=1000)



# nested crossbreaks

ncrossbreak <- function(data, 
                        breaks, 
                        outvar, 
                        nestvars,
                        cell_suppression_threshold = 3, 
                        base_suppression_threshold = 10
                        ) {
  
  results <- map(nestvars, function(nvar) { 
    
    interim_results <- 
      map(breaks, function(xxx){  
        interim_tbl <- 
          data %>% count(.data[[nvar]], .data[[xxx]], .data[[outvar]]) %>% 
          group_by(.data[[nvar]], .data[[xxx]]) %>% 
          mutate(p = n / sum(n), base = sum(n)) %>% 
          rename(cbreak_levels = xxx, 
                 nest_levels = nvar) %>% 
          mutate(cbreak_var = xxx,
                 nest_var = nvar, 
                 cbreak_levels = as.character(cbreak_levels), 
                 nest_levels = as.character(nest_levels)) %>% 
          relocate(nest_var, nest_levels, cbreak_var, cbreak_levels) %>% 
          mutate(base_suppress = case_when(base < base_suppression_threshold ~ 1, TRUE ~ 0),
                 cell_suppress = case_when(n < cell_suppression_threshold ~ 1, TRUE ~ 0),
                 n_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ n), 
                 p_sprs = case_when(base_suppress == 1 | cell_suppress == 1 ~ NA, TRUE ~ p), 
                 base_sprs = case_when(base_suppress == 1 ~ NA, TRUE ~ base)) %>%
        
        group_by(nest_levels, cbreak_levels) %>%
        
        # Cell suppress..
        mutate(
          
          # Concern is when we have 1 suppressed cell in a group, need to suppress 1 more.
          mis_vals = sum(is.na(n_sprs)),
          suppress_id = case_when(mis_vals == 1 ~ 1, TRUE ~ 0),
          
          # Identify the non-missing values in that group we might suppress
          # Must be a character or sample will behave oddly
          # e.g. if you sample from 9 it will automatically sample from a vector of 1:9
          choices = list(as.character(unique(na.omit(n_sprs)))), 
          choices = case_when(all(is.na(n_sprs)) ~ list("missing"), TRUE ~ choices),
          
          # Select one non missing value to suppress
          choice = case_when(
            suppress_id == 1 ~ sample(choices[[1]], 1)),
          
          # Suppress that value
          row_to_suppress = case_when(suppress_id == 1 & n_sprs == as.numeric(choice) ~ 1, TRUE ~ 0),
          n_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ n_sprs),
          p_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ p_sprs)
        ) %>% ungroup
        
        levels_to_use <-
          interim_tbl %>%
          group_by(nest_levels, cbreak_levels) %>%
          summarise(mis_vals = sum(is.na(base_sprs)))     %>%
          mutate(mis_vals = case_when(mis_vals > 1 ~ 1, TRUE ~ 0)) %>%
          filter(mis_vals == 1) %>% nrow
        
        if(levels_to_use == 1){
          
          interim_tbl <- interim_tbl %>%
            # Base suppress...
            mutate(
              
              # Concern is when we have 1 suppressed cell in a group, need to suppress 1 more.
              suppress_id = 1,
              
              # Identify the non-missing values in that group we might suppress
              # Must be a character or sample will behave oddly
              # e.g. if you sample from 9 it will automatically sample from a vector of 1:9
              choices = list(as.character(unique(na.omit(base_sprs)))),
              
              # Select one non missing value to suppress
              choice = case_when(suppress_id == 1 ~ sample(choices[[1]], 1)),
              
              # Suppress that value
              row_to_suppress = case_when(suppress_id == 1 & base_sprs == as.numeric(choice) ~ 1, TRUE ~ 0),
              base_sprs = case_when(row_to_suppress == 1 ~ NA, TRUE ~ base_sprs),
              n_sprs = case_when(is.na(base_sprs) ~ NA, TRUE ~ n_sprs),
              p_sprs = case_when(is.na(base_sprs) ~ NA, TRUE ~ p_sprs)
            )
        }
        
        interim_tbl <- interim_tbl %>%
          mutate(any_suppress = case_when(row_to_suppress == 1 ~ 3,
                                          base_suppress == 1 ~ 2,
                                          cell_suppress == 1 ~ 1,
                                          TRUE ~ 0)) %>%
          select(-suppress_id,-row_to_suppress,-choices, -choice, -mis_vals, -suppress_id) %>%
          ungroup()
        
        return(interim_tbl)
      
  
        })
    
  })
  
  return(results)

}  
vars <- c("region", "income","statename")

df %>% ncrossbreak(breaks = "region", outvar = "bush", nestvars = "income", cell_suppression_threshold = 150, base_suppression_threshold = 30) %>%
  bind_rows() %>%
  print(n=1000)


df %>% ncrossbreak(breaks = "income", outvar = "bush", nestvars = c("statename", "region")) %>%
  bind_rows() %>%
  print(n=1000)
