source(here::here("Scripts_new", "calc_stats.R"))

data(api, package = "survey")
apistrat$meals3 <- factor(dplyr::ntile(apistrat$meals, 3), labels = c("Low", "Mid", "High"))
design <- apistrat %>% srvyr::as_survey_design(ids = 1, weights = pw)

print(weighted_perc(design, outcomes = "sch.wide", predictors = "meals3", conf = "se"))
print(weighted_perc_svyby(design, outcomes = "sch.wide", predictors = "meals3", conf = "se"))
