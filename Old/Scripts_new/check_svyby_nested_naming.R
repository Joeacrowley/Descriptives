source(here::here("Scripts_new", "calc_stats.R"))

data(api, package = "survey")
apistrat$meals3 <- factor(dplyr::ntile(apistrat$meals, 3), labels = c("Low", "Mid", "High"))
design <- apistrat %>% srvyr::as_survey_design(ids = 1, weights = pw)

design_base <- design
class(design_base) <- setdiff(class(design_base), "tbl_svy")

props <- survey::svyby(~sch.wide, ~meals3 + stype, design = design_base, FUN = survey::svymean,
                        keep.var = TRUE, vartype = "se", covmat = TRUE)

cat("\n--- print(props) ---\n")
print(props)

cat("\n--- rownames(props) ---\n")
print(rownames(props))

cat("\n--- names(coef(props)) ---\n")
print(names(coef(props)))

cat("\n--- dimnames(vcov(props))[[1]] ---\n")
print(dimnames(vcov(props))[[1]])
