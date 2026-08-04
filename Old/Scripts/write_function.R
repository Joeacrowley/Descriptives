write_function <- 
function (func_name, folder) 
{
    func_name_str <- deparse(substitute(func_name))
    assign_statement <- paste0(func_name_str, " <- ")
    file_name <- file.path(folder, paste0(func_name_str, ".R"))
    function_to_export <- get(func_name_str)
    writeLines(c(assign_statement, deparse(function_to_export)), 
        con = file_name)
}
