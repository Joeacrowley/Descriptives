source_folder_r <- 
function (folder, verbose = FALSE) 
{
    r_files <- list.files(folder, pattern = "\\.R$", full.names = TRUE)
    if (verbose) {
        cat("Sourcing files:\n")
        print(basename(r_files))
    }
    invisible(lapply(r_files, source))
}
