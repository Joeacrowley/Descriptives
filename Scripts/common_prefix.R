common_prefix <- 
function (x) 
{
    split_names <- strsplit(x, "")
    min_length <- min(lengths(split_names))
    prefix <- character(0)
    for (i in seq_len(min_length)) {
        chars <- sapply(split_names, `[`, i)
        if (length(unique(chars)) == 1) {
            prefix <- c(prefix, chars[1])
        }
        else {
            break
        }
    }
    paste0(prefix, collapse = "")
}
