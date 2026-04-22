df_to_dense_flextable <- 
function (df, font_size = 8, padding = 2, merge = NULL, slice = NULL) 
{
    if (!is.null(slice)) {
        df <- df %>% slice(slice)
    }
    ht <- df %>% as_hux %>% map_background_color(by_rows("#f5f7fa", 
        "#ffffff")) %>% huxtable::set_background_color(1, everywhere, 
        "grey95") %>% set_wrap(TRUE)
    if (!is.null(merge)) {
        ht <- ht %>% merge_repeated_rows() %>% set_valign(value = "middle")
    }
    ft <- flextable::autofit(flextable::theme_booktabs(flextable::padding(flextable::fontsize(huxtable::as_flextable(ht), 
        size = font_size, part = "all"), padding = padding, part = "all")))
    if (!is.null(merge)) {
        ft <- ft %>% border(border = officer::fp_border(color = "grey"), 
            part = "body")
    }
    format_numeric_cells <- function(ft, data) {
        flextable::set_formatter(ft, .formatter = function(x) {
            if (is.numeric(x)) {
                formatC(x, format = "f", digits = 1)
            }
            else {
                as.character(x)
            }
        })
    }
    ft <- format_numeric_cells(ft, df)
    return(ft)
}
