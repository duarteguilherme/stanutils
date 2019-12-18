#' Create numeric index for stan
#'
#' This function creates numeric index for variables
#' inside a tibble
#' 
#' @param data tibble or data.frame object
#' @param ... columns to be transformed to index
#'
#' @example
#' df <- data.frame(test = c(rep("dog", 5), rep("cat", 6)))
#' create_index(df, k = test)
#' @export
create_index <- function(data, ...) {
    cols <- enexprs(...)
    dplyr::select(data, !!!cols) %>%
        mutate_all(. %>%
                   as.factor %>%
                   as.numeric) %>%
    dplyr::bind_cols(data)


}

