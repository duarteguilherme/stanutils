#' Create numeric index for stan
#'
#' This function creates numeric index for variables
#' inside a tibble
#' 
#' @param data tibble or data.frame object
#' @param ... columns to be transformed to index
#'
#' @examples
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


#' Create stan data
#' 
#' This function creates a data object 
#' to be used in Stan
#' 
#' @param data tibble or data.frame object
#' @param ... index
#' 
#' 
create_stan_data <- function(data, ...) {
    cols <- enexprs(...) 
    col_names <- purrr::as_vector(purrr::map(cols, as.character))
    data <- select(data, !!!cols)
    list(
        purrr::map(cols, ~ data[[.x]]),
        purrr::map(cols, ~ max(data[[.x]]))
        ) %>%
    purrr::flatten() %>%
    set_names(col_names,
              paste0("n_", col_names))
}
