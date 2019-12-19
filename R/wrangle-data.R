#' Create numeric index for stan
#'
#' This function creates numeric index for variables
#' inside a tibble
#' 
#' @param data tibble or data.frame object
#' @param ... columns to be transformed to index
#'
#' @importFrom rlang enexprs
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate_all
#'
#' @examples
#' df <- data.frame(test = c(rep("dog", 5), rep("cat", 6)))
#' create_index(df, k = test)
#' @export
create_index <- function(data, ...) {
    cols <- enexprs(...)
    select(data, !!!cols) %>%
        mutate_all(function(x) as.numeric(as.factor(x))) %>%
        bind_cols(data)


}


#' Create stan data
#' 
#' This function creates a data object 
#' to be used in Stan
#' 
#' @param data tibble or data.frame object
#' @param ... index
#' 
#' @examples
#' df <- data.frame(test = c(rep("dog", 5), rep("cat", 6)))
#' df <- create_index(df, k = test)
#' df <- create_stan_data(df, k)
#'
#' @importFrom rlang enexprs
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom purrr as_vector
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom purrr set_names
#'
#' @export
create_stan_data <- function(data, ...) {
    cols <- enexprs(...) 
    col_names <- as_vector(map(cols, as.character))
    data <- select(data, !!!cols)
    list(
        map(cols, ~ data[[.x]]),
        map(cols, ~ max(data[[.x]]))
        ) %>%
    flatten() %>%
    set_names(col_names,
              paste0("n_", col_names))
}
