#' Automatize creation of data blocks
#' 
#' This function automatizes the creation of  data block for stan models
#' from a stan data list
#' 
#' @param stan_data a list with stan data
#'
#' @importFrom purrr imap
#' @importFrom purrr partial
#' @importFrom rlang exec
#' @export
create_model_from_data <- function(stan_data) {
   stan_model <- create_stan_model()
   add_data_funcs <- imap(stan_data, ~ create_declaration(.x, .y))
   exec("compose", !!!add_data_funcs)(stan_model)
}


create_declaration <- function(x, name) {
    if ( !any( is.logical(x), is.numeric(x), is.integer(x)))
        stop("Error - Non quantitative variable introduced")
    partial(add_data,
            declaration = ifelse(test_numeric(x), "real", "int"),
            name = name,
            minimum = floor(min(x)),
            maximum = ceiling(max(x)),
            length = length(x)
            )
}

test_numeric <- function(vec) {
    for (i in vec) {
        if ( floor(i) != i )
            return(TRUE)
    }
    FALSE
}

