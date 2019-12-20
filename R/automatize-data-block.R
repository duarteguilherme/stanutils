#' Automatize creation of data block
#' 
#' This function automatizes the creation of  data block for stan models
#' from a stan data list
#' 
#' @param stan_data a list with stan data
#'
#' @importFrom purrr imap
#' @export
create_model_from_data <- function(stan_data) {
   create_stan_model(
                     data = imap(stan_data, ~ create_declaration(.x, .y))  
                     )
}

create_declaration <- function(x, name) {
    if ( !any( is.logical(x), is.numeric(x), is.integer(x)))
        stop("Error - Non quantitative variable introduced")
    list(
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

