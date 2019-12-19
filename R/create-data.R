#' Create data block
#' 
#' This function creates data block for stan models
#' from a stan data list
#' 
#' @param stan_data a list with stan data
#'
#' @importFrom purrr imap
#' @export
create_data_block <- function(stan_data) {
   imap(stan_data, ~ create_declaration(.x, .y))  %>%
       paste0(collapse = "\n")
}

create_declaration <- function(x, name) {
    if ( !any( is.logical(x), is.numeric(x), is.integer(x)))
        stop("Error - Non quantitative variable introduced")
    declaration <- ifelse(test_numeric(x), "real", "int")
    minimum <- floor(min(x))
    maximum <- ceiling(max(x))
    paste0(declaration, "<lower=", minimum, 
           ",upper=", maximum, "> ", name,
           guess_length(x), ";")
}

test_numeric <- function(vec) {
    for (i in vec) {
        if ( floor(i) != i )
            return(TRUE)
    }
    FALSE
}

guess_length <- function(x) {
    n <- length(x)
    if ( n == 1 )
        return("")
    paste0("[", n, "]")
}


