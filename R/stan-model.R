#' Create stan model
#'
#' This function returns a stan_model object.
#' 
#' 
#' @export
create_stan_model <- function(data = list(), parameters = list(), model = list()) {
    stan_model <- list("data" = data, 
                       parameters = parameters,
                       model = model)
    class(stan_model) <- "stan_model"
    stan_model
}

#' Generate stan code
#' 
#' This functions gets a stan_model object
#' and generate stan code from it.
#'
#' @param stan_model A object of class `stan_model`
#'
#' @importFrom purrr imap
#'
#' @export
generate_stan_code <- function(stan_model) {
    imap(stan_model, ~ paste0(.y, ' {\n', .x, '\n}')) %>%
        paste0(collapse='\n')
}

unparse_data_block <- function(declaration = "", name = "", minimum = "", maximum = "", length = "") {
    paste0(declaration, "<lower=", minimum, 
           ",upper=", maximum, "> ", name,
           length, ";")
}

