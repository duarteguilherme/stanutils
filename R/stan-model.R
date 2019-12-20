#' Create stan model
#'
#' This function returns a stan_model object.
#' 
#' @param data List of data declarations
#' @param parameters List of parameters
#' @param model Model definition
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
#' @importFrom rlang exec
#'
#' @examples
#' stan_model <- create_stan_model()
#' stan_model <- add_data(stan_model, declaration = "int", name = "test", 
#'    minimum = 3, maximum = 5, length = 500)
#' generate_stan_code(stan_model)
#' 
#'
#' @export
generate_stan_code <- function(stan_model) {
    stan_model$data <- map(stan_model$data, ~ exec("unparse_block", !!!.x)) %>%
        paste0(collapse="\n")
    stan_model$parameters <- map(stan_model$parameters, ~ exec("unparse_block", !!!.x)) %>%
        paste0(collapse="\n")

    imap(stan_model, ~ paste0(.y, ' {\n', .x, '\n}')) %>%
        paste0(collapse='\n')
}

create_var_bound <- function(minimum = NULL, maximum = NULL) {
    if ( !is.null(minimum) )
        minimum <- paste0("lower=", minimum)
    if ( !is.null(maximum) )
        maximum <- paste0("upper=", maximum)
    bound <- paste0(c(minimum, maximum), collapse = ",")
    if ( bound != "" )
        bound <- paste0("<", bound, ">")
    bound
}

create_var_length <- function(length) {
    if ( !any(!test_numeric(length), is.numeric(length), is.integer(length), is.logical(length) ) )
        stop("Invalid format for length")
    if ( length < 0 ) 
        stop("Invalid value for length")
    ifelse(length == 1, '', paste0('[', length, ']'))
}

unparse_block <- function(declaration = "", name = "", minimum = NULL, maximum = NULL, length = 1) {
    bound <- create_var_bound(minimum, maximum)
    length <- create_var_length(length)
    paste0(declaration, bound, " ", name, length,  ";")
}

#' Add data to stan model
#'
#' Introduce a data declaration to stan model
#' 
#' @param stan_model A object of class 'stan_model'
#' @param ... Mandatory arguments are declaration and name. minimum, maximum, and length are optional.
#' 
#' @examples
#' stan_model <- create_stan_model()
#' add_data(stan_model, declaration = "int", name = "test", 
#' minimum = 3, maximum = 5, length = 500)
#' 
#' @export
add_data <- function(stan_model, ...) {
    arg_list <- rlang::list2(...)
    if ( !all(c("declaration", "name") %in% names(arg_list) ) )
        stop("Error. Please insert declaration and name")
    stan_model$data[[arg_list$name]] <- arg_list
    stan_model

}

#' Add parameter to stan model
#'
#' Introduce a parameter declaration to stan model
#' 
#' @param stan_model A object of class 'stan_model'
#' @param ... Mandatory arguments are declaration and name. minimum, maximum, and length are optional.
#' 
#' @examples
#' stan_model <- create_stan_model()
#' add_parameter(stan_model, declaration = "int", name = "test",
#' minimum = 3, maximum = 5, length = 500)
#' 
#' @export
add_parameter <- function(stan_model, ...) {
    arg_list <- rlang::list2(...)
    if ( !all(c("declaration", "name") %in% names(arg_list) ) )
        stop("Error. Please insert declaration and name")
    stan_model$parameters[[arg_list$name]] <- arg_list
    stan_model

}

#' Add model elements to stan model
#'
#' Introduce a model element to stan model
#' 
#' @param stan_model A object of class 'stan_model'
#' @param string model element in text
#' 
#' @examples
#' stan_model <- create_stan_model()
#' add_model(stan_model, string = 'y ~ normal(0,1)')
#' 
#' @export
add_model <- function(stan_model, string) {
    stan_model$model <- c(stan_model$model, string)
    stan_model

}
