#' Run Stan model
#' 
#' Run stan model with data and stan_model inputs
#' 
#' 
#' @param stan_model stan_model object
#' @param stan_data data in stan template
#' @param ... other arguments. Check out stan manual
#'
#' @importFrom rstan stan
#'
#' @export
run_stan <- function(stan_model, stan_data, ...) {
    stan_model <- generate_stan_code(stan_model)
    stan(model_code = stan_model, data = stan_data, ...)
}
