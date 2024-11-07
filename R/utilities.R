
#' Utility function to access predefined highcharts templates
#'
#' @param env
#' a local environment created in each function and passed here to grab the
#' selected template
#'
#' @export
#'



get_themes <- function(env) {
  # create or overwrite objects in an environment

  theme_options <- list(theme_cc = theme_cc,
                        theme_fbhc = theme_fbhc)

  env$theme_opts <- theme_options
}
