
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

  env$theme_opts <- list(theme_cc = theme_cc,
                         theme_fbhc = theme_fbhc)
}
