
#' Utility function to access predefined highcharts templates
#'
#' @param env
#' a local environment created in each function and passed here to grab the
#' selected template
#' @param theme_options
#' named list of predefined hc_theme objects
#' @export
#'


get_themes <- function(env, theme_options=theme_options) {
  # create or overwrite objects in an environment
  env$theme_opts <- theme_options

}
