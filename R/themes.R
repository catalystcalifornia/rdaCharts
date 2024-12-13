#' An exposed list of predefined hc_themes
#'
#' A list that can be accessed by users of the package.
#'
#' @format A list with 2 elements.
#' @usage data(theme_options)
#' @keywords themes
#' @import usethis

theme_options <- list(theme_cc = theme_cc,
                      theme_fbhc = theme_fbhc)

# This line exports the object to be accessible to users
usethis::use_data(theme_options, overwrite = TRUE)
