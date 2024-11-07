#' Highcharts horizontal bar chart
#'
#' @param x
#' string of the independent variable to appear on y-axis
#' @param df
#' name of dataframe
#' @param y
#' string of the dependent variable to appear on x-axis
#' @param title
#' provide as a string
#' @param subtitle
#' provide as a string
#' @param tooltip_text
#' provide as a string
#' @param caption
#' provide as a string
#' @param export_data_label
#' provide as a string (can include JS and HTML syntax), e.g., "	'{point.rate:.1f}%'"
#' @param theme
#' a hc_theme object. can be created by user or be a predefined project theme (options include: "theme_cc", "theme_fbhc"). default value is theme_cc.
#' @return
#' A responsive horizontal bar chart
#' @import highcharter
#' @import dplyr
#' @import rlang
#' @export
#'


rdaBar <- function(
    df,
    x, # independent variable
    y, # dependent variable
    theme="theme_cc",
    title="", # chart title
    subtitle="",
    tooltip_text="",
    caption="",
    export_data_label="") {

  ##### Confirm and select pre-defined theme_cc #####
  theme_env <- new.env(parent = emptyenv())
  get_themes(env=theme_env)

  list_of_themes <- theme_env$theme_opts

  selected_theme <- list_of_themes[[theme]]

  if ((theme %in% names(.GlobalEnv)) == TRUE) {
    get_theme_object <-  get(theme, envir = .GlobalEnv)
    check_valid_theme <- class(get_theme_object)
    print(check_valid_theme)

    if (check_valid_theme=="hc_theme") {
    selected_theme <- get_theme_object
    warning("Using a hc_theme created by user instead of pre-defined template in rdaCharts")
    }

  } else if (is.null(selected_theme) == TRUE) {
    selected_theme <- list_of_themes[["theme_cc"]]
    warning(paste0("The provided theme (", theme,
                  ") is not pre-defined in rdaCharts. To return a chart, ",
                  "the theme was replaced with the default value: theme_cc. ",
                  "Please consult this list of available theme options: ",
                  paste(names(list_of_themes), collapse = ", ") ,". ",
                  "If an issue persists, please contact Hillary."))

  }

  ##### Chart function #####
  df <-  df %>%
    arrange(desc(y))

  hchart(df,
         "bar",
         hcaes(x = !!rlang::ensym(x), y = !!rlang::ensym(y))) %>%

    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text

    hc_title(text = title) %>%

    hc_subtitle(text = subtitle) %>%

    hc_caption(text = caption) %>%

    hc_yAxis(title = list(text = paste0(""),
                          labels = list(style=list(fontSize="12px")))) %>%

    hc_xAxis(title = list(text = paste0(""),
                          labels=list(position="bottom",
                                      style=list(fontSize="12px")))) %>%

    hc_legend(enabled = TRUE,
              reversed =  TRUE,
              x=20) %>%

    hc_add_theme(selected_theme) %>%

    hc_chart(marginRight=120) %>%

    hc_exporting(
      enabled = TRUE,
      sourceWidth=900,
      sourceHeight=600,
      chartOptions=list(
        plotOptions=list(
          series=list(
            dataLabels=list(
              enabled=TRUE,
              format=export_data_label)))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV'))))

  }
