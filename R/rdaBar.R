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
#' a hc_theme object. can be created by user or be a predefined project theme (options include: theme_cc, theme_fbhc). default value is theme_cc.
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
    theme=theme_cc,
    title="", # chart title
    subtitle="",
    tooltip_text="",
    caption="",
    export_data_label="") {

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

    hc_add_theme(theme)%>%

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
