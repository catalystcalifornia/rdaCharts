#' Highcharts horizontal stacked bar chart with RDA customization
#'
#' @param df
#' Name of data frame with data to visualize
#' @param x
#' String of the column name of the independent variable (e.g., Race/Ethnicity) to be plotted on the x-axis
#' @param y
#' String of the column name of the dependent variable (e.g., Rates, Counts, etc.) to be plotted on the y-axis
#' @param z
#' String of the column name of the grouping variable (e.g., subcategories to further cut the data within x) to create stacked bars
#' @param bar_colors
#' List of hexcode colors that the chart should use (e.g., c("#EFEFEF", "#F25922", "#CEEA01"). By default will use pre-defined CC colors: meteorite, lavender, peridot, papaya, ccblue, black, gainsboro.
#' @param title
#' Chart title provided as string - appears at the top of chart
#' @param subtitle
#' Chart subtitle provided as string (also name of exported file) - appears directly underneath title
#' @param tooltip_text
#' Content displayed when cursor hovers over bars. Provide as string (can include Js and HTML syntax).
#' @param caption
#' Chart caption provided as string - appears at bottom
#' @param export_data_label
#' provide as a string (can include Js and HTML syntax), e.g., "	'{point.rate:.1f}%'"
#' @param theme
#' a hc_theme object. can be created by user or be a predefined project theme (options include: "theme_cc", "theme_fbhc"). default value is theme_cc.
#' @return
#' An interactive stacked bar highchart
#' @import highcharter
#' @import dplyr
#' @import rlang
#' @export
#'


rdaStackedBar <- function(
    df,
    x, # independent variable
    y, # dependent variable
    z, # grouping variable
    bar_colors=list(meteorite, lavender, peridot, papaya, ccblue, black, gainsboro),
    theme="theme_cc",
    title="", # chart title
    subtitle="",
    tooltip_text="",
    caption="",
    export_data_label="") {


  ##### Create environment to grab pre-defined theme_cc #####
  theme_env <- new.env(parent = emptyenv())
  get_themes(env=theme_env)

  list_of_themes <- theme_env$theme_opts

  selected_theme <- list_of_themes[[theme]]

  if (is.null(selected_theme) == TRUE) {
    selected_theme <- list_of_themes[["theme_cc"]]
    warning(paste0("The provided theme (", theme,
                   ") is not pre-defined in rdaCharts. To return a chart, ",
                   "the theme was replaced with the default value: theme_cc. ",
                   "Please consult this list of available theme options: ",
                   paste(names(list_of_themes), collapse = ", ") ,". ",
                   "If an issue persists, please contact Hillary."))

  }


  ##### Chart function #####

  result <- hchart(df,
         "bar",
         stacking = "normal",
         hcaes(
           x = !!rlang::ensym(x),
           y = !!rlang::ensym(y),
           group = !!rlang::ensym(z))) %>%

    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text

    hc_colors(bar_colors)%>%

    hc_title(text = title) %>%

    hc_subtitle(text = subtitle) %>%

    hc_caption(text = caption) %>%

    hc_yAxis(title = list(text = "")) %>%

    hc_xAxis(title = list(text = "",
                          labels=list(position="bottom"))) %>%

    hc_legend(enabled = TRUE,
              reversed =  TRUE,
              x=20) %>%

    hc_add_theme(selected_theme) %>%

    hc_chart(marginRight=120,
             height=480) %>%

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

  return(result)
  }
