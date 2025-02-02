
#' Highchart horizontal grouped bar chart with RDA customization
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
#' An interactive grouped bar highchart
#' @import highcharter
#' @import dplyr
#' @import rlang
#' @export
#'


rdaGroupedBar <- function(
    df,
    x, # independent variable
    y, # dependent variable
    z, # grouping variable
    bar_colors=list(),
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

  # set bar styling for a given theme
  # if no bar_colors provided
  if (length(bar_colors)==0) {
    # Provide a default list using CC colors - if selected theme is theme_cc
    if (theme == "theme_cc") {
      bar_colors <- cc_colors
    }
    # Provide a default list using FBHC colors - if selected theme is theme_fbhc
    if (theme == "theme_fbhc") {
      bar_colors <- fbhc_colors
    }
    # Provide a list of colors based on the theme provided (if not pre-defined in rdaCharts)
    if (theme != "theme_fbhc" & theme != "theme_cc") {
      bar_colors <- selected_theme$hc_colors
    }

    warning("No list provided to bar_colors argument - defaulting to color scheme based on selected theme. If no theme selected, returns grey shades.")
  }

  if (length(bar_colors)==0) {
    bar_colors <- list("#dddee2", "#acadb1", "#7e7f82", "#525357", "#2b2b2e")

  }


  ##### Chart function #####

  result <- highchart() %>%

    hc_add_series(df,
                  'bar',
                  hcaes(
                    x = !!rlang::ensym(x),
                    y = !!rlang::ensym(y),
                    group = !!rlang::ensym(z))) %>%

    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE, # allows tooltip to read <br> html in reformatted tooltip_text
               style = list(fontSize = "14px")) %>%

    hc_colors(bar_colors)%>%

    hc_title(
      text = paste0(title),
      style = list(
        useHTML = TRUE,
        fontSize = "21px",
        lineHeight = "28px"
      )) %>%

    hc_subtitle(
      text = paste0(subtitle),
      style = list(
        fontSize = "16px",
        lineHeight = "22px"
      )) %>%

    hc_caption(
      text = caption,
      useHTML = TRUE,
      style = list(
        fontSize = "12px",
        lineHeight = "18px"
      )) %>%

    hc_yAxis(title = list(text = ""),
             style = list(fontSize = "12px")) %>%

    hc_xAxis(title = list(text = ""),
             type = "category",
             labels = list(position="bottom",
                           style = list(
                             fontSize = "12px")),
             overflow = "justify") %>%

    hc_legend(enabled = TRUE,
              style = list(fontSize = "12px"))%>%

    hc_add_theme(selected_theme) %>%

    hc_chart(
      height = 600,
      reflow = TRUE,
      marginLeft = 120,
      marginRight = 120,
      events = list(
        load = JS("function() {
          var chart = this;
          function updateSize() {
            var width = chart.containerWidth;
            var height = Math.max(600, width * 0.7);
            if (width < 500) {
              chart.update({
                chart: {
                  height: height,
                  marginLeft: 10,
                  marginRight: 10
                },
                title: { style: { fontSize: '16px' } },
                subtitle: { style: { fontSize: '14px' } },
                caption: { style: { fontSize: '12px' } }
              }, false);
            } else {
              chart.update({
                chart: {
                  height: height,
                  marginLeft: 120,
                  marginRight: 120
                },
                title: { style: { fontSize: '21px' } },
                subtitle: { style: { fontSize: '16px' } },
                caption: { style: { fontSize: '12px' } }
              }, false);
            }
            chart.redraw();
          }
          updateSize();
          window.addEventListener('resize', updateSize);
        }")
      )
    ) %>%

    hc_exporting(
      enabled = TRUE,
      sourceWidth = 900,
      sourceHeight = 600,
      chartOptions = list(
        plotOptions = list(
          series = list(
            dataLabels = list(
              enabled = TRUE,
              format = paste0(export_data_label),
              style = list(fontSize = "14px")
            )
          )
        )
      ),
      filename = paste0(
        subtitle,
        "_Catalyst California, catalystcalifornia.org, ",
        format(Sys.Date(), "%Y"),
        "."
      ),
      buttons = list(
        contextButton = list(
          menuItems = list(
            "downloadPNG",
            "downloadSVG",
            "downloadXLS",
            "downloadCSV"
          )
        )
      )
    )

  return(result)
}



