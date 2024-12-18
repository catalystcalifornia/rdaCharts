
#' Highcharts Bubblepop chart - testing mobile friendliness
#'
#' @param df
#' name of dataframe
#' @param x
#' string of the independent variable to appear on y-axis
#' @param df
#' name of dataframe
#' @param y
#' string of the dependent variable to appear on x-axis
#' @param z
#' string of the variable that stores count to determine bubble size
#' @param title
#' provide as a string
#' @param subtitle
#' provide as a string
#' @param tooltip_text
#' provide as a string
#' @param caption
#' provide as a string
#' @param yaxis_label
#' provide as a string
#' @param export_data_label
#' provide as a string (can include Js and HTML syntax), e.g., "	'{point.rate:.1f}%'"
#' @param theme
#' a hc_theme object. can be created by user or be a predefined project theme (options include: "theme_cc", "theme_fbhc"). default value is theme_cc.
#' @return
#' A responsive html bubblepop chart. Bubblepops are a combination of bar and
#' @import highcharter
#' @import dplyr
#' @import rlang
#' @export
#'


rdaBubblepop_test <- function(
    df,
    x,
    y,
    z,
    theme="theme_cc",
    title="",
    subtitle= "",
    tooltip_text="",
    caption = "",
    yaxis_label = "''",
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

  yaxis_label_JS <- paste0("function() {
        	return this.value +", yaxis_label, "}")

  # set bubble styling for a given theme
  if (theme == "theme_cc") {
    fill_color <- lavender
    line_color <- meteorite
    bar_color <- meteorite
  }

  if (theme == "theme_fbhc") {
    fill_color <- community_safety
    line_color <- primary_color
    bar_color <- primary_color
  }

  # add RIPA flag to update bubblepop legend
  ripa_flag <- ""
  if (grepl("Police", caption, ignore.case=TRUE) == TRUE) {
    ripa_flag <- " stopped"
  }

  ##### Chart function #####
result <- highchart() %>%
  hc_tooltip(headerFormat = '',
             pointFormat = tooltip_text,
             useHTML = TRUE,
             style = list(fontSize = "14px")) %>%  # Increased tooltip font size
  hc_add_series(df,
                "bar",
                name = "bubbleBar",
                invert = TRUE,
                hcaes(x = !!rlang::ensym(x), y = !!rlang::ensym(y)),
                showInLegend = FALSE,
                enableMouseTracking = FALSE,
                pointWidth = 2,
                states = list(inactive = list(opacity = 1)),
                color = bar_color) %>%
  hc_add_series(df,
                "bubble",
                name = "bubblePop",
                invert = TRUE,
                hcaes(x = !!rlang::ensym(x),
                      y = !!rlang::ensym(y),
                      size = !!rlang::ensym(z)),
                maxSize = "15%",
                showInLegend = FALSE,
                clip = FALSE,
                states = list(inactive = list(opacity = 1)),
                marker = list(
                  lineWidth = 2,
                  fillOpacity = 1,
                  lineColor = line_color,
                  fillColor = fill_color
                )) %>%
  hc_xAxis(
    title = list(text = ""),
    type = "category",
    categories = df$x,
    startOnTick = TRUE,
    min = 0,
    labels = list(
      style = list(
        fontSize = "14px",  # Increased from 12px
        textOverflow = 'ellipsis'
      ),
      overflow = 'justify'
    )
  ) %>%
  hc_yAxis(
    title = list(text = ""),
    labels = list(
      formatter = JS(yaxis_label_JS),
      style = list(fontSize = "14px")  # Increased from 12px
    )
  ) %>%
  hc_legend(
    title = list(
      text = paste0(
        "<span style='color: #000000; font-weight: 400;'>",
        "<b>Line shows<br><i>the rate.</i></b><br>",
        "<b>Bubble shows<br><i>total people", ripa_flag, ".</i></b></span>"
      ),
      style = list(fontSize = "14px")  # Added explicit font size
    ),
    enable = TRUE,
    align = "right",
    verticalAlign = "middle",
    width = "12%",
    layout = "vertical",
    borderWidth = 1,
    borderRadius = 5,
    itemMarginBottom = 10,
    itemStyle = list(fontSize = "14px"),  # Increased legend item font size
    bubbleLegend = list(
      enabled = TRUE,
      connectorDistance = 20,
      labels = list(
        format = "{value:,.0f}",
        style = list(fontSize = '14px')  # Increased from 12px
      )
    )
  ) %>%
  hc_title(
    text = paste0(title),
    style = list(
      useHTML = TRUE,
      fontSize = '21px',  # Maintained larger size for hierarchy
      lineHeight = '28px'  # Increased for better readability
    )
  ) %>%
  hc_subtitle(
    text = paste0(subtitle),
    style = list(
      fontSize = '16px',  # Increased from 12px
      lineHeight = '22px'
    )
  ) %>%
  hc_caption(
    text = caption,
    useHTML = TRUE,
    style = list(
      fontSize = '14px',  # Increased from 11px
      lineHeight = '20px'
    )
  ) %>%
  hc_add_theme(selected_theme) %>%
  hc_chart(
    inverted = TRUE,
    height = NULL,  # Remove fixed height
    reflow = TRUE,  # Enable reflowing
    marginLeft = 120,  # Increased margin for larger font sizes
    marginRight = 120,
    style = list(fontFamily = "Arial, sans-serif"),  # Ensuring accessible font
    events = list(
      load = JS("function() {
          var chart = this;
          function updateSize() {
            var width = chart.containerWidth;
            var height = Math.max(480, width * 0.7);  // Increased ratio for better readability
            if (width < 500) {  // Mobile breakpoint
              chart.update({
                chart: {
                  height: height,
                  marginLeft: 20,
                  marginRight: 20
                },
                legend: {
                  align: 'center',
                  verticalAlign: 'bottom',
                  width: '100%',
                  layout: 'horizontal',
                  itemStyle: { fontSize: '14px' }  # Maintained minimum font size
                },
                title: { style: { fontSize: '18px' } },  # Reduced but still readable
                subtitle: { style: { fontSize: '14px' } },
                caption: { style: { fontSize: '14px' } }
              }, false);
            } else {
              chart.update({
                chart: {
                  height: height,
                  marginLeft: 120,
                  marginRight: 120
                },
                legend: {
                  align: 'right',
                  verticalAlign: 'middle',
                  width: '12%',
                  layout: 'vertical'
                },
                title: { style: { fontSize: '21px' } },
                subtitle: { style: { fontSize: '16px' } },
                caption: { style: { fontSize: '14px' } }
              }, false);
            }
            chart.redraw();
          }
          updateSize();
          window.addEventListener('resize', updateSize);
        }")
    )
  ) %>%
  # hc_accessibility(
  #   announceNewData = list(announceNewData = TRUE),
  #   description = paste(title, subtitle),  # Added for screen readers
  #   enabled = TRUE
  # ) %>%
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
            style = list(fontSize = "14px")  # Added explicit font size
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
          'downloadPNG',
          'downloadSVG',
          'downloadXLS',
          'downloadCSV'
        )
      )
    )
  )

return(result)
}
