
#' Highcharts Bubblepop chart with Catalyst California theme
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


rdaBubblepop <- function(
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

    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text

    hc_add_series(df,
                  "bar",
                  name = "bubbleBar",
                  invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y)),
                  showInLegend=FALSE,
                  enableMouseTracking=FALSE, # disables tooltip from popping up when mouse moves over bars
                  pointWidth=2,
                  states=list(inactive=list(opacity=1)),
                  color=bar_color) %>% # disables transparency of bars when hovering over bubbles

    hc_add_series(df,
                  "bubble",
                  name = "bubblePop",
                  invert=TRUE,
                  hcaes(x=!!rlang::ensym(x),
                        y=!!rlang::ensym(y),
                        size=!!rlang::ensym(z)),
                  maxSize="15%",
                  showInLegend=FALSE,
                  clip=FALSE,
                  states=list(inactive=list(opacity=1)),
                  marker=list(lineWidth = 2,
                              fillOpacity = 1,
                              lineColor = line_color,
                              fillColor = fill_color
                              )) %>%

    hc_xAxis(title = list(text = ""),
             type="category",
             categories=df$x,
             startOnTick = TRUE,  # Force start at first tick
             min = 0) %>%             # Explicitly set minimum

    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS(yaxis_label_JS),
                           style=list(fontSize="12px")))  %>%

    hc_legend(title=list(
      text=paste0("<span style='color: #000000; font-weight: 400;'>",
                  "<b>Line shows<br><i>the rate.</i></b><br>",
                  "<b>Bubble shows<br><i>total people", ripa_flag,".</i></b></span>")),
              enable = TRUE,
              align = "right",
              verticalAlign="middle",
              width="12%",
              layout="vertical",
              borderWidth=1,
              borderRadius=5,
              itemMarginBottom=10,
              bubbleLegend =  list(enabled = TRUE,
                                   connectorDistance=20,
                                   labels = list(
                                     format="{value:,.0f}",
                                     style=list(fontSize='14px')))) %>%

    hc_title(
      text = paste0(title),
      style = list(useHTML = TRUE)) %>%

    hc_subtitle(text = paste0(subtitle)) %>%

    hc_caption(
      text = caption,
      useHTML=TRUE) %>%

    hc_add_theme(selected_theme) %>%

    hc_chart(inverted = T,
             height = 480) %>%

    hc_exporting(
      enabled = TRUE,
      sourceWidth=900,
      sourceHeight=600,
      chartOptions=list(plotOptions=list(
        series=list(
          dataLabels=list(
            enabled=TRUE,
            format=paste0(export_data_label))))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, ", format(Sys.Date(), "%Y"), "."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV')))
    )

  return(result)

  }
