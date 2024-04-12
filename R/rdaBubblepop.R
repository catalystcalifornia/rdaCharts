
#' Title
#'
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
#' provide as a string
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
    title="",
    subtitle= "",
    tooltip_text="",
    caption = "",
    yaxis_label = "''",
    export_data_label="") {


  #### Set themes and options ####
  # set global options to ensure that comma separator is a comma for highchart graphs
  lang <- getOption("highcharter.lang")
  lang$thousandsSep <- ","
  options(highcharter.lang = lang)

  ## COLORS ##
  papaya <- "#F25922"
  peridot <- "#CEEA01"
  lavender <- "#CEC4E2"
  meteorite <- "#3A207D"
  ccblue <- "#0860BC"

  black <- "#000000"
  alabaster<-"#FBFBFB"
  gainsboro <- "#DEDEDE"

  divergent_color_ramp <- c("#372278", "#5F3B63","#A8683C", "#D58424", "#FF9900")
  grouped_color_ramp <-c(meteorite, papaya, peridot, "#BDAFE9", "#E2D9FF")
  group_colors_item <- c(meteorite, papaya, peridot, lavender, ccblue, "#5F3B63","#A8683C", "#D58424", "#FF9900")
  ## FONTS ##
  main_font <- "Inter"
  regular_font_weight <- 400
  black_font_weight <- 800
  semi_bold_font_weight <- 600

  ##### highchart theme #####
  cc_theme <- hc_theme(
    colors = c(meteorite, lavender, papaya, peridot
    ),
    chart = list(
      backgroundColor = alabaster,
      style = list(
        fontFamily = main_font, # font_subtitle
        color=alabaster
      )
    ),
    title = list(widthAdjust = -50,
                 style = list(
                   color = black,
                   fontFamily = main_font, # font_title
                   fontWeight = black_font_weight,
                   textAlign="left",
                   fontSize='21px'),
                 align = "left"
    ),
    subtitle = list(
      style = list(
        color = black,
        fontFamily = main_font, # font_subtitle
        fontWeight = regular_font_weight,
        fontSize='14px'),
      align='left'
    ),
    caption = list(
      style = list(
        color = black,
        fontFamily = main_font, # font_caption
        fontWeight = regular_font_weight,
        fontSize = "10px",
        textAlign = "left",
        width = 50),
      useHTML = TRUE,
      floating = FALSE
    ),

    xAxis=list(
      labels=list(
        style=list(
          color=black,
          fontFamily = main_font, # font_x_label
          fontWeight = semi_bold_font_weight,
          width=120,  #argument to modify the width of the labels
          min=0,
          # spacingLeft = "150px",
          fontSize="10px")),
      lineColor=gainsboro
    ),

    yAxis=list(
      labels=list(
        style=list(
          color=black,
          fontFamily = main_font, # font_axis_label
          fontWeight = regular_font_weight,
          fontSize="10px",
          margin = 50)),
      gridLineWidth=0, # removes vertical grid lines
      visible=TRUE, # makes axis line visible
      lineWidth=1,
      lineColor=gainsboro,
      min=0,
      tickAmount=6,
      tickWidth=1
    ),

    legend = list(
      itemStyle = list(
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        color = black,
        fontSize = '12px'
      ),

      itemHoverStyle = list(
        fontFamily = main_font, # font_table_text
        fontWeight = regular_font_weight,
        color = black
      ),
      plotLines=list(color=gainsboro)
    )
  )

  yaxis_label_JS <- paste0("function() {
        	return this.value +", yaxis_label, "}")

  df <-  df %>%
    arrange(desc(y))

  highchart() %>%

    hc_tooltip(headerFormat='', # removes series label from top of tooltip
               pointFormat = tooltip_text,
               useHTML=TRUE) %>%  # allows tooltip to read <br> html in reformatted tooltip_text

    hc_add_series(df, "bar", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x), y=!!rlang::ensym(y)),
                  showInLegend=FALSE,
                  enableMouseTracking=FALSE)%>% # disables tooltip from popping up when mouse moves over bars

    hc_add_series(df, "bubble", invert=TRUE,
                  hcaes(x=!!rlang::ensym(x),
                        y=!!rlang::ensym(y),
                        size=!!rlang::ensym(z)),
                  maxSize="15%",
                  # tooltip =  list(pointFormat = formatted_tooltip),
                  showInLegend=FALSE,
                  clip=FALSE) %>%

    hc_xAxis(title = list(text = ""),
             type="category",
             categories=df$x,
             labels=list(style=list(fontSize="12px"))) %>%

    hc_yAxis(title = list(text = ""),
             labels = list(formatter = JS(yaxis_label_JS),
                           style=list(fontSize="12px")))  %>%

    hc_legend(title=list(text=paste0('<span style="color: #000000; font-weight: 400; font-size:10px;"><b>Line shows<br>    <i>the rate.</i></b><br><b>Bubble shows<br>    <i>total people.</i></b></span>')),
              enable = TRUE,
              align = "right",
              verticalAlign="middle",
              width="12%",
              layout="vertical",
              borderColor=gainsboro,
              borderWidth=1,
              borderRadius=5,
              itemMarginBottom=10,
              bubbleLegend =  list(enabled = TRUE,
                                   connectorDistance=20,
                                   borderColor=meteorite,
                                   connectorColor=meteorite,
                                   labels = list(
                                     format="{value:,.0f}",
                                     style=list(fontSize='10px')),
                                   marker = list(
                                     fillColor = lavender))) %>%

    # Sets bar width
    hc_plotOptions(series=list(pointWidth=2,
                               marker=list(fillColor=lavender,
                                           lineColor=meteorite,
                                           fillOpacity=1),
                               states=list(inactive=list(opacity=1))), # disables transparency of bars when hovering over bubbles
                   spacingLeft=200) %>%

    # title elements
    hc_title(
      text = paste0(title),
      style = list(useHTML = TRUE)) %>%

    hc_subtitle(text = paste0(subtitle)) %>%

    hc_caption(
      text = caption,
      useHTML=TRUE
    ) %>%

    hc_add_theme(cc_theme)%>%

    hc_chart(inverted = T,
             height = 480) %>%

    hc_exporting(
      enabled = TRUE, sourceWidth=900, sourceHeight=600,
      chartOptions=list(plotOptions=list(
        series=list(
          dataLabels=list(
            enabled=TRUE,
            format=paste0(export_data_label))))),
      filename = paste0(subtitle,"_Catalyst California, catalystcalifornia.org, 2023."),
      buttons=list(contextButton=list(menuItems=list('downloadPNG', 'downloadSVG',
                                                     'downloadXLS', 'downloadCSV')))
    )}
