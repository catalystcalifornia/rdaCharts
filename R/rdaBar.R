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
#' Content displayed when cursor hovers over bars. Provide as string (can include Js and HTML syntax).
#' @param caption
#' provide as a string
#' @param export_data_label
#' provide as a string (can include Js and HTML syntax).
#' e.g., "	'{point.rate:.1f}%'"
#'
#' @return
#' A responsive horinzontal bar chart
#' @import highcharter
#' @import dplyr
#' @import rlang

#' @export
#'

rdaBar <- function(
    df,
    x, # independent variable
    y, # dependent variable
    title="", # chart title
    subtitle="",
    tooltip_text="",
    caption="",
    export_data_label="") {

  ##### Set themes and options ####
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
    colors = c(meteorite, lavender, papaya, peridot),
    chart = list(
      backgroundColor = alabaster,
      style = list(
        fontFamily = main_font, # font_subtitle
        color=alabaster)),
    title = list(widthAdjust = -50,
                 style = list(
                   color = black,
                   fontFamily = main_font, # font_title
                   fontWeight = black_font_weight,
                   textAlign="left",
                   fontSize='21px'),
                 align = "left"),
    subtitle = list(
      style = list(
        color = black,
        fontFamily = main_font, # font_subtitle
        fontWeight = regular_font_weight,
        fontSize='14px'),
      align='left'),
    caption = list(
      style = list(
        color = black,
        fontFamily = main_font, # font_caption
        fontWeight = regular_font_weight,
        fontSize = "10px",
        textAlign = "left",
        width = 50),
      useHTML = TRUE,
      floating = FALSE),

    xAxis=list(
      labels=list(
        style=list(
          color=black,
          fontFamily = main_font, # font_x_label
          fontWeight = semi_bold_font_weight,
          width=120,  #argument to modify the width of the labels
          min=0,
          fontSize="10px")),
      lineColor=gainsboro),

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
      tickWidth=1),

    legend = list(
      itemStyle = list(
        fontFamily = main_font, # font_axis_label
        fontWeight = regular_font_weight,
        color = black,
        fontSize = '12px'),

      itemHoverStyle = list(
        fontFamily = main_font, # font_table_text
        fontWeight = regular_font_weight,
        color = black),
      plotLines=list(color=gainsboro)))

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

    hc_add_theme(cc_theme)%>%

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
                                                     'downloadXLS', 'downloadCSV'))))}
