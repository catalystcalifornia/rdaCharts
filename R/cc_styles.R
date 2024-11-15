library(highcharter)

##### Set themes and options ####
# set global options to ensure that comma separator is a comma for highchart graphs
lang <- getOption("highcharter.lang")
lang$thousandsSep <- ","
options(highcharter.lang = lang)

## COLORS ##
# Catalyst California
papaya <- "#F25922"
peridot <- "#CEEA01"
lavender <- "#CEC4E2"
meteorite <- "#3A207D"
cc_colors <- c(meteorite, lavender, papaya, peridot)

black <- "#000000"
alabaster<-"#FBFBFB"
gainsboro <- "#DEDEDE"

# CC-Recommended colors and colors ramps for data analysis
ccblue <- "#0860BC"
divergent_color_ramp <- c("#372278", "#5F3B63","#A8683C", "#D58424", "#FF9900")
grouped_color_ramp <-c(meteorite, papaya, peridot, "#BDAFE9", "#E2D9FF")
group_colors_item <- c(meteorite, papaya, peridot, lavender, ccblue, "#5F3B63","#A8683C", "#D58424", "#FF9900")

## FONTS ##
cc_font <- "Inter"
regular_font_weight <- 400
black_font_weight <- 800
semi_bold_font_weight <- 600

##### highchart theme #####
theme_cc <- hc_theme(
  colors = cc_colors,

  chart = list(
    backgroundColor = alabaster,
    style = list(
      fontFamily = cc_font, # font_subtitle
      color=alabaster)),

  title = list(widthAdjust = -50,
               style = list(
                 color = black,
                 fontFamily = cc_font, # font_title
                 fontWeight = black_font_weight,
                 textAlign="left",
                 fontSize='21px'),
               align = "left"),

  subtitle = list(
    style = list(
      color = black,
      fontFamily = cc_font, # font_subtitle
      fontWeight = regular_font_weight,
      fontSize='14px'),
    align='left'),

  caption = list(
    style = list(
      color = black,
      fontFamily = cc_font, # font_caption
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
        fontFamily = cc_font, # font_x_label
        fontWeight = semi_bold_font_weight,
        width=120,  #argument to modify the width of the labels
        min=0,
        fontSize="10px")),
    lineColor=gainsboro),

  yAxis=list(
    labels=list(
      style=list(
        color=black,
        fontFamily = cc_font, # font_axis_label
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
      fontFamily = cc_font, # font_axis_label
      fontWeight = regular_font_weight,
      color = black,
      fontSize = '12px'),
    itemHoverStyle = list(
      fontFamily = cc_font, # font_table_text
      fontWeight = regular_font_weight,
      color = black),
    borderColor=gainsboro,
    plotLines=list(color=gainsboro)),

  bubbleLegend =  list(
    borderColor=meteorite,
    connectorColor=meteorite,
    marker = list(
      fillColor = lavender)),

  plotOptions = list(
    bubble=list(
      marker=list(
        fillColor=lavender,
        lineColor=meteorite))))
