library(highcharter)

## COLORS ##
# Fresno BHC
primary_color <- "#053451"
community_safety <- "#FDB913"
primary_color_gradienct <- c("#FFE8B3", "#FFDC8B", "#FED163", "#FEC53B", "#FDB913")
community_safety_gradient <- c("#B4D5FA", "#88ADD0", "#5D85A6", "#315C7B", "#053451")
divergent_color_gradient <- c("#053451", "#2E4A47", "#58603C", "#817732", "#AA8D28", "#D4A31D", "#FDB913")



fbhc_colors <- c(community_safety, primary_color, )

# Neutrals
fbhc_black <- "#0A0A0A"
fbhc_white <-"#FFFFFF"
fbhc_grey <- "#DDDEE2"
fbhc_grey_gradient <- c("#DDDEE2", "#BABBBF", "#98999D", "#75767A", "#525357")

# Fresno BHC Recommended colors and colors ramps for data analysis


## FONTS ##
fbhc_h1_font <- "Source Serif Pro"
fbhc_title_font <- "Montserrat"
fbhc_body_font <- "Open Sans"

regular_font_weight <- 400
black_font_weight <- 800
semi_bold_font_weight <- 600

##### highchart theme #####
theme_fbhc <- hc_theme(
  colors = fbhc_colors,

  chart = list(
    backgroundColor = fbhc_white,
    style = list(
      fontFamily = fbhc_body_font,
      color=fbhc_white)),

  title = list(widthAdjust = -50,
               style = list(
                 color = fbhc_black,
                 fontFamily = fbhc_title_font,
                 fontWeight = black_font_weight,
                 textAlign="left",
                 fontSize='21px'),
               align = "left"),

  subtitle = list(
    style = list(
      color = fbhc_black,
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      fontSize='14px'),
    align='left'),

  caption = list(
    style = list(
      color = fbhc_black,
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      fontSize = "10px",
      textAlign = "left",
      width = 50),
    useHTML = TRUE,
    floating = FALSE),

  xAxis=list(
    labels=list(
      style=list(
        color=fbhc_black,
        fontFamily = fbhc_body_font,
        fontWeight = semi_bold_font_weight,
        width=120,  #argument to modify the width of the labels
        min=0,
        fontSize="10px")),
    lineColor=fbhc_grey),

  yAxis=list(
    labels=list(
      style=list(
        color=fbhc_black,
        fontFamily = fbhc_body_font,
        fontWeight = regular_font_weight,
        fontSize="10px",
        margin = 50)),
    gridLineWidth=0, # removes vertical grid lines
    visible=TRUE, # makes axis line visible
    lineWidth=1,
    lineColor=fbhc_grey,
    min=0,
    tickAmount=6,
    tickWidth=1),

  legend = list(
    itemStyle = list(
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      color = fbhc_black,
      fontSize = '12px'),
    itemHoverStyle = list(
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      color = fbhc_black),
    borderColor=fbhc_grey,
    plotLines=list(color=fbhc_grey),
    bubbleLegend =list(
      borderColor=primary_color,
      connectorColor=primary_color,
      color=community_safety)))

