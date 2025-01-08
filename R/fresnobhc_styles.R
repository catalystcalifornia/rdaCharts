library(highcharter)

## COLORS ##
# [Colors section remains unchanged]

## FONTS ##
fbhc_h1_font <- "source-serif-pro"
fbhc_title_font <- "montserrat"
fbhc_body_font <- "open-sans"

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
      color = fbhc_white
    )
  ),

  title = list(
    style = list(
      color = fbhc_black,
      fontFamily = fbhc_title_font,
      fontWeight = black_font_weight,
      textAlign = "left"
    ),
    align = "left"
  ),

  subtitle = list(
    style = list(
      color = fbhc_black,
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight
    ),
    align = "left"
  ),

  caption = list(
    style = list(
      color = fbhc_black,
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      textAlign = "left"
    ),
    useHTML = TRUE,
    floating = FALSE
  ),

  xAxis = list(
    labels = list(
      style = list(
        color = fbhc_black,
        fontFamily = fbhc_body_font,
        fontWeight = semi_bold_font_weight,
        min = 0
      )
    ),
    lineColor = fbhc_grey,
    title = list(
      style = list(
        fontFamily = fbhc_body_font
      )
    )
  ),

  yAxis = list(
    labels = list(
      style = list(
        color = fbhc_black,
        fontFamily = fbhc_body_font,
        fontWeight = regular_font_weight
      )
    ),
    title = list(
      style = list(
        fontFamily = fbhc_body_font
      )
    ),
    gridLineWidth = 0,
    visible = TRUE,
    lineWidth = 1,
    lineColor = fbhc_grey,
    min = 0,
    tickAmount = 6,
    tickWidth = 1
  ),

  legend = list(
    itemStyle = list(
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      color = fbhc_black
    ),
    itemHoverStyle = list(
      fontFamily = fbhc_body_font,
      fontWeight = regular_font_weight,
      color = fbhc_black
    ),
    borderColor = fbhc_grey,
    plotLines = list(
      color = fbhc_grey
    ),
    bubbleLegend = list(
      borderColor = primary_color,
      connectorColor = primary_color,
      color = community_safety
    )
  ),

  # Add data labels styling
  plotOptions = list(
    series = list(
      dataLabels = list(
        style = list(
          fontFamily = fbhc_body_font,
          fontWeight = regular_font_weight
        )
      )
    )
  ),

  # Add tooltip styling
  tooltip = list(
    style = list(
      fontFamily = fbhc_body_font
    )
  )
)
