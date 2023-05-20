library(shiny)
library(shinydashboard)
library(htmltools)
library(dplyr)
library(tidyr)
library(forcats)
library(rstanarm)
library(wjake)
library(glue)
library(ggplot2)
library(ggdist)
library(ggtext)
library(showtext)

sysfonts::font_add_google("Source Sans Pro", "Source Sans Pro")

abTestApp <- function() {
  ui <- dashboardPage(
    title = "A/B Testing",
    ## header -----
    dashboardHeader(title = logo_grey_light, titleWidth = 200),
    ## sidebar -----
    dashboardSidebar(collapsed = TRUE, width = 200,
                     sidebarMenu(
                       menuItem("A/B Test", icon = icon("th"),
                                tabName = "menu_top"),
                       menuItem("Github", icon = icon("github"),
                                href = "https://github.com/wjakethompson/ab-test"))),
    ## body -----
    dashboardBody(theme_wjake_shiny,
      posteriorUI("post")
    )
  )
  
  server <- function(input, output, server) {
    posteriorServer("post")
  }
  
  shinyApp(ui, server)
}
