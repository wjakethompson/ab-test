#' Run the A/B Test Shiny App
#'
#' @return A shiny app.
#' @export
abTestApp <- function() {
  sysfonts::font_add_google("Source Sans Pro", "Source Sans Pro")
  
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
