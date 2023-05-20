posteriorUI <- function(id) {
  tabItems(
    tabItem(
      tabName = "menu_top",
      fluidRow(
        box(title = HTML("<b>Inputs</b>"), width = 11, solidHeader = TRUE, status = "primary",
            fluidRow(
              column(1, p(HTML("<b>LB</b>"),
                          span(icon("info-circle"), id = "info_lb"),
                          numericInput(NS(id, 'lb'), NULL, .05),
                          tippy::tippy_this(elementId = "info_lb",
                                            tooltip = "Lower bound of expected conversion rates",
                                            placement = "right"))),
              column(11,
                     column(3, p(HTML("<b>Group 1:</b> Visitors (A)"), numericInput(NS(id, 'a1_vis'), NULL, 0))),
                     column(3, p("Conversions (A)", numericInput(NS(id, 'a1_con'), NULL, 0))),
                     column(3, p("Visitors (B)", numericInput(NS(id, 'b1_vis'), NULL, 0))),
                     column(3, p("Conversions (B)", numericInput(NS(id, 'b1_con'), NULL, 0))))
            ),
            fluidRow(
              column(1, p(HTML("<b>UB</b>"),
                          span(icon("info-circle"), id = "info_ub"),
                          numericInput(NS(id, 'ub'), NULL, .95),
                          tippy::tippy_this(elementId = "info_ub",
                                            tooltip = "Upper bound of expected conversion rates",
                                            placement = "right"))),
              column(11,
                     column(3, p(HTML("<b>Group 2:</b> Visitors (A)"), numericInput(NS(id, 'a2_vis'), NULL, 0))),
                     column(3, p("Conversions (A)", numericInput(NS(id, 'a2_con'), NULL, 0))),
                     column(3, p("Visitors (B)", numericInput(NS(id, 'b2_vis'), NULL, 0))),
                     column(3, p("Conversions (B)", numericInput(NS(id, 'b2_con'), NULL, 0))))
            )
        ),
        column(1, actionButton(NS(id, "calculate"), "Calculate!", icon = icon("calculator")))
      ),
      fluidRow(
        box(
          title = HTML("<b>Marginal Results by Condition</b>",), width = 12, solidHeader = TRUE,
          status = "primary",
          column(6, plotOutput(NS(id, "marg_cond_posteriors"))),
          column(6, plotOutput(NS(id, "marg_cond_contrast")))
        )
      ),
      fluidRow(
        box(
          title = HTML("<b>Marginal Results by Group</b>",), width = 12, solidHeader = TRUE,
          status = "primary",
          column(6, plotOutput(NS(id, "marg_group_posteriors"))),
          column(6, plotOutput(NS(id, "marg_group_contrast")))
        )
      ),
      fluidRow(
        box(
          title = HTML("<b>Conditional Results</b>",), width = 12, solidHeader = TRUE,
          status = "primary",
          column(6, plotOutput(NS(id, "conditional_posteriors"),
                               height = "700px")),
          column(6, plotOutput(NS(id, "conditional_contrast"),
                               height = "700px"))
        )
      )
    )
  )
}
