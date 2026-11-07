# =============================================================================
# app.R  --  Meridian Life CI incidence dashboard
# =============================================================================
# Reads the tables saved by 02_incidence.R:
#   output/incidence_overall.csv, output/incidence_by_year.csv, output/incidence_by_age.csv
#
# Run from the project root (the folder that contains output/):
#   shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(dplyr)
library(ggplot2)

read_table <- function(file) read.csv(file.path("output", file), check.names = FALSE)

overall <- read_table("incidence_overall.csv")
by_year <- read_table("incidence_by_year.csv")
by_age  <- read_table("incidence_by_age.csv") %>%
  mutate(`Age Band` = factor(`Age Band`, levels = c("<30", "30-39", "40-49", "50-59", "60+")))

PRODUCT_LABELS <- c("All products" = "All products", "VC20" = "VitalCare (VC20)", "LS30" = "LifeSecure (LS30)")
COLOURS        <- c("All products" = "#14213D", VC20 = "#1B6CA8", LS30 = "#E07A1F")
Y_LABEL        <- "CI claims per 1,000 policy-years"
years          <- range(by_year$`Calendar Year`)


# =============================================================================
# LAYOUT
# =============================================================================
kpi <- function(id, label) {
  div(style = "background:#F3F6FA; border-radius:8px; padding:16px 20px; margin-bottom:16px;",
      div(style = "font-size:34px; font-weight:bold; color:#14213D;", textOutput(id, inline = TRUE)),
      div(style = "color:#6B7280;", label))
}

ui <- fluidPage(
  titlePanel("Meridian Life: critical illness incidence"),
  sidebarLayout(
    sidebarPanel(width = 3,
      checkboxGroupInput("products", "Products",
                         choiceNames = unname(PRODUCT_LABELS), choiceValues = names(PRODUCT_LABELS),
                         selected = names(PRODUCT_LABELS)),
      sliderInput("years", "Calendar years", min = years[1], max = years[2],
                  value = c(2008, years[2]), step = 1, sep = ""),
      numericInput("min_exposure", "Hide years with fewer policy-years than", value = 500, min = 0, step = 100),
      helpText("Incidence = all admitted CI claims (major and early-stage) per 1,000 policy-years.")
    ),
    mainPanel(width = 9,
      fluidRow(
        column(4, kpi("kpi_rate",   "CI claims per 1,000 policy-years (all products)")),
        column(4, kpi("kpi_claims", "admitted CI claims")),
        column(4, kpi("kpi_years",  "policy-years of exposure"))),
      tabsetPanel(
        tabPanel("By age",  br(), plotOutput("age_chart",  height = "380px"), tableOutput("age_table")),
        tabPanel("By year", br(), plotOutput("year_chart", height = "380px"), tableOutput("year_table")),
        tabPanel("By product", br(), tableOutput("overall_table")))
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output) {

  # ---- headline numbers (all products, whole period) ----
  all_products <- filter(overall, Product == "All products")
  output$kpi_rate   <- renderText(sprintf("%.2f", all_products$`Rate per 1,000`))
  output$kpi_claims <- renderText(format(all_products$Claims, big.mark = ","))
  output$kpi_years  <- renderText(format(round(all_products$`Policy Years`), big.mark = ","))

  # ---- filtered data ----
  age_data  <- reactive(filter(by_age, Product %in% input$products))
  year_data <- reactive(filter(by_year, Product %in% input$products,
                               between(`Calendar Year`, input$years[1], input$years[2]),
                               `Policy Years` >= input$min_exposure))

  # ---- by age ----
  output$age_chart <- renderPlot({
    ggplot(age_data(), aes(`Age Band`, `Rate per 1,000`, fill = Product)) +
      geom_col(position = position_dodge(0.8), width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", `Rate per 1,000`)), position = position_dodge(0.8),
                vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = COLOURS, labels = PRODUCT_LABELS) +
      labs(x = "Attained age", y = Y_LABEL, fill = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })
  output$age_table <- renderTable(arrange(age_data(), Product, `Age Band`), digits = 2)

  # ---- by year ----
  output$year_chart <- renderPlot({
    ggplot(year_data(), aes(`Calendar Year`, `Rate per 1,000`, colour = Product)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_colour_manual(values = COLOURS, labels = PRODUCT_LABELS) +
      scale_x_continuous(breaks = function(limits) seq(ceiling(limits[1]), floor(limits[2]), by = 2)) +
      labs(x = "Calendar year", y = Y_LABEL, colour = NULL) +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })
  output$year_table <- renderTable(
    year_data() %>% arrange(desc(`Calendar Year`), Product) %>% mutate(`Calendar Year` = as.integer(`Calendar Year`)),
    digits = 2)

  # ---- by product ----
  output$overall_table <- renderTable(filter(overall, Product %in% input$products), digits = 2)
}

shinyApp(ui, server)
