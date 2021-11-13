#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

source('ingr.rec.R')

eng <- readRDS("sim.RDS")

counts <- eng$counts %>%
  {
    \(vec) tibble(x = vec, y = names(vec))
  }()
choices <- fct_reorder(counts$y, counts$x, .desc = TRUE) %>%
  levels()

# predict <- function(input, adventurous = 1) {
#   curr <- input %>%
#     Filter(f = \(x) x %in% colnames(sim))
# 
#   curr_row <- sim %>%
#     colnames() %>%
#     map_lgl(~ . %in% curr)
# 
#   sims <- sim[curr_row, ] %>%
#     sqrt()
# 
# 
#   tibble(
#     base = sims - min(sims),
#     spice = adventurous * 0.05 * (log(max(counts$x)) - log(counts$x)),
#     total = base + spice,
#     name = colnames(sim)
#   ) %>%
#     arrange(-total) %>%
#     filter(!(name %in% curr)) %>%
#     head(10)
# }

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Ingredient Recommender"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("ingredients",
        "Ingredients:",
        choices = choices,
        selected = c("olive oil", "sausage", "onion"),
        multiple = TRUE
      ),
      sliderInput("spice",
        HTML("Spice<sup>*</sup> factor:"),
        min = 0,
        max = 2,
        value = 1,
        step = 1,
      ),
      hr(),
      print(HTML("<sup>*</sup> metaphorical spice"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (length(input$ingredients) > 0) {
      predict(eng, input$ingredients, pow = 0.25, adventurous = input$spice) %>%
        mutate(name = fct_reorder(name, total)) %>%
        pivot_longer(c("base", "spice"), names_to = "type", values_to = "val") %>%
        mutate(type = factor(type, levels = c("spice", "base"))) %>%
        ggplot(aes(val, name)) +
        geom_col(aes(fill = type), position = "stack") +
        labs(
          y = "Ingredient",
          x = "Score",
          fill = "Score type"
        ) +
        scale_fill_manual(values = c("#fc0356", "#03befc"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
