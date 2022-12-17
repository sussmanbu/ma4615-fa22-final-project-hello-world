library(tidyverse)
library(shiny)




lin_reg <- read_csv(here::here("dataset", "Training.csv"))



library(tidyverse)
library(shiny)
df <- read_csv(here::here("dataset/training.csv"))
year <- Data %>% pull(year) %>% unique() %>% sort()
which_count <- c("PW_GPCI", "PE_GPCI", "MP_GPCI", "LOS", "status", "GDP", "Year2019")
ui <- fluidPage(
  titlePanel("Linear Regression"),
  selectInput(inputId = "year", label = "year", choices = year),
  selectInput(inputId= "which", label = "Count", choices = which_count),
  plotOutput("distPlot")
)
server <- function(input, output, session) {
  year <- reactive({
    filter(df, state == input$year)
  })
  observeEvent(state(), {
    choices <- unique(state()$city)
    updateSelectizeInput(inputId = "city", choices = choices, selected = "Boston", options = list(maxItems = 9999))
  })
  
  output$distPlot <- renderPlot({
    Data %>% filter(year == input$year) %>%
      ggplot(aes_string(x = input$which, y = "Medicare_Coverage")) +
      geom_point() + geom_smooth(method = lm) + labs(x = "Length of Stay (days)", y = "Medicare Coverage (%)")
  }) }
shinyApp(ui = ui, server = server)

