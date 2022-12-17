library(tidyverse)
library(shiny)

#Data_Combined <- read_csv(here::here("dataset", "Data_Combined.csv"))
#cont_medicare_coverage <- read_csv(here::here("cont_medicare_coverage.csv"))

lin_reg <- Data_Combined %>%
  inner_join(cont_medicare_coverage, by = c("state" = "STUSPS"))

which_count <- c("BENE_AVG_RISK_SCRE", "ACUTE_HOSP_READMSN", "Mean_Discharged", "PW_GPCI", "PE_GPCI", "MP_GPCI", "LOS", "status", "GDP", "Year2019")

ui <- fluidPage(
  titlePanel("Linear Regression"),
  selectInput(inputId= "which", label = "Count", choices = which_count),
  plotOutput("distPlot")
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
        ggplot(aes_string(x = input$which, y = "Medicare_Coverage")) +
      geom_point() + geom_smooth(method = lm)
  }) }

shinyApp(ui = ui, server = server)

