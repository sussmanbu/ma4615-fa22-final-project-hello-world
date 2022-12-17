library(tidyverse)
library(shiny)
library(DT)
library(dplyr)
library(shiny)
library(tmap)
library(sf)
library(USAboundaries)

usaLat <- 47
usaLon <- -107.6660877
usaZoom <- 3

#Data_Combined <- read_csv(here::here("dataset", "Data_Combined.csv"))
#cont_medicare_coverage <- read_csv(here::here("cont_medicare_coverage.csv"))

lin_reg <- cont_medicare_coverage %>%
  inner_join(Data_Combined, by = c("STUSPS" = "state")) %>%
  relocate(NAME, .before = STATEFP)

which_count <- c("BENE_AVG_RISK_SCRE", "ACUTE_HOSP_READMSN_PCT", "Mean_Discharge", "PW_GPCI", "PE_GPCI", "MP_GPCI", "LOS", "GDP", "premium_avg", "Health_Status")

ui <- fluidPage(
  fluidRow(
    column(6,
           tags$h1("US Map"),
           selectInput(inputId= "which", label = "Variables", choices = which_count),
           tmapOutput(outputId = "tmapMap"))),
  fluidRow(
    column(6, 
           h2("2020 Data"),
           DT::dataTableOutput("mytable")))
  )

server <- function(input, output) {
  output$tmapMap <- renderTmap({
    tm_shape(lin_reg) +
      tm_view(set.view = c(usaLon, usaLat, usaZoom)) +
      tm_polygons(col = input$which, palette = "RdBu", title = "Variable Selected", n = 9) +
      tm_layout(frame.lwd = 17)
  })
  output$mytable = DT::renderDataTable({
    Data_Combined
  })
  }

shinyApp(ui, server)
