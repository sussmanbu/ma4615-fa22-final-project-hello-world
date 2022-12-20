library(tidyverse)
library(shiny)
library(DT)
library(dplyr)
library(tmap)
library(sf)
library(USAboundaries)

usaLat <- 47
usaLon <- -107.6660877
usaZoom <- 3

#Create a map for the US
epsg_us <- 2163
us_states <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp") %>%
  st_transform(epsg_us)

not_contiguous <-
  c("Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico", "United States Virgin Islands")
us_cont <- us_states %>%
  filter(!(NAME %in% not_contiguous)) %>%
  select(STATEFP, STUSPS, NAME)

#Plot a map colored with medicare coverage level
medicare_data_sum <- read_csv(here::here("medicare_data_sum.csv"))
cont_medicare_coverage <- inner_join(us_cont, medicare_data_sum, by = c("STUSPS" = "state"))

Data_Combined <- read_csv(here::here("Data_Combined.csv")) %>%
  rename(Hierarchical_Condition_Category_Score = BENE_AVG_RISK_SCRE, Hospital_Readmission_Rate = ACUTE_HOSP_READMSN_PCT, Geographic_Practice_Cost_Index = PE_GPCI, Length_of_Stay = LOS, Premium = premium_avg)

Data_Table <- Data_Combined %>%
  select(state, Mean_Medicare_Payment, Mean_Total_Payment, Hierarchical_Condition_Category_Score, Hospital_Readmission_Rate, Geographic_Practice_Cost_Index, Length_of_Stay, GDP, Premium, Health_Status)

lin_reg <- cont_medicare_coverage %>%
  inner_join(Data_Combined, by = c("STUSPS" = "state")) %>%
  relocate(NAME, .before = STATEFP) %>%
  relocate(geometry, .before = STATEFP) %>%
  select(-Mean_Medicare_Payment.y, -Mean_Total_Payment.y) %>%
  rename(Mean_Medicare_Payment = Mean_Medicare_Payment.x, Mean_Total_Payment = Mean_Total_Payment.x)

which_count <- c("Mean_Medicare_Payment", "Mean_Total_Payment", "Hierarchical_Condition_Category_Score", "Hospital_Readmission_Rate", "Geographic_Practice_Cost_Index", "Length_of_Stay", "GDP", "Premium", "Health_Status")

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
    tm_shape(lin_reg, class = "sf") +
      tm_view(set.view = c(usaLon, usaLat, usaZoom)) +
      tm_polygons(col = input$which, palette = "Blues", title = "Variable Selected", n = 9)
  })
  output$mytable = DT::renderDataTable({
    Data_Table
  })
}

shinyApp(ui, server)




