library(tidyverse) 
library(shiny) 
library(rlang)

medicare_data <- read_csv(here::here("Medicare_Data.csv") , 
                          col_types = cols( Avg_Mdcr_Pymt_Amt=col_number(), 
                                            Avg_Tot_Pymt_Amt=col_number(), 
                                            Avg_Submtd_Cvrd_Chrg=col_number()))

medicare_data_clean <- medicare_data %>% 
  filter(!grepl('Unknown', medicare_data$Rndrng_Prvdr_RUCA_Desc)) %>%
  drop_na() %>%      
  select(-Rndrng_Prvdr_CCN, 
         -Rndrng_Prvdr_St, 
         -Rndrng_Prvdr_State_FIPS, 
         -Rndrng_Prvdr_Zip5, 
         -Rndrng_Prvdr_RUCA_Desc)
medicare_data_clean_int <- medicare_data_clean %>%
  rename(hospital = Rndrng_Prvdr_Org_Name, city = Rndrng_Prvdr_City, total_discharge = Tot_Dschrgs, state = Rndrng_Prvdr_State_Abrvtn, total_payment = Avg_Tot_Pymt_Amt, medicare_payment = Avg_Mdcr_Pymt_Amt) %>%
  mutate(medicare_coverage = medicare_payment/total_payment)
states <- medicare_data_clean_int %>% pull(state) %>% unique() %>% sort() 
cities <- medicare_data_clean_int %>% pull(city) %>% unique() %>% sort() 
hospitals <- medicare_data_clean_int %>% pull(hospital) %>% unique() %>% sort() 
which_count <- c("total_payment")

ui <- fluidPage(titlePanel("Medicare Coverage"), 
                fluidRow(
                  column(width = 3, wellPanel(
                    helpText("Select which hospital(s) you want to look at or compare."),
                    selectInput(inputId= "which", label = "Count", choices = which_count), 
                    selectInput(inputId= "state", label = "State", choices = states, multiple = TRUE, selected = "MA"), 
                    selectizeInput(inputId= "city", label = "City", choices = NULL),
                    selectizeInput(inputId= "hospital", label = "Hospital", choices = NULL)
                  )
                  ),
                  mainPanel(plotOutput("distPlot")) ))
              

server <- function(input, output, session){
  state <- reactive({
    filter(medicare_data_clean_int, state == input$state)
  })
  observeEvent(state(), {
    choices <- unique(state()$city)
    updateSelectizeInput(inputId = "city", choices = choices, selected = "Boston", options = list(maxItems = 9999))
  })
  city <- reactive({
    req(input$city)
    filter(state(), city == input$city)
    })
  observeEvent(city(), {
    choices <- unique(city()$hospital)
    updateSelectizeInput(inputId = "hospital", choices = choices, selected = "Boston Medical Center Corporation-", options = list(maxItems = 9999))
  })
  output$distPlot <- renderPlot({
    req(input$hospital)
    city() %>% 
      filter(hospital == input$hospital) %>% 
      ggplot(aes_string(x = input$which, y = "medicare_payment", color = "hospital")) + 
      geom_point() + 
      geom_smooth() + 
      scale_y_log10() +
      labs(x = "Total Payment (dollars)", y = "Medicare Payment (dollars)")
  })
}
shinyApp(ui = ui, server = server)


