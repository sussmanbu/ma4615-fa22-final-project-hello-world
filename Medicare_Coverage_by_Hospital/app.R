library(tidyverse) 
library(shiny) 

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
DRG <- medicare_data_clean_int %>% pull(DRG_Desc) %>% unique() %>% sort()
ui <- fluidPage(titlePanel("Medicare Coverage"), 
                selectInput(inputId= "which", label = "Count", choices = which_count), 
                selectInput(inputId= "state", label = "State", choices = states), 
                selectInput(inputId= "city", label = "City", choices = cities),
                selectInput(inputId= "hospital", label = "Hospital", choices = hospitals),
                plotOutput("distPlot")
) 
server <- function(input, output) {
  output$distPlot <- renderPlot({medicare_data_clean_int %>% 
      filter(state == input$state, city == input$city, hospital == input$hospital) %>% 
      ggplot(aes_string(x = input$which, y = "medicare_payment")) + 
      geom_point() + 
      geom_smooth() + 
      scale_y_log10() +
      labs(x = "Total Payment (dollars)", y = "Medicare Payment (dollars)")
  })
}
shinyApp(ui = ui, server = server)



