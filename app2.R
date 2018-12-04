#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

data <- read_rds("fulfillment_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
   
   # Application title
   navbarPage("The Harvard Shop Web Fulfillment Data, 11/17 - 10/18",
   
   tabPanel("Orders Per Month By Country", 
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "billing_country",
                     label = "Country:",
                     choices = unique(data$billing_country),
                     selected = "United States")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
),

    tabPanel("Avg. Order Value Per Month By Country", 
         
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("distPlot2")

))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    x <- input$`Orders Per Month By Country`
    updateSelectInput(session, "Avg. Order Value Per Month By Country", selected = x)
  })
  
  observe({
    y <- input$`Avg. Order Value Per Month By Country`
    updateSelectInput(session, "Orders Per Month By Country", selected = y)
  })
  
   dataInput <- reactive({ 
     data %>%
       filter(billing_country == input$billing_country) %>%
       mutate(num_orders = as.numeric(orders))
     })
  
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      final_data <- dataInput()
      
      ggplot(data = final_data, aes(x = month, y = num_orders)) +
        geom_bar(stat = "identity", na.rm = FALSE) + 
        labs(x = "Month",
             y = "Number of Orders",
             title = "Orders Per Month by Country",
             subtitle = print(final_data$billing_country)) 
   })
      
   
   
      dataInput2 <- reactive({ 
        data %>%
          filter(billing_country == input$billing_country) %>%
          mutate(num_orders = as.numeric(orders))
      })
      
      
      output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        final_data2 <- dataInput2()
        
        ggplot(data = final_data2, aes(x = month, y = average_order_value)) +
          geom_bar(stat = "identity", na.rm = FALSE) + 
          labs(x = "Month",
               y = "Average Order Value",
               title = "Avg. Order Value Per Month By Country",
               subtitle = print(final_data2$billing_country))
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

