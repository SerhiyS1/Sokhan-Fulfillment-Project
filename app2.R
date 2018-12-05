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
library(scales)

data <- read_rds("fulfillment_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

   # Application title
   navbarPage("The Harvard Shop Web Fulfillment Data, 11/17 - 10/18",
    
   tabPanel("Introduction",
            h3("What Is Web Fulfillment?"),
            h5("People buy a LOT of Harvard apparel.  When an order is placed on theharvardshop.com,
                it is up to the Web Fullfillment team to fulfill online orders.  Why is this relevant?  Well, 
                for the past year, I have been the Web Fulfillment Manager of The Harvard Shop.  With a team of 
                15-20 employees, I was responsible for the packaging, shipping, and customer service of over 
                12,000 orders.  These 12,000 orders were shipped to countries all around the world."),
            h3("What Is the Signifigance of This Analysis?"),
            h5("The analysis presented in this project serves two main purposes.  The first is to allow those
               that are interested to explore the amount of orders and average order value per month for 
               different countries.  The second is to provide my successor with an app that can give him
               insights into which periods of the year are busier and which periods have higher average
               basket values.  As the manager, it is crucial to schedule employees well enough to maintain
               budget while also sticking to our policy of shipping orders out within one business day.
               In addition, it is important to order enough shipping supplies for the month. The 
               quantities and types of supplies needed depend on the amount of orders and the size of the
               average order."),
            h3("Data"),
            h5("The Harvard Shop uses the e-commerce platform Shopify.  Shopify is an avid supporter of using 
              data in business frequently to allow retailers to identify trends and make data-driven 
              decisions.  As such, the platform does a fantastic job of providing clean data that can be 
              exported easily.  I purposely exported and used data that does not reveal any information about
              unique orders or customers.  The dataset is in my GitHub repository, linked
              below."),
            h3("GitHub Repository Link"),
            h5("To access my code and data,"),
            tags$a(href="https://github.com/SerhiyS1/Sokhan-Fulfillment-Project", "Click here!")
            ),          
              
                        
   tabPanel("Orders Per Month By Country", 
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "billing_country",
                     label = "Country:",
                     choices = unique(data$billing_country),
                     selected = "United States"),
         tags$h6("Above, select a country to see the number of orders per month in it. The
                 average order value per month in this country is in the next tab."),
         tags$h4("Legend"),
         tags$b("X-Axis:"),
         tags$h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                 November 2017, when I first took over as the manager."),
         tags$b("Y-Axis:"),
         tags$h6("The Y-Axis is the number of orders.")
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
             plotOutput("distPlot2"),
             
             tags$h6("Above are the average order values per month for the country selected in the 
                     previous tab."),
             tags$h6("Note: The subtitle of the bar graph is the country selected in the previous tab."),
             tags$h4("Legend"),
             tags$b("X-Axis:"),
             tags$h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                     November 2017, when I first took over as the manager."),
             tags$b("Y-Axis:"),
             tags$h6("The Y-Axis is the average order value, in U.S. Dollars."))),
             
    tabPanel("Plot",
             
             mainPanel(
               plotOutput("plots"),
               
               tags$h6("Above are the total sales per month for all countries."),
               tags$h6("Note: The subtitle of the bar graph is the country selected in the previous tab."),
               tags$h4("Legend"),
               tags$b("X-Axis:"),
               tags$h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                       November 2017, when I first took over as the manager."),
               tags$b("Y-Axis:"),
               tags$h6("The Y-Axis is the total sales for all countries, in U.S. Dollars.")))
   
))

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
               y = "Average Order Value ($)",
               title = "Average Order Value Per Month By Country",
               subtitle = print(final_data2$billing_country))
        
   })
      output$plots <- renderPlot({
        ggplot(data = data, aes(x = month, y = total_sales)) +
          geom_bar(stat = "identity", na.rm = FALSE) + 
          scale_y_continuous(labels = comma) +
          labs(x = "Month",
               y = "Total Sales ($)",
               title = "Total Sales Per Month")
      })
}


# Run the application 
shinyApp(ui = ui, server = server)

