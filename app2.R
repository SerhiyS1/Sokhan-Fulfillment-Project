#First, I began by loading in the necessary packages for my app.
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)

#In the line below, I read in the fulfillment_data.rds file that I created in my R Markdown using read_rds, and
#named this data.
data <- read_rds("fulfillment_data.rds")

# Define UI for application that creates a barplot.  I used the shinythemes package and used the shinytheme
# "darkly" to make my app more visually appealing.  The following link was very useful for learning about the 
# shinythemes package and the themes it offers: https://rstudio.github.io/shinythemes/ 
ui <- fluidPage(theme = shinytheme("darkly"),

   # Application title.  In the line below, I named my application.  I started my analysis with November 2017, 
   # which is when I first took over as the Web Fulfillment Manager.  I ended it with October 2018 because 
   # it was the last month that I was fully in charge of the Web Fulfillment Team. Starting with November 2018,
   # my successor began taking over responsibilities.
   navbarPage("The Harvard Shop Web Fulfillment Data, 11/17 - 10/18",
    
   # In the code below, I created the first tab of my Shiny App.  I titled it "Introduction", and used h3() and
   # h5() to insert text of the sizes that were most clear and visually appealing.  I was aware of my audience
   # and knew that not many people would know what web fulfillment is, so I started off the introduction page
   # with a basic definition of web fulfillment.  From class and guest lectures, we learned how important 
   # presentation is in the field of data visualization.  With this in mind, I started off with a sentence that
   # draws people in.  Further on in the introduction, I explain why this analysis is both relevant and 
   # significant.  
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
      # Since this project will enter my professional portfolio, I knew that I should provide a link to the 
      # Github repo for this project.  As we have learned many times in class, the ability to have clean and 
      # organized Github repositories is necessary in the professional world.  I used the following link to 
      # learn how to create a hyperlink: https://shiny.rstudio.com/articles/tag-glossary.html
            h3("GitHub Repository Link"),
            h5("To access my code and data,"),
            tags$a(href="https://github.com/SerhiyS1/Sokhan-Fulfillment-Project", "Click here!")
            ),          
              
   #In the code below, I created my second tab, which will allow the user to analyze the orders per month by 
   #country.  
   tabPanel("Orders Per Month By Country", 
   
   # In the code below, I created a sidebar that allows users to choose the country that they want to see the 
   # number of orders per month for.
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "billing_country",
                     label = "Country:",
    # Originally, I manually typed in every single country under choices, which took a lot of time and double
    # checking to ensure that I did not miss a country.  This also took up many lines of code and did not look
    # organized.  I followed the real-world advice from our course and used Google to find a better alternative.
    # Fortunately, I found a very useful link and just had to enter one line of code, as seen below, and used
    # unique() to make the choices in the sidebar every single country. The link is below:
    # https://stackoverflow.com/questions/42957746/shiny-create-reactive-filter-using-different-variables
                     choices = unique(data$billing_country),
    # I made the automatically selected option the United States, as most of my target audience for this project
    # resides in the U.S. and The Harvard Shop sells more goods to the U.S. than any other country.
                     selected = "United States"),
    # Below, I used different font sizes and tags$b (bold text) to create a legend for the graphic.
         h6("Above, select a country to see the number of orders per month in it. The
                 average order value per month in this country is in the next tab."),
         h4("Legend"),
         tags$b("X-Axis:"),
         h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                 November 2017, when I first took over as the manager."),
         tags$b("Y-Axis:"),
         h6("The Y-Axis is the number of orders.")
      ),
      
      # Showing the plot
      mainPanel(
         plotOutput("distPlot")
      )
   )
),

    #Below, I created my third tab, which will allow the user to see the average order value per month by the
    #country he or she selected in the previous tab.
    tabPanel("Avg. Order Value Per Month By Country", 

           # Showing the plot
           mainPanel(
             plotOutput("distPlot2"),
    #Using h6(), h4(), and tags$b to create a legend for the graphic.     
             h6("Above are the average order values per month for the country selected in the 
                     previous tab."),
             h6("Note: The subtitle of the bar graph is the country selected in the previous tab."),
             h4("Legend"),
             tags$b("X-Axis:"),
             h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                     November 2017, when I first took over as the manager."),
             tags$b("Y-Axis:"),
             h6("The Y-Axis is the average order value in U.S. Dollars."))),
    
    # Below, I created my fourth tab, which is a plot of the total sales per month for all countries.  Although
    # this is not interactive, it still has two main purposes: 1) to allow those interested to see the total 
    # sales per month and 2) to allow my successor to see the total numbers for each month.  This data will
    # allow my predecessor to schedule employee hours based on the how much product is moved per month. 
    # Furthermore, he will be able to identify which months require more or less shipping supplies than we 
    # normally order.
    tabPanel("Total Sales Per Month",
             
             mainPanel(
               plotOutput("plots"),
    #Once again, I used h4(), h6(), and tags$b to create a legend.
               h6("Above are the total sales per month for all countries."),
               h6("Note: The subtitle of the bar graph is the country selected in the previous tab."),
               h4("Legend"),
               tags$b("X-Axis:"),
               h6("The X-Axis is the month of web fulfillment in YYYY-MM format.  It starts with
                       November 2017, when I first took over as the manager."),
               tags$b("Y-Axis:"),
               h6("The Y-Axis is the total sales for all countries in U.S. Dollars.")))
   
))

# Defining server logic required to create my graphics
server <- function(input, output, session) {

# I wanted to create graphics that were interactive.  More specifically, I wanted the selected input country
# from the first tab to automatically be the selected country in the second tab.  After some research, I found
# the following link: https://stackoverflow.com/questions/50852305/one-reactive-function-to-be-displayed-on-two-different-pages-interactively
# The answer on that link showed me how to create a reactive function that is displayed on two different 
# pages interactively.  The key is to use observe() and updateSelectInput.  Thus, whenever an input is selected
# on the first tab, it automatically becomes the selected input on the second tab.
  observe({
    x <- input$`Orders Per Month By Country`
    updateSelectInput(session, "Avg. Order Value Per Month By Country", selected = x)
  })
  
  observe({
    y <- input$`Avg. Order Value Per Month By Country`
    updateSelectInput(session, "Orders Per Month By Country", selected = y)
  })

# In this portion, I had to get the data of just the specific billing countries onto my plots and get 
# the number of orders to come up in my graphics.  I just had to filter the billing country to be the
# the billing country selected in the input.  I also just created a new variable, named num_orders, and took
# the original number of orders variable and changed it to a numeric.
   dataInput <- reactive({ 
     data %>%
       filter(billing_country == input$billing_country) %>%
       mutate(num_orders = as.numeric(orders))
     })
  
# Below, I created the output for my first bar plot, which is in the first tab.    
   output$distPlot <- renderPlot({

# I took the dataInput variable, which I created a few lines above this, and named it final_data for simplicity.    
      final_data <- dataInput()

# I used ggplot() to create the bar plot.  In my geom_bar(), I had to make stat equal to "identity".  If I did 
# not do this, the bar plots would reflect the proportion the number of orders per month make up out of the
# entirety of that country's orders for the year.  Setting stat equal to "identity" in geom_bar() produces
# a bar plot in which the y-values are the actual number of of orders.  I then used labs() to make my labels
# for the x-axis, y-axis, the title, and the subtitle.  In the subtitle, I used print() to have the the country
# the bar plot represents actually in the bar plot.  If a user wants to screenshot the graphic, he or she will
# be able to tell what country the data is for.
      ggplot(data = final_data, aes(x = month, y = num_orders)) +
        geom_bar(stat = "identity", na.rm = FALSE) + 
        labs(x = "Month",
             y = "Number of Orders",
             title = "Orders Per Month by Country",
             subtitle = print(final_data$billing_country)) 
   })
   
# Below, I repeated the steps from a few lines above.  I filtered the billing country to be the the billing 
# country selected in the input.  I then made a variable that gives me the number of orders.
    
      dataInput2 <- reactive({ 
        data %>%
          filter(billing_country == input$billing_country) %>%
          mutate(num_orders = as.numeric(orders))
      })
      
# Below, I created the output for my second bar plot, which is in the second tab.         
      output$distPlot2 <- renderPlot({

#I took the dataInput2 variable, which I created a few lines above this, and named it final_data2 for simplicity.
        final_data2 <- dataInput2()

# Once again, I used ggplot().  This time, the change was that the Y-axis is the average order value.  It was
# more important to print() the country in the subtitle in this chart because I do not have a sidebar on this
# tab in which the user can select the country, so the subtitle is the best way to see which country the bar
# plot is for.  
        ggplot(data = final_data2, aes(x = month, y = average_order_value)) +
          geom_bar(stat = "identity", na.rm = FALSE) + 
          labs(x = "Month",
               y = "Average Order Value ($)",
               title = "Average Order Value Per Month By Country",
               subtitle = print(final_data2$billing_country))
        
   })
      
#In the code below, I created the bar plot for my Total Sales tab.  Once again, I used ggplot().
      output$plots <- renderPlot({
        ggplot(data = data, aes(x = month, y = total_sales)) +
          geom_bar(stat = "identity", na.rm = FALSE) + 
# Since the total sales per month were always in thousands, I wanted to separate the numeric values by commmas
# to make the figures easily readable to my audience.  I used scale_y_continuous() from the scales package and
# made labels equal to comma.  I got this information from the following link:
# https://stackoverflow.com/questions/32427639/include-a-comma-separator-for-data-labels
          scale_y_continuous(labels = comma) +
          labs(x = "Month",
               y = "Total Sales ($)",
               title = "Total Sales Per Month")
      })
}


# Finally, this last line lets us run the application 
shinyApp(ui = ui, server = server)

