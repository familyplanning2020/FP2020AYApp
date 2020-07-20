#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(tidyr)
library(stringr)

myaydata <- read.csv("Data/MyAYData.csv", stringsAsFactors = FALSE)
myaydata.long <- myaydata %>% gather(Age_Group, Count, Women_ReproductiveAge, Young_Adolescents, Youth, Older_Adolescents, Older_Youth)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AY Data R Applet"),

    # Drop Down List
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        "Select Country",
                        choices = myaydata$Country)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("graph"),
           plotOutput("wide")
        )
    )
)

# Draw Bargraph
server <- function(input, output) {
   
    res_data <- reactive({
        res <- myaydata.long %>% filter(myaydata.long$Country == input$country)
    })
    output$graph <- renderPlot({

        ggplot(res_data(), aes(x=input$country, y= Count, fill = Age_Group)) + geom_bar(position="stack", stat="identity") +
           labs(x="Country", y="Count") + ylim(0, 360000000)
    })
    
    output$wide <- renderPlot({
        ggplot(res_data(), aes(x=Age_Group, y= Count)) + geom_bar(position="stack", stat="identity") +
            labs(x="Country", y="Ages 15 to 49")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
