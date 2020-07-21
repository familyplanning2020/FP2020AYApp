#hello, this is mabelle. so sorry. very bad at commenting code. if you download this and are confused, i am very sorry
#will comment when i have the chance

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(formattable)
library(plotly)

?geom_bar

aypopdata <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "AYPOP")
aypopdata.long <- aypopdata %>% gather(Age_Group,Count,`Young Adolescents (10-14)`,`Older Adolescents (15-19)`,`Older Youth (20-24)`)

kle_age <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "KLEAgeEvents")

kle_marriage <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "KLEMarriage")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AY Data R Applet"),

    # Drop Down List
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        "Select Country",
                        choices = aypopdata.long$Country)
        ),

        # Shows the plots created in server function
        mainPanel(
           plotOutput("graph"),
           plotOutput("wide")
        )
    )
)

# Draw Bargraphs and Figures
server <- function(input, output) {
   
   #createa reactive data set to build bar graph with
    res_data <- reactive({
        res <- aypopdata.long %>% filter(aypopdata.long$Country == input$country)
    })
    
    small_res <- reactive({
        res1 <- aypopdata %>% filter(aypopdata$Country == input$country)
    })
    
    #designs the bar graphs
    output$graph <- renderPlot({
        
        bar_one <- (ggplot(res_data(), aes(Country, Count, fill = Age_Group)) + geom_bar(stat = "identity")
        + labs(x="Country", y="Ages 15 to 49"))
        
        bar_one + theme_classic() + coord_flip()
    })
   
     #designs the bar graphs
    output$wide <- renderPlot({
    
        bartwo <- (ggplot(small_res(), aes(Country, `Women of Reproductive Age (15-49)`)) + geom_bar(stat = "identity")
                             + labs(x= "Country", y="Count"))
        bartwo + theme_classic() + coord_flip()
    })
    
    
    #timeline
    
}

# Run the application 
shinyApp(ui = ui, server = server)
