#hello, this is mabelle. so sorry. very bad at commenting code. if you download this and are confused, i am very sorry
#will comment when i have the chance

library(shiny)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(tidyr)
library(stringr)
library(readxl)
library(formattable)

?geom_bar

#myaydata <- read.csv("ay/Data/MyAYData.csv", stringsAsFactors = FALSE)
#myaydata.long <- myaydata %>% gather(Age_Group, Count, Women_ReproductiveAge, Young_Adolescents, Youth, Older_Adolescents, Older_Youth)

aypopdata <- read_excel("Data/CleanedAYData.xlsx", sheet = "AYPOP")
aypopdata.long <- aypopdata %>% gather(Age_Group,Count,`Women of Reproductive Age (15-49)`,`Young Adolescents (10-14)`,`Older Adolescents (15-19)`,`Older Youth (20-24)`,`Youth (15-24)`)

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

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("graph")
           #plotOutput("wide")
        )
    )
)

# Draw Bargraph
server <- function(input, output) {
   
    # res_data <- reactive({
    #     res <- myaydata.long %>% filter(myaydata.long$Country == input$country)
    # })
    # 
    res_data <- reactive({
        res <- aypopdata.long %>% filter(aypopdata.long$Country == input$country)
    })
    output$graph <- renderPlot({

        # ggplot(res_data(), aes(x=input$country, y = Count, fill = Age_Group)) + geom_bar(position="stack", stat="identity") +
        #    labs(x="Country", y="Count")
        
        onebar <- (ggplot(res_data(), aes(Country, Count, fill = Age_Group)) + geom_bar(stat = "identity")
        + labs(x="Country", y="Ages 15 to 49"))
        
        onebar + theme_classic()

        # onebar <- (ggplot(res_data(), aes(Age_Group, Count)) + geom_bar(stat = "identity")
        #            + labs(x= "Country", y="Count"))

    })
 
    # output$wide <- renderPlot({
    #     ggplot(res_data(), aes(x=Age_Group, y= Count)) + geom_bar(position="stack", stat="identity") +
    #         labs(x="Country", y="Ages 15 to 49")
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
