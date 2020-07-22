#hello, this is mabelle. so sorry. very bad at commenting code. if you download this and are confused, i am very sorry
#will comment when i have the chance

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(formattable)
library(plotly)



aypopdata <- read_excel("Data/CleanedAYData.xlsx", sheet = "AYPOP")
aypopdata.long <- aypopdata %>% gather(Age_Group,Count,`Young Adolescents (10-14)`,`Older Adolescents (15-19)`,`Older Youth (20-24)`)

kle_age <- read_excel("Data/CleanedAYData.xlsx", sheet = "KLEAgeEvents")

kle_marriage <- read_excel("Data/CleanedAYData.xlsx", sheet = "KLEMarriage")


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
           plotOutput("graph", width = "75%", height = "200px"),
           plotOutput("wide", width = "75%", height = "200px"),
           plotOutput("linegraph", width = "75%", height = "200px")
           #plotOutput("marrtable")
        )
    )
)

# Draw Bargraphs and Figures
server <- function(input, output) {
   
   #createa reactive data set to build bar graph with
    ay_res <- reactive({
        res <- aypopdata.long %>% filter(aypopdata.long$Country == input$country)
    })
    
    small_ay_res <- reactive({
        res1 <- aypopdata %>% filter(aypopdata$Country == input$country)
    })
    
    kle_age_res <- reactive({
        res2 <- kle_age %>% filter(kle_age$Country == input$country)
        res2.long <- res2 %>% gather(Event, Age, `First Marriage`,`First Sex`, `First Birth`)
        status_levels <- c("First Marriage", "First Sex", "First Birth")
        status_colors <- c("#0070C0", "#00B050", "#FFC000")
        
        df$status <- factor(res2.long$Event, levels=status_levels, ordered=TRUE)
       
        res2.long
    })
    
   kle_mar_res <- reactive({
       res3 <- kle_marriage %>% filter(kle_marriage$Country == input$country)
   })
    
    #designs the bar graphs
    output$graph <- renderPlot({
        
        bar_one <- (ggplot(ay_res(), aes(Country, Count, fill = Age_Group)) + geom_bar(stat = "identity")
        + labs(x="Country", y="Ages 15 to 49"))
        
        bar_one + theme_classic() + coord_flip()
    })
   
     #designs the bar graphs
    output$wide <- renderPlot({
    
        bartwo <- (ggplot(small_ay_res(), aes(Country, `Women of Reproductive Age (15-49)`)) + geom_bar(stat = "identity")
                             + labs(x= "Country", y="Count"))
        bartwo + theme_classic() + coord_flip()
    })
    
    
    #timeline
    output$linegraph <- renderPlot({
        # age_buffer <- 2
        # age_range <- seq(min(kle_age_res$Age) - (age_buffer), max(kle_age_res$Age) + (age_buffer), by=1)
        # age_df <- data.frame(age_range)
        # 
        
        #Create Plot
        timeline_plot<- ggplot(kle_age_res(), aes(x=Age,y=0, label=Event))
        timeline_plot<- timeline_plot + 
            theme_classic() + 
            labs(col="Events") +
            scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
        
        timeline_plot<- timeline_plot + geom_hline(yintercept=0, color = "black", size=0.3)
        
        # Plot scatter points at zero and date
        timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)
        
        # Don't show axes, appropriately position legend
        timeline_plot<- timeline_plot + theme(axis.line.y=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              axis.ticks.y=element_blank(),
                                              axis.text.x =element_blank(),
                                              axis.ticks.x =element_blank(),
                                              axis.line.x =element_blank(),
                                              legend.position = "bottom"
        )
        
        timeline_plot <- timeline_plot + geom_text(aes (x = Age, y = 0.1, label = Event), size = 2.5) + geom_text(aes (x = Age, y = -0.05, label = Age), size = 2.5)
            #geom_text(data= age_df, aes(x = age_range , y= - 0.01, label = age_range), size=1.5, vjust=0.5, color='black')
        
        timeline_plot <- timeline_plot + ggtitle("Key Life Events")
        
        timeline_plot
    })
    
    # output$marrtable({ 
    #     kle_mar_res
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
