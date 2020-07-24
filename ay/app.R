
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(formattable)
library(plotly)
library(gt)
library(plyr)

#Pulling in data from excevl
aypopdata <- read_excel("Data/CleanedAYData.xlsx", sheet = "AYPOP")
aypopdata.long <- aypopdata %>% gather(Age_Group,Count,`Young Adolescents (10-14)`,`Older Adolescents (15-19)`,`Older Youth (20-24)`)

kle_age <- read_excel("Data/CleanedAYData.xlsx", sheet = "KLEAgeEvents")

kle_marriage <- read_excel("Data/CleanedAYData.xlsx", sheet = "KLEMarriage")

ayfp <- read_excel("Data/CleanedAYData.xlsx", sheet = "AYFPUse")

ayfp_sex <- ayfp %>% select(2,6,7)
#ayfp_never_sex <- ayfp %>% select(2,4,5)

data.frame(colnames(ayfp))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AY Data R Applet"),

    # Drop Down List
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        "Select Country",
                        choices = as.list(aypopdata.long$Country))
        ),

        # Shows the plots created in server function
        mainPanel(
          # plotOutput("graph", width = "75%", height = "100px"),
          # plotOutput("wide", width = "75%", height = "100px"),
          # plotOutput("linegraph", width = "75%", height = "200px"),
           plotOutput("sex_activity_graph", width = "50%", height = "100px"),
           plotOutput("never_sex_graph", width = "50%", height = "100px"),
           plotOutput("mod_con", width = "50%", height = "200px"),
           plotOutput("mod_marr",  width = "50%", height = "200px"),
           plotOutput("con_use", width = "50%", height = "100px")
           
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
        #status_levels <- c("First Marriage", "First Sex", "First Birth")
        #status_colors <- c("#0070C0", "#00B050", "#FFC000")
        #df$status <- factor(res2.long$Event, levels=status_levels, ordered=TRUE)
        res2.long
    })
    
    kle_mar_res <- reactive({
        res3 <- kle_marriage %>% filter(kle_marriage$Country == input$country)
        res3
    })
    
    #NEW PLOT
    ayfp_sex_res <- reactive({
      
        res4 <- ayfp %>% select(2,6,7)  %>% filter(ayfp$Country == input$country)
        res4$"15-19" <- res4$"Recent sex older adolescents aged 15-19"
        res4$"20-24" <- res4$"Recent sex older youth aged 20-24"
        res4 <- res4[,-1:-3]
        res4.long <- res4 %>% gather("Age.Group", "Percent", "15-19" , "20-24")
        res4.long
        
    })
    
    output$sex_activity_graph <- renderPlot({
        fig <- (ggplot(ayfp_sex_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
        fig + coord_flip() + theme_classic() + 
            geom_text(aes(label=`Percent`), color="black", size=3.5) + 
            labs(title = "Sexually Active %") + theme(axis.line.y=element_blank(),
                                                    axis.text.y=element_blank(),
                                                    axis.title.x=element_blank(),
                                                    axis.title.y=element_blank(),
                                                    axis.ticks.y=element_blank(),
                                                    axis.text.x =element_blank(),
                                                    axis.ticks.x =element_blank(),
                                                    axis.line.x =element_blank(),
                                                    legend.position = "bottom")
          
    })
    #END PLOT
    
    
    #NEW PLOT
    ayfp_never_res <- reactive({
      res5 <- ayfp %>% select(2,4,5) %>% filter(ayfp$Country == input$country)
      ayfp_never_res
      res5$"15-19" <- res5$"Never have had sex older adolescents aged 15-19"
      res5$"20-24" <- res5$"Never have had sex older youth aged 20-24"
      res5 <- res5[,-1:-3]
      res5.long <- res5 %>% gather("Age.Group", "Percent", "15-19" , "20-24")
      res5.long
    })
    
    output$never_sex_graph <- renderPlot({
      fig <- (ggplot(ayfp_never_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
                                                                                                          
      fig + coord_flip() + theme_classic() + 
        geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(title= "Never Had Sex %") + theme(axis.line.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.title.x=element_blank(),
                                               axis.title.y=element_blank(),
                                               axis.ticks.y=element_blank(),
                                               axis.text.x =element_blank(),
                                               axis.ticks.x =element_blank(),
                                               axis.line.x =element_blank(),
                                               legend.position = "bottom")

    })
    #END PLOT
    
  

    #designs the bar graphs
    output$graph <- renderPlot({
        
        bar_one <- (ggplot(ay_res(), aes(Country, Count, fill = Age_Group)) + geom_bar(stat = "identity")
        + labs(x="Country", y="Women of Reproductive Age (15 to 49)"))
        
        bar_one + theme_minimal() + coord_flip() 
        
    })
   
     #designs the bar graphs
    output$wide <- renderPlot({
    
        bartwo <- (ggplot(small_ay_res(), aes(Country, `Women of Reproductive Age (15-49)`)) + geom_bar(stat = "identity")
                             + labs(x= "Country", y="Count"))
        bartwo + theme_classic() + coord_flip()
    })
    
    
    #timeline of key life events
    output$linegraph <- renderPlot({
        # age_buffer <- 2
        # age_range <- seq(min(kle_age_res$Age) - (age_buffer), max(kle_age_res$Age) + (age_buffer), by=1)
        # age_df <- data.frame(age_range)
        # 
        
        #Create Plot
        timeline_plot<- ggplot(kle_age_res(), aes(x=Age,y=0, label=Event))
        timeline_plot<- timeline_plot + 
            theme_classic() + 
            labs(col="Events")
            #scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
        
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
    
    

    #NEW PLOT
    ayfp_mod_res <- reactive({
      res <- ayfp %>% select(2,8, 9) %>% filter(ayfp$Country == input$country)
      res$`15-19` <- res$`MCPR for unmarried sexually active adolescents (15-19)**`
      res$`20-24` <- res$`MCPR for unmarried sexually active youth (20-24)**`
      res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24")
      return(res.long)
    })

    output$mod_con <- renderPlot({
      fig <- (ggplot(ayfp_mod_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))

      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(title= "Modern Contraceptive Use %", subtitle = "Unmarried Sexually Active %") + 
        theme(axis.line.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.title.x=element_blank(),
                                               axis.title.y=element_blank(),
                                               axis.ticks.y=element_blank(),
                                               axis.text.x =element_blank(),
                                               axis.ticks.x =element_blank(),
                                               axis.line.x =element_blank(),
                                               legend.position = "bottom")

    })
    #END PLOT
    
    ### NEW PLOT Modern Contraceptive Use: Married Women
    #Notes: Fix Decimal Points
    
    ayfp_mod_marr<- reactive({
      res <- ayfp %>% select(2,10, 11, 12) %>% filter(ayfp$Country == input$country)
      res$`15-19` <- res$`MCPR for married adolescents (15-19)`
      res$`20-24` <- res$`MCPR for married youth (20-24)`
      res$`15-24` <- res$`MCPR for married adolescent and youth (15-24)`
      res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24", "15-24")
      return(res.long)
    })
    
    output$mod_marr <- renderPlot({
      fig <- (ggplot(ayfp_mod_marr(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + labs(subtitle = "Married Women %") +
        theme(axis.line.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x =element_blank(),
              axis.ticks.x =element_blank(),
              axis.line.x =element_blank(),
              legend.position = "bottom")
    })
    
    #END PLOT
    
    #NEW PLOT
    ayfp_con_res<- reactive({
      res <- ayfp %>% select(2,29) %>% filter(ayfp$Country == input$country)
      res$`15-24` <- res$`Condom use during last sex: 15-24 year olds`
      res.long <- res %>% gather("Age.Group", "Percent", "15-24")
      return(res.long)
    })
    
    output$con_use <- renderPlot({
      fig <- (ggplot(ayfp_con_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + labs(subtitle = "Condom Use During Last Sex %") +
        theme(axis.line.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x =element_blank(),
              axis.ticks.x =element_blank(),
              axis.line.x =element_blank(),
              legend.position = "bottom")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
