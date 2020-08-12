
list.of.packages <- c("shiny", "dplyr", "ggplot2", "tidyr",
                      "readxl", "formattable", "plotly", "gt",
                      "plyr", "factorial2x2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(formattable)
library(plotly)
library(gt)
library(plyr)
library(factorial2x2)


#Pulling in data from excel
aypopdata <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "AYPOP")
aypopdata.long <- aypopdata %>% gather(Age_Group,Count,`Young Adolescents (10-14)`,`Older Adolescents (15-19)`,`Older Youth (20-24)`)
kle_age <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "KLEAgeEvents")
kle_marriage <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "KLEMarriage")
ayfp <- read_excel("ay/Data/CleanedAYData.xlsx", sheet = "AYFPUse")

# Round the counts of AY popualtion & WRA Population  ==> SHIZA DID THIS <== 
aypopdata.long$Round_Count <- round(aypopdata.long$Count, -5)
aypopdata.long$Round_Count_WRA <- round(aypopdata.long$`Women of Reproductive Age (15-49)`, -5)

#FP2020 Colors
cbp1 <- c("#bdd1ff", "#82d816", "#73d8bf", "#248c85", "#f7bc1b", "#ff7314", "#4fb3ff", "#00158a")


# Define UI for application 
ui <- navbarPage(title = "Adolescent & Youth Population Data Applet",
                 tabPanel("Profile",
                          fluidRow(
                            column(6,
                                   wellPanel(
                                     selectInput("country",
                                                 "Select Country",
                                                 choices = as.list(aypopdata.long$Country))
                                   )
                            ),
                            column(6,
                            )),
                          fluidRow( 
                            titlePanel("Adolescent & Youth Population"),
                            column(8,
                                   plotOutput("graph", width = "100%", height = "150px")    
                            ),
                            column(4,
                                   ),
                            ),
                          
                           fluidRow(
                             column(8,
                                    plotOutput("wide", width = "100%", height = "150px")
                             ),
                             column(4,
                                    ),
                             ),
                          
                          fluidRow(
                            titlePanel("Key Life Events"),
                            column(8,
                                   plotOutput("linegraph", width = "75%", height = "200px")
                                   ),
                            column(4,
                                   tableOutput("table")
                                   ),
                            ),
                          fluidRow(
                            titlePanel("Sexual Activity %"),
                            column(6,
                                   plotOutput("sex_activity_graph", width = "60%", height = "200px")
                            ),
                            column(6,
                                   plotOutput("never_sex_graph", width = "60%", height = "200px")
                            ),
                          ),
                          fluidRow(
                            titlePanel("Modern Contraceptive Prevalence %"),
                            column(4,
                                     plotOutput("mod_con", width = "60%", height = "200px")
                             ),
                            column(4,
                                     plotOutput("mod_marr",  width = "60%", height = "200px")
                            ),
                            column(4,
                                     plotOutput("con_use", width = "60%", height = "200px")
                            )
                          ),
                          fluidRow(
                            titlePanel("Traditional Method Use"),
                            column(6,
                                   plotOutput("trad_unmarr", height = "300px")
                            ),
                            column(6,
                                   plotOutput("trad_marr", height = "300px")
                            )
                          ),
                          fluidRow(
                            titlePanel("More Information"),
                            column(12,
                                   tags$h1(""), "If you woud like to learn more, the A&Y Data Set used to create this App can be found on the ",
                                   tags$a(href = "https://www.familyplanning2020.org/ayfp", "FP2020 Site"),
                                   tags$h2(""), "The code used to create this App can be found on our",
                                   tags$a(href = "https://github.com/mabellezhang/AYData.git", "GitHub Account")
                            ),
                            
                          )
                     ),
                 tabPanel("Compare",
                          fluidRow(
                            column(6,
                                   wellPanel(
                                     selectizeInput("country",
                                                    "Select Up to 4 Countries to Compare",
                                                    choices = as.list(aypopdata.long$Country),
                                                    multiple = TRUE,
                                                    options = list(maxItems = 4))
                                   )
                            ),
                            column(6,
                            )),
                          
                 ),
                 tabPanel("Analyze")
        )

# Draw Bargraphs and Figures
server <- function(input, output) {
   
   #NEW PLOT: Population by Age Groups
    ay_res <- reactive({
        res <- aypopdata.long %>% filter(aypopdata.long$Country == input$country)
        req(nrow(res) > 0)
        res
    })
    
    output$graph <- renderPlot({
      bar_one <- (ggplot(ay_res(), aes(Country, Count, fill = Age_Group)) + geom_bar(stat = "identity") + 
                  geom_text(aes(label=`Round_Count`), color="black", size=3.5, position = position_stack(vjust = 0.5)))
      bar_one + theme_classic() + coord_flip() + labs(subtitle = "Adolescents and Youth") + scale_fill_manual(values = cbp1) + theme(axis.line.y=element_blank(),
                                                                                        axis.text.y=element_blank(),
                                                                                        axis.title.y=element_blank(),
                                                                                        axis.title.x = element_blank(),
                                                                                        axis.ticks.y=element_blank(),
                                                                                        axis.text.x =element_blank(),
                                                                                        axis.ticks.x =element_blank(),
                                                                                        axis.line.x =element_blank(),
                                                                                        legend.position = "right")
      
    })
    #END PLOT
    
    #NEW PLOT: Women of Reproductive Age
    small_ay_res <- reactive({
        res1 <- aypopdata %>% filter(aypopdata$Country == input$country)
        req(nrow(res1) > 0)
        res1
    })

    output$wide <- renderPlot({
      
      bartwo <- (ggplot(small_ay_res(), aes(Country, `Women of Reproductive Age (15-49)`)) +
                   geom_bar(stat = "identity", fill = "#7294D4") + 
                   labs(subtitle = "Women of Reproductive Age (15-49)")  +
                   geom_text(aes(label=Round_Count_WRA), color="black", size=3.50, hjust = 5.0))
      
      bartwo + theme_classic() + coord_flip()  + theme(axis.line.y=element_blank(),
                                                     axis.text.y=element_blank(),
                                                     axis.title.x=element_blank(),
                                                     axis.title.y=element_blank(),
                                                     axis.ticks.y=element_blank(),
                                                     axis.text.x =element_blank(),
                                                     axis.ticks.x =element_blank(),
                                                     axis.line.x =element_blank(),
                                                     legend.position = "bottom")
    
      })
    ### END PLOT
    
    
    #NEW PLOT: Table
    #TO BE COMPLETED
    kle_mar_res <- reactive({
        res3 <- kle_marriage %>% filter(kle_marriage$Country == input$country)
        req(nrow(res3) > 0)
        table1 <- matrix(c(res3[[1,2]], NA, res3[[1,3]], res3[[1,5]], res3[[1,4]], NA, NA, res3[[1,6]]), ncol = 2, byrow = TRUE)
        colnames(table1) <- c("% Married", "% Married before 18 Years Old")
        rownames(table1) <- c("15-19", "20-24", "15-24", "25-29")
        table2 <- as.table(table1)
        
        table2
        
    })
    
    output$table <- renderTable(kle_mar_res())
    
    #NEW PLOT:Recent Sexual Activity 
    ayfp_sex_res <- reactive({
      
        res4 <- ayfp %>% select(2,6,7)  %>% filter(ayfp$Country == input$country)
        req(nrow(res4) > 0)
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
            scale_fill_manual(values = cbp1) + 
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
    
    #NEW PLOT: Ever Sexual Activity 
    ayfp_never_res <- reactive({
      res5 <- ayfp %>% select(2,4,5) %>% filter(ayfp$Country == input$country)
      req(nrow(res5) > 0)
      ayfp_never_res
      res5$"15-19" <- res5$"Never have had sex older adolescents aged 15-19"
      res5$"20-24" <- res5$"Never have had sex older youth aged 20-24"
      res5.long <- res5 %>% gather("Age.Group", "Percent", "15-19" , "20-24")
      res5.long
    })
    
    output$never_sex_graph <- renderPlot({
      fig <- (ggplot(ayfp_never_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
                                                                                                          
      fig + coord_flip() + theme_classic() + 
        geom_text(aes(label=`Percent`), color="black", size=3.5) + scale_fill_manual(values = cbp1) + 
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

    #NEW PLOT
    #timeline of key life events
    kle_age_res <- reactive({
        res2 <- kle_age %>% filter(kle_age$Country == input$country)
        req(nrow(res2) > 0)
        res2.long <- res2 %>% gather(Event, Age, `First Marriage`,`First Sex`, `First Birth`)
        res2.long
    })
    
    output$linegraph <- renderPlot({

        #Create Plot
        timeline_plot<- ggplot(kle_age_res(), aes(x=Age, y=0, col=Event, label=Event))
        timeline_plot<- timeline_plot + 
            theme_classic() + 
            labs(col="Events") + 
            scale_color_manual(values = cbp1)
        
        timeline_plot<- timeline_plot + geom_hline(yintercept=0, color = "black", size=1)
      
        timeline_plot<- timeline_plot + geom_point(aes(y=0), size=6) 
        
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
        timeline_plot <- timeline_plot +  
          geom_text(aes (x = Age, y = -0.05, label = Age), size = 3.5, color = "black") + 
          geom_text(aes (x = Age, y = 0.1, label = ""), size = 3.5) +
          ggtitle("Median Age at First Marriage, Sex and Birth")
        timeline_plot
    })
    
    

    #NEW PLOT
    ayfp_mod_res <- reactive({
      res <- ayfp %>% select(2,8, 9) %>% filter(ayfp$Country == input$country)
      req(nrow(res) > 0)
      res$`15-19` <- res$`MCPR for unmarried sexually active adolescents (15-19)**`
      res$`20-24` <- res$`MCPR for unmarried sexually active youth (20-24)**`
      res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24")
      res.long
    })

    output$mod_con <- renderPlot({
      fig <- (ggplot(ayfp_mod_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))

      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs( subtitle = "Unmarried Sexually Active %") + 
        scale_fill_manual(values = cbp1) + 
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
      req(nrow(res) > 0)
      
      res$`15-19` <- res$`MCPR for married adolescents (15-19)`
      res$`20-24` <- res$`MCPR for married youth (20-24)`
      res$`15-24` <- res$`MCPR for married adolescent and youth (15-24)`
    
      res.long <- res %>% gather("Age.Group", "Percent", "15-19" , "20-24", "15-24")
      res.long
    })
    
    output$mod_marr <- renderPlot({
      fig <- (ggplot(ayfp_mod_marr(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(subtitle = "Married Women %") +
        scale_fill_manual(values = cbp1) + 
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
      req(nrow(res) > 0)
      
      res$`15-24` <- res$`Condom use during last sex: 15-24 year olds`
      res.long <- res %>% gather("Age.Group", "Percent", "15-24")
    
    })
    
    output$con_use <- renderPlot({
      fig <- (ggplot(ayfp_con_res(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(subtitle = "Condom Use During Last Sex %") +
        scale_fill_manual(values = cbp1) + 
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
    
    #TRADITIONAL USE PLOTS START
    #NEW PLOT: UNMARRIED SEXUALLY ACTIVE
    ayfp_trad_unmarr<- reactive({
      res <- ayfp %>% select(2,13,14) %>% filter(ayfp$Country == input$country)
      req(nrow(res) > 0)
      
      res$`15-19` <- res$`% of unmarried sexually active** older adolescents aged 15-19 using a traditional method`
      res$`20-24` <- res$`% of unmarried sexually active** older youth aged 20-24 using a traditional method`
    
      res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24")
      
    })
    
    output$trad_unmarr <- renderPlot({
      fig <- (ggplot(ayfp_trad_unmarr(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(subtitle = "Unmarried Sexually Active %") +
        scale_fill_manual(values = cbp1) + 
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
    
    #NEW PLOT
    ayfp_trad_marr<- reactive({
      res <- ayfp %>% select(2,15,16,17) %>% filter(ayfp$Country == input$country)
      req(nrow(res) > 0)
      
      res$`15-19` <- res$`% of married older adolescents aged 15-19 using a traditional method`
      res$`20-24` <- res$`% of married older youth aged 20-24 using a traditional method`
      res$`15-24` <- res$`% of married youth aged 15-24 using a traditional method`
      
      res.long <- res %>% gather("Age.Group", "Percent", "15-19", "20-24", "15-24")
    })
    
    output$trad_marr <- renderPlot({
      fig <- (ggplot(ayfp_trad_marr(), aes(x= `Age.Group`, y = `Percent`, fill = `Age.Group`)) + geom_bar(stat = "identity"))
      
      fig + coord_flip() + theme_classic() + geom_text(aes(label=`Percent`), color="black", size=3.5) + 
        labs(subtitle = "Married Sexually Active %") +
        scale_fill_manual(values = cbp1) + 
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
