#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
library(janitor)
library(plotly)
library(ggthemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(version = 4,
                     bootswatch = "flatly"
                     ),
 
    # Application title
    titlePanel("Lab Results"),
    navbarPage("",
               tabPanel("Raw Data",
               fluidRow(
                 column(
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("n",
                                 "Number of patients:",
                                 min = 1,
                                 max = 6,
                                 value = 3,
                                 step = 1),
                     actionButton("randomize", label = "Re-select"),
                     
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     plotlyOutput("plot_selected")
                   )
                 ),width = 12))
                        
                        ),
               
               tabPanel("Analysis",
                        
                        fluidRow(
                          
                          column(
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("lab_measure", label = h4("Laboratory measurement"),
                                             choices = list(
                                               "Alanine Aminotransferase" = "ALT",
                                               "C-Reactive Protein" = "CRP",
                                               "Immunoglobulin A" = "IGA"),
                                             selected = "ALT")
                           
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("plot_analysis")
                              )
                            ), width = 12)
                          )
                        
               )
    
))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot_selected <- renderPlotly({
        # generate bins based on input$bins from ui.R
        d_patient <- read_tsv("Random_PatientLevelInfo_2020.tsv")
        
        d_lab <- read_tsv("Random_LabValuesInfo_2020.tsv")
        
        d<- d_patient %>% 
            full_join(d_lab)
        
        d$AVISIT <-  recode(d$AVISIT,
               "SCREENING" = "SCREENING",
               "BASELINE" = "BASELINE",
               "WEEK 1 DAY 8" = "W1",
               "WEEK 2 DAY 15" = "W2",
               "WEEK 3 DAY 22" = "W3",
               "WEEK 4 DAY 29" = "W4",
               "WEEK 5 DAY 36" = "W5"
               ) 
        d$AVISIT<- factor(d$AVISIT,levels = c(
            "SCREENING",
            "BASELINE",
            "W1",
            "W2",
            "W3",
            "W4",
            "W5"
        ))
        
    
        
        #output$value <- renderPrint({ input$action })
        id_set <- unique(d$USUBJID)
        set.seed(input$randomize*rnorm(1))
        id_select <- sample(id_set,input$n)
        p <- d %>% 
            filter(USUBJID %in% id_select) %>% 
          
            ggplot(aes(AVISIT,AVAL,col=USUBJID,group=c(LBTEST)))+
            geom_point()+
            geom_line()+
            facet_grid(ACTARM~LBTESTCD)+
            labs(title = "Raw data of selected patients")+
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 45),
                  axis.title.x=element_blank()
                  ) 
        print(ggplotly(p))
    })
    
    output$plot_analysis <- renderPlotly({
      # generate bins based on input$bins from ui.R
      d_patient <- read_tsv("Random_PatientLevelInfo_2020.tsv")
      
      d_lab <- read_tsv("Random_LabValuesInfo_2020.tsv")
      
      d<- d_patient %>% 
        full_join(d_lab)
      
      d$AVISIT <-  recode(d$AVISIT,
                          "SCREENING" = "SCREENING",
                          "BASELINE" = "BASELINE",
                          "WEEK 1 DAY 8" = "W1",
                          "WEEK 2 DAY 15" = "W2",
                          "WEEK 3 DAY 22" = "W3",
                          "WEEK 4 DAY 29" = "W4",
                          "WEEK 5 DAY 36" = "W5"
      ) 
      d$AVISIT<- factor(d$AVISIT,levels = c(
        "SCREENING",
        "BASELINE",
        "W1",
        "W2",
        "W3",
        "W4",
        "W5"
      ))
      
      
      dwide <- d %>%
        filter(LBTESTCD == input$lab_measure) %>% 
        pivot_wider(values_from = AVAL,names_from = AVISIT,id_cols = c(USUBJID,ACTARM)) %>% 
        mutate("week 1" = W1 - BASELINE,
               "week 2" = W2 - BASELINE,
               "week 3" = W3 - BASELINE,
               "week 4" = W4 - BASELINE,
               "week 5" = W5 - BASELINE
               ) 
      
      
      
      p <- dwide %>% 
        pivot_longer(cols = starts_with("week ")) %>% 
        ggplot(aes(ACTARM,value,col=ACTARM))+
        geom_boxplot()+
        facet_grid(.~name)+
        labs(title = paste0("Lab measurement compared to baseline"," (",input$lab_measure,")"), 
             y = "Measurement - Baseline")+
        theme(legend.title =  element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()
        ) 
      #print(p)
      print(ggplotly(p))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
