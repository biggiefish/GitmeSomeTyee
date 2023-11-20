# SpatioTemporal Approach

# https://www.paulamoraga.com/book-geospatial/sec-shiny.html#fig:shiny-intro-snapshot-appexample2


# load the shiny package
      library(shiny)
      library(tidyverse)
      library(tidyr)
      library(dplyr)
      library(lubridate)
      library(reshape2)
      library(ggplot2)
      library(readxl)
      library(downloadthis)
    
## Prepare Datasets ----    
    #  Prepare functions
            ## Calculate Decade
            floor_decade    = function(value){ return(value - value %% 10) }
            
            ## Run T-Test
            t_test <- function(x1, x2) {
              test <- t.test(x1, x2)
              
              # use sprintf() to format t.test() results compactly
              sprintf("p value: %0.3f\n[%0.2f, %0.2f]",
                      test$p.value,
                      test$conf.int[1],
                      test$conf.int[2])
            }
        
    # Load the datasets package wich contains the WorldPhones dataset
        # Set working directory
            setwd("C:/Users/evogt/R Analysis/EAV_Analysis/ShinyTestes/SpatioTemporalTutorial")
            dir("Data/")
        # Select File
            catch.dat <- "Data/TyeeCatchData.w.Roster.xlsx"
        # Load File
            raw_catch.data  <- read_excel(catch.dat) # Date, time and size of capture
                colnames(raw_catch.data)
            
        # Format Catch Data #
            raw_catch <- raw_catch.data %>% 
                              mutate(YearF        = as.factor(Year),
                                     decade       = as.factor(floor_decade(Year))) %>%
                              select(decade, YearF, Weight2) %>%
                              rename(year = YearF,
                                     weight = Weight2)
            

            ## Prep dataset that is max weight per year
            raw_catch_max <- raw_catch.data %>% 
                             group_by(Year) %>%
                             summarize(max.weight = max(Weight2)) %>%
                             mutate(decade = as.factor(floor_decade(as.numeric(Year))),
                                    year   = as.factor(Year)) %>%
                             select(decade, year, max.weight)
                              
    
            
            raw_catch2 <- raw_catch%>% 
                              group_by(year) %>%
                              mutate(row = row_number()) %>%
                              tidyr::pivot_wider(names_from = year, values_from = weight) %>%
                              select(-row)

            
## Build Application  ----                    
    # define the user interface object with the appearance of the app
          ui <- fluidPage(
            fluidRow(
              column(4,
                ## Create First Input Name
                selectInput(
                  inputId = "DecadeSelect1",
                  label = "Select First Decade",
                  choices = unique(raw_catch_max$decade),
                  multiple = FALSE)
                  ),
              
              ## Create second input name
              column(4,
              selectInput(
                inputId = "DecadeSelect2",
                label = "Select Second Decade",
                choices = unique(raw_catch_max$decade),
                multiple = FALSE)
                ),
          ), 
           fluidRow(
             column(9,  plotOutput(outputId = "densityplot.decade")),
             column(3,
                    verbatimTextOutput("ttest")),
            

          ),
          

           fluidRow(
                     tabPanel("Data Summary",
                     column(width = 4, tableOutput("x1summary")),
                     column(width = 4, tableOutput("x2summary")))
          ),
          
          fluidRow(
                   tabPanel("Raw Data",
                   column(width = 4, tableOutput("datatableX1")),
                   column(width = 4, tableOutput("datatableX2"))))
)
    # define the server function with instructions to build the
    # objects displayed in the ui
            server <- function(input, output) {
              observeEvent(eventExpr = {
                input$DecadeSelect1
                input$DecadeSelect2
              },
              handlerExpr = {
                output$densityplot.decade <- renderPlot({

                  filter(raw_catch_max,
                         decade %in% c(input$DecadeSelect1, input$DecadeSelect2)) %>%
         
                    ggplot(aes(x = max.weight, colour = decade)) +
                        geom_density() +
                        labs(x = "Weight (lb)", y = "Density") +
                        scale_x_continuous(limits = c(30, 70), breaks = seq(30, 70, 2)) +
                        theme_bw() 
                      
                  
                })

                ## Define variables for T-test
                   x1 <- filter(raw_catch_max, 
                                decade %in% input$DecadeSelect1)  %>% 
                         select(decade, year, max.weight)

                          
                              
                              
                    x2 <- filter(raw_catch_max, 
                                 decade %in% input$DecadeSelect2) %>% 
                          select(decade, year, max.weight)
                    
                
                ## Print T-test
                    output$ttest <- renderText({
                      t_test(x1$max.weight, x2$max.weight)
                      })
                
                ## Summary tables    
                    ## Prep Data
                    x1summary <- raw_catch_max %>% filter(decade %in% input$DecadeSelect1) %>%
                                                   summarise(n    = length(max.weight), 
                                                             Mean = mean(max.weight),
                                                             SD = sd(max.weight)) 
                    
                    x2summary <- raw_catch_max %>% filter(decade %in% input$DecadeSelect2) %>%
                                                   summarise(n    = length(max.weight), 
                                                             Mean = mean(max.weight),
                                                             SD = sd(max.weight)) 
                    
                ## Print
                      # Summary Tables
                    output$x1summary <- renderTable(x1summary)
                    
                    output$x2summary <- renderTable(x2summary)
                                              
                ## Print raw data
                    output$datatableX1 <- renderTable(x1)
                    
                    output$datatableX2 <- renderTable(x2)
              
              })
            }
          
# call shinyApp() which returns the Shiny app object
        shinyApp(ui = ui, server = server)

