library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

source(file = "source.R")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Global Electricity Production", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Global Level", tabName = "global", icon = icon("globe"),
               
               menuSubItem("Energy sources", tabName = "primary_f"),
               
               menuSubItem("Regions", tabName = "regions"),
               
               menuSubItem("Sub-regions", tabName = "subregions")
           
                 ),
      
            menuItem("Country Level", tabName = "country", icon = icon("map-marker"))
   
      
        )
    
    ),
  dashboardBody(
    
            tabItems(
              tabItem("primary_f",
                      
                      
                      fluidRow(
                        
                        box(plotlyOutput("plot4", height = 400),  width = 9, status = "warning"),
                        
                        box(
                          selectInput("pri_f", "Select energy source:",
                                      choices = unique(global_power_plants$primary_fuel),
                                      selected = "Coal" 
                          ),
                          checkboxGroupInput("filter", "Filter country:",
                          choices = c("China", "United States of America", "India", "Japan", "Russia"))
                          
                          ,width = 3, status = "warning"
                          
                        ),
                        box(DT::dataTableOutput("tbl1", height = 400),  width = 9, status = "warning")
                        
                      )                 
              ),
              tabItem("country",
                      
                    fluidRow(
                        infoBoxOutput("installed", width = 3),
                        infoBoxOutput("generated", width = 3),
                        infoBoxOutput("population", width = 3),
                        infoBoxOutput("green", width = 3)
                        
                      ),
                     
              fluidRow(
                
                box(plotOutput("plot1", height = 400),  width = 9, status = "warning"),
                
                box(
                  selectInput("country", "Select country:",
                                choices = unique(global_power_gwh$country_long),
                                selected = "Kenya" 
                ),"NOTE:",textOutput("note"), width = 3, status = "warning"
                
                  
                ),
                
                box(plotOutput("plot5", height = 400),  width = 9, status = "warning")
                
              )
                  
            ),
      
              tabItem("regions",
      
              fluidRow(
                
                box(plotOutput("plot2", height = 400),  width = 9, status = "warning"),
                
                box(
                  radioButtons("region", "Regions", 
                               choices = c(
                                 "Whole World",
                                 "Asia",
                                 "Europe",
                                 "Africa",
                                 "Americas",
                                 "Oceania"
                               ), selected = "Whole World"
                               
                  ),width = 3, status = "warning"
                ),
                box(DT::dataTableOutput("tbl2", height = 400),  width = 9, status = "warning")
                
              )
            ),
              tabItem("subregions",
                      
                      fluidRow(
                        
                        box(plotOutput("plot3", height = 400),  width = 9, status = "warning"),
                        
                        box(
                          selectInput("subregion", "Select sub-region:",
                                      choices = unique(global_power_gwh$sub.region),
                                      selected = "Sub-Saharan Africa" 
                          ), width = 3, status = "warning"
                        ),
                        box(DT::dataTableOutput("tbl3", height = 400),  width = 9, status = "warning")
                        
                          )
                        )
                      )
                    )
                  )
server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    mix_plot_fn(input$country)
  })
  
  output$plot2 <- renderPlot({
    region_mix_plot(input$region)
  })
  
  output$plot3 <- renderPlot({
    subregion_plot(input$subregion)
  })
  
  filtr <- reactive(input$filter)
  
  output$plot4 <-renderPlotly({
    
  pri_fn(input$pri_f, filtr())
    
  })
  
  output$plot5 <- renderPlot({
    growth_mw(input$country)
  })
  output$population <- renderInfoBox({
    infoBox(
      "Population", pop_fn(input$country), icon = icon("users"),
      color = "maroon", fill = TRUE
    )
  })
  
  output$installed <- renderInfoBox({
    infoBox(
      "Installed capacity", cap_fn(input$country), icon = icon("power-off"),
      color = "orange", fill = TRUE
    )
  })
  
  output$generated <- renderInfoBox({
    infoBox(
      "Generated GWH", gwh_fn(input$country), icon = icon("plug"),
      color = "purple", fill = TRUE
    )
  })
  
  output$green <- renderInfoBox({
    infoBox(
      "Renewable sources", green_fn(input$country), icon = icon("star"),
      color = "green", fill = TRUE
    )
  })
  
  output$note <- renderText({
  
       est_fn(input$country)
    
  })
  output$tbl1 = DT::renderDataTable({
    rank_fn(input$pri_f)
  })
  output$tbl2 = DT::renderDataTable({
    rgn_tbl_fn(input$region)
  })
  
  output$tbl3 = DT::renderDataTable({
    subrgn_tbl_fn(input$subregion)
  })
}

shinyApp(ui, server)