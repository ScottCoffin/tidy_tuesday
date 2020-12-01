### Shiny Practice
###Scott Coffin 
### Trainer: Heili Lowman
### September 21, 2020

#Load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)

#Load data
setwd("C:/Users/SCoffin/Documents/OFFLINE/DATA/R/Tidy Tuesday/Shiny Tutorial")
spooky <-read_csv(("spooky_data.csv"))

#Creat the User Interface
##### User Interface #####
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Spooky Shiny App"),
  sidebarLayout(
    sidebarPanel("Put Widgets Here",
    selectInput(inputId = "state_select", # drop down menu
                label = "Choose a state",
                choices = unique(spooky$state)
                ),
    radioButtons(inputId = "region_select", # multiple choice
                 label = "Choose a region:",
                 choices = unique(spooky$region_us_census))
    ),
    mainPanel("Put Outputs Here",
              p("State's top candies:"),
              tableOutput(outputId = "candy_table"), # candy table
                          p("Region's top costumes:"),
                          plotlyOutput(outputId =  "costume_graph") #costume plot
                          ) 
              )
  )
  

#Create the server function: 
##### Server Function #####
server <- function(input, output) {
  
  state_candy <- reactive({
    spooky %>%
      filter(state == input$state_select) %>%
      select(candy,pounds_candy_sold)
  })
  
  output$candy_table <- renderTable({
    state_candy()
  })
  
  region_costume <- reactive({
    spooky %>%
      filter(region_us_census == input$region_select) %>%
      count(costume, rank)
  })
  
  output$costume_graph <- renderPlotly({
    ggplot(region_costume(), aes(x = costume, y = n)) +
      geom_col(aes(fill = rank)) +
      coord_flip() + #horizantal
      scale_fill_manual(values = c("black", "purple", "orange")) +
      theme_minimal()
  })
}

#Combine them into an ap:
shinyApp(ui = ui, server = server)

