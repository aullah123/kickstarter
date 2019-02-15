## app.R ##
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "DataI", icon = icon("book")),
      menuItem("Data Exploration", tabName = "DataE", icon = icon("dashboard")),
      menuItem("Visualizing Data", tabName = "DataV", icon = icon("eye-open", lib = "glyphicon")),
      menuItem("Logistic Regression Model", tabName = "DataR", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "DataI",
              h1("Introductions"),
              h2("Subsection"),
              p("Here I want to this and that...")
      ),
      
      # Second tab content
      tabItem(tabName = "DataE",
              h1("Data E")
      ),
      
      # Third tab content
      tabItem(tabName = "DataV",
              h1("Data V"),
              fluidRow(
                box(plotlyOutput("plot"))
                
                
              )
              
      ),

      # Forth tab content
      tabItem(tabName = "DataR",
              h1("Regression")
      )
    )
  )
)

server <- function(input, output) {
  setwd("/Users/ishraq/Desktop/Project RShiny/Kickstarter")
  Main<-read.csv("Subset of Main.csv",header= T) #Load data 
  
  
  Main$newstate<- ifelse(Main$state=="live" | Main$state=="successful", "successful","fail" )

  output$plot <- renderPlotly({# pplotly#
    p<-ggplot(data=Main,aes(x = newstate,y= usd.pledged, colour=newstate))+geom_bar(stat="identity")
    ggplotly(p)
    
  })
}

shinyApp(ui, server)
