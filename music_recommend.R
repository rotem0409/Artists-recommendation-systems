# Load R packages
library(shiny)
library(shinythemes)

data.germany <- read.csv(file="lastfm-matrix-germany.csv")
data.germany <- (data.germany[,!(names(data.germany) %in% c("user"))])
artists <- colnames(data.germany)
CF_rec <- read.csv("CF.csv")
AR_rec <- read.csv("AR.csv")


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$h1("Music Recommendation System"),


                fluidRow(
                  column(4,selectInput(inputId = "Artist", 
                                       label = "Choose an artist that you like:",
                                       choices = artists),
                         h4("You might like the following artists too!")
                  )
                ),
                
                tags$hr(),    
                fluidRow(
                  column(8, 
                         tags$h3("Item-based Collaborative Filtering"),
                         tags$br(), 
                         tableOutput("CF_rec_df"))
                ),
                
                tags$hr(),
                
                fluidRow(
                  column(8, 
                         tags$h3("Association Rules"),
                         tags$br(), 
                         tableOutput("AR_rec_df"))
                )
                
)


# Define server function  
server <- function(input, output) {
  
  output$CF_rec_df <- renderTable({ 
    output$CF_rec_df <- 
      renderTable({rbind(CF_rec[row.names(CF_rec)==input$Artist,2:6])})
  })
  
  output$AR_rec_df <- renderTable({ 
    output$AR_rec_df <- 
      renderTable({rbind(AR_rec[row.names(AR_rec)==input$Artist,2:6])})
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)

