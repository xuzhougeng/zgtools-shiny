#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("scripts/seqkit.R")

options(shiny.maxRequestSize=500*1024^2) 

dashboardsidebar <- dashboardSidebar(

  
  
)

dashboardbody <- dashboardBody(
  
  fluidRow(
    
    shinydashboard::box(fileInput("seqfile", label = "Upload FASTA"),
                        textInput("chr", label = "Chromosome/Contig"),
                        numericInput("start", label = "Start", value = 1),
                        numericInput("end", label = "End", value=200),
                        actionButton("submit1", label = "Extract")
                        
                        ),
    shinydashboard::box(htmlOutput ("seqout")
                        )
    
  )
  
  
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header  = dashboardHeader(title = "Simple Useful Tool"),
  sidebar = dashboardsidebar,
  body    = dashboardbody
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  global_value <- reactiveValues(
    ref = NULL,
    chr = NULL,
    start = NULL,
    end = NULL
  )
  
  
  observeEvent(input$submit1,{
    
    global_value$ref <- input$seqfile$datapath
    global_value$chr <- input$chr
    global_value$start <- input$start
    global_value$end <- input$end
    

    
  })
  
  output$seqout <-  renderText({
    
    validate(
      need(! is.null(global_value$ref), "Submit FASTA first" )
    )
    seq_out <- seqkit_extract_genomic(
      global_value$ref,
      global_value$chr,
      global_value$start,
      global_value$end
      
    )
    seq_out
    
  }
  )
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

