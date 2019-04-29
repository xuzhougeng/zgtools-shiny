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
  
  sidebarMenu(
    menuItem("Extract by Position", tabName = "bypos", icon = icon("dna")),
    menuItem("Extract by ID", tabName = "byid", icon = icon("hubspot"))
    
    
  )

  
  
)

dashboardbody <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "bypos",
    
    fluidRow(
    
    shinydashboard::box(fileInput("seqfile", label = "Upload FASTA"),
                        
                        selectizeInput("chr",
                                       label = "Chromosome/Contig",
                                       choice = NULL,
                                       multiple = FALSE,
                                       options = list(
                                         placeholder = "select chr/contig",
                                         maxOptions = 20
                                       )
                                       ),
                        
                        
                        numericInput("start", label = "Start", value = 1),
                        numericInput("end", label = "End", value=200),
                        actionButton("submit1", label = "Extract")
                        
                        ),
    shinydashboard::box(htmlOutput ("seqout")
                        )
    
  )), # end of first tabItem
  
  
  tabItem(tabName = "byid",
          
          fluidRow(
            
            shinydashboard::box(fileInput("seqfile2", label = "Upload FASTA"),
                                
                                selectizeInput("chr2",
                                               label = "ID",
                                               choice = NULL,
                                               multiple = FALSE,
                                               options = list(
                                                 placeholder = "select ID",
                                                 maxOptions = 50
                                               )
                                ),
                                
                                actionButton("submit2", label = "Extract")
                                
            ),
            shinydashboard::box(htmlOutput ("seqout2")
            )
            
          )) # end of second tabItem
  
  
  ) # end of tabItems
  
  
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
    chroms = NULL, # by pos
    chroms2 = NULL, # by id
    ref = NULL, # by pos
    ref2 = NULL, # by id
    chr = NULL, # by pos
    chr2 = NULL,  # by id
    start = NULL,
    end = NULL
  )
  
  # update the selections for by position
  observeEvent({
    input$seqfile
    
  },{
    global_value$chroms <- seqkit_get_name(input$seqfile$datapath)
  })
  
  observe({
    updateSelectizeInput(session,
                         inputId = "chr",
                         choices = global_value$chroms,
                         server = TRUE
    )
  })
  
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
  
  
  # update the selection for by ID
  observeEvent({
    input$seqfile2
  }, {
    global_value$chroms2 <- seqkit_get_name(input$seqfile2$datapath)
  }
  )
  observe({
    updateSelectizeInput(session,
                         inputId = "chr2",
                         choices = global_value$chroms2,
                         server = TRUE
    )
  })
  
  observeEvent(input$submit2,{
    
    global_value$ref2 <- input$seqfile2$datapath
    global_value$chr2 <- input$chr2
    
  })
  
  output$seqout2 <-  renderText({
    
    validate(
      need(! is.null(global_value$ref2), "Submit FASTA first" )
    )
    seq_out <- seqkit_extract_by_id(
      global_value$ref2,
      global_value$chr2 
    )
    seq_out
    
  }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

