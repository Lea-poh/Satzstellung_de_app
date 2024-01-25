#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjqui)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Deutsche Satzstellung"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("story","Suche dir eine Kurzgeschichte aus",c(""), selected=character(0)),
      checkboxInput("check_own_text","Wähle deinen Eigenen Text?"),
      conditionalPanel("input.check_own_text",
                       textInput("own_text", label = "")
                       )
    ),
    mainPanel(
      uiOutput("selection_box"),
      br(),
      br(),
      actionButton("Check","Check"),
      br(),
      # conditionalPanel("input.Check%2==1", textOutput("results")),
      conditionalPanel("input.Check", uiOutput("results")),
      br(),
      br(),
      actionButton("Schummeln","Schummeln"),
      conditionalPanel("input.Schummeln%2==1", textOutput("loesung"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  Texte <- readRDS("Texte.Rds")
  
  updateRadioButtons(session,"story", choiceNames = paste0(Texte$Titel, " - ", Texte$Autor), 
                     choiceValues = c(Texte$Text), selected = character(0))
  
  sentences <- reactive({
    if(!input$check_own_text){
      if(length(input$story) != 0){
        text <- unlist(strsplit(input$story,"\\? |\\. |\\! "))
      }else{
        text <- "Wähle einen Text"
      }
    }else{
      text <- unlist(strsplit(input$own_text,"\\? |\\. |\\! "))
      if(is.null(text) | length(text) == 0){
        text <- "Wähle einen Text"
      }
    }
    text
  })
  
  n_sentences <- reactive({
    length(sentences())
  })
  
  get_words <- function(word_string){
    sample(unlist(strsplit(word_string," ")))
  }
  
  output$selection_box <- renderUI({
    lapply(1:n_sentences(), function(i) {
      orderInput(paste0('satz',i), paste0("Bitte ordnen (",i,")"), items = get_words(sentences()[[i]]), as_source = FALSE, placeholder = 'Hier einfügen ...', style = "width: 100%;")
    })
  })
  
  
  observe({
      out <- sapply(1:n_sentences(),function(i){
        paste0(input[[paste0("satz",i)]], collapse = " ")
      })
    if(sum(sentences() == out) == n_sentences()){
      output$results <- renderUI({
        tags$video(
          width="320", height="240", autoplay = "",
          tags$source(src="banana.mp4", type="video/mp4")
        )
      })
    }else{
      if(input$Check %%2 == 1){
        output$results <- renderUI({
          HTML(paste0(sum(sentences() == out), " von ",n_sentences(), " Sätzen richtig"))
        })
      }else{
        output$results <- renderUI({
          HTML(paste0("Falsch sind Sätze ", paste0(which(sentences() != out), collapse = ", ")))
        })
      }

    }
  })
  
  output$loesung <- renderText({
    if(!input$check_own_text){
      if(length(input$story) != 0){
        out <- out <- input$story
      }else{
        out <- "Wähle einen Text"
      }
    }else{
      out <- input$own_text
      if(is.null(out) | length(out) == 0){
        out <- "Wähle einen Text"
      }
    }
    out
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
