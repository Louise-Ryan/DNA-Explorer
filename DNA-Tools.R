# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                tags$style(HTML(".shiny-options-group {font-size: 18px;} .shiny-input-radiogroup > label {font-size: 22px;}")),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "DNA Tools",
                  
                  tabPanel("Reverse Complement",
                           radioButtons(
                             "sequence_type",
                             "Sequence type:",
                             choices = c("DNA", "RNA"),
                             selected = "DNA",
                             inline = TRUE, 
                           ),
                           fluidRow(
                             column( width = 12,
                             tags$h3("Input Sequence:"),
                             textAreaInput("txt1", "Nucleotide sequence:", "", width="100%"),
                             actionButton("submit", "Submit"))
                           ), 
                           
                           fluidRow(
                             column(width = 12,
                             h3("Reverse Complement:"),
                             verbatimTextOutput("txtout"))
                             
                           ) 
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Six Frame Translation", "This panel is intentionally left blank"),
                  tabPanel("Get Subsequence", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function
server <- function(input, output) {
  
  # Run calculation only when Submit is clicked
  sequence_data <- eventReactive(input$submit, {
    
    # Get input sequence
    text <- input$txt1
    
    # Separate FASTA header and sequence if present
    if (grepl("^>", text)) {
      header <- sub("[\r\n].*$", "", text)
      
      if (grepl("[\r\n]", text)) {
        sequence <- sub("^[^\r\n]*[\r\n]+", "", text)
      } else {
        sequence <- ""
      }
      
    } else {
      header <- ">Reverse_Complement"
      sequence <- text
    }
    
    # Check whether a sequence was provided
    if (sequence == "") {
      return("Please enter a nucleotide sequence")
    }
    
    # Remove line breaks and convert to uppercase
    sequence <- gsub("\\s+", "", sequence)
    sequence <- toupper(sequence)
    
    # Get complementary sequence
    if (input$sequence_type == "RNA") {
      complement <- chartr("AUGC", "UACG", sequence)
    } else {
      complement <- chartr("ATGC", "TACG", sequence)
    }
    
    # Get reverse complement
    reverse_complement <- rev(strsplit(complement, "")[[1]])
    
    # Print header and reverse complement
    paste(header, paste(reverse_complement, collapse = ""), sep = "\n")
    
  })
  
  # Display result
  output$txtout <- renderText({
    sequence_data()
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
