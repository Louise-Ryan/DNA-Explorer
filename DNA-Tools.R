# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                tags$style(HTML("
                  /* Change navbar banner colour */
                  .navbar {
                    background-color: #6f2555 !important;
                    border-color: #6f2555 !important;
                  }
                  /* Change selected tab background colour */
                  .navbar-default .navbar-nav > .active > a,
                  .navbar-default .navbar-nav > .active > a:hover,
                  .navbar-default .navbar-nav > .active > a:focus {
                    background-color: #ca2591 !important;
                    color: white !important;
                  }
                  /* Disable hover effect on DNA Tools title */
                  .navbar-default .navbar-brand:hover,
                  .navbar-default .navbar-brand:focus {
                    background-color: transparent !important;
                    color: white !important;
                    cursor: default;
                  }
                  /* Increase DNA/RNA radio button text size */
                  .shiny-options-group {font-size: 18px;}
                
                  /* Increase Sequence Type heading size */
                  .shiny-input-radiogroup > label {font-size: 22px;}
                
                  /* Set minimum and maximum output box height and enable scrolling */
                  #txtout {min-height: 150px; max-height: 300px; overflow-y: auto;}
                ")),
                tags$script(HTML("
                  $(document).on('click', '#copy_sequence', function() {
                    var output = document.querySelector('#txtout pre');
                
                    if (!output) {
                      alert('Nothing to copy, please submit a sequence!');
                      return;
                    }
                
                  var sequence = output.textContent
                    .replace(/\r\n/g, '\n')
                    .replace(/\n{2,}/g, '\n')
                    .trim();                
                    if (navigator.clipboard && window.isSecureContext) {
                      navigator.clipboard.writeText(sequence)
                        .then(function() {
                          alert('Sequence copied to clipboard!');
                        })
                        .catch(function() {
                          copyFallback(sequence);
                        });
                    } else {
                      copyFallback(sequence);
                    }
                
                    function copyFallback(text) {
                      var textarea = document.createElement('textarea');
                      textarea.value = text;
                      document.body.appendChild(textarea);
                      textarea.select();
                
                      try {
                        document.execCommand('copy');
                        alert('Sequence copied to clipboard!');
                      } catch (error) {
                        alert('Unable to copy sequence.');
                      }
                
                      document.body.removeChild(textarea);
                    }
                  });
                ")),
                
                
                
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
                             textAreaInput("txt1", "Nucleotide sequence:", "", width="100%", rows = 10),
                             actionButton("submit", "Submit"))
                           ), 
                           
                           fluidRow(
                             column(width = 12,
                                h3("Reverse Complement:"),
                                uiOutput("txtout"),
                                conditionalPanel(
                                  condition = "input.submit > 0",
                                  div(style = "text-align: right;",
                                      actionButton("copy_sequence", "Copy to Clipboard")))))
                  ), 
                  # Next panels, tabPanel
                  tabPanel("Six Frame Translation", "This feature will be built soon"),
                  tabPanel("Get Subsequence", "This feature will be built soon")
                  
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
      header <- ""
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
    
    # Wrap sequence at 80 chars
    sequence_string <- paste(reverse_complement, collapse = "")
    sequence_length <- nchar(sequence_string)
    
    starts <- seq(1, sequence_length, by = 80)
    ends <- pmin(starts + 79, sequence_length)
    
    reverse_complement <- paste(
      substring(sequence_string, starts, ends),
      collapse = "\n"
    )
    
    # Check sequence lengths
    print(paste("Original sequence:", nchar(sequence)))
    print(paste("Reverse complement:", sum(nchar(strsplit(reverse_complement, "\n")[[1]]))))
    
    # Print header and reverse complement
    if (header != "") {
      paste(header, reverse_complement, sep = "\n")
    } else {
     reverse_complement
    }
  })
  
  # Print text at beginning
  output$txtout <- renderUI({
    if (input$submit == 0) {
      return(tags$pre(
        style = "font-style: italic;",
        "Your reverse complement sequence will appear here."
      ))
    }
    
    tags$pre(sequence_data())
  })
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
