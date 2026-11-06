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
                  id = "selected_tab",
                  
                  #####################################
                  # 1. Tool 1: Get reverse complement
                  #####################################
                  
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
                             actionButton("submit_reverse_comp", "Submit"))
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
                  
                  #########################
                  # 2. Tool 2: Translation 
                  #########################
                  
                  tabPanel("Six Frame Translation",
                  fluidRow(
                    column( width = 12, 
                            tags$h3("Input Sequence:"),
                            textAreaInput("txt2", "Nucleotide sequence:", "", width="100%", rows = 10),
                            actionButton("submit", "Submit")))
                  ), 
         
                  ########################
                  # 3. Get subsequence
                  ########################
                  
                  tabPanel("Get Subsequence", "This feature will be built soon")
                  
                ) # navbarPage
) # fluidPage


# Define server function

# Define server function
server <- function(input, output) {
  
  #####################################
  # 1. Tool 1: Get reverse complement
  #####################################
  
  # Run calculation only when Submit is clicked
  sequence_data <- eventReactive(input$submit_reverse_comp, {
    
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
    
    if (input$submit_reverse_comp == 0) {
      return(tags$pre(
        style = "font-style: italic;",
        "Your reverse complement sequence will appear here."
      ))
    }
    
    tags$pre(sequence_data())
  })
  
  
  #########################
  # 2. Tool 2: Translation
  #########################
  
  codon_table <- data.frame(
    Codon = c(
      "TTT", "TTC", "TTA", "TTG",
      "TCT", "TCC", "TCA", "TCG",
      "TAT", "TAC", "TAA", "TAG",
      "TGT", "TGC", "TGA", "TGG",
      "CTT", "CTC", "CTA", "CTG",
      "CCT", "CCC", "CCA", "CCG",
      "CAT", "CAC", "CAA", "CAG",
      "CGT", "CGC", "CGA", "CGG",
      "ATT", "ATC", "ATA", "ATG",
      "ACT", "ACC", "ACA", "ACG",
      "AAT", "AAC", "AAA", "AAG",
      "AGT", "AGC", "AGA", "AGG",
      "GTT", "GTC", "GTA", "GTG",
      "GCT", "GCC", "GCA", "GCG",
      "GAT", "GAC", "GAA", "GAG",
      "GGT", "GGC", "GGA", "GGG"
    ),
    Amino_Acid = c(
      "F", "F", "L", "L",
      "S", "S", "S", "S",
      "Y", "Y", "*", "*",
      "C", "C", "*", "W",
      "L", "L", "L", "L",
      "P", "P", "P", "P",
      "H", "H", "Q", "Q",
      "R", "R", "R", "R",
      "I", "I", "I", "M",
      "T", "T", "T", "T",
      "N", "N", "K", "K",
      "S", "S", "R", "R",
      "V", "V", "V", "V",
      "A", "A", "A", "A",
      "D", "D", "E", "E",
      "G", "G", "G", "G"
    )
  )
  
  translation_data <- eventReactive(input$submit_translation, {
    
    # Translation code goes here
    
  })
  
  
  ########################
  # 3. Get subsequence
  ########################
  
  subsequence_data <- eventReactive(input$submit_subsequence, {
    
    # Subsequence code goes here
    
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
