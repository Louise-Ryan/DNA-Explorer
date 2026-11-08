# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                tags$style(HTML("
                  /* Add padding to bottom of page */
                  body {
                    padding-bottom: 60px;
                  }
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
                  
                  /* Make translation summary container full width */
                  #translation_summary {
                    display: block;
                    width: 100% !important;
                  }
                  
                  /* Make the actual table fill the container */
                  #translation_summary table,
                  table#translation_summary {
                    width: 100% !important;
                    min-width: 100% !important;
                    table-layout: fixed;
                  }
                  
                  /* Translation summary table */
                  #translation_summary {
                    width: 100%;
                    border-collapse: separate;
                    border-spacing: 0;
                    border-radius: 8px;
                    overflow: hidden;
                    background-color: #222;
                    color: #eeeeee;
                    font-size: 14px;
                    margin-top: 15px;
                  }
                  
                  /* Table headings */
                  #translation_summary th {
                    background-color: #6f2555;
                    color: white;
                    font-weight: 600;
                    text-align: center;
                    padding: 14px 12px;
                    border: none;
                    letter-spacing: 0.3px;
                  }
                  
                  /* Table cells */
                  #translation_summary td {
                    text-align: center;
                    padding: 12px;
                    border: none;
                    border-bottom: 1px solid #444;
                  }
                  
                  /* Alternating row colours */
                  #translation_summary tbody tr:nth-child(odd) {
                    background-color: #292929;
                  }
                  
                  #translation_summary tbody tr:nth-child(even) {
                    background-color: #333333;
                  }
                  
                  /* Hover effect */
                  #translation_summary tbody tr:hover {
                    background-color: #492b43;
                    transition: background-color 0.2s ease;
                  }
                  
                  /* First column - frame names */
                  text-align: center;
                  #translation_summary td:first-child {
                    color: white;
                    font-weight: 600;
                  }
                  
                  /* Remove final row border */
                  #translation_summary tbody tr:last-child td {
                    border-bottom: none;
                  }
                  
                ")),
                tags$script(HTML("
                
                $(document).on('click', '#copy_sequence, #copy_translation', function() {
                
                  // Identify which output to copy
                  var output_id = this.id === 'copy_sequence' ? '#txtout' : '#txtout2';
                  var output = document.querySelector(output_id + ' pre');
                
                  if (!output) {
                    alert('Nothing to copy, please submit a sequence!');
                    return;
                  }
                
                  // Extract sequence text
                  var sequence = output.textContent.trim();
                
                  // Copy sequence to clipboard
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
                
                  // Fallback clipboard function
                  function copyFallback(text) {
                    var textarea = document.createElement('textarea');
                    textarea.value = text;
                    textarea.style.position = 'fixed';
                    textarea.style.opacity = '0';
                    document.body.appendChild(textarea);
                    textarea.focus();
                    textarea.select();
                
                    try {
                      var successful = document.execCommand('copy');
                      alert(successful ? 'Sequence copied to clipboard!' : 'Unable to copy sequence.');
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
                             inline = TRUE 
                           ),
                           
                           # Input sequence
                           fluidRow(
                             column(width = 12, 
                                    tags$h3("Input Sequence:"),
                                    textAreaInput("txt1", "Nucleotide sequence:", "", width = "100%", rows = 10),
                                    actionButton("submit_reverse_comp", "Submit")
                             )
                           ), 
                           
                           # Reverse complement output
                           fluidRow(
                             column(width = 12,
                                    
                                    h3("Reverse Complement:"),
                                    uiOutput("txtout"),
                                    
                                    # Copy to Clipboard button
                                    conditionalPanel(
                                      condition = "input.submit_reverse_comp > 0",
                                      div(
                                        style = "text-align: right;",
                                        actionButton("copy_sequence", "Copy to Clipboard"))
                                    ),
                                    
                                    # Reverse complement diagram
                                    h3("How It Works:", style = "margin-top: 40px;"),
                                    
                                    tags$div(
                                      style = "text-align: left; margin-top: 20px; padding-bottom: 100px;",
                                      tags$img(
                                        src = "Reverse-complement.svg",
                                        style = "width: 90%; height: auto;"))))
                  ),
                  
                  #########################
                  # 2. Tool 2: Translation 
                  #########################
                  
                  tabPanel("Six Frame Translation",
                  fluidRow(
                    column( width = 12, 
                            tags$h3("Input Sequence:"),
                            textAreaInput("txt2", "Nucleotide sequence:", "", width="100%", rows = 10),
                            actionButton("submit_translation", "Submit"))
                  ), 
                  # Translation summary
                  conditionalPanel(
                    condition = "input.submit_translation > 0",
                    fluidRow(
                      column(width = 12,
                             h3("Translation Summary:"),
                             tableOutput("translation_summary")))
                    ),
                  fluidRow(
                    column(width = 12,
                           h3("Six Frame Translation:"),
                           uiOutput("txtout2"),
                           conditionalPanel(
                             condition = "input.submit_translation > 0",
                             div(style = "text-align: right;",
                                 actionButton("copy_translation", "Copy to Clipboard")))))
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
      sequence <- chartr("T", "U", sequence)
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
    # Get input sequence
    text <- input$txt2
    
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
    sequence <- chartr("U", "T", sequence)
    nucleotide_length = nchar(sequence)
    
    # Loop through six reading frames
    
    # Initialise all translations
    all_translations <- character()
    
    # Initialise all translations
    all_amino_strings <- character()
    
    # Loop through six reading frames
    for (frame in 1:6) {
      
      # Initialise translated sequence
      translations <- character()
      
      # Frames 1-3: Original sequence
      if (frame <= 3) {
        
        frame_sequence <- sequence
        start <- frame
        
      } else {
        # Frames 4-6: Reverse complement sequence
        complement <- chartr("ATGC", "TACG", sequence)
        
        # Get reverse complement
        reverse_complement <- paste(
          rev(strsplit(complement, "")[[1]]),
          collapse = ""
        )
        
        frame_sequence <- reverse_complement
        start <- frame - 3
      }
      
      # Translate sequence 3 nucleotides at a time
      if (nchar(frame_sequence) >= start + 2) {
        for (i in seq(start, nchar(frame_sequence) - 2, by = 3)) {
          
          codon <- substring(frame_sequence, i, i + 2)
          
          amino_acid <- codon_table$Amino_Acid[
            codon_table$Codon == codon
          ]
          
          translations <- c(translations, amino_acid)
        }
      }
      
      # Combine amino acids into a single string
      translations <- paste(translations, collapse = "")
      
      #Add them to all translations without header
      all_amino_strings <- c(all_amino_strings, translations)
      
      # Add frame header
      #translations <- paste0(">Frame_", frame, "\n", translations)
      
      # Split translation into lines of 80 characters
      wrapped_translation <- vapply(
        seq(1, nchar(translations), by = 80),
        function(x) substr(translations, x, x + 79),
        character(1)
      )
      
      # Add frame header
      translations <- paste0(
        ">Frame_", frame, "\n",
        paste(wrapped_translation, collapse = "\n")
      )
      
      # Store completed translation
      all_translations <- c(all_translations, translations)
      
    }
    
    # Initialise dataframe
    translation_summary <- data.frame(
      Frame = paste0("Frame ", 1:6),
      Nucleotide_Length = rep(nucleotide_length, 6),
      Translated_Length = NA,
      Longest_ORF = NA,
      Stop_Codons = NA,
      Start_Codons = NA
    )
      
    # Read through each translation
    for (i in seq_along(all_amino_strings)) {
      seq_i <- (all_amino_strings[i])
      
      ###################
      #translated length
      ####################
      len <- nchar(seq_i)
      translation_summary$Translated_Length[i] <- len
      
      ###############
      # Longest ORF
      ###############
      orf_lengths <- c()
      
      # Find all start codons
      start_positions <- gregexpr("M", seq_i)[[1]]
      
      # Find all stop codons
      stop_positions <- gregexpr("\\*", seq_i)[[1]]
      
      # Check that both start and stop codons exist
      if (start_positions[1] != -1 && stop_positions[1] != -1) {
        
        # Check each start codon
        for (start in start_positions) {
          
          # Find stop codons occurring after the start
          valid_stops <- stop_positions[stop_positions > start]
          
          if (length(valid_stops) > 0) {
            
            # Get first downstream stop codon
            stop <- min(valid_stops)
            
            # Calculate ORF length (excluding stop codon)
            orf_lengths <- c(orf_lengths, stop - start)
          }
        }
      }
      
      # Store longest ORF
      longest_orf <- if (length(orf_lengths) > 0) {
        max(orf_lengths)
      } else {
        0
      }
      
      translation_summary$Longest_ORF[i] <- longest_orf
      
      ###############################
      # Count in-frame stop codons
      ###############################
      stop_codons <- length(
        gregexpr("\\*", seq_i)[[1]]
      )
      stop_codons <- if (stop_positions[1] == -1) {
        0
      } else {
        length(stop_positions)
      }
      
      translation_summary$Stop_Codons[i] <- stop_codons
      
      ######################
      # Count start codons
      ######################
      start_positions <- gregexpr("M", seq_i)[[1]]
      
      start_codons <- if (start_positions[1] == -1) {
        0
      } else {
        length(start_positions)
      }
      
      translation_summary$Start_Codons[i] <- start_codons
    
    }
    
    #########################
    # Combine all six frames
    #########################
    translations <- paste(all_translations, collapse = "\n")

    # Return translations and summary dataframe
    return(list(
      translations = translations,
      summary = translation_summary
    ))
  })
  
  # Print text at beginning
  output$txtout2 <- renderUI({
    
    if (input$submit_translation == 0) {
      return(tags$pre(
        style = "font-style: italic;",
        "Your translated sequences will appear here."
      ))
    }
    
    tags$pre(translation_data()$translations)
  })
  # Display translation summary
  output$translation_summary <- renderTable({
    summary <- translation_data()$summary
    colnames(summary) <- gsub("_", " ", colnames(summary))
    summary
  }, digits = 0, align = "c")
  
  
  ########################
  # 3. Get subsequence
  ########################
  
  subsequence_data <- eventReactive(input$submit_subsequence, {
    
    # Subsequence code goes here
    
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
