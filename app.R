# Load R packages
library(shiny)
library(shinythemes)
#library(dplyr)
library(Biostrings)
library(DT)

# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                tags$head(
                  tags$meta(
                    name = "viewport",
                    content = "width=device-width, initial-scale=1"
                  )
                ),
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

                
                  /* Set minimum and maximum output box height and enable scrolling */
                  #txtout {min-height: 150px; max-height: 300px; overflow-y: auto;}
                  
                   /* Set minimum and maximum output box height and enable scrolling txt3 */
                  #txtout3 {min-height: 150px; max-height: 300px; overflow-y: auto;}
                  
                  
                  /* Reverse complement output box */
                  #txtout {
                    display: block;
                    height: auto;
                    min-height: 0;
                    margin-bottom: 20px;
                  }
                  
                  #txtout pre {
                    display: block;
                    margin-bottom: 0;
                    white-space: pre-wrap;
                    overflow-wrap: break-word;
                  }
                  
                /* Make summary containers full width */
                #translation_summary,
                #bulk_translation_summary,
                #compare_sets_table {
                  display: block;
                  width: 100% !important;
                }
                
                /* Make the actual tables fill their containers */
                #translation_summary table,
                #bulk_translation_summary table,
                #compare_sets_table table,
                table#translation_summary,
                table#bulk_translation_summary,
                table#compare_sets_table {
                  width: 100% !important;
                  min-width: 100% !important;
                  table-layout: fixed;
                }
                
                /* Summary tables */
                #translation_summary,
                #bulk_translation_summary,
                #compare_sets_table {
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
                #translation_summary th,
                #bulk_translation_summary th,
                #compare_sets_table th {
                  background-color: #6f2555;
                  color: white;
                  font-weight: 600;
                  text-align: center;
                  padding: 14px 12px;
                  border: none;
                  letter-spacing: 0.3px;
                }
                
                /* Table cells */
                #translation_summary td,
                #bulk_translation_summary td,
                #compare_sets_table td {
                  text-align: center;
                  padding: 12px;
                  border: none;
                  border-bottom: 1px solid #444;
                }
                
                /* Alternating row colours */
                #translation_summary tbody tr:nth-child(odd),
                #bulk_translation_summary tbody tr:nth-child(odd),
                #compare_sets_table tbody tr:nth-child(odd) {
                  background-color: #292929;
                }
                
                #translation_summary tbody tr:nth-child(even),
                #bulk_translation_summary tbody tr:nth-child(even),
                #compare_sets_table tbody tr:nth-child(even) {
                  background-color: #333333;
                }
                
                /* Hover effect */
                #translation_summary tbody tr:hover,
                #bulk_translation_summary tbody tr:hover,
                #compare_sets_table tbody tr:hover {
                  background-color: #492b43;
                  transition: background-color 0.2s ease;
                }
                
                /* First column */
                #translation_summary td:first-child,
                #bulk_translation_summary td:first-child,
                #compare_sets_table td:first-child {
                  color: white;
                  font-weight: 600;
                  text-align: center;
                }
                
                /* Remove final row border */
                #translation_summary tbody tr:last-child td,
                #bulk_translation_summary tbody tr:last-child td,
                #compare_sets_table tbody tr:last-child td {
                  border-bottom: none;
                }
                
                /* Compare Sets filter inputs */
                #filter_gene,
                #filter_set1,
                #filter_set2,
                #filter_set3 {
                  background-color: #6f2555;
                  color: white;
                  border: 1px solid #8a3970;
                }
                
                /* Dropdown options */
                #filter_set1 option,
                #filter_set2 option,
                #filter_set3 option {
                  background-color: #6f2555;
                  color: white;
                }
                
                /* Placeholder text */
                #filter_gene::placeholder {
                  color: #dddddd;
                  opacity: 1;
                }
                
                /* Input focus */
                #filter_gene:focus,
                #filter_set1:focus,
                #filter_set2:focus,
                #filter_set3:focus {
                  border-color: #ca2591;
                  box-shadow: 0 0 5px #ca2591;
                }
                /* Set dropdowns */
                #filter_set1 + .selectize-control .selectize-input,
                #filter_set2 + .selectize-control .selectize-input,
                #filter_set3 + .selectize-control .selectize-input {
                  background: #6f2555 !important;
                  color: white !important;
                  border: 1px solid #8a3970 !important;
                  box-shadow: none !important;
                }
                
                /* Selected text */
                #filter_set1 + .selectize-control .item,
                #filter_set2 + .selectize-control .item,
                #filter_set3 + .selectize-control .item {
                  color: white !important;
                }
                
                /* Dropdown menu */
                .selectize-dropdown {
                  background: #6f2555 !important;
                  color: white !important;
                  border: 1px solid #8a3970 !important;
                }
                
                /* Dropdown options */
                .selectize-dropdown .option {
                  color: white !important;
                }
                
                /* Hovered option */
                .selectize-dropdown .active {
                  background: #ca2591 !important;
                  color: white !important;
                }
                  
                ")),
                tags$script(HTML("
                
                $(document).on('click', '#copy_sequence, #copy_translation, #copy_bulk_translation', function() {
                
                  // Identify which output to copy
                  var output_id = this.id === 'copy_sequence' ? '#txtout' :
                                  this.id === 'copy_translation' ? '#txtout2' :
                                  this.id === 'copy_bulk_translation' ? '#txtout3' : '';
                  
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
                           tags$h3("Sequence type:"),
                           radioButtons(
                             "sequence_type",
                             label = tags$label(
                               "Please select your sequence type. If you select RNA, uracil (U) will be used instead of thymine (T) and paired with adenine (A).",
                               style = "font-weight: normal;"
                             ),
                             choices = c("DNA", "RNA"),
                             selected = "DNA",
                             inline = TRUE 
                           ),
                           

                           # Input sequence
                           fluidRow(
                             column(width = 12, 
                                    tags$h3("Input Sequence:"),
                                    textAreaInput(
                                      "txt1",
                                      tags$label("Please enter your nucleotide sequence in the text box below. Fasta headers are accepted. Please enter one sequence at a time.",
                                                 style = "font-weight: normal;"),
                                      "",
                                      width = "100%",
                                      rows = 10
                                    ),
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
                                    condition = "input.submit_reverse_comp > 0",
                                    h3("How It Works:", style = "margin-top: 60px;padding-bottom: 20px"),
                                    
                                    tags$div(
                                      style = "text-align: left; margin-top: 20px; padding-bottom: 100px;",
                                      tags$img(
                                        src = "Reverse-complement.svg",
                                        style = "width: 60%; height: auto;"))))
                  ),
                  
                  #########################
                  # 2. Tool 2: Translation 
                  #########################
                  
                  tabPanel("Six Frame Translation",
                  fluidRow(
                    column( width = 12, 
                            tags$h3("Input Sequence:"),
                            textAreaInput(
                              "txt2",
                              tags$label(
                                "Please enter your nucleotide sequence in the text box below. FASTA headers are accepted but subsequently removed.",
                                tags$br(),
                                "Please enter one sequence at a time. RNA sequences will be converted to DNA (U → T).",
                                style = "font-weight: normal;"
                              ),
                              "",
                              width = "100%",
                              rows = 10
                            ),
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
                  # 3. Bulk translation
                  ########################
                  tabPanel("Bulk Translation",
                           fluidRow(
                             column( width = 12, 
                                     tags$h3("Input Sequence:"),
                                     textAreaInput(
                                       "txt3",
                                       tags$label("Please enter your nucleotide dataset. Sequences must be in fasta format (i.e fasta header is required). Multiple sequences accepted.",
                                                  style = "font-weight: normal;"),
                                       "",
                                       width = "100%",
                                       rows = 10
                                     ),
                                     actionButton("submit_bulk_translation", "Submit"))
                           ), 
                           # Translation summary
                           conditionalPanel(
                             condition = "input.submit_bulk_translation > 0",
                             fluidRow(
                               column(width = 12,
                                      h3("Translation Summary:"),
                                      DTOutput("bulk_translation_summary")))
                                      #tableOutput("bulk_translation_summary")))
                           ),
                           fluidRow(
                             column(width = 12,
                                    h3("Translated Sequences:"),
                                    uiOutput("txtout3"),
                                    conditionalPanel(
                                      condition = "input.submit_bulk_translation > 0",
                                      div(style = "text-align: right;",
                                          actionButton("copy_bulk_translation", "Copy to Clipboard")))))
                  ),
                  tabPanel(
                    "Compare Sets",
                    
                    tags$h3("Compare Sets"),
                    tags$p(
                      "Compare two or three sets to identify shared and unique items. Useful for gene names, IDs, genomic coordinates and other lists. Enter each list with one item per line. The results include a Venn diagram and a summary of items shared between lists.",
                      style = "font-weight: normal;"
                    ),
                    
                    tags$h3("Number of Sets:"),
                    radioButtons(
                      "no_sets",
                      label = NULL,
                      choices = c("2 Sets", "3 Sets"),
                      selected = "2 Sets",
                      inline = TRUE
                    ),
                    tags$p(
                      "Please note: This feature accepts lists of items, not FASTA formated sequences.",
                      style = "font-style: italic; color: #8b8b8b;"
                    ),
                    
                    # Layout for two sets
                    conditionalPanel(
                      condition = "input.no_sets == '2 Sets'",
                      fluidRow(
                        column(
                          width = 6,
                          textAreaInput(
                            "set1", "Set 1:",
                            placeholder = "Enter Set 1",
                            width = "100%",
                            rows = 8
                          )
                        ),
                        column(
                          width = 6,
                          textAreaInput(
                            "set2", "Set 2:",
                            placeholder = "Enter Set 2",
                            width = "100%",
                            rows = 8
                          )
                        )
                      )
                    ),
                    
                    # Layout for three sets
                    conditionalPanel(
                      condition = "input.no_sets == '3 Sets'",
                      fluidRow(
                        column(
                          width = 4,
                          textAreaInput(
                            "set1_3", "Set 1:",
                            placeholder = "Enter Set 1",
                            width = "100%",
                            rows = 8
                          )
                        ),
                        column(
                          width = 4,
                          textAreaInput(
                            "set2_3", "Set 2:",
                            placeholder = "Enter Set 2",
                            width = "100%",
                            rows = 8
                          )
                        ),
                        column(
                          width = 4,
                          textAreaInput(
                            "set3", "Set 3:",
                            placeholder = "Enter Set 3",
                            width = "100%",
                            rows = 8
                          )
                        )
                      )
                    ),
                    tags$div(
                      tags$p(
                        "If case sensitive is selected, GENE1 and gene1 will be treated as different entries.",
                        style = "font-style: italic; color: #8b8b8b; margin-top: -10px;"
                      ),
                      checkboxInput(
                        "case_sensitive",
                        "Case-sensitive",
                        value = FALSE
                      )
                    ),
                    actionButton(
                      "submit_compare_sets",
                      "Compare Sets",
                      class = "btn-primary"
                    ),
                    conditionalPanel(
                      condition = "input.submit_compare_sets > 0",
                      fluidRow(
                        column(
                          width = 12,
                          h3("Comparison Results:"),
                          
                          fluidRow(
                            column(
                              width = 6,
                              textInput(
                                "filter_gene",
                                "Search item:",
                                placeholder = "Enter item name or ID..."
                              )
                            ),
                            column(
                              width = 2,
                              selectInput(
                                "filter_set1",
                                "Set 1:",
                                choices = c("All", "Yes", "No"),
                                selected = "All"
                              )
                            ),
                            column(
                              width = 2,
                              selectInput(
                                "filter_set2",
                                "Set 2:",
                                choices = c("All", "Yes", "No"),
                                selected = "All"
                              )
                            ),
                            column(
                              width = 2,
                              conditionalPanel(
                                condition = "input.no_sets == '3 Sets'",
                                selectInput(
                                  "filter_set3",
                                  "Set 3:",
                                  choices = c("All", "Yes", "No"),
                                  selected = "All"
                                )
                              )
                            )
                          ),
                          
                          DTOutput("compare_sets_table")
                        )
                      )
                    )
                  ),
                  
                  tabPanel("Summarise Dataset", "This feature will be built soon"),
                  tabPanel("Get Subsequence", "This feature will be built soon"),
  
                ) # navbarPage
) # fluidPage


# Define server function

# Define server function
server <- function(input, output) {
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
  
  
  #####################################
  # 1. Tool 1: Get reverse complement
  #####################################
  
  # Run only when Submit is clicked
  sequence_data <- eventReactive(input$submit_reverse_comp, {
    
    # Get input sequence
    text <- input$txt1
    
    # Strip header if present
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
    
    # Remove line breaks and convert to uppercase
    sequence <- gsub("\\s+", "", sequence)
    sequence <- toupper(sequence)
    
    # Check whether a sequence was provided
    if (sequence == "") {
      return("Please enter a nucleotide sequence")
    }
    
    # Define valid nucleotide characters
    valid_chars <- if (input$sequence_type == "DNA") {
      "ACGTRYSWKMBDHVN"
    } else {
      "ACGURYSWKMBDHVN"
    }
    
    # Check for invalid characters
    if (!grepl(paste0("^[", valid_chars, "]+$"), sequence)) {
      return("Invalid nucleotide sequence. Please use valid IUPAC nucleotide codes.")
    }
    
    # Check for incorrect nucleotide type
    else if (input$sequence_type == "DNA" && grepl("U", sequence)) {
      return("Invalid DNA sequence. Did you mean to select RNA?")
    }
    
    else if (input$sequence_type == "RNA" && grepl("T", sequence)) {
      return("Invalid RNA sequence. Did you mean to select DNA?")
    }
    
    else {
      
      # Convert to Biostrings object
      if (input$sequence_type == "RNA") {
        sequence <- RNAString(sequence)
      } else {
        sequence <- DNAString(sequence)
      }
      
      # Get reverse complement
      reverse_complement <- reverseComplement(sequence)
      
      # Convert to character string
      sequence_string <- as.character(reverse_complement)
      
      # Wrap sequence at 80 chars
      sequence_length <- nchar(sequence_string)
      
      starts <- seq(1, sequence_length, by = 80)
      ends <- pmin(starts + 79, sequence_length)
      
      reverse_complement <- paste(
        substring(sequence_string, starts, ends),
        collapse = "\n"
      )
      
      # Check sequence lengths
      print(paste("Original sequence:", length(sequence)))
      print(paste("Reverse complement:", nchar(gsub("\\n", "", reverse_complement))))
      
      # Print header and reverse complement
      if (header != "") {
        paste(header, reverse_complement, sep = "\n")
      } else {
        reverse_complement
      }
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
    
    # Remove line breaks and convert to uppercase
    sequence <- gsub("\\s+", "", sequence)
    sequence <- chartr("U", "T", sequence)
    sequence <-toupper(sequence)
    nucleotide_length = nchar(sequence)
    
    
    # Define valid nucleotide characters
    valid_chars <- "ACGTRYSWYKMBDHVN"
 
    if (length(gregexpr(">", text, fixed = TRUE)[[1]]) > 1) {
      return(list(
        translations = "More than one sequence dectected. \nPlease enter one sequence at a time, or check your sequence for misplaced '>' characters.",
        summary = data.frame()
      ))
    }
    
    # Check for invalid characters
    else if (!grepl(paste0("^[", valid_chars, "]+$"), sequence)) {
      return(list(
        translations = "Invalid nucleotide sequence. Please use valid IUPAC nucleotide codes",
        summary = data.frame()
      ))
    }
    
    # Check minimum sequence length
    else if (nucleotide_length < 6) {
      return(list(
        translations = "Please enter a sequence containing at least 6 nucleotides",
        summary = data.frame()
      ))
    }
    
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
  # 3. Bulk Translation
  ########################
  
  bulk_translation_data <- eventReactive(input$submit_bulk_translation, {
    
    # Get input FASTA text
    text <- input$txt3
    
    # Split input into individual lines
    lines <- strsplit(text, "\n")[[1]]
    lines <- trimws(lines)
    
    # Remove empty lines
    lines <- lines[lines != ""]
    
    # Identify FASTA headers
    header_positions <- which(grepl("^>", lines))
    
    # Check that FASTA headers are present
    if (length(header_positions) == 0) {
      return("Please enter sequences in FASTA format")
    }
    # Check that FASTA headers are present
    if (length(header_positions) == 0) {
      return(tags$p(
        "Please enter sequences in FASTA format",
        style = "color: red; font-weight: bold;"
      ))
    }
    
    # Initialise list to store sequences
    fasta_sequences <- list()
    
    # Extract each sequence
    for (i in seq_along(header_positions)) {
      
      # Get header
      header <- lines[header_positions[i]]
      
      # Determine sequence boundaries
      start <- header_positions[i] + 1
      
      if (i < length(header_positions)) {
        end <- header_positions[i + 1] - 1
      } else {
        end <- length(lines)
      }
      
      # Combine sequence lines
      sequence <- paste(lines[start:end], collapse = "")
      
      # Remove whitespace and convert to uppercase
      sequence <- toupper(gsub("\\s+", "", sequence))
      
      # Convert RNA to DNA
      sequence <- chartr("U", "T", sequence)
      
      # Store header and sequence
      fasta_sequences[[i]] <- list(
        header = header,
        sequence = sequence
      )
    }
    
    # Initialise translated sequences
    all_translations <- character()
    
    
    # Extract nucleotide sequences
    sequences <- vapply(
      fasta_sequences,
      function(x) x$sequence,
      character(1)
    )
    
    # Extract FASTA headers
    headers <- vapply(
      fasta_sequences,
      function(x) x$header,
      character(1)
    )
    
    # Remove empty sequences
    valid_sequences <- sequences != ""
    sequences <- sequences[valid_sequences]
    headers <- headers[valid_sequences]
    
    
    # Remove empty sequences
    valid_sequences <- sequences != ""
    sequences <- sequences[valid_sequences]
    headers <- headers[valid_sequences]
    
    # Convert RNA to DNA
    sequences <- chartr("U", "T", sequences)
    
    # Check for invalid characters
    valid_chars <- "ACGTRYSWKMBDHVN"
    
    if (any(!grepl(paste0("^[", valid_chars, "]+$"), sequences))) {
      return(list(
        translations = "Invalid nucleotide sequence detected in dataset.\nPlease use valid IUPAC nucleotide codes only.\nAmbiguous character or gaps can be denoted by 'N",
        summary = data.frame()
      ))
    }
    
    # Create DNAStringSet
    dna_sequences <- DNAStringSet(sequences)
    
    # Retain FASTA headers
    names(dna_sequences) <- headers
    
    # Translate sequences
    translations <- translate(
      dna_sequences,
      if.fuzzy.codon = "X"
    )
    
    # Initialise and populate summary dataframe
    bulk_translation_summary <- data.frame(
      Sequence = paste0("Seq_", seq_along(dna_sequences)),
      Nucleotide_Length = width(dna_sequences),
      Amino_Acid_Length = width(translations),
      Begins_With_Start_Codon = width(dna_sequences) >= 3 &
        as.character(subseq(dna_sequences, start = 1, width = 3)) == "ATG",
      Ends_With_Stop_Codon = vapply(
        as.character(translations),
        function(x) endsWith(x, "*"),
        logical(1)
      ),
      In_Frame_Stop_Codons = NA_integer_
    )
    
    # Count internal stop codons
    for (i in seq_along(translations)) {
      
      # Extract translated sequence
      amino_acids <- as.character(translations[[i]])
      
      # Count all stop codons
      stop_count <- sum(strsplit(amino_acids, "")[[1]] == "*")
      
      # Exclude terminal stop codon if present
      if (endsWith(amino_acids, "*")) {
        stop_count <- stop_count - 1
      }
      
      bulk_translation_summary$In_Frame_Stop_Codons[i] <- stop_count
    }
    
    # Initialise translated sequences
    all_translations <- character()
    
    # Format translated sequences
    for (i in seq_along(translations)) {
      
      # Convert amino acid sequence to character
      amino_acids <- as.character(translations[[i]])
      
      # Wrap translated sequence into lines of 80 characters
      wrapped_translation <- if (nchar(amino_acids) > 0) {
        vapply(
          seq(1, nchar(amino_acids), by = 80),
          function(x) substr(amino_acids, x, x + 79),
          character(1)
        )
      } else {
        character()
      }
      
      # Add translated sequence with original header
      all_translations <- c(
        all_translations,
        names(translations)[i],
        wrapped_translation
      )
    }
    
    # Return translated sequences in FASTA format
    # Return translated sequences and summary
    list(
      translations = paste(all_translations, collapse = "\n"),
      summary = bulk_translation_summary
    )
  })
    
  # Display bulk translations
  output$txtout3 <- renderUI({
    
    if (input$submit_bulk_translation == 0) {
      return(tags$pre(
        style = "font-style: italic;",
        "Your translated sequences will appear here."
      ))
    }
    
    result <- bulk_translation_data()
    
    if (!is.null(result$error)) {
      return(tags$p(
        result$error,
        style = "color: red; font-weight: bold;"
      ))
    }
    
    tags$pre(result$translations)
  })
  # Display translation summary
  output$bulk_translation_summary <- renderDT({
    
    summary <- bulk_translation_data()$summary
    colnames(summary) <- gsub("_", " ", colnames(summary))
    
    datatable(
      summary,
      filter = "none",
      options = list(
        pageLength = 10,
        scrollY = "550px",
        scrollCollapse = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Nucleotide Length", "Amino Acid Length", "In Frame Stop Codons"),
        digits = 0
      )
  })

  ##########################
  # 4. Compare Sets
  ##########################
  
  compare_sets_data <- eventReactive(input$submit_compare_sets, {
    
    # Retrieve input lists
    if (input$no_sets == "2 Sets") {
      sets <- list(
        "Set 1" = input$set1,
        "Set 2" = input$set2
      )
    } else {
      sets <- list(
        "Set 1" = input$set1_3,
        "Set 2" = input$set2_3,
        "Set 3" = input$set3
      )
    }
    
    # Split lists by newline
    sets <- lapply(sets, function(x) {
      
      if (is.null(x) || !nzchar(trimws(x))) {
        return(character(0))
      }
      
      x <- gsub("\r", "", x)
      x <- strsplit(x, "\n", fixed = TRUE)[[1]]
      x <- trimws(x)
      
      # Convert to uppercase if comparison is not case-sensitive
      if (!isTRUE(input$case_sensitive)) {
        x <- toupper(x)
      }
      
      # Remove empty entries and duplicates
      unique(x[nzchar(x)])
      
    })
    
    # Validate number of populated sets
    validate(
      need(
        sum(lengths(sets) > 0) >= 2,
        "Please enter items in at least two sets before comparing."
      )
    )
    
    # Return lists
    return(sets)
    
  })
  
  
  # Generate comparison table
  compare_sets_table_data <- reactive({
    
    # Retrieve submitted lists
    sets <- compare_sets_data()
    
    # Identify all unique items
    all_items <- sort(unique(unlist(sets)))
    
    # Initialise summary dataframe
    summary <- data.frame(
      Item = all_items,
      stringsAsFactors = FALSE
    )
    
    # Determine membership of each item in each set
    for (i in seq_along(sets)) {
      
      summary[[names(sets)[i]]] <- ifelse(
        all_items %in% sets[[i]],
        "Yes",
        "No"
      )
      
    }
    
    return(summary)
    
  })
  # Display comparison table
  output$compare_sets_table <- DT::renderDT({
    
    DT::datatable(
      filtered_compare_sets_table(),
      rownames = FALSE,
      filter = "none",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "ftip"
      )
    )
    
  })
  # Filter comparison table
  filtered_compare_sets_table <- reactive({
    
    data <- compare_sets_table_data()
    # Filter by gene name / item
    if (nzchar(input$filter_gene)) {
      data <- data[
        grepl(input$filter_gene, data$Item, ignore.case = TRUE),
      ]
    }
    
    if (input$filter_set1 != "All") {
      data <- data[data$`Set 1` == input$filter_set1, ]
    }
    
    if (input$filter_set2 != "All") {
      data <- data[data$`Set 2` == input$filter_set2, ]
    }
    
    if ("Set 3" %in% names(data) && input$filter_set3 != "All") {
      data <- data[data$`Set 3` == input$filter_set3, ]
    }
    
    data
    
  })
  
  ############################
  # Subsequence code goes here
  ############################
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
