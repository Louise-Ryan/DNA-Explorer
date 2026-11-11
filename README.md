<p align="left">

<img src = "www/dna-tools-logo.svg" max-width=100%>

<hr>

DNA Tools is an accessible, web-based application for exploring and analysing genetic sequence data. It brings together a collection of basic tools for common DNA and RNA sequence analysis tasks, including reverse complement calculation, six-frame translation and set comparison (more features in development).

These tools come up regularly during bioinformatics workflows, but can often require finding or writing scripts to perform. This can be unnecessarily time-consuming when working with small datasets or a single target sequence of interest. DNA Tools provides a convenient alternative, bringing these tools together in one place through a simple, user-friendly interface. It may also be useful in undergraduate teaching labs and practical sessions, where students can explore and analyse sequences without needing to write code or install specialist software.

**Project currently in development, check back for updates!**

**Try it here:** 🌐 [DNA Tools](https://louryan.shinyapps.io/dna-tools/)

>Note, the application is hosted on shinyapps.io and may take a few moments to load when accessed after a period of inactivity.

<br>

## 📖 Contents
- [Accessing the App](#access)
  - [Web server](#web-server)
  - [Running the app locally](#local)
- [Features](#features)
  - [Six-Frame Translation](#six-frame-translation)
  - [Bulk Translation](#bulk-translation)
  - [Compare Sets](#compare-sets)
  - [Reverse Complement](#reverse-complement)
<!-- - [Summarise Dataset](#summarise-dataset) -->
<!-- - [Get Subsequence](#get-subsequence) -->

<br>

<a id="access"></a>
<h2>🔗 Accessing the App</h2>

<a id="web-server"></a>
<h3>🌍 Web Server</h3>

DNA Tools is hosted online using **shinyapps.io** and can be accessed directly through a web browser without requiring a local installation.

**Access the application:** [DNA Tools](https://louryan.shinyapps.io/dna-tools/)

The application may take a few moments to start if it has been inactive.

<a id="local"></a>
<h3>💻 Running the App Locally</h3>

To run DNA Tools locally, you will need to install [R](https://cran.r-project.org/). Optionally, you can also install [RStudio](https://posit.co/download/rstudio-desktop/) for an integrated development environment.

1. Clone or download the repository from GitHub.
2. Install the required packages by running the following commands in the R console:

```r
install.packages(c("shiny", "shinythemes", "DT"))

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("Biostrings")
```

4. Launch the app:

- **Using RStudio:** Open `app.R` and click **Run App** in the top-right corner of the editor.
- **Without RStudio:** Open a terminal, navigate to the project directory and run:

  ```bash
  R -e "shiny::runApp()"
  ```

<br>

<a id="features"></a>
<h2>🧬 Features</h2>

<a id="six-frame-translation"></a>
- **Six-Frame Translation**

  Translate a nucleotide sequence across all six reading frames. View the resulting amino acid sequences alongside a summary table containing information about open reading frames (ORFs).

<a id="bulk-translation"></a>
- **Bulk Translation**

  Translate multiple nucleotide sequences supplied in FASTA format. Generate amino acid sequences and a summary table containing information about each translated sequence.

<a id="compare-sets"></a>
- **Compare Sets**

  Compare two or three sets to identify shared and unique items. Example use cases include comparing gene or protein names, accession IDs, genomic coordinates and other lists.

<a id="reverse-complement"></a>
- **Reverse Complement**

  Calculate the reverse complement of a DNA or RNA sequence. Supports both DNA and RNA input.

<!--
<a id="summarise-dataset"></a>
- **Summarise Dataset**

  Explore and summarise sequence datasets.

<a id="get-subsequence"></a>
- **Get Subsequence**

  Extract a subsequence from a nucleotide sequence using specified start and end positions.
-->

<br>






