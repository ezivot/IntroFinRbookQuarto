# convert bookdown format to quarto format

library(knitr)

# Directory containing bookdown .Rmd files
book_dir <- "."

# List all .Rmd files in the project
rmd_files <- list.files(
  path = book_dir,
  pattern = "\\.Rmd$",
  full.names = TRUE,
  recursive = FALSE
)

# Reserved Quarto crossref prefixes. Heading ids that already start with one
# of these (e.g. "sec-", "fig-", "exm-") are left untouched.
quarto_reserved_prefixes <- c(
  "sec", "fig", "tbl", "lst", "tip", "nte", "wrn", "imp", "cau",
  "thm", "lem", "cor", "prp", "cnj", "def", "exm", "exr", "sol",
  "rem", "alg", "eq"
)

# Add the "sec-" prefix to bookdown-style heading ids so they become valid
# Quarto section cross-reference targets, e.g.:
#   ## Random Variables {#Random-Variables}
# becomes:
#   ## Random Variables {#sec-Random-Variables}
# Headings whose id already starts with a reserved crossref prefix (most
# commonly "sec-") are left unchanged.
prefix_bookdown_section_ids <- function(lines) {
  heading_pattern <- "^(#{1,6}\\s.*)\\{#([A-Za-z][A-Za-z0-9_-]*)((?:[^}]*))\\}\\s*$"

  vapply(lines, function(line) {
    m <- regmatches(line, regexec(heading_pattern, line, perl = TRUE))[[1]]
    if (length(m) == 0) return(line)

    heading_text <- m[2]
    id <- m[3]
    trailing_attrs <- m[4]

    prefix <- sub("-.*$", "", id)
    if (tolower(prefix) %in% quarto_reserved_prefixes) {
      return(line)
    }

    paste0(heading_text, "{#sec-", id, trailing_attrs, "}")
  }, character(1), USE.NAMES = FALSE)
}

# 1. Update Cross-References and Labels --------------------------------------
convert_bookdown_syntax <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  
  # Convert \@ref(fig:name) -> @fig-name
  content <- gsub("\\\\@ref\\(fig:([^)]+)\\)", "@fig-\\1", content)
  
  # Convert \@ref(tab:name) -> @tbl-name
  content <- gsub("\\\\@ref\\(tab:([^)]+)\\)", "@tbl-\\1", content)
  
  # Convert \@ref(eq:name) -> @eq-name
  content <- gsub("\\\\@ref\\(eq:([^)]+)\\)", "@eq-\\1", content)
  
  # Convert \@ref(thm:name) -> @thm-name
  content <- gsub("\\\\@ref\\(thm:([^)]+)\\)", "@thm-\\1", content)
  
  # Convert \@ref(lem:name) -> @lem-\\1
  content <- gsub("\\\\@ref\\(lem:([^)]+)\\)", "@lem-\\1", content)

  # Convert \@ref(exm:name) -> @exm-name (bookdown "example" theorem-type refs)
  content <- gsub("\\\\@ref\\(exm:([^)]+)\\)", "@exm-\\1", content)

  # Convert bookdown example div ids to Quarto's required "exm-" prefix:
  # ::: {#example0101 .example name="..."} -> ::: {#exm-example0101 .example name="..."}
  content <- gsub(
    "(:::+\\s*\\{#)(example[A-Za-z0-9]*)(\\s)",
    "\\1exm-\\2\\3",
    content
  )
  
  # Convert general section references \@ref(sec-name) -> @sec-sec-name
  content <- gsub("\\\\@ref\\(([^):]+)\\)", "@sec-\\1", content)

  # Convert bookdown section/heading labels to Quarto's required "sec-" prefix:
  # # Some Heading {#some-id} -> # Some Heading {#sec-some-id}
  # Skip ids that already use one of Quarto's reserved crossref prefixes
  # (e.g. sec-, fig-, tbl-, thm-, exm-, ...) since those are already valid.
  content <- prefix_bookdown_section_ids(content)
  
  # Convert LaTeX equation environments with labels:
  # \begin{equation} ... \label{eq:name} ... \end{equation} -> $$ ... $$ {#eq-name}
  # Note: Standard multiline equations are preserved; inline tags are replaced.
  content <- gsub("\\\\label\\{eq:([^}]+)\\}", "{#eq-\\1}", content)

  # Convert bookdown's native inline equation-label syntax:
  # ...(\#eq:name) -> ...{#eq-name}
  content <- gsub("\\(\\\\#eq:([^)]+)\\)", "{#eq-\\1}", content)
  
  # Write updated content back
  writeLines(content, file_path)
}

# 2. Modernize Knitr Chunk Headers to YAML (#|) Syntax -----------------------
# knitr::convert_chunk_header automatically parses comma-separated headers
# and rewrites them to Quarto-style `#|` chunk options.
convert_project_to_quarto <- function(files) {
  for (f in files) {
    message("Processing: ", f)
    
    # Step A: Update cross-reference syntax
    convert_bookdown_syntax(f)
    
    # Step B: Convert chunk options to #| style
    # output = NULL overwrites the existing file
    knitr::convert_chunk_header(input = f, output = identity, type = "yaml")
    
    # Step C: Rename file from .Rmd to .qmd
    qmd_name <- sub("\\.Rmd$", ".qmd", f)
    file.rename(f, qmd_name)
    message("Renamed to: ", qmd_name)
  }
  message("\nMigration complete! Verify chunk labels and figures before rendering.")
}

# Run migration
convert_project_to_quarto(rmd_files)