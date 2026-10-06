# R script to convert bib file of publications into individual qmd files
# Author: chatGPT
# Date: 14 October 2025

# Load required packages
if (!require("bibtex")) install.packages("bibtex", repos="https://cloud.r-project.org")
if (!require("stringr")) install.packages("stringr", repos="https://cloud.r-project.org")
if (!require("fs")) install.packages("fs", repos="https://cloud.r-project.org")

library(bibtex)
library(stringr)
library(fs)

# ----------------------------
# USER SETTINGS
# ----------------------------
bibfile <- "publications/Engelstaedter_lab_pubs.bib"   # Path to your .bib file
output_dir <- "publications"     # Directory where qmd files will be written
default_image <- "featured.gif"
default_category <- ""
overwrite_existing <- TRUE       # Set TRUE to overwrite existing folders/files


# ----------------------------
# HELPER FUNCTIONS
# ----------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

# More robust sanitize that works on vectors too
sanitize <- function(x) {
  if (is.null(x)) return("")
  # ensure character
  x <- as.character(x)
  # operate elementwise; collapse vector into single string if needed later
  x <- gsub("[{}]", "", x)
  x <- gsub("[\n\r\t]", " ", x)
  x <- gsub("\\\\&", "&", x)
  x <- gsub("\\\\%", "%", x)
  x <- gsub("\\\\_", "_", x)
  x <- gsub("\\\\#", "#", x)
  x <- gsub("\\\\textit\\{([^}]*)\\}", "_\\1_", x)
  x <- gsub("\\\\textbf\\{([^}]*)\\}", "**\\1**", x)
  x <- gsub("\\\\'a", "á", x)
  x <- gsub("\\\\'e", "é", x)
  x <- gsub("\\\\'i", "í", x)
  x <- gsub("\\\\'o", "ó", x)
  x <- gsub("\\\\'u", "ú", x)
  x <- gsub("\\\\\"o", "ö", x)
  x <- gsub("\\\\\"u", "ü", x)
  x <- gsub("\\\\\"a", "ä", x)
  x <- gsub("\\\\ss", "ß", x)
  x <- str_trim(x)
  # if length >1, join into one string with spaces
  if (length(x) > 1) x <- paste(x, collapse = " ")
  return(x)
}

# Extract one or multiple categories from note field
extract_categories <- function(note_text) {
  if (is.null(note_text) || note_text == "") return(NA)
  match <- str_match(note_text, "(?i)Category:\\s*([^\\n]+)")
  if (!is.na(match[1, 2])) {
    cats <- unlist(strsplit(match[1, 2], "[,;]"))
    cats <- str_trim(cats)
    cats <- cats[cats != ""]
    return(cats)
  }
  return(NA)
}

# Robustly extract authors as a character vector
extract_authors <- function(entry) {
  a <- entry$author
  if (is.null(a)) return(character(0))
  
  # Case 1: single character field ("A and B and C")
  if (is.character(a) && length(a) == 1) {
    authors <- unlist(strsplit(a, " and ", fixed = TRUE))
    authors <- str_trim(authors)
    authors <- vapply(authors, sanitize, FUN.VALUE = character(1), USE.NAMES = FALSE)
    return(authors)
  }
  
  # Case 2: character vector of names
  if (is.character(a) && length(a) > 1) {
    authors <- vapply(a, sanitize, FUN.VALUE = character(1), USE.NAMES = FALSE)
    return(authors)
  }
  
  # Case 3: list of person-like objects (BibTeX parsed persons)
  if (is.list(a)) {
    authors <- vapply(a, function(p) {
      # Defensive conversion to character
      if (is.character(p)) {
        s <- paste(p, collapse = " ")
        return(sanitize(s))
      }
      
      # Collapse any multiple parts (e.g., c("Charles", "R.") or c("van", "der", "Waals"))
      given_part  <- if (!is.null(p$given))  paste(p$given,  collapse = " ") else ""
      family_part <- if (!is.null(p$family)) paste(p$family, collapse = " ") else ""
      
      given_part  <- str_trim(given_part)
      family_part <- str_trim(family_part)
      
      if (given_part != "" && family_part != "") {
        full <- paste(given_part, family_part)
      } else if (family_part != "") {
        full <- family_part
      } else if (given_part != "") {
        full <- given_part
      } else {
        full <- paste(as.character(p), collapse = " ")
      }
      
      sanitize(str_trim(full))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    return(authors)
  }
  
  # Case 4: fallback
  authors <- sanitize(as.character(a))
  if (length(authors) == 1 && grepl(" and ", authors, fixed = TRUE)) {
    authors <- unlist(strsplit(authors, " and ", fixed = TRUE))
    authors <- str_trim(authors)
    authors <- vapply(authors, sanitize, FUN.VALUE = character(1), USE.NAMES = FALSE)
  }
  return(authors)
}

# ----------------------------
# READ AND SORT BIB ENTRIES
# ----------------------------
bib_entries <- read.bib(bibfile)

bib_df <- data.frame(
  idx = seq_along(bib_entries),
  year = sapply(bib_entries, function(e) as.numeric(e$year %||% NA)),
  stringsAsFactors = FALSE
)
bib_df <- bib_df[order(bib_df$year, na.last = TRUE), ]

dir_create(output_dir)

# ----------------------------
# MAIN LOOP
# ----------------------------
for (pub_number in seq_len(nrow(bib_df))) {
  i <- bib_df$idx[pub_number]
  entry <- bib_entries[[i]]
  
  title <- sanitize(entry$title %||% "")
  year <- entry$year %||% "XXXX"
  
  authors <- extract_authors(entry)
  if (length(authors) == 0) authors <- "Unknown"
  
  first_author_for_folder <- authors[1]
  first_author_for_folder <- str_replace_all(first_author_for_folder, "\\s+", "_")
  first_author_for_folder <- str_replace_all(first_author_for_folder, "[^A-Za-z0-9_\\-]", "")
  
  short_title <- str_sub(str_replace_all(title, "\\W+", "_"), 1, 30)
  folder_name <- sprintf("%s_%s_%s", year, first_author_for_folder, short_title)
  folder_path <- file.path(output_dir, folder_name)
  qmd_path <- file.path(folder_path, "index.qmd")
  
  # Check if it already exists
  if (!dir_exists(folder_path)) {
    dir_create(folder_path)
  }
  if (file_exists(qmd_path) && !overwrite_existing) {
    message(sprintf("⏭ Skipping [%02d]: %s (already exists)", pub_number, folder_name))
  } else {
    journ <- sanitize(entry$journal %||% entry$booktitle %||% "")
    volume <- entry$volume %||% ""
    issue <- entry$number %||% ""
    pages <- entry$pages %||% ""
    doi <- entry$doi %||% ""
    url <- entry$url %||% if (!is.null(doi) && doi != "") paste0("https://doi.org/", doi) else ""
    
    note_text <- entry$note %||% ""
    categories <- extract_categories(note_text)
    if (all(is.na(categories))) categories <- default_category
    
    publication <- paste0(journ,
                          if (volume != "") sprintf(", **%s**", volume) else "",
                          if (pages != "") sprintf(", _%s_", pages) else "",
                          sprintf(" (%s).", year))
    authors_yaml <- paste(sprintf('"%s"', authors), collapse = ", ")
    categories_yaml <- paste(sprintf('"%s"', categories), collapse = ", ")
    
    yaml <- c(
      "---",
      sprintf('title: "%s"', title),
      sprintf('date: %s-01-01', year),
      sprintf('author: [%s]', authors_yaml),
      sprintf('publication: "%s"', publication),
      sprintf('categories: [%s]', categories_yaml),
      sprintf('url_source: %s', url),
      'url_preprint: ',
      sprintf('journ: "%s"', journ),
      sprintf('volume: %s', volume),
      sprintf('issue: %s', issue),
      sprintf('page: %s', pages),
      sprintf('year: %s', year),
      sprintf('#image: %s', default_image),
      sprintf('pub_number: %s', pub_number),
      "---"
    )
    
    writeLines(yaml, qmd_path)
    message(sprintf("✔ Created [%02d]: %s", pub_number, qmd_path))
  }
}

message("\n✅ Conversion complete! All .qmd files saved to: ", output_dir)