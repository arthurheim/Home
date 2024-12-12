library(pacman)
pacman::p_load(
  tidyverse,
  jsonlite,
  purrr,
  httr)


parse_bib_for_quarto <- function(file) {
  # Convert BibTeX to JSON using pandoc
  bib_json <- system2("pandoc", 
                      args = c("-f", "bibtex", 
                               "-t", "csljson", 
                               file), 
                      stdout = TRUE)
  
  # Parse JSON
  bib <- fromJSON(bib_json, simplifyVector = FALSE)
  
  # Convert to tibble
  bib_df <- map_dfr(bib, ~{
    # Flatten nested lists
    flat <- flatten(.x)
    # Convert all elements to character
    map_chr(flat, ~ifelse(is.null(.x), NA_character_, as.character(.x[1])))
  })
  
  # Add a formatted citation column
  bib_df <- bib_df %>%
    mutate(citation = paste0(author, " (", issued, "). ", title, ". ", 
                             container_title, ", ", volume, "(", issue, "), ", 
                             page, ". ", URL))
  
  return(bib_df)
}
