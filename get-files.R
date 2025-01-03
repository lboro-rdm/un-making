library(httr)  # for the GET requests
library(xml2)  # for handling the HTML in the description
library(tibble) # for creating tibbles
library(dplyr) # for mutating tibbles
library(conrad) # for linking to Microsoft's 'Cognitive Services Text to Speech REST' API

# Get list of article_ids -------------------------------------------------

url <- "https://api.figshare.com/v2/collections/7154647/articles?page_size=100"

response <- GET(url)

articles_info <- content(response, as = "parsed")

article_ids <- sapply(articles_info, function(article) article$id)

num_article_ids <- length(article_ids)

counter <- 0

# Initialize a tibble to store article details
articles_tibble <- tibble(
  article_id = integer(),
  title = character(),
  doi = character(),
  tags = character(),
  description = character()
)

# -------------------------------------------------------------------------

for (article_id in article_ids) {
  url <- paste0("https://api.figshare.com/v2/articles/", article_id)
  
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    cat("Failed to retrieve article ID:", article_id, "Status code:", status_code(response), "\n")
    next  # Skip to the next iteration
  }
  
  article_info <- content(response, as = "parsed")
  
  # Get image ---------------------------------------------------------------
  
  if (!is.null(article_info$files) && length(article_info$files) > 0) {
    file_url <- article_info$files[[1]]$download_url
    output_file <- file.path("www", paste0(article_id, ".jpg"))
    
    download_response <- GET(file_url, write_disk(output_file, overwrite = TRUE))
    
    if (status_code(download_response) == 200) {
      cat("Image downloaded successfully for article ID:", article_id, "\n")
    } else {
      cat("Failed to download image for article ID:", article_id, "Status code:", status_code(download_response), "\n")
    }
  } else {
    cat("No files found for article ID:", article_id, "\n")
  }
  
  # Get description ---------------------------------------------------------
  
  description <- article_info$description
  
  parsed_description <- read_html(description)
  
  clean_description <- xml_text(parsed_description) 
  
  split_description <- strsplit(clean_description, "This object is part of", fixed = TRUE)[[1]]
  
  first_part <- trimws(split_description[1])
  
  # Get file info -----------------------------------------------------------
  
  title <- article_info$title
  doi <- article_info$doi
  tags <- paste(article_info$tags, collapse = ", ")
  
  articles_tibble <- add_row(articles_tibble, article_id = article_id, title = title, doi = doi, tags = tags, description = first_part)
  
  counter <- counter + 1
  cat("Record", counter, "of", num_article_ids, "done!\n")
  
}


# -------------------------------------------------------------------------

articles_tibble <- articles_tibble %>%
  mutate(
    tags = tolower(tags),  # Ensure all tags are lowercase
    tags = strsplit(tags, ", "),  # Split tags into individual items
    tags = lapply(tags, function(tag_list) {
      tag_list <- tag_list[startsWith(tag_list, "csc_")]  # Keep only tags starting with csc_
      sub("csc_", "", tag_list)  # Remove the prefix
    }),
    tags = sapply(tags, paste, collapse = ", ")  # Recombine tags
  )


saveRDS(articles_tibble, "articles_tibble.rds")


# Generating tts files ----------------------------------------------------

# https://uksouth.api.cognitive.microsoft.com/


