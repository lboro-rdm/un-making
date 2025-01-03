library(shiny)
library(tidyverse)

# Define server logic
server <- function(input, output, session) {
  
  # Load the article data
  articles_tibble <- readRDS("articles_tibble.rds")  # Load the tibble containing image information
  
  # Populate the tag filter dynamically and sort tags alphabetically
  observe({
    all_tags <- unique(unlist(strsplit(paste(articles_tibble$tags, collapse = ", "), ", ")))
    sorted_tags <- sort(all_tags)  # Sort tags alphabetically
    updateSelectInput(session, "tag_filter", choices = sorted_tags)
  })
  
  # Filter the articles based on selected tags and exclude those without tags
  filtered_articles <- reactive({
    articles_tibble %>%
      filter(!is.na(tags) & tags != "") %>%  # Exclude rows with empty or NA tags
      filter(if (is.null(input$tag_filter) || length(input$tag_filter) == 0) TRUE
             else map_lgl(tags, ~ any(input$tag_filter %in% strsplit(.x, ", ")[[1]])))
  })
  
  # Render image grid
  output$image_grid <- renderUI({
    articles <- filtered_articles()
    
    fluidRow(
      lapply(1:nrow(articles), function(i) {
        column(3,  # Adjust column width as needed
               img(src = paste0(articles$article_id[i], ".jpg"),
                   alt = articles$title[i],
                   style = "width: 100%; height: auto;"),
               h4(articles$title[i]),
               p(articles$description[i])
        )
      })
    )
  })
}
