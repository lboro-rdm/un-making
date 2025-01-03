library(shiny)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Image Gallery"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "tag_filter",
        label = "Filter by Tags:",
        choices = NULL,  # Choices will be populated dynamically
        selected = NULL,
        multiple = TRUE  # Allow selecting multiple tags
      )
    ),
    
    mainPanel(
      uiOutput("image_grid")
    )
  )
)
