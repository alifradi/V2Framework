source('imports/libRaries.R')

## Part I ------------------------------

ui <- fluidPage(

  fileInput('fileInput', 'Upload your  Excel File here')
)



## Part II ------------------------------

server <- function(input, output) {

  
  
}


# Section Run Application ------------------------------

shinyApp(ui = ui, server = server)
