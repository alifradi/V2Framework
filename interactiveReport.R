# Section Load classes ------------------------------
source('imports/libRaries.R')
# Section size to support ------------------------------
options(shiny.maxRequestSize=1000*1024^2)

## Part I ------------------------------

ui <- fluidPage(
# UI Phase1 ------------------------------
  
uiOutput('Chapter1'),uiOutput('Section1'),
selectInput(label = 'Select file type',inputId = 'FileType',choices = c('Excel','RDS','CSV'),selected = 'Excel'),
fileInput('fileIN', 'Upload your  RDS File here'),
uiOutput('SpecifyDataIN'), uiOutput('headTable')

)



## Part II ------------------------------

server <- function(input, output) {
# Sever Phase 1 ------------------------------
  output$Chapter1 <- renderUI({HTML("<b><u style='color:#054236;''><h3>Chapter 1: Data understanding</h3></u></b>")})
  output$Section1 <- renderUI({HTML("<h4 style='color:#079C80;''>Section 1: Collect data</h4>")})
  output$SpecifyDataIN <- renderUI({
    if(input$FileType=='Excel'){
      inFile <- input$fileIN
      if (is.null(inFile)) return(NULL)
      path<-inFile$datapath
      return(
        selectInput(label = 'Select Excel sheet',inputId = 'ExcelSheet', choices = getSheetNames(path), selected = getSheetNames(path)[1])
             )
    } else if(input$FileType=='CSV'){
      return(
      selectInput(label = 'Select CSV Separator',inputId = 'CSVSep', choices = c('Comma','SemiColon','Space', 'tab'), selected = 'Comma')
      )
    } else {
      return()
    }
  })
  DATA <- reactive({
    inFile <- input$fileIN
    if (is.null(inFile)) return(NULL)
    if(input$FileType=='Excel'){a<-read_excel(inFile$datapath,sheet =input$ExcelSheet,col_names = TRUE)}
    if(input$FileType=='RDS'){a<-readRDS(inFile$datapath)}
    if(input$FileType=='CSV'){
      if (input$CSVSep == 'Comma'){a<-read.csv(inFile$datapath,sep = ',')}else if(input$CSVSep == 'SemiColon')
      {a<-read.csv(inFile$datapath,sep = ';')} else if(input$CSVSep == 'Space')
      {a<-read.csv(inFile$datapath,sep = ' ')} else if(input$CSVSep == 'tab')
      {a<-read.csv(inFile$datapath,sep = '|')} 
      }
    return(a)
  })
  output$PreviewRawData <- renderTable({
    req(DATA)
    data<-DATA()
    head(data)
  })
  output$headTable<-renderUI({
    if (is.null(DATA()))
      return()
    else
      box(
        width = NULL,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        style = 'overflow-x: scroll',
        tableOutput('PreviewRawData')
      )
  })
}


# Section Run Application ------------------------------

shinyApp(ui = ui, server = server)
