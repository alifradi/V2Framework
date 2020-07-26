# Section Load classes ------------------------------
source('imports/libRaries.R')
source('imports/FeatureSelection.R')
source('imports/CollectData.R')
source('imports/Describe.R')
# Section size to support ------------------------------
options(shiny.maxRequestSize=1000*1024^2)

## Part I ------------------------------

ui <- fluidPage(
# UI Phase1 ------------------------------
  
uiOutput('Chapter1'),uiOutput('Section1'),
selectInput(label = 'Select file type',inputId = 'FileType',choices = c('Excel','RDS','CSV','_'),selected = '_'),
uiOutput('FileInputUI'),
uiOutput('SpecifyDataIN'),
checkboxInput(label = 'Show data head and types',inputId = 'showPrev',value = TRUE),
uiOutput('headTable'),
uiOutput('column'),
verbatimTextOutput('strData') 

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
    {
      if(input$showPrev==TRUE){
        box(
          width = NULL,
          status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          style = 'overflow-x: scroll',
          tableOutput('PreviewRawData')
        )
      }else{
        return()
      }
    }
  })
  output$FileInputUI <-renderUI({
    if(input$FileType!='_'){fileInput('fileIN', 'Upload your data File here')} else
    {return()}
  })
  output$column <- renderUI({
    data<-DATA()
    if (is.null(DATA()))
    {return()}
    else
    {
      NUM<-dplyr::select_if(as.data.frame(data), is.numeric)
      Logica<-dplyr::select_if(as.data.frame(data), is.logical)
      facto<-dplyr::select_if(as.data.frame(data), is.factor)
      Char<-dplyr::select_if(as.data.frame(data), is.character)
      
      if(input$showPrev==TRUE){
        return(
          bucket_list(
            header = "Drag columns in the right bucket",
            group_name = "bucket_list_group",
            orientation = "horizontal",
            
            add_rank_list(
              text = "Charachter",
              labels = names(Char),
              input_id = "rank_list_1"
            ),
            add_rank_list(
              text = "Logical",
              labels = names(Logica),
              input_id = "rank_list_2"
            ),
            add_rank_list(
              text = "Factor",
              labels = names(facto),
              input_id = "rank_list_3"
            ),
            add_rank_list(
              text = "Numeric",
              labels = names(NUM),
              input_id = "rank_list_4"
            )
          )
        )
      }
      
     }
    
  })
  toCorrectFormat <- reactive({
    req(DATA)
    data<-DATA()
    
    NUM <- input$rank_list_4
    Logical <- input$rank_list_2
    Factor <- input$rank_list_3
    Char <- input$rank_list_1
    
    df = data.frame(matrix(ncol = 1, nrow = nrow(data)))
    if(length(NUM)>0){
      NumData <- data %>% mutate_if(names(data) %in% NUM, as.numeric)
      NumData <-NumData[,NUM]
      df = cbind(df,NumData)
    }
    if(length(Logical)>0){
      LogData <- data %>% mutate_if(names(data) %in% Logical, as.logical)
      LogData <-LogData[,Logical]
      df = cbind(df,LogData)
    }
    if(length(Factor)>0){
      FacData <- data %>% mutate_if(names(data) %in% Factor, factor)
      FacData <-FacData[,Factor]
      df = cbind(df,FacData)
    }
    if(length(Char)>0){
      CharData <- data %>% mutate_if(names(data) %in% Char, as.character)
      CharData <-CharData[,Char]
      df = cbind(df,CharData)
    }
    df = df[,2:ncol(df)]
    df
  })
  output$strData <-renderPrint({
    req(toCorrectFormat)
    data<-toCorrectFormat()
    str(data)
  })
  
}


# Section Run Application ------------------------------

shinyApp(ui = ui, server = server)
