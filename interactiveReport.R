## Section Import Libraries ------------------------------
library("shiny")
library("stringr")
library("shinydashboard")
library("naniar")
library("skimr")
library("dplyr")
library("discretization")
library('data.table')
library("DT")
library("ggplot2")
library("FactoMineR")
library("factoextra")
library('psych')
library('GGally')
library("e1071")
library("plotly")
library("smbinning")
library("openxlsx")
library('grid')
library('tidyr')
library("missMDA")
library("VIM")
library("mice")
library("plotrix")
library("caret")
library("rsample")
library("dendextend")
library("ClustOfVar")
library('formattable')
library("sortable")
options(shiny.maxRequestSize=1000*1024^2)
## Section Sidebar Chapters ------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    # menuItem Begin ------------------------------
    menuItem("Collect data", tabName = "CData", icon = icon("hand-spock"),
             # menuSubItem Exploratory Data Analysis ------------------------------
             menuSubItem("Excel", tabName = "ExcelInput", icon = icon('file-excel')), 
             menuSubItem("CSV", tabName = "CSVInput", icon = icon('file-code')))
    ,
    # menuItem Begin ------------------------------
    menuItem("Exploratory Data Analysis", tabName = "EDA", icon = icon("stethoscope"),
             # menuSubItem Exploratory Data Analysis ------------------------------
             menuSubItem("Univariate analysis", tabName = "UVA", icon = icon('eye')), 
             menuSubItem("Multivariate analysis", tabName = "MVA", icon = icon('tint')),
             menuSubItem("Exploring missings", tabName = "EM" , icon = icon('syringe'))
    ),
    menuItem("Features Engineering", tabName = "FE", icon = icon("ambulance"),
             # menuSubItem Features Engineering ------------------------------
             menuSubItem("Features categorization", tabName = "FC", icon = icon('cut')),
             menuSubItem("Variable encoding", tabName = "VEnc", icon = icon('wrench')),
             menuSubItem("Tree categorization", tabName = "TC", icon = icon('weight')),
             menuSubItem("Normalize Continuous Features", tabName = "NCF", icon = icon('pills'))
             #,menuSubItem("Score Modeling Binning", tabName = "SMB", icon = icon('glasses'))
    ),
    menuItem("Dealing with exceptions", tabName = "DMDO", icon = icon("heartbeat"),
             # menuSubItem Dealing with missing data/outliers ------------------------------
             menuSubItem("Missings imputations", tabName = "MImp", icon = icon('first-aid')),
             menuSubItem("Outliers treatments", tabName = "OutL", icon = icon('user-md'))
    ),
    menuItem("Data Understanding", tabName = "DU", icon = icon("walking"),
             # menuSubItem Exploratory Factor Analysis ------------------------------
             menuSubItem("Exploratory Factor Analysis", tabName = "EFA", icon = icon('wpexplorer')),
             menuSubItem("Clustering Variables", tabName = "ClustOfVar", icon = icon('folder-open')),
             menuSubItem("Linear Discriminant Analysis", tabName = "LDA", icon = icon('columns'))
    ),
    menuItem("Modeling", tabName = "Model", icon = icon("swimmer"),
             # menuSubItem Modeling ------------------------------
             menuSubItem("Modeling", tabName = "MOD", icon = icon('brain'))
    )
    # menuItem End ------------------------------
  )
)

## Section Body ------------------------------
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  tabItems(
    # Section Begin Items ------------------------------
    tabItem(tabName = "ExcelInput", h2("Compress Excel to RDS"),
            
            fluidRow(box(background = "navy", solidHeader = TRUE,
                         fileInput('fileIn', 'Upload your  Excel File here'),
                         uiOutput('selectSheet'),
                         downloadButton("asRDS", "As RDS data"), width = 3  ),
                     box(background = "olive", solidHeader = TRUE,
                         dataTableOutput("TABLook"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                         width = 9, title = 'Preview data: head' )) ,
            uiOutput('column'),
            verbatimTextOutput('strData') 
            
            
    ),
    tabItem(tabName = "CSVInput", h2("Compress CSV to RDS"),
            fluidRow(box(background = "navy", solidHeader = TRUE,
                         fileInput('fileInCSV', 'Upload your  CSV File here'),
                         downloadButton("asRDSCSV", "As RDS data"),
                         width = 3  ),
                     box(background = "olive", solidHeader = TRUE,
                         dataTableOutput("TABLookCSV"),
                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                         width = 9, title = 'Preview data: head' )
            ),
            uiOutput('columnCSV'),
            verbatimTextOutput('strDataCSV') 
            
            
    ),
    
    
    # Section Univariate descriptive analysis ------------------------------
    tabItem(tabName = "UVA", h2("Univariate descriptive analysis"),
            fluidRow(box(title = "Inputs", background = "navy", solidHeader = TRUE,
                         fileInput('file0', 'Upload your  RDS File here'))),
            fluidRow(box(uiOutput('skimTabNuM') ,style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                         width = 12, title = 'Numeric features')
            ),
            fluidRow(box(uiOutput('skimTabFac') ,style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                         width = 12, title = 'Factor features')
            ),
            fluidRow(box(uiOutput('skimTabOther') ,style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                         width = 12, title = 'Other features')
            )),
    # Section Multivariate descriptive analysis ------------------------------ 
    tabItem(tabName = "MVA", h2("Multivariate descriptive analysis"),
            box(title = "Inputs", background = "navy", solidHeader = TRUE,
                fileInput('file1', 'Upload your  RDS File here'),
                uiOutput('targetName'),
                uiOutput('features'),
                radioButtons(label = 'Correlation  method',inputId ='rho',
                             choices = c("pearson","spearman","kendall"),
                             selected = "spearman"), width = 3 ),
            uiOutput('tabs1')),
    # Section Exploring missings ------------------------------
    tabItem(tabName = "EM", h2("Exploring missings"),
            fluidRow(box(title = "Inputs", background = "navy", solidHeader = TRUE,
                         fileInput('file2', 'Upload your  RDS File here'),
                         uiOutput('centFeatures'),
                         downloadButton("CM", "Download centered matrix"),
                         uiOutput('cluscheckbox') ),
                     box(title = "Missing columns", solidHeader = TRUE,uiOutput('missReport'),
                         style = "height:350px; overflow-y: scroll;overflow-x: scroll;",
                         width = 6)),
            fluidRow(uiOutput('plot3UI')),
            
            fluidRow( h2("Missings dependency")),
            
            fluidRow(box(uiOutput('tracker')),box(uiOutput('tracked'))),
            fluidRow(box(uiOutput('plotdenstrack')),box(uiOutput('plotboxtrack'))),
            fluidRow(box(uiOutput('MissV1'),uiOutput('MissV2')),box(uiOutput('plotBimiss')))
            
            
    ),
    # Section Features categorization ------------------------------
    tabItem(tabName = "FC",  h2("Features categorization"),
            
            fluidRow(
              box( title = "Inputs", background = "navy", solidHeader = TRUE,
                   fileInput('fileFC', 'Upload your  RDS File here'),
                   uiOutput('resptar'),
                   uiOutput('tobin'), width = 5 ),
              box(h2('Khi2 categorization'),
                  sliderInput(label = 'Chi2 threshold', inputId = "khi2", 
                              min = 0.001, max = 0.1, step = 0.001, value = 0.05),
                  actionButton("Get", label = 'Discretize it', icon = icon("cut")),
                  downloadButton("DiscreteData", "Download discretized data"),
                  plotlyOutput('generatedDis'))
            ),
            fluidRow(box(h2('WoE categorization'), width = 12),
                     box(uiOutput('WoEplots'), width = 12),
                     box(sliderInput(label = 'Observation minSplit', inputId = 'MinSpWoE',value = 0.05, min = 0.001,max = 0.49),
                         downloadButton("WOETABLE", "Download WoE table"),
                         checkboxInput(label = 'Invert labels', inputId = 'InvLab',value = FALSE)
                         ,width = 12),
                     box(uiOutput('WoEtab'),style = "height:520px; overflow-y: scroll;overflow-x: scroll;",width = 12))
            
    ),
    # Section Tree categorization ------------------------------
    tabItem(tabName = "TC", h2("Tree categorization"),
            fluidRow(box(title = "Inputs", background = "navy", solidHeader = TRUE,
                         fileInput('fileTC', 'Upload your  RDS File here'))),
            fluidRow( box(uiOutput('targetDis'),uiOutput('featuresDis')),
                      box(numericInput(label = 'Min Split', inputId = 'ms',value = 0.05, step = 0.01,min=0.01,max = 0.49))),
            fluidRow(uiOutput("DistreePlot"))
    ),
    # Section Features re-encoding ------------------------------
    tabItem(tabName = "VEnc",  h2("Features encoding"),
            
            fluidRow(
              box(title = "Inputs", background = "navy", solidHeader = TRUE,
                  fileInput('fileVEnc', 'Upload your  RDS File here'),
                  uiOutput('varsToEncode'),
                  downloadButton("EncodedasRDS", "Encoded table")
              ),
              box(uiOutput("levelsInput"),
                  style = "height:230px; overflow-y: scroll;")
            ),
            #tableOutput('show_inputs'),
            fluidRow(
              box(dataTableOutput('tableToEncode'),
                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
                  width = 6,
                  title = 'Input data' ),
              box(uiOutput('EncodeTAB') ,
                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
                  width = 6,
                  title = 'Recoded data' )
            )
    ),
    # Section Dealing with missing data/outliers ------------------------------
    tabItem(tabName = "MImp", h2("Imputing data"),
            fluidRow(
              box(title = "Inputs", background = "navy", solidHeader = TRUE,
                  fileInput('fileImp', 'Upload your  RDS File here'),
                  checkboxInput(label = 'Impute all', inputId = 'allNo',value = TRUE),
                  uiOutput('ImputeAt2'),
                  uiOutput("ImputationMethods"),
                  downloadButton("dwnImp", "Download Imputed as CSV"),
                  downloadButton("dwnImprds", "Download Imputed as RDS"),
                  uiOutput('varToPlot')
              )
              ,box(uiOutput('CompareImputedOrigPlot')) 
            )
            
    ),
    tabItem(tabName = "OutL", h2("Dealing with outliers"),
            box(title = 'LOF'),
            box(title = 'KNN'),
            box(title = 'Random Forest')),
    # Section Exploratory Factor Analysis ------------------------------
    tabItem(tabName = "EFA", h2("Exploratory Factor Analysis"),
            fluidRow(
              box(title = 'Input data', background = "navy", solidHeader = TRUE,
                  fileInput('fileEFA', 'Upload your  RDS File here'),
                  selectInput("dim1", label = "X-dim",choices = c(1,2,3,4,5), selected = 1),
                  selectInput("dim2", label = "Y-dim",choices = c(1,2,3,4,5), selected = 2), width = 3),
              box(uiOutput('PlotMCAindivs'), width = 5), box(uiOutput('PlotMCAplanContrib'), width = 4)),
            fluidRow(
              box(sliderInput(inputId = 'alphaInd',label = 'Individuals transparency',
                              min=0,max = 1,value = 0.1,step = 0.1), width = 8),
              box(sliderInput(inputId = 'alpha',label = 'Quality of representation Cos',
                              min=0,max = 1,value = 0.02,step = 0.001), width = 8),
              box(radioButtons(label ='Top selecting condition',inputId = 'cont',choices = c('contrib','cos2')),
                  checkboxInput(label = 'Repel',inputId = 'rep',value = TRUE), width = 4  )
            ),
            fluidRow(box(uiOutput('PlotMCAtopmod'),title = 'Variance of modalities', width = 12))
    ),
    # Section CluserOfVars ------------------------------
    
    tabItem(tabName = "ClustOfVar", 
            fluidRow(
              box( background = "navy", solidHeader = TRUE, title = 'Inputs',
                   fileInput('fileClusOfVars', 'Upload your  RDS File here'),
                   uiOutput('optimalk'),
                   uiOutput('bootSamp')),
              box(uiOutput('Plotbootscriteria'))
            ),
            fluidRow(uiOutput('PlothtreeOfVars'))
            
    ),
    
    # Section Linear descriminant analysis ------------------------------
    
    tabItem(tabName = "LDA", 
            box( 
              background = "navy", solidHeader = TRUE, title = 'Inputs',
              fileInput('fileLDA', 'Upload your  RDS File here')
            )    
    ),
    # Section Normalize ------------------------------
    tabItem(tabName = "NCF",  h2("Normalizing continuous variabales"),
            fluidRow(
              box(title = "Inputs", background = "navy", solidHeader = TRUE,
                  fileInput('fileNCF', 'Upload your  RDS File here'),
                  downloadButton("transformed", "Download  data")),
              box(uiOutput('UnNormal'),
                  uiOutput('Transformation')),
              box(uiOutput('kurtSkewTAB')),box(uiOutput('kurtSkew2TAB'))
            ),
            fluidRow(
              box(uiOutput('uNnORMALPlotly')),
              box(uiOutput('NormalPlotly')))
            
            
    ),
    # Section Modeling ------------------------------
    tabItem(tabName = "MOD",  h2("Predictive modeling"),
            fluidRow(
              box(title = 'Data Balance', background = "navy", solidHeader = TRUE,
                  fileInput('fileDB', 'Upload your  RDS File here'),
                  checkboxInput(label = 'Invert labels', inputId = 'InvLab2',value = FALSE),
                  uiOutput('targetbalance'),
                  
              ),
              box(sliderInput(label = 'Train/Test split proportion', inputId = 'splitPct',value = 0.75,
                              min = 0.01, max = 0.99),
                  downloadButton("train", "Download train data"),
                  downloadButton("test", "Download test data"),
                  numericInput(label = 'K-fold Cross validation', inputId = 'kfold',value = 3,max = 5,min = 2))
            ),
            
            fluidRow(box(uiOutput('tosplitPlot'), width = 4),
                     box(uiOutput('trainPlot'), width = 4),
                     box(uiOutput('testPlot'), width = 4)),
            fluidRow(sliderInput(label = 'Level of significance', inputId = 'alphaSig', 
                                 value = 0.05, max = 0.2,min = 0.01, step = 0.001)),
            fluidRow( downloadButton("modelCoeffdwn", "Download model's coefficients"), dataTableOutput('modelCoeffTable'),
                      box(uiOutput('AccuracyModel')
                          , infoBoxOutput("Kappa")
                          ,infoBoxOutput("AccuracySD"),
                          infoBoxOutput("KappaSD"),width = 12)
            )
            # box(title = 'Model metrics'),
            # box(title = 'Model tunning')     
    )
    # Section End Items ------------------------------
  )
  
)

#header <- dashboardHeader()




## Section User Interface ------------------------------

ui <- dashboardPage(
  skin = "purple",
  header = dashboardHeader(title = "Axefinance"),
  sidebar = sidebar,
  body = body)
## Section Server------------------------------

server <- function(input, output) {
  # Section From Excel to correct format Server ------------------------------
  
  FromExcel <- reactive({
    inFile <- input$fileIn
    if (is.null(inFile)) return(NULL)
    a<-readxl::read_excel(inFile$datapath,sheet =input$ExcelSheet[1])
    a<-as.data.frame(a)
    return(a)
  })
  output$selectSheet <- renderUI({
    inFile <- input$fileIn
    if (is.null(inFile)) return(NULL)
    path<-inFile$datapath
    selectInput(label = 'Select Excel Sheet', inputId = 'ExcelSheet', choices = getSheetNames(path))
    
  })
  toCorrectFormat <- reactive({
    req(FromExcel)
    data<-FromExcel()
    
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
  output$tabHEAD <-renderTable({
    req(toCorrectFormat)
    data<-toCorrectFormat()
    data
  })
  output$TABLook <-renderDataTable({
    req(FromExcel)
    data<-FromExcel()
    head(data)
  })
  output$strData <-renderPrint({
    req(toCorrectFormat)
    data<-toCorrectFormat()
    str(data)
  })
  output$asRDS <- downloadHandler(filename = function() {
    paste0("RawDataCompressed_", Sys.Date(), ".rds")
  },
  content = function(file) {
    save_list <- toCorrectFormat()
    saveRDS(save_list, file)
  })
  output$column <- renderUI({
    data<-FromExcel()
    if (is.null(FromExcel()))
    {return()}
    else
    {
      
      NUM<-dplyr::select_if(as.data.frame(data), is.numeric)
      Logica<-dplyr::select_if(as.data.frame(data), is.logical)
      facto<-dplyr::select_if(as.data.frame(data), is.factor)
      Char<-dplyr::select_if(as.data.frame(data), is.character)
      
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
      )}
    
  })
  # Section From CSV to correct format Server ------------------------------
  
  FromCSV <- reactive({
    inFile <- input$fileInCSV
    if (is.null(inFile)) return(NULL)
    a<-read.csv(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  toCorrectFormatCSV <- reactive({
    req(FromCSV)
    data<-FromCSV()
    
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
  output$tabHEADCSV <-renderTable({
    req(toCorrectFormatCSV)
    data<-toCorrectFormatCSV()
    data
  })
  output$TABLookCSV <-renderDataTable({
    req(FromCSV)
    data<-FromCSV()
    head(data)
  })
  output$strDataCSV <-renderPrint({
    req(toCorrectFormatCSV)
    data<-toCorrectFormatCSV()
    str(data)
  })
  output$asRDSCSV <- downloadHandler(filename = function() {
    paste0("RawDataCompressed_", Sys.Date(), ".rds")
  },
  content = function(file) {
    save_list <- toCorrectFormatCSV()
    saveRDS(save_list, file)
  })
  output$columnCSV <- renderUI({
    data<-FromCSV()
    if (is.null(FromCSV()))
    {return()}
    else
    {
      
      NUM<-dplyr::select_if(as.data.frame(data), is.numeric)
      Logica<-dplyr::select_if(as.data.frame(data), is.logical)
      facto<-dplyr::select_if(as.data.frame(data), is.factor)
      Char<-dplyr::select_if(as.data.frame(data), is.character)
      
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
      )}
    
  })
  
  # Section Multivariate descriptive analysis Server ------------------------------
  
  Data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$plot<-renderPlot({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    ggpairs(data[,input$X],  ggplot2::aes(colour=factor(data[,input$Y])))
  })
  output$plot2<-renderPlot({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    pairs.panels(data[,input$X], 
                 method = as.character(input$rho), # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
    )
  })
  output$mat3<-renderPlot({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    
    pairs(data[,input$X], pch = 19,  cex = 0.5,
          col = factor(data[,input$Y]),
          lower.panel=NULL)
  })
  output$targetName <- renderUI({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    selectInput(label = 'Color by',inputId = 'Y', choices = names(data),selected = names(data)[ncol(data)])
  })
  output$features <- renderUI({
    req(Data)
    data<-Data()
    if (is.null(data())) {
      return ()
    }
    selectizeInput(label = 'Features in pair wise matrix',inputId = 'X', choices = names(data),multiple=TRUE,
                   selected = names(data)[1:2])
  })
  
  # Section Exploring missings Server ------------------------------
  
  MisData <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  shadowData <- reactive({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    data<-data %>%
      bind_shadow(only_miss = TRUE)
    data
  })
  output$plot3<-renderPlot({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    
    if(nrow(data)<10000){
      return(vis_miss(data, cluster = input$clus, sort_miss = TRUE))
    }
    else{
      return(aggr(data, col=c('blue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.8, gap=3, 
                  ylab=c("Histogram of missing data", "Red is Missing Data")))
    }
  })
  output$misstable<-renderDataTable({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    
    data<-miss_var_summary(data) %>% filter(pct_miss > 0)
    data$pct_miss<-round(data$pct_miss,2)
    names(data)<-c("Features","Number of missing values", "% of missing values")
    datatable(data ,options = list(pageLength = 4))
  })
  output$centFeatures <- renderUI({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    selectizeInput(label = 'Features to keep',inputId = 'pX', choices = names(data),multiple=TRUE,
                   selected = names(data))
  })
  centredData <- reactive({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    a<-data[complete.cases(data),input$pX]
  })
  output$denstrack<-renderPlotly({
    req(shadowData)
    data<-shadowData()
    if (is.null(data())) {
      return ()
    }
    p<-ggplot(data,aes_string(x = input$tracked,fill = input$trackM)) + geom_density(alpha=0.45)
    ggplotly(p)
  })
  output$boxtrack<-renderPlotly({
    req(shadowData)
    data<-shadowData()
    if (is.null(data())) {
      return ()
    }
    p<-ggplot(data,aes_string(y=input$tracked,x=input$trackM,fill = input$trackM)) + geom_boxplot()
    ggplotly(p)
    
  })
  output$bimiss<-renderPlotly({
    req(MisData)
    data<-MisData()
    if (is.null(data())) {
      return ()
    }
    
    p<-ggplot(data,aes_string(x = input$V1,y = input$V2)) + geom_miss_point()
    ggplotly(p)
    
    
  })
  output$CM <- downloadHandler(filename = function() {
    paste('Centered matrix', ".csv", sep = "")
  },content = function(file) {
    write.csv(centredData(), file, row.names = FALSE)
  })
  # Section Univariate descriptive analysis Server ------------------------------
  repo <- reactive({
    inFile <- input$file0
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  
  output$skimNum<-renderDataTable({
    req(repo)
    data<-repo()
    if (is.null(data())) {
      return ()
    }
    data<-  data %>%
      select_if(is.numeric)
    if(is.data.frame(data) && ncol(data)==0){return()}
    X<-as.data.frame(skim(data))
    X[,c(1:6,12)]
  })
  output$skimFac<-renderDataTable({
    req(repo)
    data<-repo()
    if (is.null(data())) {
      return ()
    }
    
    data<-  data %>%
      select_if(is.factor)
    if(is.data.frame(data) && ncol(data)==0){return()}
    skim(data)
    
  })
  output$skimother<-renderDataTable({
    req(repo)
    data<-repo()
    if (is.null(data())) {
      return ()
    }
    num<-  data %>%
      select_if(is.numeric)
    fac <- data %>%
      select_if(is.factor)
    data <- data [,!names(data) %in% c(names(num),names(fac))]
    if(is.data.frame(data) && ncol(data)==0){return()}
    skim(data)#[,c(2:4,10:12)]
    
  })
  
  
  # Section Tree categorization Server ------------------------------
  
  treeInput <- reactive({
    inFile <- input$fileTC
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$targetDis <- renderUI({
    req(treeInput)
    data<-treeInput()
    if (is.null(data())) {
      return ()
    }
    data<-as.data.frame(data)
    selectInput(label = 'Target',inputId = 'Ytree',
                choices = names(data),selected = names(data)[ncol(data)])
  })
  output$featuresDis <- renderUI({
    req(treeInput)
    data<-treeInput()
    if (is.null(data())) {
      return ()
    }
    data<-as.data.frame(data)
    selectizeInput(label = 'Features',inputId = 'Xtree',multiple=TRUE,
                   choices = names(data[,names(data)!=input$Ytree]),
                   selected = names(data[,names(data)!=input$Ytree]))
  })
  output$plotTree<-renderPlot({
    data<-treeInput()
    data<-as.data.frame(data)
    df<-data[,names(data) %in% c(as.character(input$Xtree),as.character(input$Ytree))]
    df <- as.data.frame(sapply(data, as.factor))
    df <- data %>% select(which(names(data) %in% c(input$Xtree, input$Ytree) ))
    ctree = ctree(as.formula(paste(as.character(input$Ytree), "~.")),
                  data = df,
                  na.action = na.exclude,
                  control = ctree_control(minbucket = ceiling(input$ms * nrow(df))))
    
    
    plot(ctree)
    
  })
  
  # Section Features categorization Server ------------------------------
  
  toDis <- reactive({
    inFile <- input$fileFC
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  dis <- eventReactive(input$Get,{
    req(toDis)
    data<-toDis()
    if (is.null(data())) {
      return ()
    }
    D<-data.frame(data[,input$Xdis],as.factor(data[,input$Ydis]))
    disc=chiM(D,alpha=input$khi2)
    disc$Disc.data<-cbind(disc$Disc.data,cut(data[,input$Xdis],
                                             breaks=c(min(data[,input$Xdis]), 
                                                      round(unlist(disc$cutp),0)-1,
                                                      max(data[,input$Xdis])), 
                                             include.lowest=TRUE,right = FALSE))
    names(disc$Disc.data)<-c('Groups', 'Target', 'Partitions')
    as.data.frame(disc$Disc.data)
  })
  discretized<-reactive({
    disc <- dis()
    data<-toDis()
    x<-names(data)
    y<-paste('Cat', input$Xdis, sep = '_')
    a<-data.frame(data,disc[,'Partitions'])
    names(a)<-c(x,y)
    a
  })
  output$generatedDis<-renderPlotly({
    data<-discretized()
    data<-as.data.frame(data)
    data[,input$Ydis]<-as.factor(data[,input$Ydis])
    ggplot(data) +
      geom_bar(aes_string(paste('Cat', input$Xdis, sep = '_'), fill = input$Ydis), position = 'fill')+
      coord_flip() +
      scale_fill_manual(values=c('green','red'))
  })
  output$resptar <- renderUI({
    req(toDis)
    data<-toDis()
    if (is.null(data())) {
      return ()
    }
    selectInput(label = 'In respect to',inputId = 'Ydis', choices = names(data),selected = names(data)[ncol(data)])
  })
  output$tobin <- renderUI({
    req(toDis)
    data<-toDis()
    if (is.null(data())) {
      return ()
    }
    selectInput(label = 'Feature to discretize',inputId = 'Xdis', choices = names(select_if(as.data.frame(data), is.numeric)))
  })
  output$DiscreteData <- downloadHandler(filename = function() {
    paste('Discretized_',input$Xdis, ".csv", sep = "")
  },content = function(file) {
    write.csv(discretized(), file, row.names = FALSE)
  })
  
  disWOE2<-reactive({
    req(toDis)
    data<-toDis()
    if (is.null(data())) {
      return ()
    }
    
    if(input$InvLab == TRUE){
      data[,input$Ydis]<-ifelse(data[,input$Ydis]==1,0,1)
    }
    data[,input$Ydis]<-as.numeric(data[,input$Ydis])
    
    numericData<-select_if(as.data.frame(data[,names(data)!=input$Ydis]), is.numeric)
    factorData<-select_if(as.data.frame(data[,names(data)!=input$Ydis]), is.factor)
    Q<-data.frame()
    for (k in 1:ncol(numericData)) {
      result=smbinning(df=data,y=input$Ydis,x=names(numericData)[k], p = input$MinSpWoE ) 
      if(!is.character(result)){
        tab<-as.data.frame(result$ivtable) %>% mutate(Variable = names(numericData)[k])
        Q<-rbind(Q,tab)
      }
    }
    maxcut<-max(sapply(factorData, function(x) length(unique(x))) )
    for (k in 1:ncol(factorData)) {
      result=smbinning.factor(data,x=names(factorData)[k],y=input$Ydis, maxcat=maxcut)
      tab<-as.data.frame(result$ivtable) %>% mutate(Variable = names(factorData)[k])
      Q<-rbind(Q,tab)
    }
    Q<-Q[,c('Variable',names(Q[,1:14]))]
    return(Q)
  })
  output$WOETAB<-renderDataTable({
    req(disWOE2)
    data<-disWOE2()
    if (is.null(data())) {
      return ()
    }
    names(data)<- c('Variable','Cut point', 'Class count', 'Good count', 'Bad count',
                    'Cumulative class count ', 'Cumulative good count ','Cumulative bad count ',
                    'Cut percentage','Good rate', 'Bad rate', 'Odds','LnOdds', 'WoE',
                    'Information Value IV')
    data
  })
  output$WOETABLE <- downloadHandler(filename = function() {
    paste('WoE_Table', ".xlsx", sep = "")
  },content = function(file) {
    write.xlsx(disWOE2(), file, row.names = FALSE)
  })
  output$WoEplot<-renderPlot({
    req(disWOE2)
    data<-disWOE2()
    if (is.null(data())) {
      return ()
    }
    sub<-filter(data,Variable == input$Xdis) %>% 
      filter(! Cutpoint %in% c('Missing','Total')) %>%
      mutate(sign= ifelse(WoE >0,'>0',ifelse(WoE<0,'<0',NA)))
    
    dat<-sub[,c("Cutpoint","GoodRate","BadRate","WoE","Odds","sign")]
    dat<-gather(dat,
                key = "Event",
                value = "Rates",
                -c(Cutpoint,WoE,Odds,sign))
    p1<-dat %>% 
      ggplot( aes(x=Cutpoint, y=WoE)) +
      labs(col = 'sign of WoE') +
      geom_bar(stat = "identity",aes(fill=factor(sign))) +
      scale_fill_manual(values=c('red','green')) +
      geom_text(aes(label = WoE), color = "black", size = 4)+
      labs(fill = 'sign of WoE')
    p2<-ggplot(data=dat, aes(x=Cutpoint, y=Rates, fill = factor(Event))) +
      geom_bar(stat="identity")+
      scale_fill_manual(values=c('red','green'))+
      labs(fill = 'Event')
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  })
  
  
  # Section Feature re-encoding Server ------------------------------
  EncodeInput <- reactive({
    inFile <- input$fileVEnc
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$varsToEncode <- renderUI({
    data <- EncodeInput()
    selectInput(label = 'Variables', inputId = 'toRec',choices = names(data))
  })
  output$levelsInput <- renderUI({
    x<-EncodeInput()
    x<-x[,input$toRec]
    # Section get factor's levels ------------------------------
    
    out <- "<table><tbody>"
    if (is.factor(x) )
    {levs <- levels(x)}else
    { levs <- stats::na.omit(unique(x))}
    if (any(is.na(x))) {levs <- c(levs, NA)}
    
    # Section Generate table ------------------------------
    
    for (l in levs) {
      out <- paste0(out, "<tr>")
      out <- paste0(out, "<td class=\"right vertical-align\">",
                    htmltools::htmlEscape(l), 
                    "&nbsp;<span class=\"glyphicon glyphicon-arrow-right left-sep\" aria-hidden=\"true\"></span> &nbsp;</td>")
      label <- l
      l <- gsub(":", "_", l)
      id <- paste0("ireclev_", l)
      if (id == "ireclev_NA") {
        label <- "NA"
      }
      if (id == "ireclev_") {
        label <- ""
      }
      id=paste(id,'_',input$toRec,'_',sep = '')
      out <- paste0(out, "<td class=\"vertical-align\">",
                    textInput(id, "", label), "</td>")
      out <- paste0(out, "</tr>")
    }
    out <- paste0(out, "</tbody></table>")
    HTML(out)
  })
  output$tableToEncode <- renderDataTable({
    data<-EncodeInput()
    datatable(data = data,options = list(pageLength = 5))
  })
  EncodedTable <- reactive({
    data<-EncodeInput()
    x <- data[,input$toRec]
    y<- input$toRec
    
    choices <- AllInputs() 
    
    
    txt <- paste('data$',y, ' <- recode(data$',y, sep = '')
    for (lev in 1:nrow(choices)) {
      n <-choices[lev,'New']
      o <-choices[lev,'Old']
      txt <- paste(txt,',' ,"'", o,"'",' = ',"'",n,"'",sep = '')
    }
    txt <- paste(txt, ')',sep = '')
    eval(parse(text=txt))
    return(data)
  })
  output$tableEncoded <- renderDataTable({
    data<-EncodedTable()
    datatable(data = data,options = list(pageLength = 5))
  })
  AllInputs <- reactive({
    Inputs <- c()
    InputValue <-c()
    for(i in 1:length(names(input))){
      if(str_detect(names(input)[i],'irec')){
        Inputs[i] <- names(input)[i]
        InputValue[i] <- input[[names(input)[i]]]
      } 
      
      #InputValue[i] <- input[[names(input)[i]]]
      
    }
    df <- data.frame(In = Inputs, Val = InputValue)
    df<-df %>% filter(!is.na(In)) %>%
      mutate(Feature = str_detect(In,input$toRec)) %>%
      filter(Feature==TRUE)
    p <- separate(df,In, into =c('A','OldVal','B'), sep = '_' )
    
    return(data.frame(Input = df$In,Old = p$OldVal,New = df$Val, selected = df$Feature))
  })
  output$show_inputs <- renderTable({
    data<-AllInputs()
    data
  })
  output$EncodedasRDS <- downloadHandler(filename = function() {
    paste0("EcodedData_", Sys.Date(), ".rds")
  },
  content = function(file) {
    save_list <- EncodedTable()
    saveRDS(save_list, file)
  })
  output$EncodeTAB <-renderUI({
    if (is.null(EncodeInput()))
      return()
    else
    {return(dataTableOutput('tableEncoded'))}
  })
  # Section Exploratory Factor Analysis Server ------------------------------
  EFAdata <- reactive({
    inFile <- input$fileEFA
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    col_names <- names(a)
    a[col_names] <- lapply(a[col_names] , factor)
    return(a)
  })
  output$indivMCA<-renderPlot({
    req(EFAdata)
    data<-EFAdata()
    if (is.null(data())) {
      return ()
    }
    res.mca<-MCA(data,ncp = 5, graph = FALSE)
    a1<-as.numeric(input$dim1)
    a2<-as.numeric(input$dim2)
    eig.val <- get_eigenvalue(res.mca)
    fviz_mca_ind (res.mca,
                  label = "none", # masquer le texte des individus
                  habillage = "response", # colorer par groupes
                  palette = c ('#43F912','#FB2B06'),
                  addEllipses = TRUE, ellipse.type = "confidence",
                  ggtheme = theme_minimal (),
                  alpha.ind=input$alphaInd,
                  axes = c(a1,a2))
  })
  output$fiz_contib<-renderPlot({
    req(EFAdata)
    data<-EFAdata()
    if (is.null(data())) {
      return ()
    }
    res.mca<-MCA(data,ncp = 5, graph = FALSE)
    a1<-as.numeric(input$dim1)
    a2<-as.numeric(input$dim2)
    fviz_contrib(res.mca, choice = "var", axes = a1:a2, top = 15)
  })
  output$fiz_contib2<-renderPlot({
    req(EFAdata)
    data<-EFAdata()
    if (is.null(data())) {
      return ()
    }
    res.mca<-MCA(data,ncp = 5, graph = FALSE)
    a1<-as.numeric(input$dim1)
    a2<-as.numeric(input$dim2)
    fviz_mca_var(res.mca, col.var = as.character(input$cont),
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                 repel = input$rep, 
                 ggtheme = theme_minimal(),
                 select.var = list (cos2 = as.numeric(input$alpha)),shape.var = 17,
                 map = "symmetric",
                 axes = c(a1,a2)
    )
  })
  # Section Cluster of variables Server ------------------------------
  clustOfvars <- reactive({
    inFile <- input$fileClusOfVars
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    NonNum <-a %>%
      select_if(Negate(is.numeric))
    NUM <-a %>%
      select_if(is.numeric)
    
    if(ncol(a)==ncol(NUM))
    {df = NUM}else if (ncol(a)==ncol(NonNum))
    {df = NonNum} else {df = cbind(NonNum, NUM)}
    return(df)
  })
  output$hclustplot<-renderPlot({
    req(clustOfvars)
    data<-clustOfvars()
    vars_quanti <- data %>%
      select_if(is.numeric)
    
    vars_quali <- data%>%
      select_if(is.factor)
    
    
    if(ncol(data)==ncol(vars_quanti))
    {
      tree <- hclustvar(X.quanti=vars_quanti)
      dend_players <- as.dendrogram(tree)
      plot(dend_players)
      rect.hclust(tree, k=input$Ncluster,border =1:input$Ncluster)
    }else if (ncol(data)==ncol(vars_quali))
    {
      tree <- hclustvar(X.quali=vars_quali)
      dend_players <- as.dendrogram(tree)
      plot(dend_players)
      rect.hclust(tree, k=input$Ncluster,border =1:input$Ncluster)
    } else {
      tree <- hclustvar(X.quanti=vars_quanti,X.quali=vars_quali)
      dend_players <- as.dendrogram(tree)
      plot(dend_players)
      rect.hclust(tree, k=input$Ncluster,border =1:input$Ncluster)
    }
    
  })
  output$bootSamp<-renderUI({
    req(clustOfvars)
    data<-clustOfvars()
    numericInput(label = 'Bootstrap samples', inputId = 'bootStab', value = 40, step = 1,
                 min = 2, max = 100)
    
  })
  output$bootplot<-renderPlot({
    req(clustOfvars)
    data<-clustOfvars()
    vars_quanti <- data %>%
      select_if(is.numeric)
    
    vars_quali <- data%>%
      select_if(is.factor)
    
    
    if(ncol(data)==ncol(vars_quanti))
    {
      tree <- hclustvar(X.quanti=vars_quanti)
    }else if (ncol(data)==ncol(vars_quali))
    {
      tree <- hclustvar(X.quali=vars_quali)
    } else {
      tree <- hclustvar(X.quanti=vars_quanti,X.quali=vars_quali)
    }
    
    stability(tree,B=input$bootStab)
    
  })
  # Section Normalize Server------------------------------
  toNormalize <- reactive({
    inFile <- input$fileNCF
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$kurtSkew<-renderTable({
    req(toNormalize)
    data<-toNormalize()
    if (is.null(data())) {
      return ()
    }
    
    data.frame(Feature=input$noNorm,
               Kurtosis = round(kurtosis(data[,input$noNorm]), 2),
               Skewness = round(skewness(data[,input$noNorm]), 2))
    
  })
  Normalized <- reactive({
    req(toNormalize)
    data<-toNormalize()
    if (is.null(data())) {
      return ()
    }
    v<-data[,input$noNorm]
    ifelse(input$TransNorm == 'Sqr',w<- sapply(v, function(x){x^2}),
           ifelse(input$TransNorm == 'Cube',w<-sapply(v, function(x){x^3}),
                  ifelse(input$TransNorm == 'Exp',w<- exp(v),
                         ifelse(input$TransNorm == 'Sqrt',w<- sqrt(v),
                                ifelse(input$TransNorm == 'Ln',w<-log(v),
                                       ifelse(input$TransNorm == 'NegativeHyperbolic',w<-sapply(v, function(x){-1/x}),
                                              ifelse(input$TransNorm == 'NegativeSqrHyperbolic',w<-sapply(v, function(x){-1/x^2}),NA)))))))
    
    q<-names(data)
    data<-data.frame(cbind(data, w))
    names(data)<-c(q, paste(input$TransNorm,'_',input$noNorm, sep = ''))
    return(data)
    
  })
  output$transformed <- downloadHandler(filename = function() {
    paste('transformed_',input$noNorm, ".csv", sep = "")
  },content = function(file) {
    write.csv(Normalized(), file, row.names = FALSE)
  })
  output$uNnORMAL <- renderPlotly({
    req(Normalized)
    data<-Normalized()
    if (is.null(data())) {
      return ()
    }
    p<-ggplot(data) + geom_density(aes_string(input$noNorm))
    ggplotly(p)
  })
  output$Normal <- renderPlotly({
    req(Normalized)
    data<-Normalized()
    if (is.null(data())) {
      return ()
    }
    p<-ggplot(data) + geom_density(aes_string(paste(input$TransNorm,'_',input$noNorm, sep = '')))
    ggplotly(p)
  })
  output$kurtSkew2<-renderTable({
    req(Normalized)
    data<-Normalized()
    if (is.null(data())) {
      return ()
    }
    
    data.frame(Feature=paste(input$TransNorm,'_',input$noNorm, sep = ''),
               Kurtosis = round(kurtosis(data[,paste(input$TransNorm,'_',input$noNorm, sep = '')]), 2),
               Skewness = round(skewness(data[,paste(input$TransNorm,'_',input$noNorm, sep = '')]), 2))
    
  })
  # Section Dealing with missing data/outliers Server------------------------------
  toImpute <- reactive({
    inFile <- input$fileImp
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    return(a)
  })
  output$ImputeAt<-renderUI({
    req(toImpute)
    data<-toImpute()
    if (is.null(data())) {
      return ()
    }
    selectizeInput(inputId = 'ColImp',label='Impute at', multiple=TRUE, choices = names(data),
                   selected=names(dplyr::select_if(as.data.frame(data), is.numeric)))
  })
  output$ImputeAt2<-renderUI({
    if(input$allNo == FALSE){
      return(uiOutput("ImputeAt"))}
    else {return()}
  })
  output$ImputationMethods<-renderUI({
    selectInput(label = 'Select Imputation technique', inputId = 'ImpTech',
                choices = c('Median','Mean','PCA','Random Forest'), selected = 'Mean')
  })
  Imputed<-reactive({
    data <- toImpute()
    if(input$allNo ==TRUE) {
      if(input$ImpTech == 'Mean') {ImputedData <-impute_mean_all(data)}
      else if(input$ImpTech == 'Median'){ImputedData <-impute_median_all(data)}
      else if (input$ImpTech == 'PCA') {
        NumData <- select_if(as.data.frame(data), is.numeric) 
        NonNumData <- data [,!names(data) %in% names(NumData)]
        nb <- estim_ncpPCA(NumData,method.cv = "Kfold", verbose = FALSE) 
        res.comp <- imputePCA(as.data.frame(NumData), ncp = nb$ncp)
        imp <- res.comp$completeObs
        ImputedData <-data.frame(cbind(imp,NonNumData))
      }
      else if(input$ImpTech == 'Random Forest'){
        ImputedData <- missForest(data, verbose = TRUE)
        ImputedData <- ImputedData$ximp
      }
    }
    else if (input$allNo ==FALSE){
      if(input$ImpTech == 'Mean') {
        ImputedData <- impute_median_at(data, .vars =input$ColImp)
      }
      else if(input$ImpTech == 'Median'){
        ImputedData <-impute_median_all(data)
      }
    }
    return(ImputedData)
  })
  
  output$EvaluateImputation<-renderPlotly({
    data1<-toImpute()
    data2<-Imputed()
    var<-c(as.data.frame(data1)[,input$varPlot],as.data.frame(data2)[,input$varPlot])
    source<-c(rep('Original dist',nrow(data1)),rep('Imputed dist',nrow(data1)))
    df<-data.frame(source=source, Variable=var)
    p <- ggplot(df,aes_string(x = "Variable",fill = 'source')) + geom_density(alpha=0.45) +labs(x = input$varPlot)
    ggplotly(p)
  })
  
  
  output$varToPlot<-renderUI({
    data<-toImpute()
    selectInput(label = 'Plot imputed', inputId = 'varPlot',choices = names(data))
  })
  output$dwnImp <- downloadHandler(filename = function() {
    paste('Imputed_',input$ImpTech, ".csv", sep = "")
  },content = function(file) {
    write.csv(Imputed(), file, row.names = FALSE)
  })
  
  
  
  
  output$dwnImprds <- downloadHandler(filename = function() {
    paste0('Imputed_'          ,input$ImpTech, ".rds")
  },
  content = function(file) {
    save_list <- Imputed()
    saveRDS(save_list, file)
  })
  # Section DataBalance Server ------------------------------
  To3Dplot <- reactive({
    inFile <- input$fileDB
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    
    return(a)
  })
  output$targetbalance<-renderUI({
    data<-To3Dplot()
    selectInput(label = 'Target', inputId = 'Balanced',choices = names(data),selected = names(data)[ncol(data)])
  })
  output$BalancePlot<-renderPlot({
    data<-To3Dplot()
    if(input$InvLab2 == TRUE){data[,input$Balanced]<-ifelse(data[,input$Balanced]==1,0,1)}
    x<-table(data[,input$Balanced])
    t<-as.data.frame(prop.table(x))
    t$Freq1<-round(100*t$Freq,digits = 2)
    t$lab<-t$Var1
    t$Pct<-rep('(%)',nrow(t))
    t<-unite(t,lbls,lab,Freq1,Pct,sep = ' ')
    slices<-t$Freq
    lbls <- t$lbls
    pie3D(slices,labels=lbls,explode=0.1,col = c('#079C80', '#054236'),
          main=paste("Pie Chart of data balance") )
    
  })
  train_test_split <- reactive({
    inFile <- input$fileDB
    if (is.null(inFile)) return(NULL)
    a<-readRDS(inFile$datapath)
    a<-as.data.frame(a)
    
    set.seed(540)
    index_train <- createDataPartition(y=a[,input$Balanced],p= input$splitPct,list=FALSE)
    # Create training set: training_set
    training_set <- a[index_train, ]
    # Create test set: test_set
    test_set <- a[-index_train, ]
    
    l = list(training_set, test_set)
    return(l)
  })
  train_split <- reactive({
    data<-train_test_split()
    as.data.frame(data[1])
  })
  test_split <- reactive({
    data<-train_test_split()
    as.data.frame(data[2])
  })
  coef_model<-reactive({
    data<-train_split()
    
    # Define training control
    set.seed(123)
    train.control <- trainControl(method = "repeatedcv",
                                  number = as.numeric(input$kfold), repeats = 3)
    
    
    model <- train(response ~., data = data,
                   trControl = train.control,
                   method = "glm",
                   family=binomial())
    
    # Train the model
    model <- train(as.formula(paste(input$Balanced,'~.')), data = data, method = "glm",
                   trControl = train.control)
    
    # print cv scores
    y<-summary(model)
    y<- as.data.frame(y$coefficients)
    return(y)
  })
  output$modelCoeffTable <- renderDataTable({
    data<-coef_model()
    if (is.null(data())) {
      return ()
    }
    data$Significant=data[,4]<input$alphaSig
    names(data) <- c('Coefficient', 'StanardError', 'z value', 'p-Value','Significant')
    
    
    as.datatable(formattable(data,
                             list(Significant = formatter("span",
                                                          style = x ~ style(color = ifelse(x, "green", "red")),
                                                          x ~ icontext(ifelse(x, "ok", "remove")))))) %>%
      formatStyle('Coefficient',
                  backgroundColor = styleInterval(0, c('#079C80', '#054236')),
                  color = 'white') %>%
      formatRound(
        'Coefficient',
        digits = 2
      )%>%
      formatRound(
        'StanardError',
        digits = 2
      )%>%
      formatRound(
        'z value', 
        digits = 2
      )%>%
      formatRound(
        'p-Value',
        digits = 2
      ) 
    
    
  })
  coef_model_dwn<-reactive({
    data<-coef_model()
    if (is.null(data())) {
      return ()
    }
    data$Significant=data[,4]<input$alphaSig
    names(data) <- c('Coefficient', 'StanardError', 'z value', 'p-Value','Significant')
    data<-data.frame(Features = row.names(data),data)
    return(data)
  })
  output$modelCoeffdwn <- downloadHandler(filename = function() {
    paste('ModelCoeff',input$alphaSig, ".xlsx", sep = "")
  },content = function(file) {
    write.xlsx(coef_model_dwn(), file)
  })
  output$train <- downloadHandler(filename = function() {
    paste('train_split',input$splitPct, ".xlsx", sep = "")
  },content = function(file) {
    write.xlsx(train_split(), file)
    #write.csv(train_split(), file, row.names = FALSE)
  })
  output$test <- downloadHandler(filename = function() {
    paste('test_split',input$splitPct, ".xlsx", sep = "")
  },content = function(file) {
    write.xlsx(test_split(), file)
    #write.csv(test_split(), file, row.names = FALSE)
  })
  output$BalancePlotTrain<-renderPlot({
    data<-train_split()
    if(input$InvLab2 == TRUE){data[,input$Balanced]<-ifelse(data[,input$Balanced]==1,0,1)}
    x<-table(data[,input$Balanced])
    t<-as.data.frame(prop.table(x))
    t$Freq1<-round(100*t$Freq,digits = 2)
    t$lab<-t$Var1
    t$Pct<-rep('(%)',nrow(t))
    t<-unite(t,lbls,lab,Freq1,Pct,sep = ' ')
    slices<-t$Freq
    lbls <- t$lbls
    pie3D(slices,labels=lbls,explode=0.1,col = c('#079C80', '#054236'),
          main=paste("Pie Chart of train set balance") )
    
  })
  output$BalancePlotTest<-renderPlot({
    data<-test_split()
    if(input$InvLab2 == TRUE){data[,input$Balanced]<-ifelse(data[,input$Balanced]==1,0,1)}
    x<-table(data[,input$Balanced])
    t<-as.data.frame(prop.table(x))
    t$Freq1<-round(100*t$Freq,digits = 2)
    t$lab<-t$Var1
    t$Pct<-rep('(%)',nrow(t))
    t<-unite(t,lbls,lab,Freq1,Pct,sep = ' ')
    slices<-t$Freq
    lbls <- t$lbls
    pie3D(slices,labels=lbls,explode=0.1,col = c('#079C80', '#054236'),
          main=paste("Pie Chart of test set balance") )
    
  })
  
  model_outputs<-reactive({
    data<-train_split()
    
    # Define training control
    set.seed(123)
    train.control <- trainControl(method = "repeatedcv",
                                  number = as.numeric(input$kfold), repeats = 3)
    
    
    model <- train(response ~., data = data,
                   trControl = train.control,
                   method = "glm",
                   family=binomial())
    
    # Train the model
    model <- train(as.formula(paste(input$Balanced,'~.')), data = data, method = "glm",
                   trControl = train.control)
    
    # print cv scores
    y<-summary(model)
    y<- as.data.frame(y$coefficients)
    x <-model$results
    out <- list(x,y)
    return(out)
  })
  
  output$approvalBoxdwn <- renderInfoBox({
    l<-model_outputs()
    infoBox(
      "Accuracy",paste(as.character(round(as.numeric(unlist(l[1])[2]),4)*100),'%'), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  output$approvalBoxup <- renderInfoBox({
    l<-model_outputs()
    infoBox(
      "Accuracy",paste(as.character(round(as.numeric(unlist(l[1])[2]),4)*100),'%'), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$Kappa <- renderInfoBox({
    l<-model_outputs()
    infoBox(
      "Kappa", as.character(round(as.numeric(unlist(l[1])[3]),4)), icon = icon("list"),
      color = "purple"
    )
  })
  output$AccuracySD <- renderInfoBox({
    l<-model_outputs()
    infoBox(
      "Accuracy Standard Deviation", as.character(round(as.numeric(unlist(l[1])[4]),4)), icon = icon("list"),
      color = "purple"
    )
  })
  output$KappaSD <- renderInfoBox({
    l<-model_outputs()
    infoBox(
      "Kappa  Standard Deviation", as.character(round(as.numeric(unlist(l[1])[5]),4)), icon = icon("list"),
      color = "purple"
    )
  })
  
  
  # Section LDA Server ------------------------------
  # InputLDA <- reactive({
  #   inFile <- input$fileLDA
  #   if (is.null(inFile)) return(NULL)
  #   a<-readRDS(inFile$datapath)
  #   a<-as.data.frame(a)
  #   return(a)
  # })
  # Section UIs Server ------------------------------
  # Section Univariate analysis UIs Server ------------------------------
  output$skimTabNuM<-renderUI({
    if (is.null(repo()))
      return()
    else
      dataTableOutput('skimNum')
  })
  output$skimTabFac<-renderUI({
    if (is.null(repo()))
      return()
    else
      dataTableOutput('skimFac')
  })
  output$skimTabOther<-renderUI({
    if (is.null(repo()))
      return()
    else
      dataTableOutput('skimother')
  })
  # Section Multivariate analysis UIs Server ------------------------------
  output$tabs1<-renderUI({
    if (is.null(Data()))
    {return()}else
    {tabBox(
      title = "Pairwise matrices",
      tabPanel("Matrix 1",plotOutput('plot')),
      tabPanel("Matrix 2", plotOutput('plot2')),
      tabPanel("Matrix 3", plotOutput('mat3'))
    )}
  })
  # Section Exploring missings UIs Server ------------------------------
  output$plotBimiss<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        plotlyOutput('bimiss')
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$plotboxtrack<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        plotlyOutput('boxtrack')
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$plotdenstrack<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        plotlyOutput('denstrack')
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$plot3UI<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        plotOutput('plot3')
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$missReport<-renderUI({
    
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        dataTableOutput('misstable')
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
    
  })
  output$tracker<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        req(shadowData)
        data_NA<-shadowData()
        D<-names(data)
        DNA<- names(data_NA[,!names(data_NA) %in% names(data)])
        selectInput(label = 'Miss tracker',inputId = 'trackM',choices = DNA,selected = DNA[1]) 
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$tracked<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        selectInput(label = 'Miss to be tracked',inputId = 'tracked',choices = names(data),selected = names(data)[1] )
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
    
  })
  output$cluscheckbox<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        if(nrow(MisData())<10000){
          checkboxInput(label = 'Cluster missings',inputId = 'clus',value = FALSE) 
        }}
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$MissV1<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        selectInput(label = 'Variable 1',inputId = 'V1',choices = names(data),selected = names(data)[1] )
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  output$MissV2<-renderUI({
    if (is.null(MisData()))
    {return()} else
    {
      req(MisData)
      data<-MisData()
      if(any_na(data)){
        selectInput(label = 'Variable 2',inputId = 'V2',choices = names(data),selected = names(data)[1] )
      }
      else{
        return(HTML("<center><b><h2 style='color:green;''>Congrats!! No missing data </h2></b></center>"))
      }
    }
  })
  # Section Features categorization UIs Server ------------------------------
  output$WoEplots<-renderUI({
    if (is.null(toDis()))
      return()
    else
      plotOutput('WoEplot')
  })
  output$WoEtab<-renderUI({
    if (is.null(toDis()))
      return()
    else
      dataTableOutput('WOETAB')
  })
  # Section Missing imputation UIs Server ------------------------------
  output$CompareImputedOrigPlot<-renderUI({
    if (is.null(toImpute()))
      return()
    else
      plotlyOutput('EvaluateImputation')
  })
  # Section Exploratory Factor Analysis Server ------------------------------
  output$PlotMCAindivs<-renderUI({
    if (is.null(EFAdata()))
      return()
    else
      plotOutput('indivMCA')
  })
  output$PlotMCAplanContrib<-renderUI({
    if (is.null(EFAdata()))
      return()
    else
      plotOutput('fiz_contib')
  })
  output$PlotMCAtopmod<-renderUI({
    if (is.null(EFAdata()))
      return()
    else
      plotOutput('fiz_contib2')
  })
  # Section clusterofvars UIs Server ------------------------------
  output$PlothtreeOfVars<-renderUI({
    if (is.null(clustOfvars()))
      return()
    else
      plotOutput('hclustplot')
  })
  output$Plotbootscriteria<-renderUI({
    if (is.null(clustOfvars()))
      return()
    else
      plotOutput('bootplot')
  })
  output$optimalk<-renderUI({
    if (is.null(clustOfvars()))
    {return()}
    else
    { 
      req(clustOfvars)
      data<-clustOfvars()
      numericInput(label = 'Number of cluster', inputId = 'Ncluster', value = 2, step = 1,
                   min = 2, max = ncol(data)-1)
    }
    
  })
  # Section Normalize UIs Server ------------------------------
  output$UnNormal <- renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      req(toNormalize)
      data<-toNormalize()
      data<-dplyr::select_if(as.data.frame(data), is.numeric)  
      selectInput(label = 'Features to normalize',inputId = 'noNorm', choices = names(data),selected = names(data))
      
    }
  })
  output$Transformation <- renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      req(toNormalize)
      data<-toNormalize()
      skew <- skewness(data[,input$noNorm])
      if(skew < 0) {LR<-c('Sqr','Cube','Exp')}
      if(skew > 0) {LR<-c('Sqrt','Ln','NegativeHyperbolic','NegativeSqrHyperbolic')}
      selectInput(label = 'Possible transformations',inputId = 'TransNorm', choices = LR,selected = LR[1])
      
    }
  })
  output$NormalPlotly<-renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      plotlyOutput('Normal')
    }
  })
  output$uNnORMALPlotly <- renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      plotlyOutput('uNnORMAL')
    }
  })
  output$kurtSkewTAB <- renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      tableOutput('kurtSkew')
    }
  })
  output$kurtSkew2TAB <- renderUI({
    if (is.null(toNormalize()))
    {return()} else
    {
      tableOutput('kurtSkew2')
    }
  })
  # Section Modeling UIs Server ------------------------------
  output$tosplitPlot<-renderUI({
    if (is.null(To3Dplot()))
      return()
    else
      plotOutput('BalancePlot')
  })
  output$trainPlot<-renderUI({
    if (is.null(To3Dplot()))
      return()
    else
      plotOutput('BalancePlotTrain')
  })
  output$testPlot<-renderUI({
    if (is.null(To3Dplot()))
      return()
    else
      plotOutput('BalancePlotTest')
  })
  output$AccuracyModel<-renderUI({
    if (is.null(model_outputs()))
      return()
    else
    {
      l<-model_outputs()
      acc <- as.numeric(unlist(l[1])[2])
      if(acc < 0.7) {
        infoBoxOutput("approvalBoxdwn") 
      } else
      {
        infoBoxOutput("approvalBoxup") 
      }
    }
  })
  
  
  # Section Tree categorization UIs ------------------------------
  output$DistreePlot<-renderUI({
    if (is.null(treeInput()))
      return()
    else
      plotOutput("plotTree")
  })
  # End Server ------------------------------
}

# RunApp ------------------------------
shinyApp(ui, server)