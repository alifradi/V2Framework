InputDataFrame <- function(input_id,sheet=NULL,separator = NULL){
  inFile <- input$input_id
  if (is.null(inFile)) return(NULL)
  Path <- as.character(inFile$datapath)
  if(str_detect(Path,"xlsx")){
    a<-readxl::read_excel(inFile$datapath,sheet =sheet,col_names = TRUE)
  } else if(str_detect(Path,"rds")){
    a<-readRDS(inFile$datapath)
  } else if(str_detect(Path,"csv")){
    a<-read.csv(inFile$datapath,sep = separator)
  } 
  as.data.frame(a)
}