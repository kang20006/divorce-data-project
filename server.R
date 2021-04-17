library(data.table)
library(MASS)

# Read in the RF model
model <- readRDS("model.rds")
#model <- readRDS("https://github.com/kang20006/divorce-data-project/raw/main/model.rds")
shinyServer(function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({
    
    df <- data.frame(
      Name = c("Atr1","Atr2","Atr3","Atr4","Atr5","Atr6","Atr7","Atr8","Atr9","Atr10","Atr11","Atr12",
               "Atr13","Atr14","Atr15","Atr16","Atr17","Atr18","Atr19","Atr20","Atr21","Atr22","Atr23","Atr24",
               "Atr25","Atr26","Atr27","Atr28","Atr29","Atr30","Atr31","Atr32","Atr33","Atr34","Atr35","Atr36",
               "Atr37","Atr38","Atr39","Atr40","Atr41","Atr42","Atr43","Atr44","Atr45","Atr46","Atr47","Atr48",
               "Atr49","Atr50","Atr51","Atr52","Atr53","Atr54"),
      
      Value = as.character(c(input$Atr1,input$Atr2,input$Atr3,input$Atr4,input$Atr5,input$Atr6,input$Atr7,input$Atr8,input$Atr9,input$Atr10,input$Atr11,input$Atr12,      
                             input$Atr13,input$Atr14,input$Atr15,input$Atr16,input$Atr17,input$Atr18,input$Atr19,input$Atr20,input$Atr21,input$Atr22,input$Atr23,input$Atr24,
                             input$Atr25,input$Atr26,input$Atr27,input$Atr28,input$Atr29,input$Atr30,input$Atr31,input$Atr32,input$Atr33,input$Atr34,input$Atr35,input$Atr36,       
                             input$Atr37,input$Atr38,input$Atr39,input$Atr40,input$Atr41,input$Atr42,input$Atr43,input$Atr44,input$Atr45,input$Atr46,input$Atr47,input$Atr48,       
                             input$Atr49,input$Atr50,input$Atr51,input$Atr52,input$Atr53,input$Atr54)),
      
      stringsAsFactors = FALSE)
    
    
    df <- rbind(df)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    pred=predict(model,test)
    
    Output <- data.frame(pred$class, round(pred$posterior, 3))
    names(Output)[2]<-"prob Class 0"
    names(Output)[3]<-"prob Class 1"
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
  
  #dataset
  output$table <- renderDataTable(data)
  
  #predicted data
  output$pred <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,sep=";")
    pred<- predict(model,df)
    out <- data.frame(pred$class, round(pred$posterior, 3))
    names(out)[2]<-"prob Class 0"
    names(out)[3]<-"prob Class 1"
    return(out)
  })
  
})




