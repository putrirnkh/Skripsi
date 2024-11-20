#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard) #membuat dashboard
library(DT) #membuat Data Tabel
library(naivebayes) #paket naivebayes
library(tm) #untuk text mining (corpus)
library(dplyr) #untuk analisis data dan tabel data
library(stringr)  #untuk operasi string umum (str_count)
library(wordcloud) #membuat wordcloud
library(RColorBrewer) #untuk warnain wordcloud
library(caret) #membuat cross validation
library(ggplot2) #membuat grafik


shinyServer(function(input, output) {
  
  # data input training #
  datainputtrain <- function(inputfiletrain){
    class(data)
    file <- input$datatraining 
    if (is.null( file))
      return(NULL)
    
    datatrain <- read.csv(file$datapath, sep = ";")
    
    # split data
    splitdata <- round(nrow(datatrain) * 0.80)
    
    
    # train
    trainingdt <- datatrain[1:splitdata, ]
    
    # test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    
    print(trainingdt)
  }
  
  # data input testing #
  
  datainputtest <- function(inputfiletest){
    class(data)
    file <- input$datatraining 
    if (is.null( file))
      return(NULL)
    
    datatrain <- read.csv(file$datapath, sep = ";")
    
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    
    print(testingdt)
  }
  
    

  
  #--- FileDataTraining---
  
  fileDataTraining <- function(inputfile){
  
    print(datainputtrain())
  }
  
  #----- Output Data Training -------
  output$dataTraining = renderDataTable({
    datatable(fileDataTraining(),  options = list(lengthChange = FALSE))
  })
  
 #preprocessing 

 
  fileDataPreprocessing <- function(dataTraining){
    print(datainputtrain())
  
    tweets.df <- as.data.frame(datainputtrain())
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
  }
  
  #----- Output preprocessing-------------------------------
  
  output$preprocessing = renderDataTable({
    datatable(fileDataPreprocessing(), options = list(lengthChange = FALSE))
  })
  
#------------ Klasifikasi -----------------------
  
  
  hasilklasifikasitrain <- function(fileDataTraining){
    datatraining <- input$datatraining 
    if (is.null( datatraining))
      return(NULL)
    
    datatrain <- read.csv( datatraining$datapath, sep = ";")
    # split data
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # train
    trainingdt <- datatrain[1:splitdata, ]
    
    # test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    
    tweets.df <- as.data.frame(trainingdt)
    
    komen <- trainingdt$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    #model Naive Bayes
    modelNB <- naive_bayes(label~positif + netral + negatif, data = trainingdt, laplace = 1 )
    #predict train
    prediksitrain <- predict(modelNB, trainingdt, type = "class")
    
    argumen <- ifelse(prediksitrain == trainingdt$label, "Benar","Tidak Benar")
    
    data <- data.frame(text = dataframe$text, sistem = prediksitrain,manual = trainingdt$label, hasil = argumen)
    
  }
  
  
  grafikklasifikasitrain <- function(hasilklasifikasitrain){
    print(hasilklasifikasitrain())
    
    hasil <- as.data.frame(hasilklasifikasitrain())
    
    library(ggplot2)
    # plot distribution of polarity
    ggplot(hasil, aes(x=sistem)) +
      geom_bar(aes(y=..count.., fill=sistem)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="klasifikasi categories", y="number of tweets") 
    
    }
  akurasiklasifikasitrain <- function(fileDataTraining){
    datatraining <- input$datatraining 
    if (is.null( datatraining))
      return(NULL)
    
    datatrain <- read.csv( datatraining$datapath, sep = ";")
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    
    tweets.df <- as.data.frame(trainingdt)
    
    komen <- trainingdt$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
   # library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    #model Naive Bayes
    modelNB <- naive_bayes(label~., data = trainingdt, laplace = 1 )
    #predict train
    prediksitrain <- predict(modelNB, trainingdt, type = "class")
    
    argumen <- ifelse(prediksitrain == trainingdt$label, "Benar","Tidak Benar")
    
    data <- data.frame(text = dataframe$text, sistem = prediksitrain,manual = trainingdt$label, hasil = argumen)
    
    hasilpenjumlahan <- data.frame(string = argumen)
    pelabelan <- data.frame(string = data)
    
    library(stringr)
    penjumlahantdk <- str_count(hasilpenjumlahan$string, "Tidak Benar")
    
    tdk <- sum(penjumlahantdk)
    
    penjumlahantotal <- str_count(hasilpenjumlahan$string, "Benar")
    total <- sum(penjumlahantotal)
    
    penjumlahanpositif <- str_count(pelabelan, "Positif")
    p <- sum(penjumlahanpositif)
    
    penjumlahannetral <- str_count(pelabelan, "Netral")
    n <- sum(penjumlahannetral)
    
    penjumlahannegatif <- str_count(pelabelan, "Negatif")
    ne <- sum(penjumlahannegatif)
    
    benar = total - tdk
    accuracy <- (benar/total)*100
    data <- data.frame(Positif = p, Netral = n, Negatif = ne, TidakBenar = tdk, Benar = benar,TotalData = total,  Akurasi = accuracy ) 
    
  }
  
  output$plotgrafik <- renderPlot({
    grafikklasifikasitrain(hasilklasifikasitrain)
  })
  
  output$klasifikasi = renderDataTable({
    datatable(hasilklasifikasitrain(), options = list(lengthChange = FALSE))
  })
  
  output$akurasi = renderDataTable({
    datatable(akurasiklasifikasitrain(), options = list(lengthChange = FALSE))
  })
  
  
  #---- WordCloud ----
  word <- function(fileDataTraining){
    print(datainputtrain())
    tweets.df <- as.data.frame(datainputtrain())
    
    komen <- tweets.df$text
    komentar <- Corpus(VectorSource(komen))
    
    
    #Cleaning
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    #stopword
    myStopwords = readLines("stopwordcloud.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #sapply(clean, katadasaR)
    
    dtm <- TermDocumentMatrix(clean)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    wordcloud(words = d$word, freq = d$freq,
              max.words=50, random.order=FALSE, rot.per=0.0,
              colors=brewer.pal(8, "Dark2"))
  }
  
  #----- Output wordcloud----
  
  output$plot <- renderPlot({
    word(fileDataTraining)
  })
  
  
  
  #---- DATA TESTING -----------------------------------------------------------
  
  
  
  #---input data testing ---
  
  fileDataTesting <- function(fileDataTraining){
    print(datainputtest())
  }
  
  #----- OUTPUT DATA TESTING ----------
  
  output$cetakdatatesting = renderDataTable({
    datatable(fileDataTesting(), options = list(lengthChange = FALSE))
  })
  
  #----- PREPROCESSING TESTING ---------
  DataPreprocessingTesting <- function(fileDataTesting){
    print(datainputtest())
    tweets.df <- as.data.frame(datainputtest())
    
    komen <- tweets.df$text
    
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
  }
  #----- OUTPUT TESTING ----
  output$cetakpreprocessingtesting = renderDataTable({
    datatable(DataPreprocessingTesting(), options = list(lengthChange = FALSE))
  })
  #-----Grafik Klasifikasi -------
  

  hasilklasifikasitesting <- function(fileDataTesting){
    datatraining <- input$datatraining 
    if (is.null( datatraining))
      return(NULL)
    datatrain <- read.csv( datatraining$datapath, sep = ";")
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    
    tweets.df <- as.data.frame(testingdt)
    
    komen <- testingdt$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
   # library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    #model Naive Bayes
    modelNB <- naive_bayes(label~ positif+netral+negatif, data = trainingdt, laplace = 1 )
    #predict train
    prediksitest <- predict(modelNB, testingdt, type = "class")
    
    argumen <- ifelse(prediksitest == testingdt$label, "Benar","Tidak Benar")
    datatst <- data.frame(text = dataframe, sistem = prediksitest, manual = testingdt$label, hasil = argumen)
    
  }
  
  akurasiklasifikasitesting <- function(grafikklasifikasitrain){
    datatraining <- input$datatraining 
    if (is.null( datatraining))
      return(NULL)
    datatrain <- read.csv( datatraining$datapath, sep = ";")
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    tweets.df <- as.data.frame(testingdt)
    
    komen <- testingdt$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    #model Naive Bayes
    modelNB <- naive_bayes(label~positif+netral+negatif, data = trainingdt, laplace = 1 )
    #predict train
    prediksitest <- predict(modelNB, testingdt, type = "class")
    
    argumen <- ifelse(prediksitest == testingdt$label, "Benar","Tidak Benar")
    data <- data.frame(text = dataframe, sistem = prediksitest,manual = testingdt$label, hasil = argumen)
    
    hasil <- data.frame(string = argumen)
    positif <- data.frame(string = data)
    
    library(stringr)
    tdkbenar <- str_count(hasil$string, "Tidak Benar")
    tdk <- sum(tdkbenar)
    
    penjumlahantdk <- str_count(hasil$string, "Benar")
     total <- sum(penjumlahantdk)
    
    penjumlahanpositif <- str_count(positif, "Positif")
    p <- sum(penjumlahanpositif)
    
    penjumlahannetral <- str_count(positif, "Netral")
    n <- sum(penjumlahannetral)
    
    penjumlahannegatif <- str_count(positif, "Negatif")
    ne <- sum(penjumlahannegatif)
    
    pengurangan = total - tdk
    accuracy <- (pengurangan/total)*100
    data <- data.frame(Positif = p, Netral = n, Negatif = ne , TidakBenar = tdk, Benar = pengurangan, TotalData = total, Akurasi = accuracy ) 
    
  }
  
  
  grafiktestingklasifikasi <- function(hasilklasifikasitesting){
    print(datainputtest())
    tweets.df <- as.data.frame(datainputtest())
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    #model Naive Bayes
    modelNB <- naive_bayes(label~ positif+netral+negatif, data = tweets.df, laplace = 1 )
    #predict train
    prediksitest <- predict(modelNB, tweets.df, type = "class")
    
    datatst <- data.frame(text = dataframe, sistem = prediksitest)
    
    library(ggplot2)
    # plot
    ggplot(datatst, aes(x=sistem)) +
      geom_bar(aes(y=..count.., fill=sistem)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="klasifikasi categories", y="number of tweets") 
  }
  
  #---------- OUTPUT KLASIFIKASI --------------
  output$plotgrafiktesting <- renderPlot({
    grafiktestingklasifikasi(fileDataTesting)
  })
  
  output$klasifikasitesting = renderDataTable({
    datatable(hasilklasifikasitesting(), options = list(lengthChange = FALSE))
  })
  
  output$akurasitest = renderDataTable({
    datatable(akurasiklasifikasitesting(), options = list(lengthChange = FALSE))
  })
  
  #----- wordcloud test----
  
  wordcloudtest <- function(fileDataTesting){
    print(datainputtest())
    tweets.df <- as.data.frame(datainputtest())
    
    komen <- tweets.df$text
    komentar <- Corpus(VectorSource(komen))
    
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    le <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    le <- tm_map(le, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    le <- tm_map(le, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    le <- tm_map(le, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    le <- tm_map(le, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    le <- tm_map(le, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    le <- tm_map(le, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    le <- tm_map(le, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    le <- tm_map(le, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    le <- tm_map(le, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    le <- tm_map(le,remove.all)
    
    le <- tm_map(le, removePunctuation)
    
    #Case Folding
    le <- tm_map(le,content_transformer(tolower))
    le <- tm_map(le, content_transformer(removeNumbers))
    
    #stopword
    myStopwords = readLines("stopwordcloud.txt")
    le <- tm_map(le, removeWords, myStopwords)
    
    #stemming
   # library(katadasaR)
    #sapply(le, katadasaR)
    
   
    dtm <- TermDocumentMatrix(le)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=100, random.order=FALSE, rot.per=0.0,
              colors=brewer.pal(4, "Set1"))
  }
  
  #---- Output Worcloud Test -----------
  
  output$plottesting <- renderPlot({
    wordcloudtest(fileDataTesting)
  })
  

  #------ Pembagian Data CrossValidation ----------
  
  library(caret)
  
  viewdatatraining <- function(fileDataTraining){
    print(datainputtrain())
    tweets.df <- as.data.frame(datainputtrain())
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    datafframe <- data.frame(text = dataframe, positif = tweets.df$positif, netral = tweets.df$netral, negatif = tweets.df$negatif, label = tweets.df$label )
  }
  
  viewdatatesting <- function(fileDataTesting){
    print(datainputtest())
    tweets.df <- as.data.frame(datainputtest())
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    datafframe <- data.frame(text = dataframe, positif = tweets.df$positif, netral = tweets.df$netral, negatif = tweets.df$negatif, label = tweets.df$label )
  }
  
  akurasicvdatatrain <- function(viewdatatraining){
    file <- input$datatraining 
    if (is.null( file))
      return(NULL)
    
    datatrain <- read.csv(file$datapath, sep = ";")
    
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    tweets.df <- as.data.frame(trainingdt)
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
    
    
    datafframe <- data.frame(text = dataframe, positif = tweets.df$positif, netral = tweets.df$netral, negatif = tweets.df$negatif, label = tweets.df$label )
    
    fitctrl <- trainControl(method = "cv", number = 3)
    splitdttr <- train(label~ positif+netral+negatif, data = dataframe, method = "naive_bayes", trControl = fitctrl)
    splitdttr$resample
  } 
  akurasicvdatatest <- function(viewdatatesting){
    file <- input$datatraining 
    if (is.null( file))
      return(NULL)
    
    datatrain <- read.csv(file$datapath, sep = ";")
    
    # Determine row to split on: split
    splitdata <- round(nrow(datatrain) * 0.80)
    
    # Create train
    trainingdt <- datatrain[1:splitdata, ]
    
    # Create test
    testingdt <- datatrain[(splitdata + 1):nrow(datatrain), ]
    tweets.df <- as.data.frame(testingdt)
    
    komen <- tweets.df$text
    
    komentar <- Corpus(VectorSource(komen))
    #Cleansing
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    clean <- tm_map(komentar, removeURL)
    
    removeNL <- function(y) gsub("\n", " ", y)
    clean <- tm_map(clean, removeNL)
    
    replacecomma <- function(y) gsub(",", "", y)
    clean <- tm_map(clean, replacecomma)
    
    removeRT <- function(y) gsub("RT ", "", y)
    clean <- tm_map(clean, removeRT)
    
    removetitik2 <- function(y) gsub(":", "", y)
    clean <- tm_map(clean, removetitik2)
    
    removetitikkoma <- function(y) gsub(";", " ", y)
    clean <- tm_map(clean, removetitikkoma)
    
    removetitik3 <- function(y) gsub("p…", "", y)
    clean <- tm_map(clean, removetitik3)
    
    removeamp <- function(y) gsub("&amp;", "", y)
    clean <- tm_map(clean, removeamp)
    
    removeUN <- function(z) gsub("@\\w+", "", z)
    clean <- tm_map(clean, removeUN)
    
    removemention <- function(x) gsub("@\\S+","",x)
    clean <- tm_map(clean, removemention)
    
    remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
    clean <- tm_map(clean,remove.all)
    
    clean <- tm_map(clean, removePunctuation)
    
    #Case Folding
    clean <- tm_map(clean,content_transformer(tolower))
    clean <- tm_map(clean, content_transformer(removeNumbers))
    
    
    #Stopword Removal
    myStopwords = readLines("stopword.txt")
    clean <- tm_map(clean, removeWords, myStopwords)
    
    #stemming
    #library(katadasaR)
    #clean <- sapply(clean, katadasaR)
    
    datafframe <- data.frame(text = clean, positif = tweets.df$positif, netral = tweets.df$netral, negatif = tweets.df$negatif, label = tweets.df$label )
    fitctrl <- trainControl(method = "cv", number = 3)
    splitdtst <- train(label~ positif+netral+negatif, data = testing, method = "naive_bayes", trControl = fitctrl)
    splitdtst$resample
  } 
  
  
  
  
  output$datatrainingcv = renderDataTable({
    datatable(viewdatatraining(), options = list(lengthChange = FALSE))
  })
  output$pembagidatatrain = renderDataTable({
    datatable(akurasicvdatatrain(), options = list(lengthChange = FALSE))
  })
  output$datatestingcv = renderDataTable({
    datatable(viewdatatesting(), options = list(lengthChange = FALSE))
  })
  output$pembagidatatest = renderDataTable({
    datatable(akurasicvdatatest(), options = list(lengthChange = FALSE))
  })
  
  inputdata <- function(inputtxt){
  
    x <- isolate(paste(input$text))
    
  }
  
  klasifikasidt <- function(inputtxt){
    datatst <- read.csv(file = "C:/Kuliah/DataSkripsiLabel/DATA/CSV/test.csv", header = T, sep = ";", stringsAsFactors = FALSE)
    tweets.df <- as.data.frame(datainputtrain())
    test <- as.data.frame(datainputtest())
    dt <- as.data.frame(inputdata())
  
    teks <- isolate(paste(input$text))
    rn <- as.data.frame(teks)

   modelNB <- naive_bayes(label~ positif+netral+negatif, data = tweets.df, laplace = 1 )
    
    #predict train
    prediksitext <- predict(modelNB, datatst, type = "prob")
    prediksi <- predict(modelNB, datatst, type = "class")
    
    #------------------------------------------------
  
    
    positif <- scan("positif.txt",what="character",comment.char=";")
    negatif <- scan("negatif.txt",what="character",comment.char=";")
    kata.positif = c(positif)
    kata.negatif = c(negatif)
    score.sentiment = function(rn, kata.positif, kata.negatif, .progress='none')
    {
      require(plyr)
      require(stringr)
      scores = laply(rn, function(kalimat, kata.positif, kata.negatif) {
        kalimat = gsub('[[:punct:]]', '', kalimat)
        kalimat = gsub('[[:cntrl:]]', '', kalimat)
        kalimat = gsub('\\d+', '', kalimat)
        kalimat = tolower(kalimat)
        
        list.kata = str_split(kalimat, '\\s+')
        kata2 = unlist(list.kata)
        positif.matches = match(kata2, kata.positif)
        negatif.matches = match(kata2, kata.negatif)
        positif.matches = !is.na(positif.matches)
        negatif.matches = !is.na(negatif.matches)
        score = sum(positif.matches) - (sum(negatif.matches))
        return(score)
        
        
        
        
        
        
        
        modelNB <- naive_bayes(label~ positif+netral+negatif, data = tweets.df, laplace = 1 )
        
        #predict train
        prediksi <- predict(modelNB, rn, type = "class")
        
        
        
        
        
        
        
        
        
        
        
      }, kata.positif, kata.negatif, .progress=.progress )
      scores.df = data.frame(score=scores, text= rn)
      return(scores.df)
    }
    #melakukan skoring text
    hasil = score.sentiment(rn, kata.positif, kata.negatif)
    # melakukan labeling pada nilai yang kurang dari 0 sebagai negatif dan lebih dari = 0 adalah positif
    hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))
    hasil$klasifikasi
    
    dt <- data.frame(teks = rn,klasifikasi = hasil$klasifikasi)
  }
  output$ujicoba <- renderTable({
    if(input$go)
      return(klasifikasidt())
    })
  
})

