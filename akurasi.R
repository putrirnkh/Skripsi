library(tm) #cleaning data
library(RTextTools) # split data
library(dplyr)
library(naivebayes) #paket Naive Bayes
library(caret) #paket cross validation
library(tidyverse) #paket data manipulasi

semuadata <- read.csv(file = "C:/Kuliah/DataSkripsiLabel/DATA/CSV/DataAkurasiFull.csv", header = T, sep = ";", stringsAsFactors = FALSE)


  # Determine row to split on: split
  splitdata <- round(nrow(semuadata) * 0.50)
  
  # Create train
  traindata <- semuadata[1:splitdata, ]
  
  # Create test
  testdata <- semuadata[(splitdata + 1):nrow(semuadata), ]
  
  label <- semuadata$label <- as.factor(semuadata$label)


prepro <- function(split){
  
  corpus <- VCorpus(VectorSource(traindata$text))
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  clean <- tm_map(corpus, removeURL)
  
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
  clean <- tm_map(corpus, tolower)
  clean <- tm_map(corpus, removeNumbers)
  
  
  #Stopword Removal
  myStopwords = readLines("C:/Users/Nana/Downloads/Skripsi (1)/Skripsi/program/Skripsi/sistem/stopword.txt")
  clean <- tm_map(clean, removeWords, myStopwords)
  
  dataframe <- data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
}

modelnaive <- function(split, prepro){
  #model Naive Bayes
  modelNB <- naive_bayes(label~., data = semuadata, laplace = 1)
  print(modelNB)
  
  #predict train
  prediksitrain <- predict(modelNB, traindata, type = "class")
  prediksitrainclass
  summary(prediksitrainclass)
  
  
  #predict test
  prediksitestclass <- predict(modelNB, testdata, type = "class")
  prediksitestclass
  summary(prediksitestclass)
}

akurasicvtrain <- function(modelnaive){
  akurasitrain <- train(
    label~ positif+netral+negatif, traindata, method = "naive_bayes",
    trControl = trainControl(
      method = "cv", 
      number = 10
    )
  )
  akurasitrain
  akurasitrain$resample
}

akurasicvtest <- function(modelnaive){
  akurasitest <- train(
    label~ positif+netral+negatif, testdata, method = "naive_bayes",
    trControl = trainControl(
      method = "cv", 
      number = 10
    )
  )
  
  akurasitest
  akurasitest$resample
}

akurasiall <- function(akurasicvtrain, akurasicvtest){
  akurasi<- train(
    label~., semuadata, method = "naive_bayes",
    trControl = trainControl(
      method = "cv", 
      number = 10,savePredictions = "final", classProbs = TRUE, returnData = TRUE
    )
  )
  
  akurasi
  akurasi$resample
}
akurasiall()
akurasicvtest()
