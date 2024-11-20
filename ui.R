#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(wordcloud) #membuat wordcloud
library(RColorBrewer) #untuk warnain wordcloud
library(caret) #membuat cross validation
library(ggplot2) #membuat grafik

ui <- dashboardPage(
    dashboardHeader(title = "SISTEM ANALISIS"),
    dashboardSidebar(  
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
            ),#tutup menu item dashboard
            menuItem("Data Training", icon = icon("folder"),
                     menuSubItem('Data Training', tabName = "datatraining"),
                     menuSubItem('Preprocessing Training', tabName = "preprocessing"),
                     menuSubItem('Grafik Klasifikasi', tabName = "klasifikasitraining"),
                     menuSubItem('Word Cloud', tabName = "worldcloudtraining")
            ),#tutup menu item data training
            menuItem("Data Testing", icon = icon("laptop"),
                     menuSubItem('Data Testing', tabName = "datatesting"),
                     menuSubItem('Preprocessing Testing', tabName = "preprocessingtesting"),
                     menuSubItem('Grafik Klasifikasi', tabName = "klasifikasitesting"),
                     menuSubItem('Word Cloud', tabName = "worldcloudtesting")
            ),#tutup menu item data testing
            menuItem("Pembagian Data", tabName = "akurasi", icon = icon("book")),
            menuItem("UjiCoba", tabName = "UjiCoba", icon = icon("file"))
            
        ) #tutup sidebarMenu
    ), #tutup dashboard sidebar
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    h1("DASHBOARD"),
                    br(), br(),
                    box (h2(" Sistem analisis sentimen ini untuk menganalisis pendapat dari masyarakat yang terus berubah-ubah secara cepat dan dapat dijadikan sebagai tolak ukur pemerintah terhadap pengadaan metode Pembelajaran Jarak Jauh. ")) 
            ),#tutup tabitem dashboard
            
            tabItem("datatraining",
                    h1("TRAINING DATA"),
                    br(), br(),
                    
                    box(fileInput("datatraining", "Choose CSV File Data Training",
                                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                    ) #Tutup data training
                    ), #Tutup File Input
                    
                    
                    br(),  br(),
                    
                    DT::dataTableOutput('dataTraining')
            ), #tutup tabItem datatraining
            
            tabItem("preprocessing",
                    h1("PRE - PROCESSING"),
                    br(), br(),
                    DT::dataTableOutput('preprocessing')
            ), #tutup tab Item Preprocessing
            tabItem("klasifikasitraining",
                    h1("GRAFIK HASIL KLASIFIKASI"),
                    br(), br(),
                    plotOutput("plotgrafik"),
                    br(),
                    DT::dataTableOutput('klasifikasi'),
                   br(),
                   DT::dataTableOutput('akurasi')
            ), #tutup tab Item Grafik Training
            
            
            tabItem("worldcloudtraining",
                    h1("WORD CLOUD"),
                    br(), br(),
                    plotOutput("plot")
            ), #tutup world cloud training
            
            #------------------------------------------------
            
            tabItem("datatesting",
                    h1("DATA TESTING"),
                    br(), br(),
                    
                    DT::dataTableOutput('cetakdatatesting')
                  
            ), #tutup data testing
            
            tabItem("preprocessingtesting",
                    h1("PREPROCESSING TESTING"),
                    br(), br(),
                    
                    DT::dataTableOutput('cetakpreprocessingtesting')
                    
            ), #tutup
            
            tabItem("klasifikasitesting",
                    h1("GRAFIK HASIL KLASIFIKASI DATA TESTING"),
                    br(), br(),
                    
                    plotOutput("plotgrafiktesting"),
                    br(),
                    DT::dataTableOutput('klasifikasitesting'),
                    br(),
                    DT::dataTableOutput('akurasitest')
                    
                    
            ), #tutup klasifikasi testing
            tabItem("worldcloudtesting",
                    h1("WORD CLOUD DATA TESTING"),
                    br(), br(),
                    plotOutput("plottesting")
            ), #tutup word cloud testing
            
            tabItem("akurasi",
                    h1("DATA TRAINING CROSS VALIDATION"),
                    br(), br(),
                    DT::dataTableOutput('datatrainingcv'),
                    br(),br(),
                    h3("Menghitung K-fold 1 - K-fold 3"),
                    br(), br(),
                    DT::dataTableOutput('pembagidatatrain'),
                    br(),
                    h1("DATA TESTING CROSS VALIDATION"),
                    DT::dataTableOutput('datatestingcv'),
                    br(),
                    h3("Menghitung K-fold 1 - K-fold 3"),
                    br(), br(),
                    DT::dataTableOutput('pembagidatatest')
            ),#tutup tabitem akurasi
            
            tabItem("UjiCoba",
                    fluidPage(
                    titlePanel("UJI COBA TEXT"),
                    sidebarLayout(
                      sidebarPanel((""),
                        textInput("text","Input Text"),actionButton("go","Klik")),#tutupsidebarPanel
                      mainPanel(("Information"),
                                #verbatimTextOutput("value"),
                                br(),
                              tableOutput("ujicoba")) #tutupmainPanel
                    )#tutupsidebarlayout
                    )#tutupfluidpage
                    
            )#tutup tabitem UjiCoba
        )#tutup tabitems
        
    )#tutup dashboardBody
) #tutup dashboardPage


