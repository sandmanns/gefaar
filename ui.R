library(shiny)
library(shinythemes)
library(DT)
library(stringi)
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(div("GEFAAR: a GEneric Framework for the Analysis of Antimicrobial Resistance",
                 img(height = 43, width = 54, src = "IMI.png",class="pull-right"),
                 img(height = 43, width = 20, src = "white.png",class="pull-right"),
                 img(height = 43, width = 50, src = "UKM.png",class="pull-right")),
                 windowTitle="GEFAAR"
  ),
  sidebarPanel(
    shinyjs::useShinyjs(),
    tabsetPanel(
      tabPanel("Input",
               hr(),
               h4("Input"),
               fileInput('inputFile',label = "Upload input file"),
               radioButtons('sepInput',label = "Separator input file",
                            choices = c("Comma","Semicolon","Tab"),selected = character(0),
                            inline = T),
               br(),
               actionButton('do_in',"Read in file",class = "btn-primary"),
               hr(),
               h4(textOutput("columnUI0")),
               tags$head(
                 tags$style(type="text/css", 
                            "#inline label.control-label, #inline .selectize-control.single { 
         display: table-cell; 
         text-align: left; 
         vertical-align: middle; 
      } 
      #inline label.control-label {
        padding-right: 20px;
      }
      #inline .form-group { 
        display: table-row;
      }
      #inline .selectize-control.single div.item {
        padding-right: 150px;
      }")
               ),
               
               tags$div(id = "inline", uiOutput("columnUI1")),
               tags$div(id = "inline", uiOutput("columnUI2")),
               tags$div(id = "inline", uiOutput("columnUI3")),
               tags$div(id = "inline", uiOutput("columnUI4")),
               tags$div(id = "inline", uiOutput("columnUI4b")),
               tags$div(id = "inline", uiOutput("columnUI5")),
               h6(textOutput("columnUI6")),
               br(),
               uiOutput("do_in2UI")
               
      ),
      tabPanel("Pathogen statistics",
               hr(),
               h4(textOutput("anweisung0pre")),
               uiOutput("erregerstatUI1"),
               uiOutput("erregerstatUI2"),
               uiOutput("erregerstatUI3.2"),
               uiOutput("erregerstatUI4"),
               uiOutput("erregerstatUI4b"),
               hr(),
               uiOutput("erregerstatUI5")),
      tabPanel("Resistance statistics",
               hr(),
               h4(textOutput("anweisung1pre")),
               uiOutput("statistikUI1"),
               uiOutput("statistikUI2"),
               uiOutput("statistikUI3.2"),
               uiOutput("statistikUI4"),
               uiOutput("statistikUI3"),
               hr(),
               uiOutput("statistikUI5"),
               hr(),
               uiOutput("statistikUI6")
      ),
      tabPanel("Trend analysis",
               br(),
               uiOutput("trendUI1"),
               uiOutput("trendUI3.2"),
               uiOutput("trendUI4"),
               uiOutput("trendUI3"),
               uiOutput("trendUI3.3"),
               hr(),
               uiOutput("trendUI5"),
               hr(),
               uiOutput("trendUI6")
      ),
                   navbarMenu("Cluster analyses",
      tabPanel("Interactive",
               hr(),
               h4(textOutput("anweisung2pre")),
               uiOutput("bakteriumUIinitial"),
               h4(textOutput("anweisung")),
               uiOutput("bakteriumUI0a"),
               uiOutput("bakteriumUI0b"),
               hr(),
               uiOutput("bakteriumUI0"),
               hr(),
               uiOutput("bakteriumUI1"),
               uiOutput("bakteriumUI2"),
               uiOutput("bakteriumUI3"),
               uiOutput("bakteriumUI4"),
               uiOutput("klinikUI1"),
               uiOutput("klinikUI2"),
               uiOutput("klinikUI3"),
               hr(),
               uiOutput("analyseUI")
      ),
      tabPanel("Export",
               hr(),
               h4(textOutput("anweisung3pre")),
               uiOutput("downloadUIinitial"),
               h4(textOutput("anweisung2")),
               uiOutput("downloadUI1"),
               uiOutput("downloadUI2"),
               hr(),
               uiOutput("downloadUIbak0a"),
               uiOutput("downloadUIbak0b"),
               hr(),
               uiOutput("downloadUI3"),
               hr(),
               uiOutput("downloadUIbak1"),
               uiOutput("downloadUIbak1b"),
               uiOutput("downloadUIbak2"),
               uiOutput("downloadUIbak3"),
               uiOutput("downloadUIbak3b"),
               uiOutput("downloadUIbak3c"),
               uiOutput("downloadUIbak3d"),
               uiOutput("downloadUIbak3e"),
               uiOutput("downloadUIbak3f"),
               uiOutput("downloadUIbak3g"),
               uiOutput("downloadUIbak3h"),
               uiOutput("downloadUIbak4"),
               hr(),
               uiOutput("downloadUIklinik1"),
               uiOutput("downloadUIklinik1b"),
               uiOutput("downloadUIklinik2"),
               uiOutput("downloadUIklinik3"),
               uiOutput("downloadUIklinik3b"),
               uiOutput("downloadUIklinik3c"),
               uiOutput("downloadUIklinik3d"),
               hr(),
               uiOutput("downloadUI4"))
      ),
      tabPanel("Info",
               hr(),
               h4(htmlOutput("text_info2h")),
               hr(),
               h3(textOutput("text_info1")),
               hr(),
               img(height = 129, width = 162, src = "IMI.png"),
               hr(),
               h5(textOutput("text_info2a")),
               h5(textOutput("text_info2c")),
               h5(textOutput("text_info2d")),
               hr(),
               h5(textOutput("text_info2b")),
               h5(textOutput("text_info2b2")),
               hr(),
               hr(),
               h3(textOutput("text_info1b")),
               hr(),
               h5(textOutput("text_info2e")),
               h5(textOutput("text_info2f")),
               hr(),
               hr(),
               hr(),
               hr(),
               hr(),
               h6(textOutput("text_info2g"))#,
               #tags$head(tags$style("#text_info2a{color: black;
              #                   font-size: 20px;
              #                   font-style: bold;
              #                   }"))
               )
    )
    
  ),
  mainPanel(
    shinyjs::useShinyjs(),
    tabsetPanel(
      tabPanel("Log",
               h3("Log"),
               div(id = "text")
      ),
      tabPanel("Pathogen statistics",
               h3(textOutput("text_erregerstat1")),
               h4(textOutput("text_erregerstat2")),
               hr(),
               DT::dataTableOutput("datatable_erregerstat1")),
      tabPanel("Resistance statistics",tabsetPanel( id = "def",
                                               #navbarMenu("Datenblatt Antibiotika",
                                               tabPanel("Data sheet antimicrobial agents",
                                                        h6(textOutput("hinweis_tabellen")),
                                                        hr(),
                                                        h3(textOutput("text_zusammenfassung0b")),
                                                        h4(textOutput("text_zusammenfassung0")),
                                                        htmlOutput("text_zusammenfassung0c"),
                                                        hr(),
                                                        uiOutput("datatables_statistik")
                                                        ),
                                               tabPanel("Figures antimicrobial agents",
                                                        h6(textOutput("hinweis_abb")),
                                                        hr(),
                                                        h3(textOutput("text_zusammenfassung0b2")),
                                                        h4(textOutput("text_zusammenfassung02")),
                                                        htmlOutput("text_zusammenfassung0c2"),
                                                        hr(),
                                                        uiOutput("plots_statistik"))
                                               ),
      ),
      tabPanel("Trend analysis",
               h6(textOutput("hinweis_tabellen_trend")),
               hr(),
               h3(textOutput("text_trend1")),
               h4(textOutput("text_trend2")),
               hr(),
               htmlOutput("text_zusammenfassung_trend"),
               hr(),
               uiOutput("plots_trend")
      ),
      tabPanel("Cluster analyses",tabsetPanel( id = "abc",         
                                    navbarMenu("Independent analysis per species",
                                               tabPanel("Heatmap (order: clinic, resistance)",
                                                        h3(textOutput("text_analyse1a")),
                                                        h4(textOutput("text_analyse1")),
                                                        uiOutput("plots1")),
                                               tabPanel("Heatmap (order: clinic, date)",
                                                        h3(textOutput("text_analyse2a")),
                                                        h4(textOutput("text_analyse2")),
                                                        uiOutput("plots2")),
                                               tabPanel("Heatmap (hierarchical clustering)",
                                                        h3(textOutput("text_analyse3a")),
                                                        h4(textOutput("text_analyse3")),
                                                        uiOutput("plots3")),
                                               tabPanel("UMAP (colored clinics)",
                                                        h3(textOutput("text_analyse4a")),
                                                        h4(textOutput("text_analyse4")),
                                                        h6(textOutput("text_analyse4b")),
                                                        uiOutput("plots4")),
                                               tabPanel("UMAP (colored clusters)",
                                                        h3(textOutput("text_analyse5a")),
                                                        h4(textOutput("text_analyse5")),
                                                        h6(textOutput("text_analyse5b")),
                                                        uiOutput("plots5")),
                                               tabPanel("Additional heatmap (order: UMAP cluster)",
                                                        h3(textOutput("text_analyse6a")),
                                                        h4(textOutput("text_analyse6")),
                                                        uiOutput("plots6")),
                                               tabPanel("Additional heatmap (order: clinic)",
                                                        h3(textOutput("text_analyse6ab")),
                                                        h4(textOutput("text_analyse6b")),
                                                        uiOutput("plots6b"))),
                                    
                                    navbarMenu("Independent analysis per clinic",
                                               tabPanel("Heatmap (order: species)",
                                                        h3(textOutput("text_analyse7a")),
                                                        h4(textOutput("text_analyse7")),
                                                        uiOutput("plots7")),
                                               tabPanel("Heatmap (hierarchical clustering)",
                                                        h3(textOutput("text_analyse8a")),
                                                        h4(textOutput("text_analyse8")),
                                                        uiOutput("plots8"))))
               
      )
    )
  )
))





