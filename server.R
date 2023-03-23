options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(pheatmap)
library(DT)
library(NbClust)
library(M3C)
library(ggplot2)
library(openxlsx)
#library(gridExtra)
#library(grid)
Sys.setlocale("LC_CTYPE", "")

shinyServer(function(input, output, session) {
  output$text_analyse1<-renderText({"Analysis not yet conducted"})
  output$text_analyse1a<-renderText({"Heatmap (order: clinics, resistance)"})
  output$text_analyse2<-renderText({"Analysis not yet conducted"})
  output$text_analyse2a<-renderText({"Heatmap (order: clinics, date)"})
  output$text_analyse3<-renderText({"Analysis not yet conducted"})
  output$text_analyse3a<-renderText({"Heatmap (hierarchical clustering)"})
  output$text_analyse4<-renderText({"Analysis not yet conducted"})
  output$text_analyse4a<-renderText({"UMAP (colored clinics)"})
  output$text_analyse5<-renderText({"Analysis not yet conducted"})
  output$text_analyse5a<-renderText({"UMAP (colored clusters)"})
  output$text_analyse6<-renderText({"Analysis not yet conducted"})
  output$text_analyse6a<-renderText({"Additional heatmap (order: UMAP cluster)"})
  output$text_analyse6b<-renderText({"Analysis not yet conducted"})
  output$text_analyse6ab<-renderText({"Additional heatmap (order: clinics)"})
  
  output$text_analyse7<-renderText({"Analysis not yet conducted"})
  output$text_analyse7a<-renderText({"Heatmap (order: species)"})
  output$text_analyse8<-renderText({"Analysis not yet conducted"})
  output$text_analyse8a<-renderText({"Heatmap (hierarchical clustering)"})
  
  output$text_zusammenfassung0<-renderText({"Analysis not yet conducted"})
  output$text_zusammenfassung02<-renderText({"Analysis not yet conducted"})
  output$text_zusammenfassung0b<-renderText({"Resistance statistics: Data sheet antimicrobial agents"})
  output$text_zusammenfassung0c<-renderText({NULL})  
  output$hinweis_tabellen<-renderText({"Note: In case all species are analyzed at once, it may take up to 30 seconds until the updated tables are displayed."})
  output$hinweis_tabellen_trend<-renderText({NULL})

  output$text_zusammenfassung0b2<-renderText({"Resistance statistics: Figures antimicrobial agents"})
  output$text_zusammenfassung0c2<-renderText({NULL})
  output$hinweis_abb<-renderText({"Note: In case all species are analyzed at once, it may take up to 30 seconds until the updated figures are displayed."})
  
  output$text_erregerstat1<-renderText({"Pathogen statistics"})
  output$text_erregerstat2<-renderText({"Analysis not yet conducted"})
  
  output$text_trend1<-renderText({"Trend analysis"})
  output$text_trend2<-renderText({"Analysis not yet conducted"})
  
  output$text_zusammenfassung1b<-renderText({"Summary clinics"})
  output$text_zusammenfassung1<-renderText({"Analysis not yet conducted"})
  output$text_zusammenfassung0c3<-renderText({NULL})
  output$text_zusammenfassung2b<-renderText({"Summary antimicrobial agents"})
  output$text_zusammenfassung2<-renderText({"Analysis not yet conducted"})
  output$text_zusammenfassung0c4<-renderText({NULL})

  output$text_info1<-renderText({"Developed by"})
  output$text_info2a<-renderText({"Institute of Medical Informatics"})
  output$text_info2c<-renderText({"Albert-Schweitzer-Campus 1, Building A11"})
  output$text_info2d<-renderText({"48149 Münster"})
  output$text_info2b<-renderText({"Univ.-Prof. Dr. med. Julian Varghese, M.Sc."})
  output$text_info2b2<-renderText({"PD Dr. Sarah Sandmann"})
  output$text_info1b<-renderText({"Contact"})
  output$text_info2e<-renderText({"www.imi.uni-muenster.de"})
  output$text_info2f<-renderText({"imi@uni-muenster.de"})
  output$text_info2g<-renderText({"Latest update: 21.03.2023"})
  output$text_info2h<-renderText({"The intended purpose of this application is to generate an overview of retrospective resistance data.
The application is not a medical device according to the Medical Devices Act or the EU Medical Device Regulation."})
  
  output$anweisung0pre<-renderText({"Please read in a file and configure input first"})
  output$anweisung1pre<-renderText({"Please read in a file and configure input first"})
  output$anweisung2pre<-renderText({"Please read in a file and configure input first"})
  output$anweisung3pre<-renderText({"Please read in a file and configure input first"})
  
  
  #jahre_verfuegbar<-c(2021,2022)
  #jahr_gewaehlt<-2021
  #input2<-read.table("www/Daten2021.txt",
  #                   header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F) 
  ########################################################################
  
  ###UPload hinzufügen!
  observeEvent(input$do_in,{
    shinyjs::html("text", paste0("<br>Input file is read in.<br><br>"), add = FALSE)
    input_temp<-input$inputFile
    if(is.null(input_temp)){
      shinyjs::html("text", paste0("ERROR: No input file defined.","<br>"), add = TRUE) 
      return()
    }
    if(is.null(input$sepInput)){
      shinyjs::html("text", paste0("ERROR: No separator defined.","<br>"), add = TRUE) 
      return()
    }
    if(input$sepInput=="Comma"){
      input2<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
    }
    if(input$sepInput=="Semicolon"){
      input2<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
    }
    if(input$sepInput=="Tab"){
      input2<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
    }
    

    
    shinyjs::html("text", paste0("Input file successfully read in.","<br>"), add = TRUE) 
    
    output$columnUI0<-renderText({"Select column containing information on..."})
    
    output$columnUI1<-renderUI({
      selectInput('column1',label = HTML("...species: &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input2))
    })
    output$columnUI2<-renderUI({
      selectInput('column2',label = HTML("...clinic:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input2))
    })
    output$columnUI3<-renderUI({
      selectInput('column3',label = HTML("...specimen:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input2))
    })
    output$columnUI4<-renderUI({
      selectInput('column4',label = HTML("...date:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input2))
    })
    output$columnUI4b<-renderUI({
      selectInput('column4b',label = HTML("&nbsp&nbsp&nbsp-> date format:"),
                  choices = c("dd.mm.yy","dd.mm.yyyy",
                              "mm/dd/yy","mm/dd/yyyy",
                              "yy-mm-dd","yyyy-mm-dd"))
    })
    
    
    output$columnUI5<-renderUI({
      selectInput('column5',label = HTML("...first antimicrobial agent:"),choices = names(input2))
    })
    output$columnUI6<-renderText({"Note: All subsequent columns are assumed to contain information on antimicrobial agents as well. 
      The following coding of resistance information is required: 'S' for susceptible, 'I' for susceptible increased exposure, 'R' for resistant, '-' for not analyzed."})
    
    output$do_in2UI<-renderUI({
      actionButton('do_in2',"Configure input",class = "btn-primary")
    })
    
    observeEvent(input$do_in2,{
      shinyjs::html("text", paste0("<br>Input file is configured.<br><br>"), add = FALSE)
      
      spalten<-c(input$column1,input$column2,input$column3,input$column4,input$column5)
      spalten_table<-table(spalten)
      if(sum(as.numeric(spalten_table)>1)>0){
        shinyjs::html("text", paste0("ERROR: Column ",names(spalten_table)[which(as.numeric(spalten_table)>1)]," selected more than once.","<br>"), add = TRUE) 
        return()
      }
      
      
      input_neu<-data.frame(PAT_PATIENTENNR=1,ORD_LABORNR=1,ORD_MATERIAL=input2[,names(input2)==input$column3],
                            ORD_FACHBEREICH=input2[,names(input2)==input$column2],
                            ORD_DATUM=input2[,names(input2)==input$column4],
                            RES_ERREGER=input2[,names(input2)==input$column1],
                            X_STATUS=1)
      input_neu<-cbind(input_neu,input2[,c(which(names(input2)==input$column5):length(input2[1,]))])
      
      input2<-input_neu
      
      shinyjs::html("text", paste0("<br>Input file successfully configured.<br><br>"), add = TRUE)

    
    output$anweisung0pre<-renderText({NULL})
    output$anweisung1pre<-renderText({NULL})
    output$anweisung2pre<-renderText({NULL})
    output$anweisung3pre<-renderText({NULL})

    
    ##Start Analyse-Konfiguration - Teil 1

    #####Bakterien
    #helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")

    if(input$column4b=="dd.mm.yy"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
    }
    if(input$column4b=="dd.mm.yyyy"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    }
    if(input$column4b=="mm/dd/yy"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
    }
    if(input$column4b=="mm/dd/yyyy"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
    }
    if(input$column4b=="yy-mm-dd"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
    }
    if(input$column4b=="yyyy-mm-dd"){
      helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
    }
    
    
    if(input$column4b=="dd.mm.yy"){
      if(nchar(input2$ORD_DATUM[1])!=8||length(grep(".",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format dd.mm.yy specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    if(input$column4b=="dd.mm.yyyy"){
      if(nchar(input2$ORD_DATUM[1])!=10||length(grep(".",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format dd.mm.yyyy specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    if(input$column4b=="mm/dd/yy"){
      if(nchar(input2$ORD_DATUM[1])!=8||length(grep("/",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format mm/dd/yy specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    if(input$column4b=="mm/dd/yyyy"){
      if(nchar(input2$ORD_DATUM[1])!=10||length(grep("/",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format mm/dd/yyyy specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    if(input$column4b=="yy-mm-dd"){
      if(nchar(input2$ORD_DATUM[1])!=8||length(grep("-",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format yy-mm-dd specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    if(input$column4b=="yyyy-mm-dd"){
      if(nchar(input2$ORD_DATUM[1])!=10||length(grep("-",input2$ORD_DATUM[1],fixed=T))==0){
        shinyjs::html("text", paste0("<br>NOTE: Date format yyyy-mm-dd specified, but first entry is ",input2$ORD_DATUM[1],"<br><br>"), add = TRUE)
      }
    }
    
    
    
    years_bakterien<-unique(format(helper1bakterien,"%Y"))
    
    output$bakteriumUIinitial<-renderUI({
      h4("Cluster analyses: Interaktive")
    })
    
    output$bakteriumUI0a<-renderUI({
      selectInput('analysis_type1a',label = "Jahr",choices = years_bakterien)
      #selectInput('analysis_type1a',label = "Jahr",choices = jahre_verfuegbar,selected = jahr_gewaehlt)
    })
    
    output$bakteriumUI0b<-renderUI({
      #input2<-read.table(paste0("www/Daten",input$analysis_type1a,".txt"),
      #                   header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F) 
      #helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      #years_bakterien<-unique(format(helper1bakterien,"%Y"))
      
      material_helper<-unique(input2$ORD_MATERIAL[format(helper1bakterien,"%Y")==input$analysis_type1a])
      material_helper<-material_helper[order(material_helper)]
      selectInput('analysis_type1b',label = "Specimen",choices = c("All",material_helper),selected="All")
    })
    
    
    output$bakteriumUI0<-renderUI({
      radioButtons('analysis_type1',label = "Analysis is independently conducted",
                   choices = c("per species","per clinic"),selected = "per species",inline = T)
    })
    
    output$anweisung<-renderText({NULL})
    
    output$bakteriumUI1<-renderUI({conditionalPanel(
      condition="input.analysis_type1=='per species'",
      h4("Independent analysis per species"),
      radioButtons('bak_select',label = "Analyze all species? (min. 30 cases)",
                   choices = c("Yes","No"),selected = "Yes",inline = T)
    )})

    output$bakteriumUI2<-renderUI({
      alle_bakterien<-input2$RES_ERREGER
      if(input$analysis_type1b=="All"){
        alle_bakterien<-alle_bakterien[format(helper1bakterien,"%Y")==input$analysis_type1a]
      }else{
        alle_bakterien<-alle_bakterien[format(helper1bakterien,"%Y")==input$analysis_type1a&input2$ORD_MATERIAL==input$analysis_type1b]
      }
      
      alle_bakterien_table<-table(alle_bakterien)
      bakterien<-names(alle_bakterien_table)[as.numeric(alle_bakterien_table)>30]
      
      conditionalPanel(
      condition="input.bak_select=='No'&&input.analysis_type1=='per species'",
      checkboxGroupInput('bak_selected',label = "Select species",
                         choices =bakterien)
    )})
    
    output$bakteriumUI3<-renderUI({conditionalPanel(
      condition="input.analysis_type1=='per species'",
      hr(),
      h5("Analysis by heatmap"),
      h6("Note: Antimicrobial agents with >70% missing values will be filtered automatically for hierarchical clustering."),
      checkboxGroupInput('cluster_type_bak_heat1',label = "Visualization heatmap",
                         choices = c("Data ordered by 1) clinic, 2) resistance",
                                     "Data ordered by 1) clinic, 2) date",
                                     "Hierarchical clustering")),
      hr(),
      h5("Analysis by dimension reduction (UMAP)"),
      h6("Note: Analysis can only be conducted on complete data. Antimicrobial agents with >20% missing values 
         are automatically filtered. Subsequently, all cases with missing values are filtered."),
      checkboxGroupInput('cluster_type_bak_umap1',label = "Visualization UMAP",
                         choices = c("Plot with colored clinics",
                                     "Plot with colored clusters"))
    )})
    
    output$bakteriumUI4 <- renderUI({
      req(input$analysis_type1=='per species'&&(input$cluster_type_bak_umap1=='Plot with colored clusters'||length(input$cluster_type_bak_umap1)==2))
      checkboxGroupInput('cluster_type_bak_umap2',label = "Additional heatmap",
                         choices = c("Data ordered by UMAP clusters",
                                     "Data ordered by clinics"))
    })
    
    
    #####Kliniken
    output$klinikUI1<-renderUI({conditionalPanel(
      condition="input.analysis_type1=='per clinic'",
      h4("Independent analysis per clinic"),
      radioButtons('clinic_select',label = "Analyze all clinics?  (min. 30 cases)",
                   choices = c("Yes","No"),selected = "Yes",inline = T)
    )})
    
    output$klinikUI2<-renderUI({
      alle_fachbereiche<-input2$ORD_FACHBEREICH
      if(input$analysis_type1b=="All"){
        alle_fachbereiche<-alle_fachbereiche[format(helper1bakterien,"%Y")==input$analysis_type1a]
      }else{
        alle_fachbereiche<-alle_fachbereiche[format(helper1bakterien,"%Y")==input$analysis_type1a&input2$ORD_MATERIAL==input$analysis_type1b]
      }
      
      alle_fachbereiche_table<-table(alle_fachbereiche)
      fachbereich<-names(alle_fachbereiche_table)[as.numeric(alle_fachbereiche_table)>30]
      
      conditionalPanel(
      condition="input.clinic_select=='No'&&input.analysis_type1=='per clinic'",
      checkboxGroupInput('clinic_selected',label = "Select clinic",
                         choices =fachbereich),
      hr()
    )})
    
    output$klinikUI3<-renderUI({conditionalPanel(
      condition="input.analysis_type1=='per clinic'",
      hr(),
      h5("Analysis by heatmap"),
      h6("Note: Antimicrobial agents with >70% missing values will be filtered automatically for hierarchical clustering."),
      checkboxGroupInput('cluster_type_klinik_heat1',label = "Type of the clustering to be conducted",
                         choices = c("Data ordered by species",
                                     "Hierarchical clustering"))
    )})
    
    output$analyseUI<-renderUI({actionButton('do_analyse',"Start analysis",class = "btn-primary")})
    
    
    
    
    
    ##Start Analyse-Konfiguration - Teil 2
    #helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    if(input$column4b=="dd.mm.yy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
    }
    if(input$column4b=="dd.mm.yyyy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    }
    if(input$column4b=="mm/dd/yy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
    }
    if(input$column4b=="mm/dd/yyyy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
    }
    if(input$column4b=="yy-mm-dd"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
    }
    if(input$column4b=="yyyy-mm-dd"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
    }
    
    years<-unique(format(helper1,"%Y"))
    
    output$statistikUI1<-renderUI({
      h4("Resistance statistics")
    })
    
    output$statistikUI2<-renderUI({
      selectInput('statistik1',label = "Year",choices = years)
    })
    
    output$statistikUI3.2<-renderUI({
      material_helper<-unique(input2$ORD_MATERIAL[format(helper1,"%Y")==input$statistik1])
      material_helper<-material_helper[order(material_helper)]
      selectInput('statistik2.2',label = "Specimen",choices = c("All",material_helper),selected="All")
    })
    
    output$statistikUI4<-renderUI({
      if(input$statistik2.2=="All"){
        fb_helper<-unique(input2$ORD_FACHBEREICH[format(helper1,"%Y")==input$statistik1])
      }else{
        fb_helper<-unique(input2$ORD_FACHBEREICH[format(helper1,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2])
      }
      fb_helper<-fb_helper[order(fb_helper)]
      statistik_fb<-c("All",fb_helper)
      
      selectInput('statistik3',label = "Clinic",choices = statistik_fb,selected = "All")
    })
    
    output$statistikUI3<-renderUI({
      alle_bakterien<-input2$RES_ERREGER
      if(input$statistik2.2=="All"&&input$statistik3=="All"){##alle fachbereiche, alle materialien
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1]
      }
      if(input$statistik2.2!="All"&&input$statistik3=="All"){##alle fachbereiche
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2]
      }
      if(input$statistik2.2=="All"&&input$statistik3!="All"){##alle materialien
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3]
      }
      if(input$statistik2.2!="All"&&input$statistik3!="All"){
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3&input2$ORD_MATERIAL==input$statistik2.2]
      }

      alle_bakterien_table<-table(alle_bakterien)
      bakterien_helper<-names(alle_bakterien_table)[as.numeric(alle_bakterien_table)>30]
      selectInput('statistik2',label = "Species",choices = c("All",bakterien_helper),selected="All")
    })
    
    output$statistikUI5<-renderUI({actionButton('do_statistik',"Start analysis",class = "btn-primary")})
    
    output$statistikUI6<-renderUI({downloadButton('do_statistik_xlsx',"xlsx export")})
    
    
    
    ##Erreger-Statistik-Konfiguration
    #helper1erreger<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    if(input$column4b=="dd.mm.yy"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
    }
    if(input$column4b=="dd.mm.yyyy"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    }
    if(input$column4b=="mm/dd/yy"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
    }
    if(input$column4b=="mm/dd/yyyy"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
    }
    if(input$column4b=="yy-mm-dd"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
    }
    if(input$column4b=="yyyy-mm-dd"){
      helper1erreger<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
    }
    
    years<-unique(format(helper1erreger,"%Y"))
    
    output$erregerstatUI1<-renderUI({
      h4("Pathogen statistics")
    })
    
    output$erregerstatUI2<-renderUI({
      selectInput('erregerstat1',label = "Year",choices = years)
    })
    
    output$erregerstatUI3.2<-renderUI({
      material_helper<-unique(input2$ORD_MATERIAL[format(helper1erreger,"%Y")==input$erregerstat1])
      material_helper<-material_helper[order(material_helper)]
      selectInput('erregerstat2.2',label = "Specimen",choices = c("All",material_helper),selected="All")
    })

    output$erregerstatUI4<-renderUI({
      if(input$erregerstat2.2=="All"){
        fb_helper<-unique(input2$ORD_FACHBEREICH[format(helper1erreger,"%Y")==input$erregerstat1])
      }else{
        fb_helper<-unique(input2$ORD_FACHBEREICH[format(helper1erreger,"%Y")==input$erregerstat1&input2$ORD_MATERIAL==input$erregerstat2.2])
      }
      fb_helper<-fb_helper[order(fb_helper)]
      erregerstat_fb<-c("All",fb_helper)
      
      selectInput('erregerstat3',label = "Clinic",choices = erregerstat_fb,selected = "All")
    })
    
    output$erregerstatUI4b<-renderUI({
      radioButtons('erregerstat3b',label = "Cut-off min. 30 cases",choices = c("Yes","No"),selected = "Yes",inline = T)
    })
    
    output$erregerstatUI5<-renderUI({actionButton('do_erregerstat',"Start analysis",class = "btn-primary")})
    

    
    
    ##Start Download-Konfiguration
    output$downloadUIinitial<-renderUI({
      h4("Cluster analyses: Export")
    })
    
    output$anweisung2<-renderText({NULL})
    
    output$downloadUI1<-renderUI({
      radioButtons('report_name',label = "File name of the PDF export",
                   choices = c("Standard (GEFAAR_Resistance-Cluster-Analyses_<date>)","Individual"),
                   selected = "Standard (GEFAAR_Resistance-Cluster-Analyses_<date>)")
    })

    output$downloadUI2<-renderUI({conditionalPanel(
      condition="input.report_name=='Individual'",
      textInput("download_name_individuell",label = NULL,value = "")
    )})
    
    output$downloadUI3<-renderUI({
      checkboxGroupInput('download_analysis_type1',label = "Analysis is independently conducted",
                   choices = c("per species","per clinic"),inline = T)
    })
    
    output$downloadUI4<-renderUI({
      downloadButton('downloadData', 'Download')
    })
    
    
    ##Bakterien
    #download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    if(input$column4b=="dd.mm.yy"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
    }
    if(input$column4b=="dd.mm.yyyy"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    }
    if(input$column4b=="mm/dd/yy"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
    }
    if(input$column4b=="mm/dd/yyyy"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
    }
    if(input$column4b=="yy-mm-dd"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
    }
    if(input$column4b=="yyyy-mm-dd"){
      download_helper1bakterien<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
    }
    
    download_years_bakterien<-unique(format(helper1bakterien,"%Y"))
    
    output$downloadUIbak0a<-renderUI({
      selectInput('download_analysis_type1a',label = "Year",choices = download_years_bakterien)
    })
    
    output$downloadUIbak0b<-renderUI({
      download_material_helper<-unique(input2$ORD_MATERIAL[format(download_helper1bakterien,"%Y")==input$download_analysis_type1a])
      download_material_helper<-download_material_helper[order(download_material_helper)]
      selectInput('download_analysis_type1b',label = "Specimen",choices = c("All",download_material_helper),selected="All")
    })
    
    output$downloadUIbak1<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      h4("Independent analysis per species")
    })
    
    output$downloadUIbak1b<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      radioButtons('download_bak_select',label = "Analyze all species? (min. 30 cases)",
                   choices = c("Yes","No"),selected = "Yes",inline = T)
    })
    
    output$downloadUIbak2<-renderUI({
      req(input$download_bak_select=='No'&&(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2))
      download_alle_bakterien<-input2$RES_ERREGER
      if(input$download_analysis_type1b=="All"){
        download_alle_bakterien<-download_alle_bakterien[format(download_helper1bakterien,"%Y")==input$download_analysis_type1a]
      }else{
        download_alle_bakterien<-download_alle_bakterien[format(download_helper1bakterien,"%Y")==input$download_analysis_type1a&input2$ORD_MATERIAL==input$download_analysis_type1b]
       }
      download_alle_bakterien_table<-table(download_alle_bakterien)
      download_bakterien<-names(download_alle_bakterien_table)[as.numeric(download_alle_bakterien_table)>30]
      checkboxGroupInput('download_bak_selected',label = "Select species",
                         choices =download_bakterien)
     })
    
    output$downloadUIbak3<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      hr()
    })
    
    output$downloadUIbak3b<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      h5("Analysis by heatmap")
    })
    
    output$downloadUIbak3c<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      h6("Note: Antimicrobial agents with >70% missing values will be filtered automatically for hierarchical clustering.")
    })
    
    output$downloadUIbak3d<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      checkboxGroupInput('download_cluster_type_bak_heat1',label = "Visualization heatmap",
                         choices = c("Data ordered by 1) clinic, 2) resistance",
                                     "Data ordered by 1) clinic, 2) date",
                                     "Hierarchical clustering"))
    })
    
    output$downloadUIbak3e<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      hr()
    })
    
    output$downloadUIbak3f<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      h5("Analysis by dimension reduction (UMAP)")
    })
    
    output$downloadUIbak3g<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      h6("Note: Analysis can only be conducted on complete data. Antimicrobial agents with >20% missing values 
         are automatically filtered. Subsequently, all cases with missing values are filtered.")
    })
    
    output$downloadUIbak3h<-renderUI({
      req(input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)
      checkboxGroupInput('download_cluster_type_bak_umap1',label = "Visualization UMAP",
                         choices = c("Plot with colored clinics",
                                     "Plot with colored clusters"))
    })
    
    output$downloadUIbak4 <- renderUI({
      req((input$download_analysis_type1=='per species'||length(input$download_analysis_type1)==2)&&(input$download_cluster_type_bak_umap1=='Plot with colored clusters'||length(input$download_cluster_type_bak_umap1)==2))
      checkboxGroupInput('download_cluster_type_bak_umap2',label = "Additional heatmap",
                         choices = c("Data ordered by UMAP clusters",
                                     "Data ordered by clinics"))
    })
    
    
    #####Kliniken
    output$downloadUIklinik1<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      h4("Independent analysis per clinic")
    })
    
    output$downloadUIklinik1b<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      radioButtons('download_clinic_select',label = "Analyze all clinics? (min. 30 cases)",
                   choices = c("Yes","No"),selected = "Yes",inline = T)
    })

    download_fachbereich<-unique(input2$ORD_FACHBEREICH)
    download_fachbereich<-download_fachbereich[order(download_fachbereich)]
    
    output$downloadUIklinik2<-renderUI({
      req(input$download_clinic_select=='No'&&(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2))
      download_alle_fachbereiche<-input2$ORD_FACHBEREICH
      if(input$download_analysis_type1b=="All"){
        download_alle_fachbereiche<-download_alle_fachbereiche[format(download_helper1bakterien,"%Y")==input$download_analysis_type1a]
      }else{
        download_alle_fachbereiche<-download_alle_fachbereiche[format(download_helper1bakterien,"%Y")==input$download_analysis_type1a&input2$ORD_MATERIAL==input$download_analysis_type1b]
      }
      download_alle_fachbereiche_table<-table(download_alle_fachbereiche)
      download_fachbereich<-names(download_alle_fachbereiche_table)[as.numeric(download_alle_fachbereiche_table)>30]
      checkboxGroupInput('download_clinic_selected',label = "Select clinics",
                         choices =download_fachbereich)
    })
    
    output$downloadUIklinik3<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      hr()
    })
    
    output$downloadUIklinik3b<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      h5("Analysis by heatmap")
    })
    
    output$downloadUIklinik3c<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      h6("Note: Antimicrobial agents with >70% missing values will be filtered automatically for hierarchical clustering.")
    })
    
    output$downloadUIklinik3d<-renderUI({
      req(input$download_analysis_type1=='per clinic'||length(input$download_analysis_type1)==2)
      checkboxGroupInput('download_cluster_type_klinik_heat1',label = "Visualization heatmap",
                         choices = c("Data ordered by species",
                                     "Hierarchical clustering"))
    })
    
    rv <- reactiveValues()
    
    runBuildModel <- function(input, output) {
      rv$outputText = paste0("<b>Year: </b>",input$statistik1,"<br>",
                             "<b>Specimen: </b>",input$statistik2.2,"<br>",
                             "<b>Clinic: </b>",input$statistik3,"<br>",
                             "<b>Species: </b>",input$statistik2)
      #shinyjs::html(id = 'text_zusammenfassung0c', rv$outputText)
      #shinyjs::html(id = 'text_zusammenfassung0c2', rv$outputText)
      rv$outputText2 = paste0("<b>Year: </b>",input$statistik1,"<br>",
                              "<b>Specimen: </b>",input$statistik2.2)
      rv$outputText3 = paste0("<b>Material: </b>",input$trend2.2,"<br>",
                              "<b>Clinic: </b>",input$trend3,"<br>",
                              "<b>Species: </b>",input$trend2,"<br>",
                              "<b>Antimicrobial: </b>",input$trend2.3)
      rv$outputText4 = paste0("<b>Year: </b>",input$erregerstat1,"<br>",
                              "<b>Specimen: </b>",input$erregerstat2.2,"<br>",
                              "<b>Clinic: </b>",input$erregerstat3)
    }

    
    
    
    
    ##Start Analyse-Konfiguration - Trend
    output$trendUI1<-renderUI({
      h4("Trend analysis")
    })
    
    if(input$column4b=="dd.mm.yy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
    }
    if(input$column4b=="dd.mm.yyyy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    }
    if(input$column4b=="mm/dd/yy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
    }
    if(input$column4b=="mm/dd/yyyy"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
    }
    if(input$column4b=="yy-mm-dd"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
    }
    if(input$column4b=="yyyy-mm-dd"){
      helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
    }
    
    #helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
    years<-unique(format(helper1,"%Y"))
    
    output$trendUI3.2<-renderUI({
      material_all<-unique(input2$ORD_MATERIAL)
      for(y in years){
        material_temp<-unique(input2$ORD_MATERIAL[format(helper1,"%Y")==y])
        material_all<-material_all[material_all%in%material_temp]
      }
      material_helper<-material_all
      material_helper<-material_helper[order(material_helper)]
      selectInput('trend2.2',label = "Specimen",choices = c("All",material_helper),selected="All")
    })
    
    output$trendUI4<-renderUI({
      if(input$trend2.2=="All"){
        fb_all<-unique(input2$ORD_FACHBEREICH)
        for(y in years){
          fb_temp<-unique(input2$ORD_FACHBEREICH[format(helper1,"%Y")==y])
          fb_all<-fb_all[fb_all%in%fb_temp]
        }
        fb_helper<-fb_all
      }else{
        fb_all<-unique(input2$ORD_FACHBEREICH[input2$ORD_MATERIAL==input$trend2.2])
        for(y in years){
          fb_temp<-unique(input2$ORD_FACHBEREICH[format(helper1,"%Y")==y&input2$ORD_MATERIAL==input$trend2.2])
          fb_all<-fb_all[fb_all%in%fb_temp]
        }
        fb_helper<-fb_all
      }
      fb_helper<-fb_helper[order(fb_helper)]
      trend_fb<-c("All",fb_helper)
      
      selectInput('trend3',label = "Clinic",choices = trend_fb,selected = "All")
    })
    
    output$trendUI3<-renderUI({
      alle_bakterien<-input2$RES_ERREGER
      if(input$trend2.2=="All"&&input$trend3=="All"){##alle fachbereiche, alle materialien
        alle_bakterien<-alle_bakterien
        helper_neu<-helper1
      }
      if(input$trend2.2!="All"&&input$trend3=="All"){##alle fachbereiche
        alle_bakterien<-alle_bakterien[input2$ORD_MATERIAL==input$trend2.2]
        helper_neu<-helper1[input2$ORD_MATERIAL==input$trend2.2]
      }
      if(input$trend2.2=="All"&&input$trend3!="All"){##alle materialien
        alle_bakterien<-alle_bakterien[input2$ORD_FACHBEREICH==input$trend3]
        helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3]
      }
      if(input$trend2.2!="All"&&input$trend3!="All"){
        alle_bakterien<-alle_bakterien[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2]
        helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2]
      }
      
      if(length(helper_neu)>0){
        all_years<-format(helper_neu,"%Y")
        alle_bakterien_table<-table(alle_bakterien,all_years)
        
        #alle_bakterien_table<-alle_bakterien_table[rowSums(alle_bakterien_table>30)==length(alle_bakterien_table[1,]),]
        #for(laenge in 1:length(alle_bakterien_table[1,])){
        #  alle_bakterien_table<-alle_bakterien_table[alle_bakterien_table[,laenge]>30,]
        #}
        #bakterien_helper<-row.names(alle_bakterien_table)
        bakterien_helper<-row.names(alle_bakterien_table)[rowSums(alle_bakterien_table>30)==length(alle_bakterien_table[1,])]
        if(length(bakterien_helper)>0){
          selectInput('trend2',label = "Species",choices = c(bakterien_helper))
        }else{
          selectInput('trend2',label = "Species",choices = c("Insufficient number of cases"))
        }
      }else{
        selectInput('trend2',label = "Species",choices = c("Insufficient number of cases"))
      }
      
    })
    
    output$trendUI3.3<-renderUI({
      if(input$trend2!="Insufficient number of cases"){
        input_filter<-input2
        if(input$trend2.2=="All"&&input$trend3=="All"){##alle fachbereiche, alle materialien
          input_filter<-input_filter[input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2!="All"&&input$trend3=="All"){##alle fachbereiche
          input_filter<-input_filter[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2=="All"&&input$trend3!="All"){##alle materialien
          input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2!="All"&&input$trend3!="All"){
          input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
        }
        all_years<-format(helper_neu,"%Y")
        
        temp_ab<-apply(input_filter[,grep("_",names(input_filter),fixed=T,invert = T)],2,function(x){sum(x!="-")})
        temp_ab<-temp_ab[temp_ab>0]
        temp_ab_namen<-names(temp_ab)
        temp_ab_namen_final<-c()
        
        for(ab_relevant in temp_ab_namen){
          for(year_is in unique(all_years)){
            temp_ab_test<-input_filter[all_years==year_is,names(input_filter)==ab_relevant]
            if(sum(temp_ab_test!="-")>30){
              temp_ab_namen_final<-c(temp_ab_namen_final,ab_relevant)
            }
          }
        }
        temp_ab_namen_final<-unique(temp_ab_namen_final)
        temp_ab_namen_final<-temp_ab_namen_final[order(temp_ab_namen_final)]
        
        selectInput('trend2.3',label = "Antimicrobial",choices = c("All",temp_ab_namen_final))
      }else{
        selectInput('trend2.3',label = "Antimicrobial",choices = c("Insufficient number of cases"))
      }
    })
    
    output$trendUI5<-renderUI({actionButton('do_trend',"Start analysis",class = "btn-primary")})
    
    output$trendUI6<-renderUI({downloadButton('do_trend_xlsx',"xlsx export")})
    
    
    
    
    
    
    

    observeEvent(input$do_erregerstat,{
      progress <- shiny::Progress$new()
      progress$set(message = "Generate pathogen statistics", value = 0)
      
      #helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      years<-unique(format(helper1,"%Y"))
      
      shinyjs::html("text", paste0("<br>Pathogen statistics are generated.<br><br>"), add = FALSE)
      
      #helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper1statistik_e<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      if(input$erregerstat2.2=="All"){
        input3<-input2[format(helper1statistik_e,"%Y")==input$erregerstat1,]
      }else{
        input3<-input2[format(helper1statistik_e,"%Y")==input$erregerstat1&input2$ORD_MATERIAL==input$erregerstat2.2,]
      }
      stat_fachbereich<-input$erregerstat3
      stat_material<-input$erregerstat2.2

      if(input$erregerstat3=="All"){#alle Fachbereiche
        ab_spalten<-grep("_",names(input3),invert = T)
        
        if(input$erregerstat2.2=="All"){##alle materialien
          table_fach_bak<-as.data.frame(cbind(names(table(input3$RES_ERREGER)),
                                              table(input3$RES_ERREGER,input3$ORD_MATERIAL)))
          row.names(table_fach_bak)<-NULL
          names(table_fach_bak)[1]<-"Species"
          for(i in 2:length(table_fach_bak[1,])){
            table_fach_bak[,i]<-as.numeric(table_fach_bak[,i])
          }
          table_fach_bak$Sum<-rowSums(table_fach_bak[,c(2:length(table_fach_bak[1,]))])
          table_fach_bak<-table_fach_bak[order(table_fach_bak$Sum,decreasing = T),]
          table_fach_bak_colsums<-c()
          for(z in 2:length(table_fach_bak[1,])){
            table_fach_bak_colsums[(z-1)]<-sum(table_fach_bak[,z])
          }
          
          output$text_erregerstat2<-renderText({NULL})
          if(input$erregerstat3b=="No"){
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
            output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                       caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                       extensions = 'Buttons', options = list(
                                                                         dom = 'Blfrtip',
                                                                         buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                         columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                             server=F)
          }else{
            table_fach_bak<-table_fach_bak[table_fach_bak$Sum>=30,]
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
            output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                       caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                       extensions = 'Buttons', options = list(
                                                                         dom = 'Blfrtip',
                                                                         buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                         columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                             server=F)
          }
        }else{
          table_fach_bak<-as.data.frame(cbind(names(table(input3$RES_ERREGER[input3$ORD_MATERIAL==input$erregerstat2.2])),
                                              table(input3$RES_ERREGER[input3$ORD_MATERIAL==input$erregerstat2.2],input3$ORD_MATERIAL[input3$ORD_MATERIAL==input$erregerstat2.2])))
          row.names(table_fach_bak)<-NULL
          names(table_fach_bak)[1]<-"Species"
          for(i in 2:length(table_fach_bak[1,])){
            table_fach_bak[,i]<-as.numeric(table_fach_bak[,i])
          }
          table_fach_bak<-table_fach_bak[order(table_fach_bak[,2],decreasing = T),]
          table_fach_bak_colsums<-c()
          for(z in 2:length(table_fach_bak[1,])){
            table_fach_bak_colsums[(z-1)]<-sum(table_fach_bak[,z])
          }
          
          output$text_erregerstat2<-renderText({NULL})
          if(input$erregerstat3b=="No"){
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
          output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                     caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                     extensions = 'Buttons', options = list(
                                                                       dom = 'Blfrtip',
                                                                       buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                     columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                           server=F
          )
          }else{
            table_fach_bak<-table_fach_bak[table_fach_bak[,2]>=30,]
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                       caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                       extensions = 'Buttons', options = list(
                                                                         dom = 'Blfrtip',
                                                                         buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                         columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                             server=F
            )
          }
        }
      }
      if(input$erregerstat3!="All"){#alle Fachbereiche
        ab_spalten<-grep("_",names(input3),invert = T)
        if(input$erregerstat2.2=="All"){##alle materialien
          table_fach_bak<-as.data.frame(cbind(names(table(input3$RES_ERREGER[input3$ORD_FACHBEREICH==input$erregerstat3])),
                                              table(input3$RES_ERREGER[input3$ORD_FACHBEREICH==input$erregerstat3],input3$ORD_MATERIAL[input3$ORD_FACHBEREICH==input$erregerstat3])))
          row.names(table_fach_bak)<-NULL
          names(table_fach_bak)[1]<-"Species"
          for(i in 2:length(table_fach_bak[1,])){
            table_fach_bak[,i]<-as.numeric(table_fach_bak[,i])
          }
          table_fach_bak$Sum<-0
          for(z in 1:length(table_fach_bak[,1])){
            table_fach_bak$Sum[z]<-sum(table_fach_bak[z,c(2:length(table_fach_bak[1,]))])
          }
          table_fach_bak<-table_fach_bak[order(table_fach_bak$Sum,decreasing = T),]
          table_fach_bak_colsums<-c()
          for(z in 2:length(table_fach_bak[1,])){
            table_fach_bak_colsums[(z-1)]<-sum(table_fach_bak[,z])
          }
          
          output$text_erregerstat2<-renderText({NULL})
          if(input$erregerstat3b=="No"){
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
          output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                     caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                     extensions = 'Buttons', options = list(
                                                                       dom = 'Blfrtip',
                                                                       buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                       columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                           server=F
          )
          }else{
            table_fach_bak<-table_fach_bak[table_fach_bak$Sum>=30,]
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
            output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                       caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                       extensions = 'Buttons', options = list(
                                                                         dom = 'Blfrtip',
                                                                         buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                         columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                             server=F
            )
          }
        }else{
          table_fach_bak<-as.data.frame(cbind(names(table(input3$RES_ERREGER[input3$ORD_MATERIAL==input$erregerstat2.2&input3$ORD_FACHBEREICH==input$erregerstat3])),
                                              table(input3$RES_ERREGER[input3$ORD_MATERIAL==input$erregerstat2.2&input3$ORD_FACHBEREICH==input$erregerstat3],input3$ORD_MATERIAL[input3$ORD_MATERIAL==input$erregerstat2.2&input3$ORD_FACHBEREICH==input$erregerstat3])))
          row.names(table_fach_bak)<-NULL
          names(table_fach_bak)[1]<-"Species"
          for(i in 2:length(table_fach_bak[1,])){
            table_fach_bak[,i]<-as.numeric(table_fach_bak[,i])
          }
          table_fach_bak<-table_fach_bak[order(table_fach_bak[,2],decreasing = T),]
          table_fach_bak_colsums<-c()
          for(z in 2:length(table_fach_bak[1,])){
            table_fach_bak_colsums[(z-1)]<-sum(table_fach_bak[,z])
          }

          output$text_erregerstat2<-renderText({NULL})
          if(input$erregerstat3b=="No"){
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
          output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                     caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                     extensions = 'Buttons', options = list(
                                                                       dom = 'Blfrtip',
                                                                       buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                      list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                       columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                           server=F
          )
          }else{
            table_fach_bak<-table_fach_bak[table_fach_bak[,2]>=30,]          
            table_fach_bak_backup<-table_fach_bak
            for(z in 1:length(table_fach_bak_colsums)){
              table_fach_bak_backup[,(z+1)]<-format(round(100*table_fach_bak[,(z+1)]/table_fach_bak_colsums[z],digits=1),nsmall=1)
            }
            table_fach_bak<-table_fach_bak_backup
            names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]<-paste0(names(table_fach_bak_backup)[2:length(table_fach_bak[1,])]," N %")
            
            output$datatable_erregerstat1 <- renderDataTable(datatable(table_fach_bak_backup,rownames=F,
                                                                       caption = htmltools::tags$caption(paste0("Clinic: ",stat_fachbereich), style="color:rgb(49,126,172);font-size: 14pt"),
                                                                       extensions = 'Buttons', options = list(
                                                                         dom = 'Blfrtip',
                                                                         buttons = list(list(extend="csv",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="excel",filename="GEFAAR_Pathogen_Statistics"),
                                                                                        list(extend="print",filename="GEFAAR_Pathogen_Statistics")),
                                                                         columnDefs = list(list(className = 'dt-right', targets = c(1:(length(table_fach_bak_backup[1,])-1)))))),
                                                             server=F
            )
          }
        }
      }
      progress$close()
      shinyjs::html("text", paste0("<br><br>","Pathogen statistics successfully generated.","<br>"), add = TRUE)  
    })
    
    
    observeEvent(input$do_statistik,{
      progress <- shiny::Progress$new()
      progress$set(message = "Generate resistance statistics", value = 0)
      
      #helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      years<-unique(format(helper1,"%Y"))
      
      shinyjs::html("text", paste0("<br>Statistical analysis is launched.<br><br>"), add = FALSE)
      coxDF<-runBuildModel(input, output)    
      
      observe(output$text_zusammenfassung0c <- renderText(HTML(rv$outputText)))
      observe(output$text_zusammenfassung0c2 <- renderText(HTML(rv$outputText)))
      observe(output$text_zusammenfassung0c3 <- renderText(HTML(rv$outputText2)))
      observe(output$text_zusammenfassung0c4 <- renderText(HTML(rv$outputText2)))
      
      
      #########################
      ###Eigentliche Analyse###
      #########################
      #input3<-input2
      #helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper1statistik<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      if(input$statistik2.2=="All"){
        input3<-input2[format(helper1statistik,"%Y")==input$statistik1,]
      }else{
        input3<-input2[format(helper1statistik,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2,]
      }
      
      
      alle_bakterien<-input2$RES_ERREGER
      if(input$statistik2.2=="All"&&input$statistik3=="All"){##alle fachbereiche, alle materialien
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1]
      }
      if(input$statistik2.2!="All"&&input$statistik3=="All"){##alle fachbereiche
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2]
      }
      if(input$statistik2.2=="All"&&input$statistik3!="All"){##alle materialien
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3]
      }
      if(input$statistik2.2!="All"&&input$statistik3!="All"){
        alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3&input2$ORD_MATERIAL==input$statistik2.2]
      }
      
      alle_bakterien_table<-table(alle_bakterien)
      bakterien_helper<-names(alle_bakterien_table)[as.numeric(alle_bakterien_table)>30]
      if(length(bakterien_helper)==0){
        shinyjs::html("text", paste0("<br>Insufficient number of cases for an analysis with the selected configuration (min. 30 cases).<br><br>"), add = TRUE) 
        
        output$text_zusammenfassung0<-renderText({"Insufficient number of cases for an analysis with the selected configuration (min. 30 cases)."})
        output$text_zusammenfassung02<-renderText({"Insufficient number of cases for an analysis with the selected configuration (min. 30 cases)."})
        
        output$datatables_statistik <- renderUI({
          datatables_statistik_output_list <- lapply(1:length(names(alle_bakterien_table)), function(k) {
            datatablesname_statistik <- paste("datatables_statistik", k, sep="")
            dataTableOutput(datatablesname_statistik)
          })
          do.call(tagList, datatables_statistik_output_list)
        })
        
        for(n in 1:length(names(alle_bakterien_table))){
          local({
            my_i <- n
            datatablesname_statistik <- paste("datatables_statistik", my_i, sep="")
            output[[datatablesname_statistik]] <- renderDataTable({NULL})
        })
        }
        
        output$plots_statistik <- renderUI({
          plot_statistik_output_list <- lapply(1:length(names(alle_bakterien_table)), function(k) {
            plotname_statistik <- paste("plot_statistik", k, sep="")
            plotOutput(plotname_statistik, height = 1000, width = 1000)
          })
          do.call(tagList, plot_statistik_output_list)
        })
        
        for(n in 1:length(names(alle_bakterien_table))){
          local({
            my_i <- n
            plotname_statistik <- paste("plot_statistik", my_i, sep="")
            output[[plotname_statistik]] <- renderPlot({NULL})
          })
        }
        
        progress$close()
        return()
      }
      
      
      
      

      ############################################
      zusammenfassung_erstellen<-"No"
      if(zusammenfassung_erstellen=="Yes"){
        shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbspGenerate summary clinics","<br>"), add = TRUE)  

        ab_spalten<-grep("_",names(input3),invert = T)
        temp<-cbind(input3$ORD_FACHBEREICH,input3$RES_ERREGER,input3[,ab_spalten])
        table_fach_bak<-as.data.frame(cbind(names(table(input3$ORD_FACHBEREICH)),
                                            table(input3$ORD_FACHBEREICH,input3$RES_ERREGER)))
        row.names(table_fach_bak)<-NULL
        names(table_fach_bak)[1]<-"Clinic"
        for(i in 2:length(table_fach_bak[1,])){
          table_fach_bak[,i]<-as.numeric(table_fach_bak[,i])
        }
        table_fach_bak$Sum<-rowSums(table_fach_bak[,c(2:length(table_fach_bak[1,]))])
        helper<-table_fach_bak[1,]
        helper[1,1]<-"Sum"
        helper[1,2:length(helper[1,])]<-colSums(table_fach_bak[,c(2:length(table_fach_bak[1,]))])
        table_fach_bak<-rbind(table_fach_bak,helper)

        shinyjs::html("text", paste0("&nbsp&nbsp&nbspGenerate summary antimicrobial agents","<br>"), add = TRUE)  
        
        temp2<-temp
        ab_names<-names(input3)[ab_spalten]
        for(i in ab_names){
          temp2[temp2[,i==names(temp2)]!="-",i==names(temp2)]<-i
          temp2[temp2[,i==names(temp2)]=="-",i==names(temp2)]<-NA
        }
        
        names(temp2)[1:2]<-c("ORD_FACHBEREICH","RES_ERREGER")
        table_anti_bak<-data.frame(Antibiotikum=ab_names)
        erreger<-names(table(temp2$RES_ERREGER))
        for(i in 1:length(erreger)){
          table_anti_bak<-cbind(table_anti_bak,V1=0)
        }
        names(table_anti_bak)<-c("Antimicrobial",erreger)
        for(i in erreger){
          for(j in ab_names){
            table_anti_bak[table_anti_bak$Antibiotikum==j,names(table_anti_bak)==i]<-sum(temp2[temp2$RES_ERREGER==i,names(temp2)==j]==j,na.rm=T)
          }
        }
        
        table_anti_bak$Sum<-rowSums(table_anti_bak[,c(2:length(table_anti_bak[1,]))])
        helper<-table_anti_bak[1,]
        helper[1,1]<-"Sum"
        helper[1,2:length(helper[1,])]<-colSums(table_anti_bak[,c(2:length(table_anti_bak[1,]))])
        table_anti_bak<-rbind(table_anti_bak,helper)

      }
      
      #########################################
      ##Statistik
      ########################################
      shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbspGenerate resistance statistics","<br>"), add = TRUE) 

      if(input$statistik3!="All"){##nur ausgewählte Kliniken/Fachbereiche
        input4<-input3[input3$ORD_FACHBEREICH==input$statistik3,]
      }else{
        input4<-input3
      }

      #helper4statistik<-as.Date(input4$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper4statistik<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }

      table_fb_anti_bak_all<-list()
      eintrag_laenge<-c()
      for(erreger in 1:length(bakterien_helper)){
        progress$inc(1/(length(bakterien_helper)+4))#,detail=bakterien_helper[erreger])
        input4b<-input4[input4$RES_ERREGER==bakterien_helper[erreger],]
        
        ab_spalten<-grep("_",names(input4b),invert = T)
        ab_names<-names(input4b)[ab_spalten]
        table_fb_anti_bak<-data.frame(Antibiotikum=ab_names,N=NA,S=NA,I=NA,R=NA,KI_R=NA)
        
        for(j in ab_names){
          table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,3]<-sum(input4b[,names(input4b)==j]=="S")
          table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,4]<-sum(input4b[,names(input4b)==j]=="I")
          table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,5]<-sum(input4b[,names(input4b)==j]=="R")
        }
        table_fb_anti_bak[,2]<-rowSums(table_fb_anti_bak[,3:5])
        
        for(j in 1:length(table_fb_anti_bak[,1])){
          if(table_fb_anti_bak[j,2]>0){
            stat_help1<-format(round(100*binom.test(x=table_fb_anti_bak[j,5],n=table_fb_anti_bak[j,2],
                                                    p=table_fb_anti_bak[j,5]/table_fb_anti_bak[j,2])$conf.int[1],1),nsmall=1)
            stat_help2<-format(round(100*binom.test(x=table_fb_anti_bak[j,5],n=table_fb_anti_bak[j,2],
                                                    p=table_fb_anti_bak[j,5]/table_fb_anti_bak[j,2])$conf.int[2],1),nsmall=1)
            table_fb_anti_bak[j,6]<-paste0(stat_help1," - ",stat_help2)          
          }
        }
        table_fb_anti_bak[,3]<-format(round(100*table_fb_anti_bak[,3]/table_fb_anti_bak[,2],1),nsmall=1)
        table_fb_anti_bak[,4]<-format(round(100*table_fb_anti_bak[,4]/table_fb_anti_bak[,2],1),nsmall=1)
        table_fb_anti_bak[,5]<-format(round(100*table_fb_anti_bak[,5]/table_fb_anti_bak[,2],1),nsmall=1)
        
        
        table_fb_anti_bak<-table_fb_anti_bak[!is.na(table_fb_anti_bak[,6]),]
        names(table_fb_anti_bak)<-c("Antimicrobial","N","S %","I %","R %","95% CI R")
        
        table_fb_anti_bak<-table_fb_anti_bak[table_fb_anti_bak$N>=30,]
        table_fb_anti_bak<-table_fb_anti_bak[order(table_fb_anti_bak[,1],decreasing = F),]
        table_fb_anti_bak<-table_fb_anti_bak[order(table_fb_anti_bak[,3],decreasing = T),]
        eintrag_laenge[erreger]<-length(table_fb_anti_bak[,1])
        table_fb_anti_bak_all[[erreger]]<-table_fb_anti_bak
      }
      
      
      output$text_zusammenfassung0<-renderText({NULL})
      
      if(input$statistik2!="All"){
        bakterien_update<-bakterien_helper[bakterien_helper%in%input$statistik2]
        eintrag_laenge_update<-eintrag_laenge[bakterien_helper%in%input$statistik2]
      }else{
        bakterien_update<-bakterien_helper
        eintrag_laenge_update<-eintrag_laenge
      }

      output$datatables_statistik <- renderUI({
        datatables_statistik_output_list <- lapply(1:length(bakterien_update), function(k) {
          datatablesname_statistik <- paste("datatables_statistik", k, sep="")
          dataTableOutput(datatablesname_statistik)
        })
        do.call(tagList, datatables_statistik_output_list)
      })
      
      for(n in 1:length(bakterien_update)){
        local({
          my_i <- n
          datatablesname_statistik <- paste("datatables_statistik", my_i, sep="")
          output[[datatablesname_statistik]] <- renderDataTable({datatable(table_fb_anti_bak_all[[which(bakterien_update[my_i]==bakterien_helper)]],
                                                                           caption = htmltools::tags$caption(bakterien_update[my_i], style="color:rgb(49,126,172);font-size: 14pt"),
                                                                           rownames=F,extensions = 'Buttons', 
                                                                           options = list(columnDefs = list(list(className = 'dt-right', targets = 2:5))))},
                                                                server=F)
        })
      }

      progress$inc(1/(length(bakterien_helper)+4))
      progress$inc(1/(length(bakterien_helper)+4))

      output$text_zusammenfassung02<-renderText({NULL})
      
      
      output$plots_statistik <- renderUI({
        plot_statistik_output_list <- lapply(1:length(bakterien_update), function(k) {
          plotname_statistik <- paste("plot_statistik", k, sep="")
          plotOutput(plotname_statistik, height = (200+50*eintrag_laenge_update[k]), width = 1000)
        })
        do.call(tagList, plot_statistik_output_list)
      })
      
      for(n in 1:length(bakterien_update)){
        local({
          my_i <- n
          plotname_statistik <- paste("plot_statistik", my_i, sep="")
          output[[plotname_statistik]] <- renderPlot({
            help_bar<-table_fb_anti_bak_all[[which(bakterien_update[my_i]==bakterien_helper)]][order(table_fb_anti_bak_all[[which(bakterien_update[my_i]==bakterien_helper)]][,1],decreasing = T),]
            help_bar<-help_bar[order(help_bar[,3],decreasing = F),]
            
            par(mar=c(4,14,6,1))
            barplot(t(as.matrix(help_bar[,5])),names.arg=help_bar[,1],horiz=T,las=1,main="",col="darkgoldenrod2",space = 0.5,border = "darkgoldenrod4",xlim = c(0,100))
            for(m in 1:length(help_bar[,1])){
              start<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][1])
              end<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][2])
              points(x=c(start,end),y=c(m*1.5-0.5,m*1.5-0.5),type="l",lwd=2)
              points(x=c(start,start),y=c(m*1.5-0.4,m*1.5-0.6),type="l",lwd=3)
              points(x=c(end,end),y=c(m*1.5-0.4,m*1.5-0.6),type="l",lwd=3)
              for(n in seq(10,100,10)){
                points(x=c(n,n),y=c(m*1.5,m*1.5-1),col="grey90",type="l")
              }
            }
            points(x=c(0,100),y=c(length(help_bar[,1])*1.5+0.025*length(help_bar[,1]),length(help_bar[,1])*1.5+0.025*length(help_bar[,1])),
                   col="grey90",lwd=3,type="l")
            title("R %    95% CI R",line = 0)
            mtext(bakterien_update[my_i],adj=-0.17,cex=1.5,col=rgb(49/255,126/255,172/255),line=2)
            
          })

        })
      }
      
      progress$inc(1/(length(bakterien_helper)+4))
      progress$inc(1/(length(bakterien_helper)+4))
      
      
      progress$close()
      shinyjs::html("text", paste0("<br><br>","Statistical analysis of ",
                                   length(bakterien_update)," species successfully conducted.","<br>"), add = TRUE)  
      shinyjs::html("text", paste0("<br>","Note:"),add=TRUE)
      shinyjs::html("text",paste0("<br>","For each species, only those antimicrobial agents for which at least 30 observations are available."), add = TRUE)  

      })
    
    output$do_statistik_xlsx <- downloadHandler(
      filename = function() {
        paste0("GEFAAR_Resistance_Statistics_",input$statistik1,"_Specimen_",input$statistik2.2,"_Species_",input$statistik2,"_Clinic_",input$statistik3,".xlsx")
        #"Test.xlsx"
      },
      content = function(file) {
        progress <- shiny::Progress$new()
        progress$set(message = "Preparing data for xlsx report", value = 0)
        
        #helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        if(input$column4b=="dd.mm.yy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
        }
        if(input$column4b=="dd.mm.yyyy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        }
        if(input$column4b=="mm/dd/yy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
        }
        if(input$column4b=="mm/dd/yyyy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
        }
        if(input$column4b=="yy-mm-dd"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
        }
        if(input$column4b=="yyyy-mm-dd"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
        }
        
        years<-unique(format(helper1,"%Y"))
        
        shinyjs::html("text", paste0("<br>Statistical analysis is launched.<br><br>"), add = FALSE)
        
        #########################
        ###Eigentliche Analyse###
        #########################
        
        #########################################
        ##Statistik
        ########################################
        shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbspGenerate xlsx report","<br>"), add = TRUE) 

        #helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        if(input$column4b=="dd.mm.yy"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
        }
        if(input$column4b=="dd.mm.yyyy"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        }
        if(input$column4b=="mm/dd/yy"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
        }
        if(input$column4b=="mm/dd/yyyy"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
        }
        if(input$column4b=="yy-mm-dd"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
        }
        if(input$column4b=="yyyy-mm-dd"){
          helper1statistik<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
        }
        
        if(input$statistik2.2=="All"){
          input3<-input2[format(helper1statistik,"%Y")==input$statistik1,]
        }else{
          input3<-input2[format(helper1statistik,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2,]
        }
        if(input$statistik3!="All"){##nur ausgewählte Kliniken/Fachbereiche
          input4<-input3[input3$ORD_FACHBEREICH==input$statistik3,]
        }else{
          input4<-input3
        }
        
        #helper4statistik<-as.Date(input4$ORD_DATUM,format = "%d.%m.%Y")
        if(input$column4b=="dd.mm.yy"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
        }
        if(input$column4b=="dd.mm.yyyy"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        }
        if(input$column4b=="mm/dd/yy"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
        }
        if(input$column4b=="mm/dd/yyyy"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
        }
        if(input$column4b=="yy-mm-dd"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
        }
        if(input$column4b=="yyyy-mm-dd"){
          helper4statistik<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
        }
        
        alle_bakterien<-input2$RES_ERREGER
        if(input$statistik2.2=="All"&&input$statistik3=="All"){##alle fachbereiche, alle materialien
          alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1]
        }
        if(input$statistik2.2!="All"&&input$statistik3=="All"){##alle fachbereiche
          alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_MATERIAL==input$statistik2.2]
        }
        if(input$statistik2.2=="All"&&input$statistik3!="All"){##alle materialien
          alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3]
        }
        if(input$statistik2.2!="All"&&input$statistik3!="All"){
          alle_bakterien<-alle_bakterien[format(helper1,"%Y")==input$statistik1&input2$ORD_FACHBEREICH==input$statistik3&input2$ORD_MATERIAL==input$statistik2.2]
        }
        
        alle_bakterien_table<-table(alle_bakterien)
        bakterien_helper<-names(alle_bakterien_table)[as.numeric(alle_bakterien_table)>30]
        if(length(bakterien_helper)==0){
          shinyjs::html("text", paste0("<br>Insufficient number of cases for an analysis with the selected configuration.<br><br>"), add = TRUE) 
          progress$close()
          return()
        }
        
        
        table_fb_anti_bak_all<-list()
        eintrag_laenge<-c()
        for(erreger in 1:length(bakterien_helper)){
          progress$inc(1/(length(bakterien_helper)))
          input4b<-input4[input4$RES_ERREGER==bakterien_helper[erreger],]

          ab_spalten<-grep("_",names(input4b),invert=T)
          ab_names<-names(input4b)[ab_spalten]
          table_fb_anti_bak<-data.frame(Antibiotikum=ab_names,N=NA,S=NA,I=NA,R=NA,KI_R=NA)
          
          for(j in ab_names){
            table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,3]<-sum(input4b[,names(input4b)==j]=="S")
            table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,4]<-sum(input4b[,names(input4b)==j]=="I")
            table_fb_anti_bak[table_fb_anti_bak$Antibiotikum==j,5]<-sum(input4b[,names(input4b)==j]=="R")
          }
          table_fb_anti_bak[,2]<-rowSums(table_fb_anti_bak[,3:5])
          
          for(j in 1:length(table_fb_anti_bak[,1])){
            if(table_fb_anti_bak[j,2]>0){
              stat_help1<-format(round(100*binom.test(x=table_fb_anti_bak[j,5],n=table_fb_anti_bak[j,2],
                                                      p=table_fb_anti_bak[j,5]/table_fb_anti_bak[j,2])$conf.int[1],1),nsmall=1)
              stat_help2<-format(round(100*binom.test(x=table_fb_anti_bak[j,5],n=table_fb_anti_bak[j,2],
                                                      p=table_fb_anti_bak[j,5]/table_fb_anti_bak[j,2])$conf.int[2],1),nsmall=1)
              table_fb_anti_bak[j,6]<-paste0(stat_help1," - ",stat_help2)          
            }
          }
          table_fb_anti_bak[,3]<-format(round(100*table_fb_anti_bak[,3]/table_fb_anti_bak[,2],1),nsmall=1)
          table_fb_anti_bak[,4]<-format(round(100*table_fb_anti_bak[,4]/table_fb_anti_bak[,2],1),nsmall=1)
          table_fb_anti_bak[,5]<-format(round(100*table_fb_anti_bak[,5]/table_fb_anti_bak[,2],1),nsmall=1)
          
          
          table_fb_anti_bak<-table_fb_anti_bak[!is.na(table_fb_anti_bak[,6]),]
          names(table_fb_anti_bak)<-c("Antimicrobial agents","N","S %","I %","R %","95% CI R")
          table_fb_anti_bak<-table_fb_anti_bak[table_fb_anti_bak$N>=30,]
          table_fb_anti_bak<-table_fb_anti_bak[order(table_fb_anti_bak[,1],decreasing = F),]
          table_fb_anti_bak<-table_fb_anti_bak[order(table_fb_anti_bak[,3],decreasing = T),]
          eintrag_laenge[erreger]<-length(table_fb_anti_bak[,1])
          table_fb_anti_bak_all[[erreger]]<-table_fb_anti_bak
          
          
        }
        progress$close()
        
        
        progress <- shiny::Progress$new()
        progress$set(message = "Generate data for xlsx report", value = 0)
        
        wb<-createWorkbook()
        addWorksheet(wb,sheetName = "Data sheet antimicrobial agents")
        
        table_export<-data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA)
        table_export[2,1]<-"Resistance statistics"
        table_export[3,1]<-"Year"
        table_export[3,2]<-input$statistik1
        table_export[4,1]<-"Species"
        table_export[4,2]<-input$statistik2
        table_export[5,1]<-"Specimen"
        table_export[5,2]<-input$statistik2.2
        table_export[6,1]<-"Clinic"
        table_export[6,2]<-input$statistik3
        
        insertImage(wb,sheet="Data sheet antimicrobial agents",file = "www/IMI.png",
                    height = 0.516, width = 0.648,
                    startRow = 1,startCol = 7)
        insertImage(wb,sheet="Data sheet antimicrobial agents",file = "www/UKM.png",
                    height = 0.516, width = 0.6,
                    startRow = 1,startCol = 1)
        
        writeData(wb,sheet="Data sheet antimicrobial agents",table_export,colNames = F,rowNames = F)
        all_white<-createStyle(fgFill="white")
        bold<-createStyle(textDecoration = "bold")
        size<-createStyle(fontSize = 10)
        size_big<-createStyle(fontSize = 14)
        linie<-createStyle(border = "bottom",borderColour = "grey85")
        linie2<-createStyle(border = "bottom",borderStyle = "medium")
        linie3<-createStyle(border = "bottom",borderStyle = "medium",borderColour = "grey85")
        right<-createStyle(halign = "right")
        hintergrund_grau<-createStyle(fgFill="grey90")
        ueberschrift<-createStyle(fontColour = "white",fgFill = rgb(42/255,77/255,125/255),
                                  fontSize = 14,textDecoration = "bold",valign = "center")
        
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=c(1:3600),cols=1:27,
                 gridExpand = T,style=all_white)
        
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=2,cols=1:7,style = ueberschrift,stack=T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=c(3:6),cols=1,style = bold,stack=T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=c(6),cols=1:7,style = linie,
                 gridExpand = T, stack=T)
        
        setColWidths(wb,sheet="Data sheet antimicrobial agents",cols = c(1:6),widths = "auto")
        setRowHeights(wb,sheet="Data sheet antimicrobial agents",rows = c(2,7),heights = 30)
        setRowHeights(wb,sheet="Data sheet antimicrobial agents",rows = 1,heights = 43)
        
        
        addWorksheet(wb,sheetName = "Figures antimicrobial agents")
        
        addStyle(wb,sheet="Figures antimicrobial agents",rows=c(1:5000),cols=1:27,
                 gridExpand = T,style=all_white)
        
        insertImage(wb,sheet="Figures antimicrobial agents",file = "www/IMI.png",
                    height = 0.516, width = 0.648,
                    startRow = 1,startCol = 8)
        insertImage(wb,sheet="Figures antimicrobial agents",file = "www/UKM.png",
                    height = 0.516, width = 0.6,
                    startRow = 1,startCol = 1)
        setRowHeights(wb,sheet="Figures antimicrobial agents",rows = 1,heights = 43)
        
        
        ##############################################
        if(input$statistik2!="All"){
          bakterien_update<-bakterien_helper[bakterien_helper%in%input$statistik2]
          eintrag_laenge_update<-eintrag_laenge[bakterien_helper%in%input$statistik2]
        }else{
          bakterien_update<-bakterien_helper
          eintrag_laenge_update<-eintrag_laenge
        }
        
        beginn<-8
        table_export_all<-data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA)
        rows_daten1<-c()
        rows_daten2<-c()
        rows_daten3<-c()
        rows_daten4<-c()
        rows_daten5<-c()
        rows_daten6<-c()
        rows_daten7<-c()
        rows_daten8<-c()
        export_abbildungen<-data.frame(V1=NA)
        add_height<-0
        for(erreger in (1:length(bakterien_update))){
          progress$inc(1/(length(bakterien_update)),detail=paste0(erreger,"/",length(bakterien_update)))
          
          table_export<-data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA)
          
          table_export[1,1]<-bakterien_update[erreger]
          table_export[2,]<-c(names(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]])[1],"",names(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]])[2:6])
          table_export[3:(2+length(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,1])),1]<-table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,1]
          table_export[3:(2+length(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,1])),3:7]<-table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,2:6]
          table_export[(3+length(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,1])),1:7]<-NA
          
          table_export_all<-rbind(table_export_all,table_export)
          
          rows_daten1<-c(rows_daten1,c((beginn+1):(beginn-2+length(table_export[,1]))))
          rows_daten2<-c(rows_daten2,(beginn+1))
          rows_daten3<-c(rows_daten3,c(beginn:(beginn+1)))
          rows_daten4<-c(rows_daten4,(beginn+1))
          rows_daten5<-c(rows_daten5,c(beginn:(beginn-1+length(table_export[,1]))))
          rows_daten6<-c(rows_daten6,c((beginn+1):(beginn-1+length(table_export[,1]))))
          rows_daten7<-c(rows_daten7,c(beginn:(beginn-1+length(table_export[,1]))))
          rows_daten8<-c(rows_daten8,c((beginn),(beginn-2+length(table_export[,1]))))
          
          beginn<-beginn+length(table_export[,1])
          
          #export_abbildungen[3+0.04*(200+50*eintrag_laenge_update[erreger])*(erreger-1),1]<-bakterien_update[erreger]
          export_abbildungen[3+add_height,1]<-bakterien_update[erreger]
          #message("Text: ",3+add_height)
          add_height<-add_height+(40/7.8)*(7.8/1000*(300+50*eintrag_laenge_update[erreger]))
        }
        
        writeData(wb,sheet="Data sheet antimicrobial agents",table_export_all,colNames = F,rowNames = F,startRow = 7)
        
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten1,cols=1:7,style = linie,gridExpand = T, stack=T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten2,cols=1:7,style = hintergrund_grau,stack=T,gridExpand = T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",cols=c(1:7),rows=rows_daten3,style=bold,stack=T,gridExpand = T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",cols=c(1:7),rows=rows_daten4,style=linie2,gridExpand = T,stack = T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten5,cols=c(2:7),gridExpand = T,stack=T,style=right)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten6,cols=c(2:7),gridExpand = T,style=size,stack = T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten7,cols=c(6:7),gridExpand = T,style=bold,stack = T)
        addStyle(wb,sheet="Data sheet antimicrobial agents",rows=rows_daten8,cols=c(1:7),gridExpand = T,style=linie3,stack = T)
        
        
        writeData(wb,sheet="Figures antimicrobial agents",export_abbildungen,colNames = F,rowNames = F)
        addStyle(wb,sheet="Figures antimicrobial agents",cols=c(1),rows=1:length(export_abbildungen[,1]),style=bold,stack=T)
        addStyle(wb,sheet="Figures antimicrobial agents",cols=c(1),rows=1:length(export_abbildungen[,1]),style=size_big,stack=T)
        
        progress$close()
        
        progress <- shiny::Progress$new()
        progress$set(message = "Write data to xlsx report", value = 0)
        add_height<-0
        for(erreger in (1:length(bakterien_update))){
          progress$inc(1/(length(bakterien_update)),detail=paste0(erreger,"/",length(bakterien_update)))
          
          help_bar<-table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][order(table_fb_anti_bak_all[[which(bakterien_update[erreger]==bakterien_helper)]][,1],decreasing = T),]
          help_bar<-help_bar[order(help_bar[,3],decreasing = F),]
          
          png(paste0(tempdir(), "/", "plot",erreger,".png"), width=800, height=(300+50*eintrag_laenge_update[erreger]),units = "px")
          par(mar=c(3,14,1,1))
          barplot(t(as.matrix(help_bar[,5])),names.arg=help_bar[,1],horiz=T,las=1,main="R %    95% CI R",
                  col="darkgoldenrod2",space = 0.5,border = "darkgoldenrod4",xlim = c(0,100))
          for(m in 1:length(help_bar[,1])){
            start<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][1])
            end<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][2])
            points(x=c(start,end),y=c(m*1.5-0.5,m*1.5-0.5),type="l",lwd=2)
            points(x=c(start,start),y=c(m*1.5-0.4,m*1.5-0.6),type="l",lwd=3)
            points(x=c(end,end),y=c(m*1.5-0.4,m*1.5-0.6),type="l",lwd=3)
            
            for(n in seq(10,100,10)){
              points(x=c(n,n),y=c(m*1.5,m*1.5-1),col="grey90",type="l")
            }
          }
          points(x=c(0,100),y=c(length(help_bar[,1])*1.5+0.5,length(help_bar[,1])*1.5+0.5),col="grey90",lwd=3,type="l")
          dev.off()
          
          insertImage(wb, sheet="Figures antimicrobial agents", paste0(tempdir(), "/", "plot",erreger,".png"), width = 6.6,height = 7.8/1000*(300+50*eintrag_laenge_update[erreger]),
                      startRow = 4+add_height,startCol = 1)
          
          add_height<-add_height+(40/7.8)*(7.8/1000*(300+50*eintrag_laenge_update[erreger]))
        }
        
        saveWorkbook(wb,file = file,overwrite=T)
        
        progress$close()
        shinyjs::html("text", paste0("<br><br>","xlsx report successfully generated.","<br>"), add = TRUE) 
      } 
      
    )
    
    
    observeEvent(input$do_trend,{
      progress <- shiny::Progress$new()
      progress$set(message = "Generate trend analysis", value = 0)
      
      helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      years<-unique(format(helper1,"%Y"))
      
      shinyjs::html("text", paste0("<br>Trend analysis is launched.<br><br>"), add = FALSE)
      coxDF<-runBuildModel(input, output)    
      
      observe(output$text_zusammenfassung_trend <- renderText(HTML(rv$outputText3)))
      
      if(input$trend2=="Insufficient number of cases"){
        shinyjs::html("text", paste0("<br>Insufficient number of cases for an analysis with the selected configuration (min. 30 cases).<br><br>"), add = TRUE) 
        output$text_trend2<-renderText({"Insufficient number of cases for an analysis with the selected configuration (min. 30 cases)"})
        
        output$plots_trend <- renderUI({
          plot_trend_output_list <- lapply(1:1, function(k) {
            plotname_trend <- paste("plot_trend", k, sep="")
            plotOutput(plotname_trend, height = 1000, width = 1000)
          })
          do.call(tagList, plot_trend_output_list)
        })
        
        for(n in 1:1){
          local({
            my_i <- n
            plotname_trend <- paste("plot_trend", my_i, sep="")
            output[[plotname_trend]] <- renderPlot({NULL})
          })
        }
        
        progress$close()
        return()
        
      }
      
      if(input$trend2!="Insufficient number of cases"){
        if(input$column4b=="dd.mm.yy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
        }
        if(input$column4b=="dd.mm.yyyy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        }
        if(input$column4b=="mm/dd/yy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
        }
        if(input$column4b=="mm/dd/yyyy"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
        }
        if(input$column4b=="yy-mm-dd"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
        }
        if(input$column4b=="yyyy-mm-dd"){
          helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
        }
        
        input_filter<-input2
        if(input$trend2.2=="All"&&input$trend3=="All"){##alle fachbereiche, alle materialien
          input_filter<-input_filter[input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2!="All"&&input$trend3=="All"){##alle fachbereiche
          input_filter<-input_filter[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2=="All"&&input$trend3!="All"){##alle materialien
          input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2]
        }
        if(input$trend2.2!="All"&&input$trend3!="All"){
          input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
          helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
        }
        all_years<-format(helper_neu,"%Y")
        
        if(length(unique(all_years))==1){
          shinyjs::html("text", paste0("<br><br>","Data on only one year is available, trend analysis cannot be conducted.","<br>"), add = TRUE)  
          output$text_trend2<-renderText({"Data on only one year is available, trend analysis cannot be conducted."})
          return()
        }
        
        if(input$trend2.3=="All"){##Alle möglichen AB
          temp_ab<-apply(input_filter[,grep("_",names(input_filter),fixed=T,invert = T)],2,function(x){sum(x!="-")})
          temp_ab<-temp_ab[temp_ab>0]
          temp_ab_namen<-names(temp_ab)
          temp_ab_namen_final<-c()
          
          for(ab_relevant in temp_ab_namen){
            for(year_is in unique(all_years)){
              temp_ab_test<-input_filter[all_years==year_is,names(input_filter)==ab_relevant]
              if(sum(temp_ab_test!="-")>30){
                temp_ab_namen_final<-c(temp_ab_namen_final,ab_relevant)
              }
            }
          }
          temp_ab_namen_final<-unique(temp_ab_namen_final)
          temp_ab_namen_final<-temp_ab_namen_final[order(temp_ab_namen_final)]
          input_filter<-input_filter[,c(grep("_",names(input_filter),fixed=T),which(names(input_filter)%in%temp_ab_namen_final))]
        }
        if(input$trend2.3!="All"){
          input_filter<-input_filter[,c(grep("_",names(input_filter),fixed=T),which(names(input_filter)%in%input$trend2.3))]
          temp_ab_namen_final<-input$trend2.3
        }
        
        output$text_trend2<-renderText({NULL})
        
        #########################################
        ##Statistik
        ########################################
        
        table_anti_bak_all<-list()
        eintrag_laenge_ab<-c()
        for(ab in 1:length(temp_ab_namen_final)){
          progress$inc(1/(length(temp_ab_namen_final)))
          table_anti_bak<-data.frame(Antibiotikum=rep(temp_ab_namen_final[ab],length(unique(all_years))),N=NA,S=NA,I=NA,R=NA,KI_R=NA,Year=NA)
          
          count_years<-1
          for(is_year in unique(all_years)){
            table_anti_bak[count_years,3]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="S")
            table_anti_bak[count_years,4]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="I")
            table_anti_bak[count_years,5]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="R")
            table_anti_bak[count_years,7]<-is_year
            count_years<-count_years+1
          }
          table_anti_bak[,2]<-rowSums(table_anti_bak[,3:5])
          for(j in 1:length(table_anti_bak[,1])){
            if(table_anti_bak[j,2]>0){
              stat_help1<-format(round(100*binom.test(x=table_anti_bak[j,5],n=table_anti_bak[j,2],
                                                      p=table_anti_bak[j,5]/table_anti_bak[j,2])$conf.int[1],1),nsmall=1)
              stat_help2<-format(round(100*binom.test(x=table_anti_bak[j,5],n=table_anti_bak[j,2],
                                                      p=table_anti_bak[j,5]/table_anti_bak[j,2])$conf.int[2],1),nsmall=1)
              table_anti_bak[j,6]<-paste0(stat_help1," - ",stat_help2)          
            }
          }
          
          table_anti_bak[,3]<-format(round(100*table_anti_bak[,3]/table_anti_bak[,2],1),nsmall=1)
          table_anti_bak[,4]<-format(round(100*table_anti_bak[,4]/table_anti_bak[,2],1),nsmall=1)
          table_anti_bak[,5]<-format(round(100*table_anti_bak[,5]/table_anti_bak[,2],1),nsmall=1)
          
          table_anti_bak<-table_anti_bak[!is.na(table_anti_bak[,6]),]
          names(table_anti_bak)<-c("Antimicrobial","N","S %","I %","R %","95% KI R","Year")
          
          table_anti_bak<-table_anti_bak[table_anti_bak$N>=30,]
          eintrag_laenge_ab[ab]<-length(table_anti_bak[,1])
          table_anti_bak_all[[ab]]<-table_anti_bak
        }
        
        output$plots_trend <- renderUI({
          plot_trend_output_list <- lapply(1:length(temp_ab_namen_final), function(k) {
            plotname_trend <- paste("plot_trend", k, sep="")
            plotOutput(plotname_trend, height = 400, width = 1000)
          })
          do.call(tagList, plot_trend_output_list)
        })
        
        for(n in 1:length(temp_ab_namen_final)){
          local({
            my_i <- n
            plotname_trend <- paste("plot_trend", my_i, sep="")
            output[[plotname_trend]] <- renderPlot({
              help_bar<-table_anti_bak_all[[which(temp_ab_namen_final[my_i]==temp_ab_namen_final)]]
              
              par(mar=c(4,14,6,1))
              plot(NULL,xlab="",ylab="",xaxt="n",yaxt="n",main="",bty="n",
                   xlim=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),ylim=c(0,100),cex.lab=2,xaxs="i")
              axis(1,at=seq(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),
                   labels=c(NA,seq(min(as.numeric(all_years)),max(as.numeric(all_years))),NA),cex.axis=1.5,tick = F)
              axis(2,at=seq(0,100,20),las=2,cex.axis=1.5)
              for(n in seq(0,100,10)){
                points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(n,n),col="grey90",lwd=1,type="l",lty=5)
              }
              points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(0,0),col="grey90",lwd=3,type="l",lty=1)
              points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(100,100),col="grey90",lwd=3,type="l",lty=1)
              for(m in 1:length(help_bar[,1])){
                start<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][1])
                end<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][2])
                points(x=c(as.numeric(help_bar$Year[m]),as.numeric(help_bar$Year[m])),y=c(start,end),type="l",lwd=2)
                points(x=c(as.numeric(help_bar$Year[m])-0.02,as.numeric(help_bar$Year[m])+0.02),y=c(start,start),type="l",lwd=3)
                points(x=c(as.numeric(help_bar$Year[m])-0.02,as.numeric(help_bar$Year[m])+0.02),y=c(end,end),type="l",lwd=3)
              }
              points(x=help_bar$Year,y=help_bar$`R %`,cex=3,type="l",lwd=5)
              points(x=help_bar$Year,y=help_bar$`R %`,cex=2,pch=21,type="p",lwd=3,bg="darkgoldenrod2",col="darkgoldenrod4")
              
              title("R %    95% KI R",line = 0.3)
              mtext(help_bar$Antimicrobial[1],adj=-0.17,cex=1.5,col=rgb(49/255,126/255,172/255),line=2)
            })
          })
        }
        
        shinyjs::html("text", paste0("<br><br>","Trend analysis successfully conducted.","<br>"), add = TRUE)  
        shinyjs::html("text", paste0("<br>","Note:"),add=TRUE)
        shinyjs::html("text",paste0("<br>","For each antimicrobial, only those years are reported for which at least 30 observations are available."), add = TRUE)  
        output$hinweis_tabellen_trend<-renderText({"For each antimicrobial, only those years are reported for which at least 30 observations are available."})
        
        progress$close()
        return()
      }
    })
    
    
    output$do_trend_xlsx <- downloadHandler(
      filename = function() {
        paste0("GEFAAR_Trend_Analysis_Specimen_",input$trend2.2,"_Species_",input$trend2,"_Clinic_",input$trend3,"_Antimicrobial_",input$trend2.3,".xlsx")
        #"Test.xlsx"
      },
      content = function(file) {
        progress <- shiny::Progress$new()
        progress$set(message = "Preparing data for xlsx report", value = 0)
        
        helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
        years<-unique(format(helper1,"%Y"))
        
        shinyjs::html("text", paste0("<br>Analysis is launched.<br><br>"), add = FALSE)
        
        #########################
        ###Eigentliche Analyse###
        #########################
        
        if(input$trend2=="Insufficient number of cases"){
          shinyjs::html("text", paste0("<br>Insufficient number of cases for an analysis with the selected configuration (min. 30 cases).<br><br>"), add = TRUE) 
          output$text_trend2<-renderText({"Insufficient number of cases for an analysis with the selected configuration (min. 30 cases)"})
          
          progress$close()
          return()
        }
        
        if(input$trend2!="Insufficient number of cases"){
          if(input$column4b=="dd.mm.yy"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
          }
          if(input$column4b=="dd.mm.yyyy"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
          }
          if(input$column4b=="mm/dd/yy"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
          }
          if(input$column4b=="mm/dd/yyyy"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
          }
          if(input$column4b=="yy-mm-dd"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
          }
          if(input$column4b=="yyyy-mm-dd"){
            helper1<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
          }
          
          input_filter<-input2
          if(input$trend2.2=="All"&&input$trend3=="All"){##alle fachbereiche, alle materialien
            input_filter<-input_filter[input2$RES_ERREGER==input$trend2,]
            helper_neu<-helper1[input2$RES_ERREGER==input$trend2]
          }
          if(input$trend2.2!="All"&&input$trend3=="All"){##alle fachbereiche
            input_filter<-input_filter[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
            helper_neu<-helper1[input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
          }
          if(input$trend2.2=="All"&&input$trend3!="All"){##alle materialien
            input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2,]
            helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$RES_ERREGER==input$trend2]
          }
          if(input$trend2.2!="All"&&input$trend3!="All"){
            input_filter<-input_filter[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2,]
            helper_neu<-helper1[input2$ORD_FACHBEREICH==input$trend3&input2$ORD_MATERIAL==input$trend2.2&input2$RES_ERREGER==input$trend2]
          }
          all_years<-format(helper_neu,"%Y")
          
          if(input$trend2.3=="All"){##Alle möglichen AB
            temp_ab<-apply(input_filter[,grep("_",names(input_filter),fixed=T,invert = T)],2,function(x){sum(x!="-")})
            temp_ab<-temp_ab[temp_ab>0]
            temp_ab_namen<-names(temp_ab)
            temp_ab_namen_final<-c()
            
            for(ab_relevant in temp_ab_namen){
              for(year_is in unique(all_years)){
                temp_ab_test<-input_filter[all_years==year_is,names(input_filter)==ab_relevant]
                if(sum(temp_ab_test!="-")>30){
                  temp_ab_namen_final<-c(temp_ab_namen_final,ab_relevant)
                }
              }
            }
            temp_ab_namen_final<-unique(temp_ab_namen_final)
            temp_ab_namen_final<-temp_ab_namen_final[order(temp_ab_namen_final)]
            input_filter<-input_filter[,c(grep("_",names(input_filter),fixed=T),which(names(input_filter)%in%temp_ab_namen_final))]
          }
          if(input$trend2.3!="All"){
            input_filter<-input_filter[,c(grep("_",names(input_filter),fixed=T),which(names(input_filter)%in%input$trend2.3))]
            temp_ab_namen_final<-input$trend2.3
          }
          
          #########################################
          ##Statistik
          ########################################
          
          table_anti_bak_all<-list()
          eintrag_laenge_ab<-c()
          for(ab in 1:length(temp_ab_namen_final)){
            progress$inc(1/(length(temp_ab_namen_final)))
            table_anti_bak<-data.frame(Antibiotikum=rep(temp_ab_namen_final[ab],length(unique(all_years))),N=NA,S=NA,I=NA,R=NA,KI_R=NA,Year=NA)
            
            count_years<-1
            for(is_year in unique(all_years)){
              table_anti_bak[count_years,3]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="S")
              table_anti_bak[count_years,4]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="I")
              table_anti_bak[count_years,5]<-sum(input_filter[is_year==all_years,names(input_filter)==temp_ab_namen_final[ab]]=="R")
              table_anti_bak[count_years,7]<-is_year
              count_years<-count_years+1
            }
            table_anti_bak[,2]<-rowSums(table_anti_bak[,3:5])
            for(j in 1:length(table_anti_bak[,1])){
              if(table_anti_bak[j,2]>0){
                stat_help1<-format(round(100*binom.test(x=table_anti_bak[j,5],n=table_anti_bak[j,2],
                                                        p=table_anti_bak[j,5]/table_anti_bak[j,2])$conf.int[1],1),nsmall=1)
                stat_help2<-format(round(100*binom.test(x=table_anti_bak[j,5],n=table_anti_bak[j,2],
                                                        p=table_anti_bak[j,5]/table_anti_bak[j,2])$conf.int[2],1),nsmall=1)
                table_anti_bak[j,6]<-paste0(stat_help1," - ",stat_help2)          
              }
            }
            
            table_anti_bak[,3]<-format(round(100*table_anti_bak[,3]/table_anti_bak[,2],1),nsmall=1)
            table_anti_bak[,4]<-format(round(100*table_anti_bak[,4]/table_anti_bak[,2],1),nsmall=1)
            table_anti_bak[,5]<-format(round(100*table_anti_bak[,5]/table_anti_bak[,2],1),nsmall=1)
            
            table_anti_bak<-table_anti_bak[!is.na(table_anti_bak[,6]),]
            names(table_anti_bak)<-c("Antimicrobial","N","S %","I %","R %","95% KI R","Year")
            
            table_anti_bak<-table_anti_bak[table_anti_bak$N>=30,]
            eintrag_laenge_ab[ab]<-length(table_anti_bak[,1])
            table_anti_bak_all[[ab]]<-table_anti_bak
          }
          
          progress$close()
          
          progress <- shiny::Progress$new()
          progress$set(message = "Writing data to xlsx report", value = 0)
          
          wb<-createWorkbook()
          addWorksheet(wb,sheetName = "Trend")
          
          all_white<-createStyle(fgFill="white")
          bold<-createStyle(textDecoration = "bold")
          size<-createStyle(fontSize = 10)
          size_big<-createStyle(fontSize = 14)
          linie<-createStyle(border = "bottom",borderColour = "grey85")
          #linie2<-createStyle(border = "bottom",borderStyle = "medium")
          #linie3<-createStyle(border = "bottom",borderStyle = "medium",borderColour = "grey85")
          #right<-createStyle(halign = "right")
          #hintergrund_grau<-createStyle(fgFill="grey90")
          ueberschrift<-createStyle(fontColour = "white",fgFill = rgb(42/255,77/255,125/255),
                                    fontSize = 14,textDecoration = "bold",valign = "center")
          
          addStyle(wb,sheet="Trend",rows=c(1:5000),cols=1:27,
                   gridExpand = T,style=all_white)
          
          insertImage(wb,sheet="Trend",file = "www/IMI.png",
                      height = 0.516, width = 0.648,
                      startRow = 1,startCol = 7)
          insertImage(wb,sheet="Trend",file = "www/UKM.png",
                      height = 0.516, width = 0.6,
                      startRow = 1,startCol = 1)
          setRowHeights(wb,sheet="Trend",rows = 1,heights = 43)
          
          
          table_export<-data.frame(V1=NA,V2=NA,V3=NA,V4=NA,V5=NA,V6=NA,V7=NA)
          table_export[2,1]<-"Trend analysis"
          table_export[3,1]<-"Specimen"
          table_export[3,2]<-input$trend2.2
          table_export[4,1]<-"Clinic"
          table_export[4,2]<-input$trend3
          table_export[5,1]<-"Species"
          table_export[5,2]<-input$trend2 
          table_export[6,1]<-"Antimicrobial"
          table_export[6,2]<-input$trend2.3
          
          writeData(wb,sheet="Trend",table_export,colNames = F,rowNames = F)
          
          addStyle(wb,sheet="Trend",rows=c(1:3600),cols=1:27,gridExpand = T,style=all_white)
          addStyle(wb,sheet="Trend",rows=2,cols=1:7,style = ueberschrift,stack=T)
          addStyle(wb,sheet="Trend",rows=c(3:6),cols=1,style = bold,stack=T)
          addStyle(wb,sheet="Trend",rows=c(6),cols=1:7,style = linie,
                   gridExpand = T, stack=T)
          
          setColWidths(wb,sheet="Trend",cols = c(1:5),widths = "auto")
          setRowHeights(wb,sheet="Trend",rows = c(2,7),heights = 30)
          
          
          ##############################################
          begin<-8
          export_abbildungen<-data.frame(V1=NA)
          add_height<-0
          for(ab in (1:length(temp_ab_namen_final))){
            export_abbildungen[add_height+1,1]<-temp_ab_namen_final[ab]
            add_height<-add_height+(40/7.8)*(7.8/1000*400)
          }
          
          writeData(wb,sheet="Trend",export_abbildungen,colNames = F,rowNames = F,startRow = begin)
          addStyle(wb,sheet="Trend",cols=c(1),rows=begin:(length(export_abbildungen[,1])+begin),style=bold,stack=T)
          addStyle(wb,sheet="Trend",cols=c(1),rows=begin:(length(export_abbildungen[,1])+begin),style=size_big,stack=T)
          
          
          
          add_height<-0
          for(ab in 1:length(temp_ab_namen_final)){
            progress$inc(1/(length(temp_ab_namen_final)),detail=paste0(ab,"/",length(temp_ab_namen_final)))
            
            help_bar<-table_anti_bak_all[[which(temp_ab_namen_final[ab]==temp_ab_namen_final)]]
            
            png(paste0(tempdir(), "/", "plot",ab,".png"), width=1000, height=400,units = "px")
            par(mar=c(5,5,2,1))
            plot(NULL,xlab="",ylab="",xaxt="n",yaxt="n",main="",bty="n",
                 xlim=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),ylim=c(0,100),cex.lab=2,xaxs="i")
            axis(1,at=seq(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),
                 labels=c(NA,seq(min(as.numeric(all_years)),max(as.numeric(all_years))),NA),cex.axis=1.5,tick = F)
            axis(2,at=seq(0,100,20),las=2,cex.axis=1.5)
            for(n in seq(0,100,10)){
              points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(n,n),col="grey90",lwd=1,type="l",lty=5)
            }
            points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(0,0),col="grey90",lwd=3,type="l",lty=1)
            points(x=c(min(as.numeric(all_years))-1,max(as.numeric(all_years))+1),y=c(100,100),col="grey90",lwd=3,type="l",lty=1)
            for(m in 1:length(help_bar[,1])){
              start<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][1])
              end<-as.numeric(strsplit(help_bar[m,6],split = " - ")[[1]][2])
              points(x=c(as.numeric(help_bar$Year[m]),as.numeric(help_bar$Year[m])),y=c(start,end),type="l",lwd=2)
              points(x=c(as.numeric(help_bar$Year[m])-0.02,as.numeric(help_bar$Year[m])+0.02),y=c(start,start),type="l",lwd=3)
              points(x=c(as.numeric(help_bar$Year[m])-0.02,as.numeric(help_bar$Year[m])+0.02),y=c(end,end),type="l",lwd=3)
            }
            points(x=help_bar$Year,y=help_bar$`R %`,cex=3,type="l",lwd=5)
            points(x=help_bar$Year,y=help_bar$`R %`,cex=2,pch=21,type="p",lwd=3,bg="darkgoldenrod2",col="darkgoldenrod4")
            
            title("R %    95% KI R",line = 0.3,cex=1.5)
            dev.off()
            
            insertImage(wb, sheet="Trend", paste0(tempdir(), "/", "plot",ab,".png"), width = 8,height = 7.8/1000*400,
                        startRow = begin+1+add_height,startCol = 1)
            
            add_height<-add_height+(40/7.8)*(7.8/1000*400)
          }
          
          saveWorkbook(wb,file = file,overwrite=T)
          
          progress$close()
          shinyjs::html("text", paste0("<br><br>","xlsx report successfully generated.","<br>"), add = TRUE) 
        }
      } 
      
    )

    
    observeEvent(input$do_analyse,{
      shinyjs::html("text", paste0("<br>Cluster analyses are launched.<br><br>"), add = FALSE)
      if(input$analysis_type1=="per species"){
        if(input$bak_select=="No" && is.null(input$bak_selected)){
          shinyjs::html("text", paste0("ERROR: Plese select at least one species.","<br>"), add = TRUE)    
          return()
        }
        if(is.null(input$cluster_type_bak_heat1) && is.null(input$cluster_type_bak_umap1)){
          shinyjs::html("text", paste0("ERROR: Please select at least one cluster analysis.","<br>"), add = TRUE)  
          return()
        }
      }
      if(input$analysis_type1=="per clinic"){
        if(input$clinic_select=="No" && is.null(input$clinic_selected)){
          shinyjs::html("text", paste0("ERROR: Please select at least one clinic.","<br>"), add = TRUE)    
          return()
        }
        if(is.null(input$cluster_type_klinik_heat1)){
          shinyjs::html("text", paste0("ERROR: Please select at least one cluster analysis.","<br>"), add = TRUE)  
          return()
        }
      }
      
      #########################
      ###Eigentliche Analyse###
      #########################

      #helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        helper1cluster<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      if(input$analysis_type1b=="All"){
        input3<-input2[format(helper1cluster,"%Y")==input$analysis_type1a,]
      }else{
        input3<-input2[format(helper1cluster,"%Y")==input$analysis_type1a&input2$ORD_MATERIAL==input$analysis_type1b,]
      }
      erstes_ab<-length(grep("_",names(input3)))+1

      
      ###########
      ###Plots###
      ###########
      
      ##Pro Spezies
      
      bakterien<-unique(input3$RES_ERREGER)
      bakterien<-bakterien[order(bakterien)]
      bakterien<-bakterien[as.vector(table(input3$RES_ERREGER))>=30]
      
      if(input$analysis_type1=='per species'){
        if(input$bak_select=="No"){
          bakterien<-input$bak_selected
        }
        ##Clustering: Heatmap, Supervised, 1) Klinik/Fachbereich, 2) Resistenz
        if(length(grep("Data ordered by 1) clinic, 2) resistance",input$cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by 1) clinic, 2) resistance'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          
          output$plots1 <- renderUI({
            plot_output_list <- lapply(1:length(bakterien), function(k) {
              plotname <- paste("plot", k, sep="")
              plotOutput(plotname, height = 400, width = 1250)
            })
            do.call(tagList, plot_output_list)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by 1) clinic, 2) resistance", value = 0)
          for(i in 1:length(bakterien)){
            local({

              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              
              temp3_2<-as.data.frame(temp3[,missing>0.03])
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              fachbereiche<-as.numeric(table(temp2$ORD_FACHBEREICH))
              fachbereiche2<-c()
              for(j in 1:length(fachbereiche)){
                fachbereiche2[j]<-sum(fachbereiche[1:j])
              }
              
              output$text_analyse1<-renderText({NULL})
              
              
              
              my_i <- i
              plotname <- paste("plot", my_i, sep="")
              
              output[[plotname]] <- renderPlot({
                einmalig<-unique(as.vector(as.matrix(temp3_2)))
                einmalig<-einmalig[!is.na(einmalig)]
                einmalig<-einmalig*2-1
                
                pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                         main=bakterien[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                         cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                         clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))), fontsize_col = 5,gaps_col = fachbereiche2)
                
              })
              progress$inc(1/length(bakterien))
            })
          }
          
          progress$close()
        }
        
        ##Clustering: Heatmap, Supervised, 1) Klinik/Fachbereich, 2) Datum
        if(length(grep("Data ordered by 1) clinic, 2) date",input$cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by 1) clinic, 2) date'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          output$plots2 <- renderUI({
            plot_output_list2 <- lapply(1:length(bakterien), function(k) {
              plotname2 <- paste("plot2", k, sep="")
              plotOutput(plotname2, height = 400, width = 1250)
            })
            do.call(tagList, plot_output_list2)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by 1) clinic, 2) date", value = 0)
          for(i in 1:length(bakterien)){
            local({
              
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              #temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%Y")
              if(input$column4b=="dd.mm.yy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%y")
              }
              if(input$column4b=="dd.mm.yyyy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%Y")
              }
              if(input$column4b=="mm/dd/yy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%m/%d/%y")
              }
              if(input$column4b=="mm/dd/yyyy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%m/%d/%Y")
              }
              if(input$column4b=="yy-mm-dd"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%y-%m-%d")
              }
              if(input$column4b=="yyyy-mm-dd"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%Y-%m-%d")
              }
              
              temp2<-temp2[order(temp2$ORD_FACHBEREICH,temp2$ORD_DATUM),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              
              temp3_2<-as.data.frame(temp3[,missing>0.03])
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                #Fachbereich = rainbow(n=length(levels(as.factor(temp2$ORD_FACHBEREICH)))),
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              fachbereiche<-as.numeric(table(temp2$ORD_FACHBEREICH))
              fachbereiche2<-c()
              for(j in 1:length(fachbereiche)){
                fachbereiche2[j]<-sum(fachbereiche[1:j])
              }
              
              output$text_analyse2<-renderText({NULL})
              
              my_i <- i
              plotname2 <- paste("plot2", my_i, sep="")
              
              output[[plotname2]] <- renderPlot({
                einmalig<-unique(as.vector(as.matrix(temp3_2)))
                einmalig<-einmalig[!is.na(einmalig)]
                einmalig<-einmalig*2-1
                
                pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                         main=bakterien[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                         cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                         clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))),
                         fontsize_col = 5,gaps_col = fachbereiche2)
                
              })
              progress$inc(1/length(bakterien))
            })
          }
          
          progress$close()
        }
        
        ##Clustering: Heatmap, Unsupervised
        if(length(grep("Hierarchical clustering",input$cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Hierarchical clustering'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          output$plots3 <- renderUI({
            plot_output_list3 <- lapply(1:length(bakterien), function(k) {
              plotname3 <- paste("plot3", k, sep="")
              plotOutput(plotname3, height = 400, width = 1250)
            })
            do.call(tagList, plot_output_list3)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Hierarchical clustering", value = 0)
          for(i in 1:length(bakterien)){
            local({
              
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(as.data.frame(temp3[,missing>0.3]))
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_2b<-as.data.frame(temp3_2[missing>0.3,])
              temp3_2<-temp3_2b
              
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                #Clinic = rainbow(n=length(levels(as.factor(temp2$ORD_FACHBEREICH)))),
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))

              output$text_analyse3<-renderText({NULL})
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2missing<-as.data.frame(temp3[,missing>0.8])
              
              missing<-rowSums(!is.na(temp3_2missing))/length(temp3_2missing[1,])
              temp3_3missing<-as.data.frame(temp3_2missing[missing==1,])
              row.names(temp3_3missing)<-NULL
              
              if(length(temp3_3missing[,1])<100){
                beste = tryCatch({
                  clusters<-NbClust(temp3_3missing,
                                    min.nc = 1, max.nc = 5, method = "ward.D",index = "duda")
                  clusters$Best.nc[1]
                }, error = function(e) {
                  1
                })
              }else{
                beste = tryCatch({
                  clusters<-NbClust(temp3_3missing,
                                    min.nc = 1, max.nc = 10, method = "ward.D",index = "duda")
                  clusters$Best.nc[1]
                }, error = function(e) {
                  1
                })
              }

              my_i <- i
              plotname3 <- paste("plot3", my_i, sep="")
              
              output[[plotname3]] <- renderPlot({
                einmalig<-unique(as.vector(as.matrix(temp3_2)))
                einmalig<-einmalig[!is.na(einmalig)]
                einmalig<-einmalig*2-1
                
                abc=data.frame(x=1)
                data<-lapply(abc,function(x){
                  tryCatch({
                    pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                                                     main=bakterien[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                                                     cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                                                     clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))),
                                                     fontsize_col = 5,cutree_cols = beste)
                    
                  },error=function(e) NULL)
                })
                if(!is.null(data[[1]])&&beste!=1){
                  pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=bakterien[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))),
                           fontsize_col = 5,cutree_cols = beste)
                }else{
                  plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                       yaxt="n",bty="n",main=paste0(bakterien[my_i]," - no clustering possible"))
                }
              })
            })
            progress$inc(1/length(bakterien))
          }
          progress$close()
        }
        
        ##Clustering: UMAP mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Plot with colored clinics",input$cluster_type_bak_umap1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate UMAP 'Plot with colored clinics'","<br>"), add = TRUE)  

          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          output$plots4 <- renderUI({
            plot_output_list4 <- lapply(1:length(bakterien), function(k) {
              plotname4 <- paste("plot4", k, sep="")
              plotOutput(plotname4, height = 400, width = 1250)
            })
            do.call(tagList, plot_output_list4)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate UMAP 'Plot with colored clinics'", value = 0)
          for(i in 1:length(bakterien)){
            local({
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
              colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
              rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))

              helper<-c()
              for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2_3$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
              output$text_analyse4<-renderText({NULL})              
              output$text_analyse4b<-renderText({"Note: In case all species are analyzed at once, it may take up to 2 minutes until the updated plots are displayed."})
              
              
              my_i <- i
              plotname4 <- paste("plot4", my_i, sep="")
              
              output[[plotname4]] <- renderPlot({
                abc=data.frame(x=1)
                data<-lapply(abc,function(x){
                  tryCatch(M3C::umap(t(as.matrix(temp3_3)),labels=names(helper),colvec = helper,seed = 42) + ggtitle(bakterien[my_i]) + 
                             theme(plot.title = element_text(size = 20,hjust = 0.5)),error=function(e) NULL)
                })
                if(!is.null(data[[1]])){
                  M3C::umap(t(as.matrix(temp3_3)),legendtextsize = min(7,floor(210/length(unique(names(helper))))), 
                            labels=names(helper),colvec = helper,seed = 42) + ggtitle(bakterien[my_i]) + 
                    theme(plot.title = element_text(size = 20,hjust = 0.5))
                }else{
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                }
                
              })
              }else{
                my_i <- i
                plotname4 <- paste("plot4", my_i, sep="")
                
                output[[plotname4]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
            })
            progress$inc(1/length(bakterien))
          }
          progress$close()
        }
        
        ##Clustering: Clustering: UMAP mit eingefärbten Clustern
        if(length(grep("Plot with colored clusters",input$cluster_type_bak_umap1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate UMAP 'Plot with colored clusters'","<br>"), add = TRUE)  

          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Preprocessing:"), add = TRUE)  
          
          weite<-c()
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate UMAP 'Plot with colored clusters' - Preprocessing", value = 0)
          moeglich_speicher<-c()
          partition_speicher<-list()
          
          for(i in 1:length(bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]),add = TRUE) 

              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              shinyjs::html("text", paste0(" (",length(temp[,1]),"x",length(temp[1,]),"=",length(temp[,1])*length(temp[1,])," elements)"), add = TRUE)

              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
              colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
              rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
              
              helper<-c()
              for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2_3$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
              
              abc=data.frame(x=1)
              data<-lapply(abc,function(x){
                tryCatch({
                  pdf(file = NULL)
                  alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                  test<-data.frame(alt$data)
                  
                  #clusters1<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="duda")
                  #clusters2<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="silhouette")
                  #clusters3<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="pseudot2")
                  #clusters<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans")
                  clusters2<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="silhouette")
                  clusters1<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="kl")
                  clusters3<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="ch")
                  clusters4<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="scott")
                  clusters5<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="duda")
                  clusters6<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="dunn")
                  
                  dev.off()
                },error=function(e) NULL)
              })
              if(!is.null(data[[1]])){
                alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                test<-data.frame(alt$data)
                pdf(file = NULL)
                clusters2<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="silhouette")
                clusters1<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="kl")
                clusters3<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="ch")
                clusters4<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="scott")
                clusters5<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="duda")
                clusters6<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="dunn")
                dev.off()
                
                beste<-table(c(clusters1$Best.nc[1],clusters4$Best.nc[1],clusters3$Best.nc[1],clusters5$Best.nc[1],clusters6$Best.nc[1]))
                
                message("Clusters1: ",clusters1)
                message("Clusters2: ",clusters2)
                message("Clusters3: ",clusters3)
                message("Clusters4: ",clusters4)
                message("Clusters5: ",clusters5)
                message("Clusters6: ",clusters6)
                
                if(beste[max(beste)==beste]>=2&&
                   sd(clusters1$All.index)>=5&&sd(clusters2$All.index)>=0.05){
                  moeglich<-as.numeric(min(names(beste[max(beste)==beste])))
                  if(moeglich==clusters1$Best.nc[1]){
                    best_cluster<-clusters1
                  }else{
                    if(moeglich==clusters3$Best.nc[1]){
                      best_cluster<-clusters3
                    }else{
                      if(moeglich==clusters4$Best.nc[1]){
                        best_cluster<-clusters4
                      }else{
                        if(moeglich==clusters5$Best.nc[1]){
                          best_cluster<-clusters5
                        }
                      }
                    }
                    
                  }
                }else{
                  moeglich<-0
                }

                if(moeglich>0){
                  weite<-c(weite,650)                
                  moeglich_speicher[i]<-moeglich
                  partition_speicher[[i]]<-best_cluster$Best.partition
                }else{
                  weite<-c(weite,1250)                
                  moeglich_speicher[i]<-0
                  partition_speicher[[i]]<-NA
                }
              }else{
                dev.off()
                weite<-c(weite,1250)                
                moeglich_speicher[i]<-NA
                partition_speicher[[i]]<-NA
              }
              }else{
                weite<-c(weite,1250)                
                moeglich_speicher[i]<-NA
                partition_speicher[[i]]<-NA
              }
              progress$inc(1/length(bakterien))
          }
          progress$close()

          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Generate plots:"), add = TRUE)  
          
          output$plots5 <- renderUI({
            plot_output_list5 <- lapply(1:length(bakterien), function(k) {
              plotname5 <- paste("plot5", k, sep="")
              plotOutput(plotname5, height = 400, width = weite[k])
            })
            do.call(tagList, plot_output_list5)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate UMAP 'Plot with colored clusters - generate plots", value = 0)

          for(i in 1:length(bakterien)){
            local({
              #start.time <- Sys.time()
              #message(bakterien[i]," Dimensionen: ",length(temp[,1]),"x",length(temp[1,])," ist ",length(temp[,1])*length(temp[1,]))
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              shinyjs::html("text", paste0(" (",length(temp[,1]),"x",length(temp[1,]),"=",length(temp[,1])*length(temp[1,])," elements)"), add = TRUE) 
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
              colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
              rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
              
              helper<-c()
              for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2_3$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
              if(!is.na(moeglich_speicher[i])){
                alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                test<-data.frame(alt$data)

                moeglich<-moeglich_speicher[i]
                partition<-partition_speicher[[i]]
                
                output$text_analyse5<-renderText({NULL})
                output$text_analyse5b<-renderText({"Note: In case all species are analyzed at once, it may take up to 2 minutes until the updated plots are displayed. werden."})
                
                my_i <- i
                plotname5 <- paste("plot5", my_i, sep="")

                output[[plotname5]] <- renderPlot({
                  if(moeglich>0){
                    #if(moegliche[my_i]>0){
                    #k_clust<-kmeans(test,centers =moeglich[1])
                    
                    M3C::umap(t(as.matrix(temp3_3)),labels=as.factor(partition),
                              legendtextsize = min(7,floor(210/length(unique(names(helper))))), 
                              seed=42,colvec = col_vec[1:length(levels(as.factor(partition)))],
                              controlscale = T,scale=3) + 
                      ggtitle(bakterien[my_i]) + 
                      theme(plot.title = element_text(size = 20,hjust = 0.5))
                    
                  }else{
                    M3C::umap(t(as.matrix(temp3_3)),labels=names(helper),colvec = helper,seed = 42,
                              legendtextsize = min(7,floor(210/length(unique(names(helper)))))) + 
                      ggtitle(paste0(bakterien[my_i]," - no unique clustering possible")) + 
                      theme(plot.title = element_text(size = 20,hjust = 0.5))
                  }
                })
              }else{
                #dev.off()
                output$text_analyse5<-renderText({NULL})
                output$text_analyse5b<-renderText({"Note: In case all species are analyzed at once, it may take up to 2 minutes until the updated plots are displayed."})
                
                my_i <- i
                plotname5 <- paste("plot5", my_i, sep="")
                
                output[[plotname5]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
              }else{
                output$text_analyse5<-renderText({NULL})
                output$text_analyse5b<-renderText({"Note: At the first call, it may take up to 60 seconds until the plots are displayed."})
                
                my_i <- i
                plotname5 <- paste("plot5", my_i, sep="")
                
                output[[plotname5]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
              
            })
            #end.time <- Sys.time()
            #message("Zeit final: ",end.time-start.time)
            progress$inc(1/length(bakterien))
          }
          progress$close()
        }
        
        ##Clustering: Zusätzliche Heatmap supervised nach UMAP-Clustern mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Data ordered by UMAP clusters",input$cluster_type_bak_umap2,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate additional heatmap 'Data ordered by UMAP clusters'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")

          output$plots6 <- renderUI({
            plot_output_list6 <- lapply(1:length(bakterien), function(k) {
              plotname6 <- paste("plot6", k, sep="")
              plotOutput(plotname6, height = 410, width = 1250)
            })
            do.call(tagList, plot_output_list6)
          })
          

          progress <- shiny::Progress$new()
          progress$set(message = "Generate additional heatmap 'Data ordered by UMAP clusters'", value = 0)
          for(i in 1:length(bakterien)){
            local({
              
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
              colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
              rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
              
              helper<-c()
              for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2_3$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
              if(!is.na(moeglich_speicher[i])){
                alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                test<-data.frame(alt$data)

                  moeglich<-moeglich_speicher[i]
                  partition<-partition_speicher[[i]]
                output$text_analyse6<-renderText({NULL})
                
                my_i <- i
                plotname6 <- paste("plot6", my_i, sep="")
                
                output[[plotname6]] <- renderPlot({
                  if(moeglich>0){
                    #k_clust<-kmeans(test,centers =moeglich[1])
                    
                    soll_order<-partition[order(partition)]
                    
                    temp3_3_ordered<-temp3_3[match(names(soll_order),row.names(temp3_3)),]
                    my_annot_ordered<-as.data.frame(my_annot[match(names(soll_order),row.names(temp3_3)),])
                    colnames(my_annot_ordered)<-c("Clinic")
                    
                    fachbereiche<-as.numeric(table(soll_order))
                    fachbereiche2<-c()
                    for(m in 1:length(fachbereiche)){
                      fachbereiche2[m]<-sum(fachbereiche[1:m])
                    }
                    
                    my_annot_ordered<-cbind(my_annot_ordered,Cluster=as.character(soll_order))
                    rownames(my_annot_ordered)<-rownames(temp3_3_ordered)
                    #colnames(my_annot_ordered)<-c("Clinic","Cluster")
                    
                    ann_colors = list(
                      Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))],
                      Cluster = col_vec[1:length(levels(as.factor(my_annot_ordered$Cluster)))]
                    )
                    names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                    names(ann_colors$Cluster)<-levels(as.factor(my_annot_ordered$Cluster))
                    
                    einmalig<-unique(as.vector(as.matrix(temp3_3_ordered)))
                    einmalig<-einmalig[!is.na(einmalig)]
                    einmalig<-einmalig*2-1
                    
                    pheatmap(t(as.matrix(temp3_3_ordered)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                             main=bakterien[my_i],annotation_col =my_annot_ordered,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                             cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                             clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))),
                             fontsize_col = 5,gaps_col = fachbereiche2)

                  }else{
                    fachbereiche<-as.numeric(table(temp2_3$ORD_FACHBEREICH))
                    fachbereiche2<-c()
                    for(j in 1:length(fachbereiche)){
                      fachbereiche2[j]<-sum(fachbereiche[1:j])
                    }
                    
                    einmalig<-unique(as.vector(as.matrix(temp3_3)))
                    einmalig<-einmalig[!is.na(einmalig)]
                    einmalig<-einmalig*2-1
                    
                    pheatmap(t(as.matrix(temp3_3)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                             main=paste0(bakterien[my_i]," - no unique clustering possible"),annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                             cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                             clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))), 
                             fontsize_col = 5,gaps_col = fachbereiche2)
                  }
                })
              }else{
                #dev.off()
                output$text_analyse6<-renderText({NULL})
                
                my_i <- i
                plotname6 <- paste("plot6", my_i, sep="")
                
                output[[plotname6]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
              }else{
                output$text_analyse6<-renderText({NULL})
                
                my_i <- i
                plotname6 <- paste("plot6", my_i, sep="")
                
                output[[plotname6]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
              
              
            })
            progress$inc(1/length(bakterien))
          }
        progress$close()
        }

        ##Clustering: Zusätzliche Heatmap supervised nach Kliniken/Fachbereichen mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Data ordered by clinics",input$cluster_type_bak_umap2,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate additional heatmap 'Data ordered by clinics'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          output$plots6b <- renderUI({
            plot_output_list6b <- lapply(1:length(bakterien), function(k) {
              plotname6b <- paste("plot6b", k, sep="")
              plotOutput(plotname6b, height = 410, width = 1250)
            })
            do.call(tagList, plot_output_list6b)
          })
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate additional heatmap 'Data ordered by clinics'", value = 0)
          for(i in 1:length(bakterien)){
            local({
              
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
              colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
              rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
              
              helper<-c()
              for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2_3$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
              if(!is.na(moeglich_speicher[i])){
                alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                test<-data.frame(alt$data)

                 moeglich<-moeglich_speicher[i]
                 partition<-partition_speicher[[i]]
                output$text_analyse6b<-renderText({NULL})
                
                my_i <- i
                plotname6b <- paste("plot6b", my_i, sep="")
                
                output[[plotname6b]] <- renderPlot({
                  if(moeglich>0){
                    #k_clust<-kmeans(test,centers =moeglich[1])
                    
                    soll_order<-partition[order(my_annot$Clinic,partition)]
                    
                    temp3_3_ordered<-temp3_3[match(names(soll_order),row.names(temp3_3)),]
                    my_annot_ordered<-as.data.frame(my_annot[match(names(soll_order),row.names(temp3_3)),])
                    colnames(my_annot_ordered)<-c("Clinic")
                    
                    fachbereiche<-as.numeric(table(my_annot_ordered$Clinic))
                    fachbereiche2<-c()
                    for(m in 1:length(fachbereiche)){
                      fachbereiche2[m]<-sum(fachbereiche[1:m])
                    }
                    
                    my_annot_ordered<-cbind(my_annot_ordered,Cluster=as.character(soll_order))
                    rownames(my_annot_ordered)<-rownames(temp3_3_ordered)
                    
                    ann_colors = list(
                      Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))],
                      Cluster = col_vec[1:length(levels(as.factor(my_annot_ordered$Cluster)))]
                    )
                    names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                    names(ann_colors$Cluster)<-levels(as.factor(my_annot_ordered$Cluster))
                    
                    einmalig<-unique(as.vector(as.matrix(temp3_3_ordered)))
                    einmalig<-einmalig[!is.na(einmalig)]
                    einmalig<-einmalig*2-1

                    pheatmap(t(as.matrix(temp3_3_ordered)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                             main=bakterien[my_i],annotation_col =my_annot_ordered,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                             cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                             clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))),
                             fontsize_col = 5,gaps_col = fachbereiche2)
                    
                  }else{
                    fachbereiche<-as.numeric(table(temp2_3$ORD_FACHBEREICH))
                    fachbereiche2<-c()
                    for(j in 1:length(fachbereiche)){
                      fachbereiche2[j]<-sum(fachbereiche[1:j])
                    }
                    
                    einmalig<-unique(as.vector(as.matrix(temp3_3)))
                    einmalig<-einmalig[!is.na(einmalig)]
                    einmalig<-einmalig*2-1

                    pheatmap(t(as.matrix(temp3_3)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                             main=paste0(bakterien[my_i]," - no unique clustering possible"),annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                             cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                             clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))),
                             fontsize_col = 5,gaps_col = fachbereiche2)
                  }
                })
              }else{
                #dev.off()
                
                output$text_analyse6b<-renderText({NULL})
                
                my_i <- i
                plotname6b <- paste("plot6b", my_i, sep="")
                
                output[[plotname6b]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
              }else{
                output$text_analyse6b<-renderText({NULL})
                
                my_i <- i
                plotname6b <- paste("plot6b", my_i, sep="")
                
                output[[plotname6b]] <- renderPlot({
                  plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                       main=paste0(bakterien[my_i]," - Insufficient data"),bty="n")
                })
              }
            })
            progress$inc(1/length(bakterien))
          }
          progress$close()
        }
      }
      
      ##Pro Spezies
      fachbereich<-unique(input3$ORD_FACHBEREICH)
      fachbereich<-fachbereich[order(fachbereich)]
      fachbereich<-fachbereich[as.vector(table(input3$ORD_FACHBEREICH))>=30]
      
      
      if(input$analysis_type1=='per clinic'){
        if(input$clinic_select=="No"){
          fachbereich<-input$clinic_selected
        }
        
        ##Clustering: Heatmap, Supervised, 1) Spezies
        if(length(grep("Data ordered by species",input$cluster_type_klinik_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by species'","<br>"), add = TRUE)  
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by species'", value = 0)
          
          farben<-rainbow(n=length(levels(as.factor(input3$RES_ERREGER))))
          names(farben)<-levels(as.factor(input3$RES_ERREGER))
          
          hoehen<-c()
          weiten<-c()
          for(i in 1:length(fachbereich)){
            temp<-input3[input3$ORD_FACHBEREICH==fachbereich[i],]
            helper<-colSums(temp=="-")
            temp2<-temp[,helper!=length(temp[,1])]
            
            for(j in length(temp2[1,]):erstes_ab){
              temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
              temp2<-temp2[order(temp2[,j]),]
              temp2[,j]<-as.character(temp2[,j])
            }
            temp2<-temp2[order(temp2$RES_ERREGER),]
            
            temp3<-temp2[,erstes_ab:length(temp2[1,])]
            temp3[temp3=="R"]<-2
            temp3[temp3=="S"]<-1
            temp3[temp3=="-"]<-NA
            temp3[temp3=="I"]<-1.5
            for(j in 1:length(temp3[1,])){
              temp3[,j]<-as.numeric(temp3[,j])
            }
            colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
            rownames(temp3)<-paste0("A",1:length(temp3[,1]))
            
            missing<-colSums(!is.na(temp3))/length(temp3[,1])
            
            temp3_2<-as.data.frame(temp3[,missing>0.03])
            
            helper<-c()
            for(j in 1:length(temp2$RES_ERREGER)){
              helper[j]<-farben[names(farben)==temp2$RES_ERREGER[j]]
            }
            names(helper)<-temp2$RES_ERREGER
            
            ann_colors = list(
              Spezies=farben[names(farben)%in%levels(as.factor(temp2$RES_ERREGER))]
            )
            names(ann_colors$Spezies)<-levels(as.factor(temp2$RES_ERREGER))
            
            my_annot<-data.frame(Spezies=temp2$RES_ERREGER)
            row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
            
            fachbereiche<-as.numeric(table(temp2$RES_ERREGER))
            fachbereiche2<-c()
            for(j in 1:length(fachbereiche)){
              fachbereiche2[j]<-sum(fachbereiche[1:j])
            }
            
            if(length(temp3_2[,1])>1){
              hoehen<-c(hoehen,600)
              weiten<-c(weiten,1250)
            }else{
              hoehen<-c(hoehen,200)
              weiten<-c(weiten,1000)
            }
          }
          
          output$plots7 <- renderUI({
            plot_output_list7 <- lapply(1:length(fachbereich), function(k) {
              plotname7 <- paste("plot7", k, sep="")
              plotOutput(plotname7, height = hoehen[k], width = weiten[k])
            })
            do.call(tagList, plot_output_list7)
          })
          
          for(i in 1:length(fachbereich)){
            local({
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",fachbereich[i]), add = TRUE)  
              
              temp<-input3[input3$ORD_FACHBEREICH==fachbereich[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$RES_ERREGER),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              
              temp3_2<-as.data.frame(temp3[,missing>0.03])
              
              helper<-c()
              for(j in 1:length(temp2$RES_ERREGER)){
                helper[j]<-farben[names(farben)==temp2$RES_ERREGER[j]]
              }
              names(helper)<-temp2$RES_ERREGER
              
              ann_colors = list(
                Spezies=farben[names(farben)%in%levels(as.factor(temp2$RES_ERREGER))]
              )
              names(ann_colors$Spezies)<-levels(as.factor(temp2$RES_ERREGER))
              
              my_annot<-data.frame(Spezies=temp2$RES_ERREGER)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              fachbereiche<-as.numeric(table(temp2$RES_ERREGER))
              fachbereiche2<-c()
              for(j in 1:length(fachbereiche)){
                fachbereiche2[j]<-sum(fachbereiche[1:j])
              }
              
              output$text_analyse7<-renderText({NULL})
              
              
              my_i <- i
              plotname7 <- paste("plot7", my_i, sep="")
              
              output[[plotname7]] <- renderPlot({
                if(length(temp3_2[,1])>1){
                  
                  einmalig<-unique(as.vector(as.matrix(temp3_2)))
                  einmalig<-einmalig[!is.na(einmalig)]
                  einmalig<-einmalig*2-1
                  
                  pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=fachbereich[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(8,floor(240/length(ann_colors$Spezies))), fontsize_col = 5,gaps_col = fachbereiche2)
                }else{
                  plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                       yaxt="n",bty="n",main=paste0(fachbereich[my_i]," - no clustering possible"))
                }
              })

            })
            progress$inc(1/length(fachbereich))
          }
          progress$close()
        }
        
        ##Clustering: Heatmap, Unsupervised: hierarchisches Clustering
        if(length(grep("Hierarchical clustering",input$cluster_type_klinik_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Hierarchical clustering'","<br>"), add = TRUE)  
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Hierarchical clustering'", value = 0)
          
          farben<-rainbow(n=length(levels(as.factor(input3$RES_ERREGER))))
          names(farben)<-levels(as.factor(input3$RES_ERREGER))
          
          hoehen<-c()
          weiten<-c()
          for(i in 1:length(fachbereich)){
            temp<-input3[input3$ORD_FACHBEREICH==fachbereich[i],]
            helper<-colSums(temp=="-")
            temp2<-temp[,helper!=length(temp[,1])]
            
            temp2<-temp2[order(temp2$RES_ERREGER),]
            
            temp3<-temp2[,erstes_ab:length(temp2[1,])]
            temp3[temp3=="R"]<-2
            temp3[temp3=="S"]<-1
            temp3[temp3=="-"]<-NA
            temp3[temp3=="I"]<-1.5
            for(j in 1:length(temp3[1,])){
              temp3[,j]<-as.numeric(temp3[,j])
            }
            colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
            rownames(temp3)<-paste0("A",1:length(temp3[,1]))
            
            missing<-colSums(!is.na(temp3))/length(temp3[,1])
            
            temp3_2<-as.data.frame(temp3[,missing>0.03])

            if(length(temp3_2[,1])>1){
              hoehen<-c(hoehen,600)
              weiten<-c(weiten,1250)
            }else{
              hoehen<-c(hoehen,200)
              weiten<-c(weiten,1000)
            }
          }
          
          output$plots8 <- renderUI({
            plot_output_list8 <- lapply(1:length(fachbereich), function(k) {
              plotname8 <- paste("plot8", k, sep="")
              plotOutput(plotname8, height = hoehen[k], width = weiten[k])
            })
            do.call(tagList, plot_output_list8)
          })
          
          for(i in 1:length(fachbereich)){
            local({
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",fachbereich[i]), add = TRUE)  
              
              temp<-input3[input3$ORD_FACHBEREICH==fachbereich[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              temp2<-temp2[order(temp2$RES_ERREGER),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.3])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_2b<-as.data.frame(temp3_2[missing>0.3,])
              temp3_2<-temp3_2b
              
              helper<-c()
              for(j in 1:length(temp2$RES_ERREGER)){
                helper[j]<-farben[names(farben)==temp2$RES_ERREGER[j]]
              }
              names(helper)<-temp2$RES_ERREGER
              
              ann_colors = list(
                Spezies=farben[names(farben)%in%levels(as.factor(temp2$RES_ERREGER))]
              )
              names(ann_colors$Spezies)<-levels(as.factor(temp2$RES_ERREGER))
              
              my_annot<-data.frame(Spezies=temp2$RES_ERREGER)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2missing<-as.data.frame(temp3[,missing>0.8])
              
              missing<-rowSums(!is.na(temp3_2missing))/length(temp3_2missing[1,])
              temp3_3missing<-as.data.frame(temp3_2missing[missing==1,])
              row.names(temp3_3missing)<-NULL
              
              beste = tryCatch({
                clusters<-NbClust(temp3_3missing,
                                  min.nc = 1, max.nc = 5, method = "ward.D",index = "duda")
                clusters$Best.nc[1]
              }, error = function(e) {
                 1
              })

              output$text_analyse8<-renderText({NULL})
              
              my_i <- i
              plotname8 <- paste("plot8", my_i, sep="")
              
              output[[plotname8]] <- renderPlot({
                if(length(temp3_2[,1])>1&&beste!=1){
                  abc=data.frame(x=1)
                  data<-lapply(abc,function(x){
                    tryCatch({pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R"),legend_breaks=c(1,1.5,2),
                                       main=fachbereich[my_i],annotation_col =my_annot,
                                       cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                                       clustering_method = "ward.D",fontsize = min(8,floor(240/length(ann_colors$Spezies))), fontsize_col = 5,cutree_cols = beste)
                    },error=function(e) NULL)
                  })
                  if(!is.null(data[[1]])){
                    einmalig<-unique(as.vector(as.matrix(temp3_2)))
                    einmalig<-einmalig[!is.na(einmalig)]
                    einmalig<-einmalig*2-1

                    pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=fachbereich[my_i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(9,floor(270/length(ann_colors$Spezies))), fontsize_col = 5,cutree_cols = beste)
                  }else{
                    plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                         yaxt="n",bty="n",main=paste0(fachbereich[my_i]," - no clustering possible"))
                  }
                }else{
                  plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                       yaxt="n",bty="n",main=paste0(fachbereich[my_i]," - no clustering possible"))
                }
              })
              
            })
            progress$inc(1/length(fachbereich))
          }
          progress$close()
        }
        
      }
      
      shinyjs::html("text", paste0("<br><br>","Cluster analyses successfully conducted.","<br>"), add = TRUE)  
      
      
    })
    
    
 
    
      output$downloadData <- downloadHandler(
        filename = function() {
          if(input$report_name=="Standard (GEFAAR_Resistance-Cluster-Analyses_<date>)"){
            paste("GEFAAR_Resistance-Cluster-Analyses_", Sys.Date(), ".pdf", sep="")
          }else{
            paste(input$download_name_individuell,".pdf", sep="")
          }
        },
        content = function(file) {
          
          update_geom_defaults("point", list(size=1))
          theme_set(theme_grey(base_size=6))
          
          pdf(file,width = 22,height = 5,paper = "a4r",pagecentre = T)
          
      shinyjs::html("text", paste0("<br>Cluster report is generated.<br><br>"), add = FALSE)
          if(is.null(input$download_analysis_type1)){
            shinyjs::html("text", paste0("ERROR: Please select at least one analysis.","<br>"), add = TRUE)    
            return()
          }
      if(length(grep("per species",input$download_analysis_type1))>0){
        if(input$download_bak_select=="No" && is.null(input$download_bak_selected)){
          shinyjs::html("text", paste0("ERROR: Please select at least one species.","<br>"), add = TRUE)    
          return()
        }
        if(is.null(input$download_cluster_type_bak_heat1) && is.null(input$download_cluster_type_bak_umap1)){
          shinyjs::html("text", paste0("ERROR: Please select at least one analysis.","<br>"), add = TRUE)  
          return()
        }
      }
      if(length(grep("per clinic",input$download_analysis_type1))>0){
        if(input$download_clinic_select=="No" && is.null(input$download_clinic_selected)){
          shinyjs::html("text", paste0("ERROR: Please select at least one clinic.","<br>"), add = TRUE)    
          return()
        }
        if(is.null(input$download_cluster_type_klinik_heat1)){
          shinyjs::html("text", paste0("ERROR: Please select at least one analysis.","<br>"), add = TRUE)  
          return()
        }
      }
      
      
      #########################
      ###Eigentliche Analyse###
      #########################
      
      
      plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1.5),xlab="",ylab="",xaxt="n",
           yaxt="n",bty="n",main="")
      text(x=0.5,y=1.3,"Resistance Cluster Analyses",cex=3)
      text(x=0.5,y=1,Sys.Date(),cex=1.5)
      ukmlogo<-readPNG("www/UKM.png")
      rasterImage(ukmlogo,0.3,0,0.4,0.4)
      imilogo<-readPNG("www/IMI.png")
      rasterImage(imilogo,0.6,0,0.7,0.4)
      
      
      
      #download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      if(input$column4b=="dd.mm.yy"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%y")
      }
      if(input$column4b=="dd.mm.yyyy"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%d.%m.%Y")
      }
      if(input$column4b=="mm/dd/yy"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%m/%d/%y")
      }
      if(input$column4b=="mm/dd/yyyy"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%m/%d/%Y")
      }
      if(input$column4b=="yy-mm-dd"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%y-%m-%d")
      }
      if(input$column4b=="yyyy-mm-dd"){
        download_helper1cluster<-as.Date(input2$ORD_DATUM,format = "%Y-%m-%d")
      }
      
      if(input$download_analysis_type1b=="All"){
        input3<-input2[format(download_helper1cluster,"%Y")==input$download_analysis_type1a,]
      }else{
        input3<-input2[format(download_helper1cluster,"%Y")==input$download_analysis_type1a&input2$ORD_MATERIAL==input$download_analysis_type1b,]
      }
      erstes_ab<-length(grep("_",names(input3)))+1

      ###########
      ###Plots###
      ###########
      
      
      ##Pro Spezies
      download_bakterien<-unique(input3$RES_ERREGER)
      download_bakterien<-download_bakterien[order(download_bakterien)]
      download_bakterien<-download_bakterien[as.vector(table(input3$RES_ERREGER))>=30]
      
      if(length(grep("per species",input$download_analysis_type1))>0){
        if(input$download_bak_select=="No"){
          download_bakterien<-input$download_bak_selected
        }
        #message(download_bakterien)
        
        plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
             yaxt="n",bty="n",main="")
        text(x=0.5,y=0.7,"Independent analysis per species",cex=2.5)
        

        ##Clustering: Heatmap, Supervised, 1) Klinik/Fachbereich, 2) Resistenz
        if(length(grep("Data ordered by 1) clinic, 2) resistance",input$download_cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by 1) clinic, 2) resistance'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Data ordered 1) clinic, 2) resistance",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by 1) clinic, 2) resistance", value = 0)
          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              
              temp3_2<-as.data.frame(temp3[,missing>0.03])
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              fachbereiche<-as.numeric(table(temp2$ORD_FACHBEREICH))
              fachbereiche2<-c()
              for(j in 1:length(fachbereiche)){
                fachbereiche2[j]<-sum(fachbereiche[1:j])
              }
              
              einmalig<-unique(as.vector(as.matrix(temp3_2)))
              einmalig<-einmalig[!is.na(einmalig)]
              einmalig<-einmalig*2-1

              print(pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                         main=download_bakterien[i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                         cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                         clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))), fontsize_col = 5,gaps_col = fachbereiche2))

              progress$inc(1/length(download_bakterien))
          }
          progress$close()
          }

        ##Clustering: Heatmap, Supervised, 1) Klinik/Fachbereich, 2) Datum
        if(length(grep("Data ordered by 1) clinic, 2) date",input$download_cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by 1) clinic, 2) date'","<br>"), add = TRUE)  

          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Data ordered by 1) clinic, 2) date",cex=1.5)
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by 1) clinic, 2) date'", value = 0)
          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              #temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%Y")
              if(input$column4b=="dd.mm.yy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%y")
              }
              if(input$column4b=="dd.mm.yyyy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%d.%m.%Y")
              }
              if(input$column4b=="mm/dd/yy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%m/%d/%y")
              }
              if(input$column4b=="mm/dd/yyyy"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%m/%d/%Y")
              }
              if(input$column4b=="yy-mm-dd"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%y-%m-%d")
              }
              if(input$column4b=="yyyy-mm-dd"){
                temp2$ORD_DATUM<-as.Date(temp2$ORD_DATUM,format = "%Y-%m-%d")
              }
              
              temp2<-temp2[order(temp2$ORD_FACHBEREICH,temp2$ORD_DATUM),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              
              temp3_2<-as.data.frame(temp3[,missing>0.03])
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                #Clinic = rainbow(n=length(levels(as.factor(temp2$ORD_FACHBEREICH)))),
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              fachbereiche<-as.numeric(table(temp2$ORD_FACHBEREICH))
              fachbereiche2<-c()
              for(j in 1:length(fachbereiche)){
                fachbereiche2[j]<-sum(fachbereiche[1:j])
              }

              einmalig<-unique(as.vector(as.matrix(temp3_2)))
              einmalig<-einmalig[!is.na(einmalig)]
              einmalig<-einmalig*2-1
              
              print(pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                             main=download_bakterien[i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                             cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                             clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))),
                             fontsize_col = 5,gaps_col = fachbereiche2))
              
              progress$inc(1/length(download_bakterien))
                
          }
          progress$close()
        }
        
        ##Clustering: Heatmap, Supervised, 1) Klinik/Fachbereich, 2) Datum
        if(length(grep("Hierarchical clustering",input$download_cluster_type_bak_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Hierarchical clustering'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Hierarchical clustering",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Hierarchical clustering'", value = 0)
          
          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
              rownames(temp3)<-paste0("A",1:length(temp3[,1]))
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(as.data.frame(temp3[,missing>0.3]))
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_2b<-as.data.frame(temp3_2[missing>0.3,])
              temp3_2<-temp3_2b
              
              helper<-c()
              for(j in 1:length(temp2$ORD_FACHBEREICH)){
                helper[j]<-farben[names(farben)==temp2$ORD_FACHBEREICH[j]]
              }
              names(helper)<-temp2$ORD_FACHBEREICH
              
              ann_colors = list(
                #Fachbereich = rainbow(n=length(levels(as.factor(temp2$ORD_FACHBEREICH)))),
                Clinic=farben[names(farben)%in%levels(as.factor(temp2$ORD_FACHBEREICH))]
              )
              names(ann_colors$Clinic)<-levels(as.factor(temp2$ORD_FACHBEREICH))
              
              my_annot<-data.frame(Clinic=temp2$ORD_FACHBEREICH)
              row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
              
              output$text_analyse3<-renderText({NULL})
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2missing<-as.data.frame(temp3[,missing>0.8])
              
              missing<-rowSums(!is.na(temp3_2missing))/length(temp3_2missing[1,])
              temp3_3missing<-as.data.frame(temp3_2missing[missing==1,])
              row.names(temp3_3missing)<-NULL

              if(length(temp3_3missing[,1])<100){
                beste = tryCatch({
                  clusters<-NbClust(temp3_3missing,
                                    min.nc = 1, max.nc = 5, method = "ward.D",index = "duda")
                  clusters$Best.nc[1]
                }, error = function(e) {
                  1
                })
              }else{
                beste = tryCatch({
                  clusters<-NbClust(temp3_3missing,
                                    min.nc = 1, max.nc = 10, method = "ward.D",index = "duda")
                  clusters$Best.nc[1]
                }, error = function(e) {
                  1
                })
              }
              
              
              einmalig<-unique(as.vector(as.matrix(temp3_2)))
              einmalig<-einmalig[!is.na(einmalig)]
              einmalig<-einmalig*2-1
              
              if(length(temp3_2[,1])>1&&beste!=1){
                print(pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                         main=download_bakterien[i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                         cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                         clustering_method = "ward.D",fontsize = min(7,floor(210/length(ann_colors$Spezies))),
                         fontsize_col = 5,cutree_cols = beste))
              }else{
                print(plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                     yaxt="n",bty="n",main=paste0(download_bakterien[i]," - no clustering possible")))
              }
              
              progress$inc(1/length(download_bakterien))
          }
          progress$close()
        }
        
        ##Clustering: UMAP mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Plot with colored clinics",input$download_cluster_type_bak_umap1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate UMAP 'Plot with colored clinics'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"UMAP",cex=2.5)
          text(x=0.5,y=0.4,"Plot with colored clinics",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate UMAP 'Plot with colored clinics'", value = 0)
          
          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
                colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
                rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
                
                helper<-c()
                for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                  helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
                }
                names(helper)<-temp2_3$ORD_FACHBEREICH
                
                ann_colors = list(
                  Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
                )
                names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                
                my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
                row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
                
                
              
              download_abc=data.frame(x=1)
              download_data<-lapply(download_abc,function(x){
                tryCatch({
                  pdf(file = NULL)
                  M3C::umap(t(as.matrix(temp3_3)),labels=names(helper),colvec = helper,seed = 42,legendtextsize = 7) + ggtitle(download_bakterien[i]) + 
                                 theme(plot.title = element_text(size = 15,hjust = 0.5))
                  dev.off()
                },error=function(e) NULL)
              })
              if(!is.null(download_data[[1]])){
                print(M3C::umap(t(as.matrix(temp3_3)),legendtextsize = min(5,floor(150/length(unique(names(helper))))), 
                                labels=names(helper),colvec = helper,seed = 42) + ggtitle(download_bakterien[i]) + 
                        theme(plot.title = element_text(size = 15,hjust = 0.5)))
              }else{
                dev.off()
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                     main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              }else{
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                           main=paste0(download_bakterien[i]," - Insufficient data"),bty="n")) 
              }
              progress$inc(1/length(download_bakterien))
          }
          progress$close()
        }
        
        ##Clustering: Clustering: UMAP mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Plot with colored clusters",input$download_cluster_type_bak_umap1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate UMAP 'Plot plot with colored clusters'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"UMAP",cex=2.5)
          text(x=0.5,y=0.4,"Plot with colored clusters",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          moeglich_speicher<-c()
          partition_speicher<-list()      
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate UMAP 'Plot plot with colored clusters'", value = 0)
          
          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
                colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
                rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
                
                helper<-c()
                for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                  helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
                }
                names(helper)<-temp2_3$ORD_FACHBEREICH
                
                ann_colors = list(
                  Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
                )
                names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                
                my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
                row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
                download_abc=data.frame(x=1)
                download_data<-lapply(download_abc,function(x){
                  tryCatch({
                    pdf(file = NULL)
                    alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                    test<-data.frame(alt$data)
                  
                    clusters2<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="silhouette")
                    clusters1<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="kl")
                    clusters3<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="ch")
                    clusters4<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="scott")
                    clusters5<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="duda")
                    clusters6<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="dunn")
                    
                    dev.off()
                  },error=function(e) NULL)
                })
                if(!is.null(download_data[[1]])){
                  alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                  test<-data.frame(alt$data)
                  pdf(file = NULL)
                  clusters2<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="silhouette")
                  clusters1<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="kl")
                  clusters3<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="ch")
                  clusters4<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="scott")
                  clusters5<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="duda")
                  clusters6<-NbClust(data = test, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 5, method = "kmeans",index="dunn")
                  dev.off()
                
                  beste<-table(c(clusters1$Best.nc[1],clusters4$Best.nc[1],clusters3$Best.nc[1],clusters5$Best.nc[1],clusters6$Best.nc[1]))
                  if(beste[max(beste)==beste]>=2&&
                     sd(clusters1$All.index)>=5&&sd(clusters2$All.index)>=0.05){
                    moeglich_speicher[i]<-moeglich<-as.numeric(min(names(beste[max(beste)==beste])))
                    if(moeglich==clusters1$Best.nc[1]){
                      best_cluster<-clusters1
                    }else{
                      if(moeglich==clusters3$Best.nc[1]){
                        best_cluster<-clusters3
                      }else{
                        if(moeglich==clusters4$Best.nc[1]){
                          best_cluster<-clusters4
                        }else{
                          if(moeglich==clusters5$Best.nc[1]){
                            best_cluster<-clusters5
                          }
                        }
                      }
                    }
                    partition_speicher[[i]]<-partition<-best_cluster$Best.partition
                  }else{
                    moeglich_speicher[i]<-moeglich<-0
                    partition_speicher[[i]]<-partition<-NA
                  }
  
                  
                  
                if(moeglich>0){
                  print(M3C::umap(t(as.matrix(temp3_3)),labels=as.factor(partition),
                                  legendtextsize = min(5,floor(150/length(unique(names(helper))))), 
                                  seed=42,colvec = col_vec[1:length(levels(as.factor(partition)))],
                                  controlscale = T,scale=3) + 
                          ggtitle(download_bakterien[i]) + 
                          theme(plot.title = element_text(size = 20,hjust = 0.5)))
                  
                }else{
                  print(M3C::umap(t(as.matrix(temp3_3)),labels=names(helper),colvec = helper,seed = 42,
                                  legendtextsize = min(5,floor(150/length(unique(names(helper)))))) + 
                          ggtitle(paste0(download_bakterien[i]," - no unique clustering possible")) + 
                          theme(plot.title = element_text(size = 13,hjust = 0.5)))
                }
              }else{
                dev.off()
                moeglich_speicher[i]<-NA
                partition_speicher[[i]]<-NA
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                           main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              }else{
                moeglich_speicher[i]<-NA
                partition_speicher[[i]]<-NA
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                           main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              progress$inc(1/length(download_bakterien))
          }
          progress$close()
        }
        
        ##Clustering: Zusätzliche Heatmap supervised nach UMAP-Clustern mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Data ordered by UMAP clusters",input$download_cluster_type_bak_umap2,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate additional heatmap 'Data ordered by UMAP clusters'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Data ordered by UMAP clusters",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate additional heatmap 'Data ordered by UMAP clusters'", value = 0)

          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
                colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
                rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
                
                helper<-c()
                for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                  helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
                }
                names(helper)<-temp2_3$ORD_FACHBEREICH
                
                ann_colors = list(
                  Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
                )
                names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                
                my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
                row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              

                if(!is.na(moeglich_speicher[i])){
                  alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                  test<-data.frame(alt$data)
                  
                  moeglich<-moeglich_speicher[i]
                  partition<-partition_speicher[[i]]

                if(moeglich>0){
                  soll_order<-partition[order(partition)]
                  
                  temp3_3_ordered<-temp3_3[match(names(soll_order),row.names(temp3_3)),]
                  my_annot_ordered<-as.data.frame(my_annot[match(names(soll_order),row.names(temp3_3)),])
                  colnames(my_annot_ordered)<-c("Clinic")
                  
                  fachbereiche<-as.numeric(table(soll_order))
                  fachbereiche2<-c()
                  for(m in 1:length(fachbereiche)){
                    fachbereiche2[m]<-sum(fachbereiche[1:m])
                  }
                  
                  my_annot_ordered<-cbind(my_annot_ordered,Cluster=as.character(soll_order))
                  rownames(my_annot_ordered)<-rownames(temp3_3_ordered)
                  
                  ann_colors = list(
                    Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))],
                    Cluster = col_vec[1:length(levels(as.factor(my_annot_ordered$Cluster)))]
                  )
                  names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                  names(ann_colors$Cluster)<-levels(as.factor(my_annot_ordered$Cluster))
                  
                  einmalig<-unique(as.vector(as.matrix(temp3_3_ordered)))
                  einmalig<-einmalig[!is.na(einmalig)]
                  einmalig<-einmalig*2-1
                  
                  print(pheatmap(t(as.matrix(temp3_3_ordered)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=download_bakterien[i],annotation_col =my_annot_ordered,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize =  min(6,floor(180/length(ann_colors$Spezies))),
                           fontsize_col = 5,gaps_col = fachbereiche2))
                  
                }else{
                  fachbereiche<-as.numeric(table(temp2_3$ORD_FACHBEREICH))
                  fachbereiche2<-c()
                  for(j in 1:length(fachbereiche)){
                    fachbereiche2[j]<-sum(fachbereiche[1:j])
                  }
                  
                  einmalig<-unique(as.vector(as.matrix(temp3_3)))
                  einmalig<-einmalig[!is.na(einmalig)]
                  einmalig<-einmalig*2-1
                  
                  print(pheatmap(t(as.matrix(temp3_3)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=paste0(download_bakterien[i]," - no unique clustering possible"),annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))), 
                           fontsize_col = 5,gaps_col = fachbereiche2))
                }
              }else{
                #dev.off()
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                     main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              }else{
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                           main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              progress$inc(1/length(download_bakterien))
          }
          progress$close()
        }
          
        ##Clustering: Zusätzliche Heatmap supervised nach Kliniken/Fachbereichen mit eingefärbten Kliniken/Fachbereichen
        if(length(grep("Data ordered by clinic",input$download_cluster_type_bak_umap2,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate additional heatmap 'Data ordered by clinic'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Data ordered by clinic",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$ORD_FACHBEREICH))))
          names(farben)<-levels(as.factor(input3$ORD_FACHBEREICH))
          
          col_vec<-c("skyblue","gold", "violet", "darkorchid", "slateblue", "forestgreen", "violetred",
                     "orange", "midnightblue", "grey31", "black")
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate additional heatmap 'Data ordered by clinic'", value = 0)

          for(i in 1:length(download_bakterien)){
              shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_bakterien[i]), add = TRUE)  
              
              temp<-input3[input3$RES_ERREGER==download_bakterien[i],]
              helper<-colSums(temp=="-")
              temp2<-temp[,helper!=length(temp[,1])]
              
              for(j in length(temp2[1,]):erstes_ab){
                temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
                temp2<-temp2[order(temp2[,j]),]
                temp2[,j]<-as.character(temp2[,j])
              }
              temp2<-temp2[order(temp2$ORD_FACHBEREICH),]
              
              temp3<-temp2[,erstes_ab:length(temp2[1,])]
              temp3[temp3=="R"]<-2
              temp3[temp3=="S"]<-1
              temp3[temp3=="-"]<-NA
              temp3[temp3=="I"]<-1.5
              for(j in 1:length(temp3[1,])){
                temp3[,j]<-as.numeric(temp3[,j])
              }
              
              missing<-colSums(!is.na(temp3))/length(temp3[,1])
              temp3_2<-as.data.frame(temp3[,missing>0.8])
              temp2_2<-as.data.frame(temp2[,c(1:(erstes_ab-1),which(missing>0.8)+(erstes_ab-1))])
              
              missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
              temp3_3<-as.data.frame(temp3_2[missing==1,])
              temp2_3<-as.data.frame(temp2_2[missing==1,])
              
              if(length(temp3_3)>0&&length(temp2_3)>0){
                colnames(temp3_3)<-names(temp2_3)[erstes_ab:length(temp2_3[1,])] 
                rownames(temp3_3)<-paste0("A",1:length(temp3_3[,1]))
                
                helper<-c()
                for(j in 1:length(temp2_3$ORD_FACHBEREICH)){
                  helper[j]<-farben[names(farben)==temp2_3$ORD_FACHBEREICH[j]]
                }
                names(helper)<-temp2_3$ORD_FACHBEREICH
                
                ann_colors = list(
                  Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))]
                )
                names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                
                my_annot<-data.frame(Clinic=temp2_3$ORD_FACHBEREICH)
                row.names(my_annot)<-paste0("A",1:length(temp3_3[,1]))
              
                if(!is.na(moeglich_speicher[i])){
                  alt<-umap(t(as.matrix(temp3_3)),seed = 42)
                  test<-data.frame(alt$data)
                  
                  moeglich<-moeglich_speicher[i]
                  partition<-partition_speicher[[i]]

                if(moeglich>0){
                  soll_order<-partition[order(my_annot$Clinic,partition)]
                  
                  temp3_3_ordered<-temp3_3[match(names(soll_order),row.names(temp3_3)),]
                  my_annot_ordered<-as.data.frame(my_annot[match(names(soll_order),row.names(temp3_3)),])
                  colnames(my_annot_ordered)<-c("Clinic")
                  
                  fachbereiche<-as.numeric(table(my_annot_ordered$Clinic))
                  fachbereiche2<-c()
                  for(m in 1:length(fachbereiche)){
                    fachbereiche2[m]<-sum(fachbereiche[1:m])
                  }
                  
                  my_annot_ordered<-cbind(my_annot_ordered,Cluster=as.character(soll_order))
                  rownames(my_annot_ordered)<-rownames(temp3_3_ordered)
                  
                  ann_colors = list(
                    Clinic=farben[names(farben)%in%levels(as.factor(temp2_3$ORD_FACHBEREICH))],
                    Cluster = col_vec[1:length(levels(as.factor(my_annot_ordered$Cluster)))]
                  )
                  names(ann_colors$Clinic)<-levels(as.factor(temp2_3$ORD_FACHBEREICH))
                  names(ann_colors$Cluster)<-levels(as.factor(my_annot_ordered$Cluster))
                  
                  einmalig<-unique(as.vector(as.matrix(temp3_3_ordered)))
                  einmalig<-einmalig[!is.na(einmalig)]
                  einmalig<-einmalig*2-1
                  
                  print(pheatmap(t(as.matrix(temp3_3_ordered)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=download_bakterien[i],annotation_col =my_annot_ordered,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))),
                           fontsize_col = 5,gaps_col = fachbereiche2))
                  
                }else{
                  fachbereiche<-as.numeric(table(temp2_3$ORD_FACHBEREICH))
                  fachbereiche2<-c()
                  for(j in 1:length(fachbereiche)){
                    fachbereiche2[j]<-sum(fachbereiche[1:j])
                  }
                  
                  einmalig<-unique(as.vector(as.matrix(temp3_3)))
                  einmalig<-einmalig[!is.na(einmalig)]
                  einmalig<-einmalig*2-1
                  
                  print(pheatmap(t(as.matrix(temp3_3)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                           main=paste0(download_bakterien[i]," - no unique clustering possible"),annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                           cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                           clustering_method = "ward.D",fontsize = min(6,floor(180/length(ann_colors$Spezies))),
                           fontsize_col = 5,gaps_col = fachbereiche2))
                }
              }else{
                #dev.off()
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                     main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              }else{
                print(plot(NULL,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",
                     main=paste0(download_bakterien[i]," - Insufficient data"),bty="n"))
              }
              progress$inc(1/length(download_bakterien))
          }
          progress$close()
      }
      
      }
      
      ##Pro Klinik/Fachbereich
      download_fachbereich<-unique(input3$ORD_FACHBEREICH)
      download_fachbereich<-download_fachbereich[order(download_fachbereich)]
      download_fachbereich<-download_fachbereich[as.vector(table(input3$ORD_FACHBEREICH))>=30]
      
      
      if(length(grep("per clinic",input$download_analysis_type1))>0){
        if(input$download_clinic_select=="No"){
          download_fachbereich<-input$download_clinic_selected
        }
        
        plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
             yaxt="n",bty="n",main="")
        text(x=0.5,y=0.7,"Independent analysis per clinic",cex=2.5)
        
        ##Clustering: Heatmap, Supervised, 1) Spezies
        if(length(grep("Data ordered by species",input$download_cluster_type_klinik_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Data ordered by species'","<br>"), add = TRUE)  
          
          farben<-rainbow(n=length(levels(as.factor(input3$RES_ERREGER))))
          names(farben)<-levels(as.factor(input3$RES_ERREGER))
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Data ordered by species",cex=1.5)
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Data ordered by species'", value = 0)
          
          for(i in 1:length(download_fachbereich)){
            shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_fachbereich[i]), add = TRUE)  
            
            temp<-input3[input3$ORD_FACHBEREICH==download_fachbereich[i],]
            helper<-colSums(temp=="-")
            temp2<-temp[,helper!=length(temp[,1])]
            
            for(j in length(temp2[1,]):erstes_ab){
              temp2[,j]<-ordered(temp2[,j],c("R","I","S","-"))
              temp2<-temp2[order(temp2[,j]),]
              temp2[,j]<-as.character(temp2[,j])
            }
            temp2<-temp2[order(temp2$RES_ERREGER),]
            
            temp3<-temp2[,erstes_ab:length(temp2[1,])]
            temp3[temp3=="R"]<-2
            temp3[temp3=="S"]<-1
            temp3[temp3=="-"]<-NA
            temp3[temp3=="I"]<-1.5
            for(j in 1:length(temp3[1,])){
              temp3[,j]<-as.numeric(temp3[,j])
            }
            colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
            rownames(temp3)<-paste0("A",1:length(temp3[,1]))
            
            missing<-colSums(!is.na(temp3))/length(temp3[,1])
            
            temp3_2<-as.data.frame(temp3[,missing>0.03])
            
            helper<-c()
            for(j in 1:length(temp2$RES_ERREGER)){
              helper[j]<-farben[names(farben)==temp2$RES_ERREGER[j]]
            }
            names(helper)<-temp2$RES_ERREGER
            
            ann_colors = list(
              Spezies=farben[names(farben)%in%levels(as.factor(temp2$RES_ERREGER))]
            )
            names(ann_colors$Spezies)<-levels(as.factor(temp2$RES_ERREGER))
            
            my_annot<-data.frame(Spezies=temp2$RES_ERREGER)
            row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
            
            fachbereiche<-as.numeric(table(temp2$RES_ERREGER))
            fachbereiche2<-c()
            for(j in 1:length(fachbereiche)){
              fachbereiche2[j]<-sum(fachbereiche[1:j])
            }
            
            if(length(temp3_2[,1])>1){
              einmalig<-unique(as.vector(as.matrix(temp3_2)))
              einmalig<-einmalig[!is.na(einmalig)]
              einmalig<-einmalig*2-1
              
              print(pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                       main=download_fachbereich[i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                       cluster_rows = F,cluster_cols = F,show_colnames = F,annotation_colors=ann_colors,
                       clustering_method = "ward.D",fontsize = min(8,floor(240/length(ann_colors$Spezies))),
                       fontsize_col = 5,gaps_col = fachbereiche2))
            }else{
              print(plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                   yaxt="n",bty="n",main=paste0(download_fachbereich[i]," - no clustering possible"),cex.main=0.8))
            }
            progress$inc(1/length(download_fachbereich))
          }
          progress$close()
        }
        
        ##Clustering: Heatmap, Unsupervised: hierarchisches Clustering
        if(length(grep("Hierarchical clustering",input$download_cluster_type_klinik_heat1,fixed=T))>0){
          shinyjs::html("text", paste0("<br><br>","&nbsp&nbsp&nbspGenerate heatmap 'Hierarchical clustering'","<br>"), add = TRUE)  
          
          plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
               yaxt="n",bty="n",main="")
          text(x=0.5,y=0.7,"Heatmap",cex=2.5)
          text(x=0.5,y=0.4,"Hierarchical clustering",cex=1.5)
          
          
          farben<-rainbow(n=length(levels(as.factor(input3$RES_ERREGER))))
          names(farben)<-levels(as.factor(input3$RES_ERREGER))
          
          progress <- shiny::Progress$new()
          progress$set(message = "Generate heatmap 'Hierarchical clustering'", value = 0)
          
          for(i in 1:length(download_fachbereich)){
            shinyjs::html("text", paste0("<br>","&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp",download_fachbereich[i]), add = TRUE)  
            
            temp<-input3[input3$ORD_FACHBEREICH==download_fachbereich[i],]
            helper<-colSums(temp=="-")
            temp2<-temp[,helper!=length(temp[,1])]
            
            temp2<-temp2[order(temp2$RES_ERREGER),]
            
            temp3<-temp2[,erstes_ab:length(temp2[1,])]
            temp3[temp3=="R"]<-2
            temp3[temp3=="S"]<-1
            temp3[temp3=="-"]<-NA
            temp3[temp3=="I"]<-1.5
            for(j in 1:length(temp3[1,])){
              temp3[,j]<-as.numeric(temp3[,j])
            }
            colnames(temp3)<-names(temp2)[erstes_ab:length(temp2[1,])] 
            rownames(temp3)<-paste0("A",1:length(temp3[,1]))
            
            missing<-colSums(!is.na(temp3))/length(temp3[,1])
            temp3_2<-as.data.frame(temp3[,missing>0.3])
            
            missing<-rowSums(!is.na(temp3_2))/length(temp3_2[1,])
            temp3_2b<-as.data.frame(temp3_2[missing>0.3,])
            temp3_2<-temp3_2b
            
            helper<-c()
            for(j in 1:length(temp2$RES_ERREGER)){
              helper[j]<-farben[names(farben)==temp2$RES_ERREGER[j]]
            }
            names(helper)<-temp2$RES_ERREGER
            
            ann_colors = list(
              Spezies=farben[names(farben)%in%levels(as.factor(temp2$RES_ERREGER))]
            )
            names(ann_colors$Spezies)<-levels(as.factor(temp2$RES_ERREGER))
            
            my_annot<-data.frame(Spezies=temp2$RES_ERREGER)
            row.names(my_annot)<-paste0("A",1:length(temp3[,1]))
            
            
            missing<-colSums(!is.na(temp3))/length(temp3[,1])
            temp3_2missing<-as.data.frame(temp3[,missing>0.8])
            
            missing<-rowSums(!is.na(temp3_2missing))/length(temp3_2missing[1,])
            temp3_3missing<-as.data.frame(temp3_2missing[missing==1,])
            row.names(temp3_3missing)<-NULL
            
            beste = tryCatch({
              clusters<-NbClust(temp3_3missing,
                                min.nc = 1, max.nc = 5, method = "ward.D",index = "duda")
              clusters$Best.nc[1]
            }, error = function(e) {
              1
            })
            
            if(length(temp3_2[,1])>1&&beste!=1){
                einmalig<-unique(as.vector(as.matrix(temp3_2)))
                einmalig<-einmalig[!is.na(einmalig)]
                einmalig<-einmalig*2-1
                
                print(pheatmap(t(as.matrix(temp3_2)),legend_labels = c("S","I","R")[einmalig],legend_breaks=c(1,1.5,2)[einmalig],
                         main=download_fachbereich[i],annotation_col =my_annot,breaks=ifelse(length(einmalig)!=1,NA,c(0,3)),
                         cluster_rows = F,cluster_cols = T,show_colnames = F,annotation_colors=ann_colors,
                         clustering_method = "ward.D",fontsize = min(9,floor(270/length(ann_colors$Spezies))), fontsize_col = 5,cutree_cols = beste))

            }else{
              print(plot(x=0.5,y=0.5,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",xaxt="n",
                   yaxt="n",bty="n",main=paste0(download_fachbereich[i]," - no clustering possible"),cex.main=0.8))
            }
            progress$inc(1/length(download_fachbereich))
          }
          progress$close()
        }
        
      }
      
      graphics.off()
      
      shinyjs::html("text", paste0("<br><br>","Cluster report successfully generated","<br>"), add = TRUE)  
      
        }
      
      )
      
    })
    
  })

  session$onSessionEnded(function() {
   
     stopApp()
  })
})



