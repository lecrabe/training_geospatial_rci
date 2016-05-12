#dashboard
####################################################################################
#######          Shiny app for accuracy assessment design       ####################
######    by remi.dannunzio@fao.org  and yelena.finegold@fao.org      ##############
####################################################################################

####################################################################################
#######          Set options and necessary packages       ##########################
####################################################################################
options(shiny.launch.browser=T)
options(stringsAsFactors=FALSE)

########################################
# include all the needed packages here #

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(ggplot2)
packages(xtable)
packages(raster)
packages(shiny)
packages(shinydashboard)
packages(dismo)
packages(stringr)
packages(snow)
packages(plyr)
packages(leaflet)
packages(RColorBrewer)
packages(DT)
packages(rgeos)
packages(rgdal)


####################################################################################
#######       PART I : Setup the Graphic Interface        ##########################
####################################################################################
print("Starting the process")

ui <- dashboardPage(skin='green',
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
    title= 'Accuracy assessment analysis',
    titleWidth = 350),

    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
      menuItem('1: Input', tabName = 'Input', icon = icon("picture-o")),
      menuItem('2: Check', tabName = 'Check', icon = icon("area-chart")),
      menuItem('3: Results', tabName = 'Results', icon = icon("map-marker"))    )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
    tabItems(
      
      ####################################################################################
      # New Tab
      tabItem(tabName = "Intro",
               fluidRow(
                 
                 ####################################################################################
                 # New box
                 box(
                   title= "Description", status = "success", solidHeader= TRUE,
                   "This interactive tool calculates results from accuracy assessment",
                   br(),
                   "The objective of this tool is to create confusion matrices and calculate adjusted estimates and confidence intervals around these estimates",
                   br(),
                   "For support, post on the ",
                   a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
                  ),
                 
                 
                 ####################################################################################
                 # New box
                 box(
                    title= "Background", status = "success", solidHeader= TRUE,
                    "The aim of a map accuracy assessment is to characterize the frequency of errors (omission and commission) for each map class.
                    Differences in these two errors may be used to adjust area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class.
                    Adjusting area estimates on the basis of a rigorous accuracy assessment represents an improvement over simply reporting the areas of map classes."
                 ),
                 
                
                 ####################################################################################
                 # New box
                 box(
                  title= "How to use the tool ?", status = "success", solidHeader= TRUE,
                  "You have to go through all the steps in the left panel, in order", 
                  br(),
                  tags$ol(
                    tags$li("Select your inputs"), 
                    tags$li("Verify that all classes with their areas are present"), 
                    tags$li("Get the results")
                  )
                ),
                
                ####################################################################################
                # Change style of the CSS style of the tabBox, making the color green
                tags$style("
                           .nav-tabs-custom .nav-tabs li.active {
                           border-top-color: #00994d;
                           }"),
                
                ####################################################################################
                # New tabBox
                tabBox(
                  ####################################################################################
                  # New tabPanel
                  tabPanel("Disclaimer",
                            br(),
                           "FAO declines all responsibility for errors or deficiencies in the database or software or 
                            in the documentation accompanying it, for program maintenance and upgrading as well as for any 
                           damage that may arise from them. FAO also declines any responsibility for updating the data and 
                           assumes no responsibility for errors and omissions in the data provided. 
                           Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.",
                           br(),
                           br(),
                           img(src="sepal-logo-EN-white.jpg", height = 100, width = 210),
                           img(src="UNREDD_LOGO_COLOUR.jpg", height = 80, width = 100),
                           img(src="Open-foris-Logo160.jpg", height = 70, width = 70),
                           br()
                  ), 
                  
                  ####################################################################################
                  # New tabPanel
                  tabPanel("References and Documents",
                           br(),
                           img(src="GFOI_MG_cover.PNG", height = 250, width = 200),
                           a(href="http://www.gfoi.org/wp-content/uploads/2015/04/GFOIMGD_English.pdf"," GFOI MGD Section 3.7 and Module 2.7",target="_blank"),
                           br(),
                           img(src="AA_cover.PNG", height = 250, width = 200),
                           a(href="https://dl.dropboxusercontent.com/u/11506740/AccuracyAssessment%20Final%20NFMA%2046%20A4.pdf"," FAO NFMA paper N46: Map accuracy assessment and area estimation",target="_blank"),
                           br(),
                           img(src="Olofsson2014_cover.PNG", height = 150, width = 200),
                           a(href="http://reddcommunity.org/sites/default/files/field/publications/Olofsson_et_al_2014_RSE.pdf"," Olofsson et al. (2014): Good practices for estimating area and assessing accuracy of land change",target="_blank")                          
                           
                  )
                )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Input',
              fluidRow(
                ####################################################################################
                # New box
                box(title= "Required input", status = "success", solidHeader= TRUE,
                    "Select the validation file exported from CollectEarth. It must be placed under the input folder",
                    br(),
                    selectInput('CEfilename',   label= 'Validation file (.csv)', list.files("../input/", pattern = "collect")),
                    selectInput('areafilename',   label= 'Area file (.csv)', list.files("../output/", pattern = "area"))
                 
              )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Check',
              h4("Check inputs"),
              fluidRow(
                ####################################################################################
                # New box
                box(
                  title= "What to check", status = "success", solidHeader= TRUE,
                  "Let us check something",
                  htmlOutput("display_check_line"),
                  htmlOutput("display_check_cols"),
                  tableOutput("table_check"),
                  h4("Location of the points collected"),
                  leafletOutput("map_check")
              
                )
                )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Results',
                fluidRow(
                  box(h4("Confusion Matrix"),
                      tableOutput("matrix_all"),
                      downloadButton('download_matrix', 'Download as CSV')
                  ),
                  box(h4("Graph"),
                      plotOutput("histogram_all")
                  ),
                  box(h4("Adjusted areas and accuracies"),
                      tableOutput("accuracy_all"),
                      downloadButton('download_accuracy', 'Download as CSV')
                  )
                )
        ) 
      
      ####################################################################################
      # End of the tabItem list
      )
    
    ####################################################################################
    # End of the Dashboard Body
    )
####################################################################################
# End of the Graphic User Interface
)


server <- function(input, output) {    
####################################################################################
####### Step 1 : compute areas of each strata of the map ###########################
####################################################################################
  

    # Map area CSV
    areas_i   <- reactive({
      print("read data of area")
      ############### Read the name chosen from dropdown menu
      #areas_i <- read.csv("../output/area.csv") 
      areas_i <- read.csv(paste('../output/', input$areafilename,sep="")) 
      
      })
  
  
  
    # Collect earth output file
    df_i  <- reactive({
      print("read data of validation")
      ############### Read the name chosen from dropdown menu
      cefile <-input$CEfilename
      #cefile <- "collectedData_mockup_results.csv"
      if (is.null(input$CEfilename))return(NULL)
      
      ############### Load the raster corresponding to the selected name
      datafolder <- paste(c('../input/', cefile),collapse='')
      dataname <- gsub(" ","",datafolder)
      df_i <- read.csv(dataname) 
      names(df_i)[16]<-"ref_code"
      df_i
    })


  ##################################################################################################################################
  ############### Legend used for the matrices
  legend_i  <- reactive({
    ############### Use multicore clusters   
    #beginCluster()
    print("Legend")
    df_i <- df_i()
    legend_i <- levels(as.factor(df_i[,"map_code"]))
    legend_i 
  })
  

  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : lines
  output$display_check_line <- renderUI({
    df <- df_i()
    
    categories <- names(df)
    print(categories)
    selectInput("check_inline",
                label = h5(paste("Lines of the pivot table")),
                choices = categories,
                selected="operator"
    )
  })
  
  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : columns
  output$display_check_cols <- renderUI({
    df <- df_i()
    
    categories <- names(df)
    print(categories)
    selectInput("check_incols",
                label = h5(paste("Columns of the pivot table")),
                choices = categories,
                selected="map_code"
    )
  })
  
  
  ################################################    
  ################ Create a pivot check table
  ################################################
  output$table_check <- renderTable({
    lines   <- input$check_inline #"operator"
    columns <- input$check_incols #"adm1_name"
      table(df_i()[,lines],df_i()[,columns])
    
  })
  
  ################################################    
  ################ Display all the points
  ################################################
  
  output$map_check <-   renderLeaflet({
    df_i<- df_i()
    dfa <- SpatialPointsDataFrame(
      coords=data.frame(df_i[,c(3,4)]),
      data=data.frame(df_i[,"map_code"]),
      proj4string=CRS("+proj=longlat +datum=WGS84"),
      match.ID=F)

    names(dfa)<-"map_code"
    factpal <- colorFactor("Spectral", dfa$map_code)
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data = dfa, color= ~factpal(map_code),
                       fillOpacity = 1,
                       radius = 1
      )
    m

  })
  
  
  ################################################    
  ################ Matrix for all classes
  ################################################
  matrix_all <- reactive({
    df <- df_i()
    areas <- areas_i()
    legend <- legend_i()
    print("test matrix")
    tmp <- as.matrix(table(df$map_code,df$ref_code))
    matrix<-matrix(0,nrow=length(legend),ncol=length(legend))
    
    for(i in 1:length(legend)){
      tryCatch({
        cat(paste(legend[i],"\n"))
        matrix[,i]<-tmp[,legend[i]]
      }, error=function(e){cat("Configuration impossible \n")}
      )
    }
    
    matrix
  })
  
  ################################################    
  ################ Table of accuracies
  ################################################
  
  accuracy_all <- reactive({
    matrix <- matrix_all()
    df <- df_i()
    areas <- areas_i()
    legend <- legend_i()
    
    matrix_w<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_w[i,j]<-matrix[i,j]/sum(matrix[i,])*areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)
        }, error=function(e){cat("Configuration impossible \n")}
        )
      }}
    
    matrix_se<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_se[i,j]<-areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*
            matrix[i,j]/
            sum(matrix[i,])*
            (1-matrix[i,j]/sum(matrix[i,]))/
            (sum(matrix[i,])-1)
        }, error=function(e){cat("Configuration impossible \n")
          print(legend[i])}
        )
      }
    }
    
    confusion<-data.frame(matrix(nrow=length(legend)+1,ncol=9))
    names(confusion)<-c("class","code","Pa","PaW","Ua","area","area_adj","se","ci")
    
    ### Integration des elements dans le jeu de donnees synthese
    for(i in 1:length(legend)){
      confusion[i,]$class<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$code<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$Pa<-matrix[i,i]/sum(matrix[,i])
      confusion[i,]$Ua<-matrix[i,i]/sum(matrix[i,])
      confusion[i,]$PaW<-matrix_w[i,i]/sum(matrix_w[,i])
      confusion[i,]$area_adj<-sum(matrix_w[,i])*sum(areas$map_area)
      confusion[i,]$area<-areas[areas$map_class==legend[i],]$map_area
      confusion[i,]$se<-sqrt(sum(matrix_se[,i]))*sum(areas$map_area)
      confusion[i,]$ci<-confusion[i,]$se*1.96
    }
    
    ### Calculer la Precision Generale
    confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$map_area),sum(areas$map_area),"","")
    confusion
  })  
  # ################################################    
  # ################ Output : Summary of accuracies 
  # ################################################
  #     
  output$accuracy_all <- renderTable({
    item<-data.frame(accuracy_all())
    item<-item[,c("class","PaW","Ua","area","area_adj","ci")]
    item$PaW<-floor(as.numeric(item$PaW)*100)
    item$Ua<-floor(as.numeric(item$Ua)*100)
    item$area<-floor(as.numeric(item$area))
    item$area_adj<-floor(as.numeric(item$area_adj))
    item$ci<-floor(as.numeric(item$ci))
    names(item) <-c("Class","PA","UA","Map areas","Adjusted areas","CI")
    item
  },include.rownames=FALSE,digits=0)
  #   
  # #################################################    
  # ################ Output item  :  confusion matrix
  # #################################################
  
  output$matrix_all <- renderTable({
    df <- df_i()
    areas <- areas_i()
    legend <- legend_i()
    
    item<-as.matrix(matrix_all())
    dimnames(item) <- list(legend,legend)
    #      dimnames(item) <- list(areas$class[areas$code %in% as.numeric(legend)],areas$class[areas$code %in% as.numeric(legend)])
    item                                  
  },digits=0)
  
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  
  output$histogram_all <- renderPlot({
    dfa<-as.data.frame(accuracy_all())
    legend <- legend_i()
    
    dfa<-dfa[c(1:length(legend)),]
    dfa[dfa=="NaN"]<-0
    dfa$ci<-as.numeric(dfa$ci)
    dfa$area_adj<-as.numeric(dfa$area_adj)
    
    avg.plot<-ggplot(data=dfa,aes(x=class,y=area_adj))
    
    avg.plot+geom_bar(stat="identity",fill="darkgrey")+geom_errorbar(aes(ymax=area_adj+ci, ymin=area_adj-ci))+theme_bw()
    
    
  })
  
  # #################################################    
  # ################ Output confusion matrix
  # #################################################
  
  output$download_matrix <- downloadHandler(
    filename = function() { 
      paste('matrix_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      legend <- legend_i()
      item<-as.matrix(matrix_all())
      dimnames(item) <- list(legend,legend)
      write.csv(item,file)
    })
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  
  output$download_accuracy <- downloadHandler(
    filename = function() { 
      paste('accuracy_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(accuracy_all(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the validation file
  # #################################################
  
  output$download_input <- downloadHandler(
    filename = function() { 
      paste('input_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(df_i(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the area file
  # #################################################
  
  output$download_area <- downloadHandler(
    filename = function() { 
      paste('area_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(areas_i(),file,row.names = F)
    })
  
  
  ################## Stop the shiny server
  ####################################################################################
  
  }

shinyApp(ui, server)