#dashboard
####################################################################################
#######          Shiny app for Geospatialtoolkit demo           ####################
######    by remi.dannunzio@fao.org  and yelena.finegold@fao.org      ##############
####################################################################################

####################################################################################
#######          Set options and necessary packages       ##########################
####################################################################################

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
# packages(ggplot2)
# packages(xtable)
# packages(raster)
# packages(shiny)
# packages(shinydashboard)
# packages(dismo)
# packages(stringr)
# packages(snow)
# packages(plyr)
# packages(leaflet)
# packages(RColorBrewer)
# packages(DT)
# packages(RStoolbox)
# packages(rgdal)
library(RStoolbox)
library(shinydashboard)
library(raster)
library(rgdal)


####################################################################################
#######          User Interface definition, all tabs       #########################
####################################################################################

print("Starting the process")

ui <- dashboardPage(skin='blue',
         ################## Main dashboard header        
         dashboardHeader(
              title= 'Wrapper for the RStoolbox',
              titleWidth = 350),
            
         ################## Side bar where all the tools are
         dashboardSidebar(
              width = 350,
              sidebarMenu(
                menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
                menuItem('Supervised classification', tabName = 'rstb', icon = icon("map-marker")),
                menuItem('TO BE FILLED!', tabName = 'end', icon = icon("bar-chart"))
              )
            ),
         ################## Side bar where all the tools are
         dashboardBody(
              tabItems(

####################################################################################
##################          Introduction tab content       #########################
####################################################################################
              tabItem(tabName = "Introduction",
                       fluidRow(
                         
                        box(
                          title= "Description", status = "primary", solidHeader= TRUE,
                          "This is a shiny wrapper for the RStoolbox ",
                          br(),
                          "The objective of this tool is to provide a demo toolbox for the RSTB functionnalities without going straight into command lines",
                          br(),
                          "Choose the command you want to test on the left"
                          ),
                        
                        box(
                          title= "Background", status = "primary", solidHeader= TRUE,
                          "The OFGT was created a long time ago to fill a gap in our lives"
                                          ),
                          # CSS style of the tabBox, making the color green
                          tags$style("
                          .nav-tabs-custom .nav-tabs li.active {
                              border-top-color: #000000;
                          }"),
                        
                        box(
                          title= "Acknowledgements", status = "warning", solidHeader= TRUE,
                          "RSToolbox package by Benjamin Leutner <benjamin.leutner@uni-wuerzburg.de> "
                        )
                      )
              ),



####################################################################################
##################       Supervised classification RSTB    #########################
####################################################################################
          tabItem(tabName = 'rstb',
                  tabsetPanel(
                    tabPanel('1: Select input',    
                             mainPanel(
                               p("Select the raster map you want to process"),
                               br(),
                               selectInput('rstbinputname' , label= 'Input raster to classify', list.files("data/input/",pattern=".tif",recursive=TRUE)),
                               selectInput('rstbtrainingname' , label= 'Input training vector', list.files("data/input/",pattern=".shp",recursive=TRUE)),
                               htmlOutput("selectUI_attr"),
                               selectInput('rstb_model',
                                           'Choose the model you want to run',read.csv("www/caret_models.csv",sep="\t")[,2], selected="mlc"),
                               textInput('rstboutputname'  , label= 'Output classified raster',value="sp_classif")
                             )),
                    
                    tabPanel('2: Results',
                             mainPanel(
                               p("This step will do something be patient"),
                               h5("progressing..."),
                               textOutput('rstb_info_progress'),
                               h4("The classification"),
                               plotOutput('rstboutputmap')
                             ))
                  )
          ),  


####################################################################################
##################      !!!!!!!  TO BE CONTINUED !!!!!!    #########################
####################################################################################

        tabItem(tabName = 'end',
                      tabPanel('nothing')
              )
         
        ################## End of the list of TAB items               
        )
    ################## End of the dashboard body
    )
################## End of the User Interface
)

##################################################################################################################################
####################################################################################
##################      Server start -> the calculation    #########################
####################################################################################
##################################################################################################################################

server <- function(input, output) {    

####################################################################################
##################      Supervised classification          #########################
####################################################################################
training <- reactive({
  training   <-  input$rstbtrainingname
  trainingbase<-substr(training,0,nchar(training)-4)
  vector<-readOGR(dsn=paste("data/input/",training,sep=""),layer=trainingbase)
})

  
  output$selectUI_attr <- renderUI({
    shp <- training()
    
    categories <- names(shp@data)
    print(categories)
    selectInput("rstbattr",
                label = h5(paste("Attribute in the training data")),
                choices = categories,
                multiple = TRUE
    )
  })
  
############### Supervised classification with RSTB
rstb_tab6 <- reactive({
  
  ############### Use multicore clusters to compute frequency
  inputfile  <- input$rstbinputname
  model_name <- input$rstb_model
  attribute  <- input$rstbattr
#   print(inputfile)
#   setwd("~/Desktop/ShinyGeoToolkit_20160205/")
#    inputfile <- "group2.tif"
#    training <- "training_banstail_poly.shp"
#    model_name <- "mlc"
#    attribute <- "code"
  
  
  
  list_models<-read.csv("www/caret_models.csv",sep="\t")
  my_model <- model_name

  ############## Call raster imagery to classify
  satImage<-brick(paste("data/input/",inputfile,sep=""))
  
  ############## Create a mask
  withProgress(
    message= 'Creating a mask... ', 
    value = 0, 
    {
      mask <- raster(paste("data/input/",inputfile,sep=""),band=1) > 0
    })
  
  ############## Call vector data as training
  vector<-training()
  proj4string(vector)<-proj4string(satImage)
  table(vector@data[,attribute])
  #vector<-spTransform(vector,proj4string(satImage))
  ############## Perform classification
  beginCluster()
    
    withProgress(
    message= paste('Classification using ',my_model,sep=""), 
    value = 0, 
    {
      SC_df <- superClass(satImage,vector,responseCol=attribute,model = my_model, tuneLength = 1, trainPartition = 0.7)
    }
    )
  endCluster()
  print("classification done")
  ############## Masking out results
  withProgress(
    message= 'Masking out results ', 
    value = 0, 
    {
      out <- mask * SC_df$map
    })

  ############## Export data
  name_out <- paste("data/output/",input$rstboutputname,Sys.Date(),".tif",sep="")
  writeRaster(SC_df$map,name_out,overwrite=T)
  print("results exported")
  out

})

##################################################################################################################################
############### Display the classification raster

output$rstb_info_progress <- renderText({
  "..........-----..........."
})
output$rstboutputmap <- renderPlot({
  plot(rstb_tab6(), axes=FALSE)
})



####################################################################################
  ################## Stop the shiny server    
}

shinyApp(ui, server)