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
    title= 'Accuracy assessment design',
    titleWidth = 350),

    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
      menuItem('1: Input map', tabName = 'Inputmap', icon = icon("picture-o")),
      menuItem('2: Map areas', tabName = 'Mapareas', icon = icon("area-chart")),
      menuItem('3: Classes to include', tabName = 'Classes', icon = icon("map-marker")),
      menuItem('4: Sampling size', tabName = 'Samplingsize', icon = icon("bar-chart")),
      menuItem('5: Response design', tabName = 'Responsedesign', icon = icon("globe"))
      )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
    tabItems(
      
      ####################################################################################
      # New Tab
      tabItem(tabName = "Introduction",
               fluidRow(
                 
                 ####################################################################################
                 # New box
                 box(
                   title= "Description", status = "success", solidHeader= TRUE,
                   "This interactive tool creates accuracy assessment designs",
                   br(),
                   "The objective of this tool is to provide a simple user interface for generating a probability sample dataset that can be used as reference data to complete an analysis of the accuracy of map data and adjusted area estimates.",
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
                    tags$li("Select the map data which will be assessed. The required input for this exercise is a vector map."), 
                    tags$li("Verify that all classes with their areas are present"), 
                    tags$li("Select the expected accuracies of the classes"),
                    tags$li("Compute the sampling size"),
                    tags$li("Draw the sampling points and export as a Collect Earth file")
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
      tabItem(tabName = 'Inputmap',
              fluidRow(
                ####################################################################################
                # New box
                box(title= "Required input", status = "success", solidHeader= TRUE,
                    "The map input must be located in the ../input folder and in vector format.",
                    "The input map can represent a single time or multiple times (change) made from satellite images or acquired from available map data of land cover or land use.",
                    br(),
                    selectInput('filename', label= 'Map data file name', list.files("../input/",pattern=".shp")),
                    htmlOutput("selectUI_class_attr"),
                    checkboxInput("IsManualArea",label="Do you want to use a column of the shapefile for the areas ?"),
                    htmlOutput("selectUI_area_attr"),
              "The map classes from this shapefile will be used as strata in the design of the accuracy assessment"
              )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Mapareas',
              h4("Map areas"),
              fluidRow(
                ####################################################################################
                # New box
                box(
                  title= "Display as table", status = "success", solidHeader= TRUE,
                  "The areas for each of the map categories need to be calculated in order to calculate the overall and stratified sample size." ,
                  textOutput("info"),
                  tableOutput("maparea")),
                box(
                  title= "Display as pie chart and download", status = "success", solidHeader= TRUE,
                  plotOutput('pie'),
                  textInput("basename_area", 
                            label = h3("Basename of area file to export"),
                            value = paste("areas_",Sys.Date(),sep="")),
                  downloadButton('download_area', 'Download the CSV with map areas')
              
                )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Classes',
              fluidRow(
                ####################################################################################
                # New box
                box(title= "What are the expected accuracies ?", status = "success", solidHeader= TRUE,
                    collapsible=T,collapsed=T,
                  "Some classes are identified easier than other classes.",
                  "Usually common classes, which occupy the majority of the map, are the easiest to identify. ",
                  "Rare classes, such as land change classes, which occupy a small portion of the map area, can be very difficult to identify.",
                  tags$ul(
                    tags$li(htmlOutput("the_ex_ua_hi")),
                    tags$li(htmlOutput("the_ex_ua_lo"))
                  ),
                  
                  "This measure will influence the overall sample size. More classes with lower confidence will increase the overall sample size"),
                  
                ####################################################################################
                # New box
                box(h4("Choose classes expected user's accuracies"),
                    htmlOutput("selectUI_cat_hi"),
                    htmlOutput("selectUI_cat_lo")  
                ),
                
                
                ####################################################################################
                # New box
                box(h4("Expected User's Accuracy (EUA) values for specific classes"),
                    sliderInput("expected_ua_hi", 
                                label = h3("High expected user accuracy "),
                                min = 0.5, max = 1, value = 0.9),
                    sliderInput("expected_ua_lo", 
                                label = h3("Low expected user accuracy "),
                                min = 0.5, max = 1, value = 0.7)
                )
                
                
                
              )
      ),
      
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Samplingsize',
              h4("Calculate sample size per class"),
              fluidRow(
                ####################################################################################
                # New box
                box(title= "Sample size", status = "success", solidHeader= TRUE,
                'In the sampling design, the sample size for each map category is chosen to ensure that the sample size is large enough to produce sufficiently precise estimates of the area of the class (GFOI, 2013)',
                # Expected overall standard error for sampling design
                numericInput("expected_overall_accuracy", 
                             label = "Expected overall accuracy", 
                             value = 0.01, step = 0.005),
                #no data value in the map
                numericInput("minsample", 
                             label = "Minimum sample size per strata",
                             value = 100),
                checkboxInput("IsManualSampling",label="Do you want to modify the sampling size?"),
                htmlOutput("selectManualSampling")
              ),
              
              box(title= "Distribution of samples", status = "success", solidHeader= TRUE,
                  br(),
                  htmlOutput("overall_sampling_size"),
                  tableOutput("sampling_table"),
                  textInput("basename_sampling", 
                            label = h3("Basename of csv to export"),                                      
                            value = paste("sampling_",Sys.Date(),sep="")),
                  downloadButton('download_sampling', 
                                 label='Download csv with sample design')
              ),
              
              
              ####################################################################################
              # New box
              box(
                title= "Formula to calculate the overall sample size", 
                status = "success", 
                solidHeader= TRUE,
                collapsible = T,
                collapsed = T,
                  "The equation below calculates an adequate overall sample size for stratified
                  random sampling that can then be distributed among the different strata.",
                  br(),
                  tags$ul(
                    tags$li("N is number of units in the area of interest (number of overall pixels if the
                  spatial unit is a pixel, number of polygons if the spatial unit is a polygon)"),
                    tags$li("S(O) is the standard error of the estimated overall accuracy that we would like to achieve"),
                    tags$li("Wi is the mapped proportion of area of class i"),
                    tags$li("Si is the standard deviation of stratum i.")
                  ),
                  img(src="AA_equation1.PNG", height = 100, width = 330)
                  
                )
              
            )
      ),
      
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Responsedesign',
              
              ####################################################################################
              # Create a fluid row of boxes
              fluidRow(
                
                ####################################################################################
                # New box
                box(
                  title= "Create a stratified random sample on the map", status = "success", solidHeader= TRUE,
                "Points are randomly distributed for each of the map classes. The number of points per class is from the 'adjusted' column in the Sample Size tab",
                leafletOutput("plotxy") 
                    ),
                
                ####################################################################################
                # New box
                box(
                    title= "Create a CSV with the sample points and attribute data", status = "success", solidHeader= TRUE,
                  "This CSV file can be input directly into Collect Earth to assign the class to the reference data.",
                  
                    selectInput('countrycode',
                              'Choose country name', getData('ISO3')[,2]),
                  
                    textInput("basename_CE", 
                            label = h3("Basename of Collect Earth file to export"),
                            value = paste("CE_",Sys.Date(),sep="")),
                  
                    downloadButton('download_CE', 
                                 label='Download CollectEarth file')
                  )
                
                ####################################################################################
                # End of the fluidrow       
              )
        ####################################################################################
        # End of the tab
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
  
  ##################################################################################################################################    
  ############### Read the input raster data under reactive variable 'lcmap'  
  lcmap <- reactive({
    print("read data")
    ############### Read the name chosen from dropdown menu
    inputfile <-input$filename
    if (is.null(input$filename))return(NULL)
    #     inputfile<-"aa_test.shp" "Change2010_2014_Northtonlesap.shp"
    
    ############### Load the raster corresponding to the selected name
    dir <- "../input/"
    base <- substr(inputfile,0,nchar(inputfile)-4)
    withProgress(
      message= 'Reading the shapefile ', 
      value = 0, 
      {
        setProgress(value=.1)
        lcmap <-readOGR(dsn=paste(dir,inputfile,sep=""),layer=base)
        }
    )
    
  })
  
  ##################################################################################################################################    
  ############### Read the attribute of the shapefile  
  output$selectUI_class_attr <- renderUI({
    shp <- lcmap()
    
    categories <- names(shp@data)
    print(categories)
    selectInput("class_attribute",
                label = h5(paste("Attribute column for the class")),
                choices = categories,
                multiple = FALSE
    )
  })
 
  
  ##################################################################################################################################    
  ############### Read the attribute of the shapefile  
  output$selectUI_area_attr <- renderUI({
    if(input$IsManualArea == T){
    shp <- lcmap()
    
    categories <- names(shp@data)
    print(categories)
    selectInput("area_attribute",
                label = h5(paste("Attribute column for the areas")),
                choices = categories,
                multiple = FALSE
    )
    }
  })

  ##################################################################################################################################
  ############### Compute the areas of each class of the map
  mapareaInput <- reactive({
    
    ############### Read the data and the attribute for defining classes
    print("Compute map area calculation")
    shp<-lcmap()
    class_attr<-input$class_attribute
    
    legend <- levels(as.factor(shp@data[,class_attr]))
    
    ############### Either compute areas or read the defined column for areas 
    if(input$IsManualArea == T){
      area_attr <-input$area_attribute
      shp@data[,area_attr]<-as.numeric(shp@data[,area_attr])
      areas  <- tapply(shp@data[,area_attr],shp@data[,class_attr],sum)
    }
    else{
      areas  <- sapply(1:length(legend),function(x){gArea(shp[shp@data[,class_attr] == legend[x],])})
    }

    maparea <- data.frame(cbind(
                legend,
                1:length(legend),
                table(shp@data[,class_attr]),
                areas)
              )
    names(maparea)<-c("map_class","map_value","nb_poly","map_area")
    maparea$map_area <- as.numeric(maparea$map_area)
    write.csv(maparea,"../output/area.csv",row.names=F)
    
    ############### Output the result as a data.frame
    mapareaInput<-maparea[,c(1,4)]
    
  })

  
  ##################################################################################################################################
  ############### Display the area data.frame as a table
  output$maparea <- renderTable({
    mapareaInput()
  },
  include.rownames=FALSE
  )
  
  ##################################################################################################################################
  ############### Display the area as pie chart
  output$pie <- renderPlot({
    df <- mapareaInput()
    #df<-sampling
    pie(df$map_area,df$map_class)
  }) 
  
  ##################################################################################################################################
  ############### Export the computed areas as a table    
  output$download_area <- downloadHandler(
    filename = function(){
      paste(input$basename_area, ".csv",sep="")},
    
    content = function(file) {
      write.csv(mapareaInput(),file,row.names = F)
    })
  
  
  ####################################################################################
  ####### Step 2 : compute sampling given expected accuracy ##########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Display the message regarding expected user's accuracy
  ##################################################################################################################################
  output$the_ex_ua_lo <- reactive({
    paste(
      "Rare classes are expected to have the lower user accuracies and should be assigned a low confidence. Here the value chosen is ",
      input$expected_ua_lo,
      sep="")
  })
  
  
  output$the_ex_ua_hi <- reactive({
    paste(
      "Common classes are expected to have high user accuracies and should be assigned a higher confidence. Here the value chosen is ",
      input$expected_ua_hi,
      sep="")
  })
  
  ##################################################################################################################################
  ############### Select classes to be included with High expected User's Accuracy 
  ##################################################################################################################################
  output$selectUI_cat_hi <- renderUI({
    maparea <- mapareaInput()
    
    categories <- as.list(unique(maparea[1]))
    print(categories)
    selectInput("cat_hi",
                label = h5(paste("Classes to include with high confidence UA=",input$expected_ua_hi,sep="")),
                choices = categories,
                multiple = TRUE
    )
  })
  
  ##################################################################################################################################
  ############### Select classes to be included with Low expected User's Accuracy 
  ##################################################################################################################################
  output$selectUI_cat_lo <- renderUI({
    maparea <- mapareaInput()
    #maparea <- sampling
    categories <- as.list(unique(maparea[1]))
    print(categories)
    selectInput("cat_lo",
                label = h5(paste("Classes to include with low confidence, UA=",input$expected_ua_lo,sep="")),
                choices = categories,
                multiple = TRUE
    )
  })

  
  ##################################################################################################################################
  ############### Compute sample size as a reactive variable
  ##################################################################################################################################
  strat_sample <- reactive({
    
    maparea <- mapareaInput()
    
    ############### Read the inputs from the dropdown menu
    list_categories_hi <- input$cat_hi
    list_categories_lo <- input$cat_lo
    exp_overall        <- input$expected_overall_accuracy
    minimum_ssize      <- input$minsample
    expected_ua_hi     <- input$expected_ua_hi
    expected_ua_lo     <- input$expected_ua_lo
    
    list_categories <- list_categories_hi
    list_categories <- list_categories_lo
    
    list_categories <- append(list_categories_hi,list_categories_lo)
    
    ############### Select only samples in selected list
    df <- maparea[maparea$map_class %in% list_categories,]
    sumofmapcategories <- sum(df[,2])
    
    ############### Add a column for Weight (wi) expected Users accuracy (eua)
    df$wi <- df[,2]/sumofmapcategories
    df$eua <- 0
    
    df[df$map_class %in% list_categories_hi,]$eua <- expected_ua_hi
    df[df$map_class %in% list_categories_lo,]$eua <- expected_ua_lo
    df$si <- sqrt(df$eua*(1-df$eua))
    
    ############### Add a column for Standard Error and Weighted SE
    df$si <- sqrt(df$eua*(1-df$eua))
    df$wisi <- df$wi*df$si
    
    ############### Compute overall sampling size
    sum.wi.si <- sum(df$wisi)
    overallsample <- (sum.wi.si/exp_overall)^2
    
    ############### Compute equal,proportional and adjusted sampling repartition
    df$equal <- floor(overallsample/nrow(df))
    
    df$proportional <- floor(df$wi*overallsample)
    
    df$min[ df$proportional < minimum_ssize ] <- minimum_ssize
    df$adjprop  <- df$map_area/(sum(df$map_area[df$proportional >= minimum_ssize]))
    df$adjusted <- df$adjprop*(overallsample-sum(df$min, na.rm=T))
    df$adjusted[df$adjusted < minimum_ssize] <- minimum_ssize
    df$adjusted <- floor(df$adjusted)
    df$final    <- df$adjusted
    print(df)
    write.csv(df[,c(1,2,7,8,11,12)],"../output/sampling.csv",row.names=F)
    write.csv(df[,c(1,2,7,8,11,12)],"../output/manual_sampling.csv",row.names=F)

    ############### Compute the total sample size and distribution between classes
    df
    
  })    
  
  ############### Display the total sample size
  output$overall_sampling_size <- reactive({
    df <- strat_sample()
    size <- floor(sum(as.numeric(df[,11])))
    paste("The computed overall size is :  ",size,sep="")
    })

  ##################################################################################################################################
  ############### Waht if you want to manually edit the file ? 
  output$selectManualSampling <- renderUI({
    
    if(input$IsManualSampling == T){
      fileInput("manualSampling",
                label = h5(paste("Choose the file with manual sampling points"))
      )
    }
    
  })
  
  ##################################################################################################################################
  ############### Display the results of sampling within the UI
  output$sampling_table <- renderTable({
    if(input$IsManualSampling == T){
      df<-read.csv(paste("../output/",input$manualSampling$name,sep=""),header = T)
      }else{
      df<- strat_sample()
      df<-df[,c(1,2,7,8,11,12)]}
      
    
    names(df)<- c('Map Class', 'Map Area', 'Equal', 'Proportional', 'Intermediate','Final')
    print(class(df))
    df
  },include.rownames=FALSE,digits=0)
  
  ##################################################################################################################################
  ############### Allow download of the file
  output$download_sampling <- downloadHandler(
    filename = function(){
      paste(input$basename_sampling, ".csv",sep="")},
    content  = function(file){
      write.csv(strat_sample(),file,row.names=F)}
  )




  
  ####################################################################################
  ####### Step 3 : Generating Sampling points               ##########################
  ####################################################################################
  
  
  ##################################################################################################################################
  ############### Generate points 
  
    all_points <- reactive({
      #strat_sample<-df
      rp <- strat_sample()
      
      if(input$IsManualSampling == T){
        rp<-read.csv(paste("../output/",input$manualSampling$name,sep=""),header = T)
      }
      
      legend <- levels(as.factor(rp$map_class))
      
      shp<-lcmap()
      class_attr<-input$class_attribute
      
      i=1
        polys <- shp[shp@data[,class_attr] == legend[i],]
        pts<-spsample(polys,as.numeric(rp[rp$map_class == legend[i],]$adjusted),type="stratified")
        att_vec <- rep(legend[i],nrow(pts@coords))
        df_pts<-data.frame(cbind(pts@coords,att_vec))
    
      for(i in 2:length(legend)){
        polys <- shp[shp@data[,class_attr] == legend[i],]
        pts<-spsample(polys,as.numeric(rp[rp$map_class == legend[i],]$adjusted),type="stratified")
        att_vec <- rep(legend[i],nrow(pts@coords))
        tmp_pts<-data.frame(cbind(pts@coords,att_vec))
        df_pts<-rbind(df_pts,tmp_pts)
    
      }
    
        
        df_pts[,1]<-as.numeric(df_pts[,1])
        df_pts[,2]<-as.numeric(df_pts[,2])
        
        sp_df <- SpatialPointsDataFrame(
          coords=data.frame(df_pts[,c(1,2)]),
          data=data.frame(df_pts[,3]),
          proj4string=CRS(proj4string(shp))
          )
    
        all_points <- sp_df
      
    })
  
  ##################################################################################################################################
  ############### Create vector layer with the points
  sp_df <- reactive({
      sp_df <- spTransform(all_points(),"+init=epsg:4326")
    }) 
  

  ##################################################################################################################################
  ############### Display the points                     
  output$plotxy  <-  renderLeaflet({
    dfa<-sp_df()
    
    names(dfa)<- c('map_class')
    factpal   <- colorFactor("Spectral", dfa@data$map_class)
    
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data = dfa, color= ~ factpal(map_class),
                       fillOpacity = 1,
                       radius = 1
                       )
    m
  })
  
  ################################################################################################################################
  ############### Create the Collect Earth file
  CEfile <- reactive({
    print("reading CEFile loop")
    ################ Get the points
    sp_df<-sp_df()
    coord <- sp_df@coords
    map_code <- sp_df@data[,1]
    nsamples <- nrow(coord)
    ID <- matrix(sample(1:nsamples , nsamples , replace=F),nrow = nsamples , ncol =1, dimnames= list(NULL,c("ID")))
    YCOORD <- coord[,2]
    XCOORD <- coord[,1]
    
    ################ Get the country boundaries and admin info
    country_line <-  input$countrycode
    print(country_line)
    
    withProgress(
      message= 'Downloading country data 1', 
      value = 0, 
      {
        setProgress(value=.1)
        country <- country_line[1]
          #getData('ISO3',path='www/')[,1][getData('ISO3',path='www/')[,2]==country]
      })
    withProgress(
      message= 'Downloading country data 2', 
      value = 0, 
      {
        setProgress(value=.1)
        adm <- getData ('GADM',path='www/', country= country, level=1)
      })
    
    proj4string(sp_df) <- proj4string(adm)
    adm1 <- over(sp_df, adm)
    
    ################ Get the SRTM DEM information for the points
    withProgress(
      message= 'Downloading elevation data', 
      value = 0, 
      {
        elevation <- getData("alt",path='www/', country = country)
      })
    slope <- terrain(elevation, opt = "slope")
    aspect <- terrain(elevation, opt = "aspect")
    
    ELEVATION <- extract(elevation, cbind(coord[,1], coord[,2]))
    SLOPE <- extract(slope, cbind(coord[,1], coord[,2]))
    ASPECT <- extract(aspect, cbind(coord[,1], coord[,2]))
    
    rm(elevation)
    rm(slope)
    rm(aspect)
    
    ADM1_NAME <- adm1[,6]
    ADM1_NAME <- str_replace_all(ADM1_NAME,"[[:punct:]]","")
    COUNTRY <- adm1[,4]

    ################ Bind all vectors together in one matrix
    m <- as.data.frame(cbind(ID, YCOORD, XCOORD, ELEVATION, SLOPE, ASPECT, ADM1_NAME, COUNTRY))
    
    ################ Add the map code
    m$class <- map_code
    write.csv(m,"../output/response.csv",row.names=F)
    m
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file
  output$download_CE <- downloadHandler(
    filename = function(){
      paste(input$basename_CE,".csv",sep="")},
    content  = function(file){
      to_export <- CEfile()
      write.csv(to_export,file,row.names=FALSE)}
  )
  
  
  ################## Stop the shiny server
  ####################################################################################
  
  }

shinyApp(ui, server)