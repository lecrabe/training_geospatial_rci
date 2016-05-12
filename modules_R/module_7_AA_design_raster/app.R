#dashboard
####################################################################################
#######          Shiny app for accuracy assessment design       ####################
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

####################################################################################
#######          Read data and fixed parameters           ##########################
####################################################################################

expected_ua_hi <- 0.9
expected_ua_lo <- 0.5

print("Starting the process")

ui <- dashboardPage(skin='green',
  dashboardHeader(
    title= 'Steps of accuracy assessment',
    titleWidth = 350),
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
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Introduction",
               fluidRow(
                box(
                  title= "Description", status = "success", solidHeader= TRUE,
                  
                  "This is the accuracy assessment interactive design tool.",
                  br(),
                  "The objective of this tool is to provide a simple user interface for generating a probability sample dataset that can be used as reference data to complete an analysis of the accuracy of map data and adjusted area adjusted.",
                  br(),
                  "Click on the different steps one after the other in order.",
                  br(),  
                  "The inputs necessary are a raster map, in this raster each of the map classes will be treated as a strata for the accuracy assessment",
                  br(),  
                  "For optimized efficiency this application should be run through the SEPAL server.",
                  br(),  
                  p("Questions? send a message here", 
                    span("http://www.openforis.org/support/questions/ask/",style = "color:blue")
                  )
                  ),
                box(
                  title= "Background", status = "success", solidHeader= TRUE,collapsible=T,
                  "Use of accuracy assessment results for area estimation
                  The aim of the accuracy assessment is to characterize the frequency of errors (omission and commission) for each land cover class.
                  Differences in these two errors may be used to adjust area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class.
                  Adjusting area estimates on the basis of a rigorous accuracy assessment represents an improvement over simply reporting the areas of map classes."
                                  ),
                  # CSS style of the tabBox, making the color green
                  tags$style("
                  .nav-tabs-custom .nav-tabs li.active {
                      border-top-color: #00994d;
                  }"),
                tabBox(
                  title = "Background guide",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  #id = "tabset1", height = "250px",
                  #status = "success", solidHeader= TRUE,
                  tabPanel("References","GFOI MGD Module 2.7, Section 3.7 "),
                  tabPanel("Documents", "links to documents" )
                ),
                box(
                  title= "Acknowledgements", status = "success", solidHeader= TRUE,
                  "SEPAL, UNREDD, OpenForis, FAO, some disclaimer and logos "
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = 'Inputmap',
              fluidRow(
                box(title= "Required input", status = "success", solidHeader= TRUE,
                    "The map input must be located in the data/input folder and in raster format.",
                    "The input map can represent a single time or multiple times (change) made from satellite images or acquired from available map data of land cover or land use.",
                    br(),
                    selectInput('filename', label= 'Map data file name', list.files("data/input/")),
              #plotOutput('map'), <- doesn't work
              "The map classes from this raster will be used as strata in the design of the accuracy assessment"
              )
              )
      ),
      # Third tab content
      tabItem(tabName = 'Mapareas',
              h4("Map areas"),
              fluidRow(
                box(
                  status = "success",
                  "The pixel count for each of the map categories need to be calculated in order to calculate the overall and stratified sample size." ,
                  textOutput("info"),
                  tableOutput("maparea"),
                  plotOutput('map'),
                  downloadButton('downloadData0', 'Download the CSV with map areas')
              
#               plotOutput("mappie"),
#               plotOutput('map', click='plot_click'),
#               verbatimTextOutput("mapInfo"),
             # h4("Download the result"),
                )
              )
      ),
      # Forth tab content
      tabItem(tabName = 'Classes',
              fluidRow(
                box(
                  status = "success",collapsible=T,collapsed=T,
                  "Some classes are identified easier than other classes. Usually common classes, which occupy the majority of the map, are the easiest to identify. Rare classes, such as land change classes, which occupy a small portion of the map area, can be very difficult to identify.",
                  "Rare classes are expected to have the lower user accuracies and should be assigned a low confidence of 0.5.",
                  "Common classes are expected to have higher user accuracies and should be assigned a higher confidence of 0.9",
                  "This measure will influence the overall sample size. More classes with lower confidence will increase the overall sample size"),
                box(h4("Choose classes"),
                  #sidebarPanel(#h4("Choose the map classes to include in the sample size calculation"),
                  htmlOutput("selectUI_cat_hi"),
                  htmlOutput("selectUI_cat_lo")  
                  )
              )
      ),
      # Fifth tab content
      tabItem(tabName = 'Samplingsize',
              h4("Calculate sample size per class"),
              fluidRow(
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
              
              ## explain more about stratified sampling
              box(title= "Formula to calculate the overall sample size", status = "success", solidHeader= TRUE,
                  'The equation below to calculate an adequate overall sample size for stratified
                  random sampling that can then be distributed among the different strata.
                  N is number of units in the area of interest (number of overall pixels if the
                  spatial unit is a pixel), S(ohat) is the standard error of the estimated overall
                  accuracy that we would like to achieve, Wi is the mapped proportion of
                  area of class i, and Si is the standard deviation of stratum i.',
                  br(),
                  img(src="AA_equation1.PNG", height = 50, width = 100),
                  #br(),
                  #h4("Sample size"),
                  tableOutput("sample2"),
                  textInput("basename2", 
                            label = h3("Basename of csv to export"),                                      
                            value = paste("sampling_",Sys.Date(),sep="")),
                  downloadButton('downloadData2', label='Download csv with sample design')
              )
            )
      ),
      # Sixth tab content
      tabItem(tabName = 'Responsedesign',
                  # for the country code i could have a csv so this could be a dropdown task.
              fluidRow(
                box(title= "Create a stratified random sample on the map", status = "success", solidHeader= TRUE,
                "Points are randomly distributed for each of the map classes. The number of points per class is from the 'adjusted' column in the Sample Size tab",
                leafletOutput("plotxy") 
                    ),
                  box(
                    title= "Create a CSV with the sample points and attribute data", status = "success", solidHeader= TRUE,
                  "This CSV file can be input directly into Collect Earth to assign the class to the reference data.",
                    selectInput('countrycode',
                              'Choose country name', getData('ISO3')[,2], selectize=FALSE),
                  #               
                  #                         textInput("countrycode", 
                  #                                   label = h3("ISO Country code of study area")
                  #                                   ), 
                  textInput("basename", 
                            label = h3("Basename of Collect Earth file to export"),
                            value = paste("CE_",Sys.Date(),sep="")),
                  downloadButton('downloadData', 
                                 label='Download CollectEarth file')
                  )
                        
              )
        )
      )
    )
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
    
    ############### Load the raster corresponding to the selected name
    datafolder <- paste(c('data/input/', inputfile),collapse='')
    print(datafolder)
    dataname <- gsub(" ","",datafolder)
    print(dataname)
    lcmap <- raster(dataname) #lcmap <- raster(lcmap)
    
    
  })
  
  ##################################################################################################################################
  ############### Display the raster as a map 
  ## to do, add a legend to the map
  output$map <- renderPlot({
   # plot(lcmap(), axes=FALSE, legend=TRUE) # the legend does not work
  }) #?plot
#   # render text to make the map interactive and click to see the map values
#   output$mapInfo <- renderText({
#     map <- lcmap()
#     print(names(map))
#     paste0("Map value X = ", input$plot_click$names, "\n Map value y=", input$plot_click$values(map))
#   })
  ##################################################################################################################################
  ############### Compute the frequency of each map value in terms of pixels
  mapareaInput <- reactive({
    
    ############### Use multicore clusters to compute frequency
    #beginCluster()
    #print("Computing frequency values")
    #freq1 <- freq(lcmap(), progress='window') 
    inputfile <-input$filename
    if (is.null(input$filename))return(NULL)
    datafolder <- paste(c('data/input/', inputfile),collapse='')
    dataname <- gsub(" ","",datafolder)
    # using oft-stat command to speed up the process
    system(paste("oft-stat -i ",dataname," -o data/output/stats.txt -um ",dataname,sep=""))
    
    #print(freq1)
    #endCluster()
    
    ############### Compute resolution of the input raster -> not used for the moment
    #res <- (xmax(lcmap())-xmin(lcmap()))/lcmap()@ncols * 111000 * 111000
    
    ############### Output the result as a data.frame
    stats <- as.data.frame(read.table("data/output/stats.txt"))
    names(stats) <- c('Map_value', 'Pixel_count')
    stats<-arrange(stats,Map_value)
    write.csv(stats[1:2],"data/output/area.csv",row.names=F)
    stats[1:2]
  })
  
  ##################################################################################################################################
  ############### Display the data.frame as a table
  output$maparea <- renderTable({
    mapareaInput()
  },
  include.rownames=FALSE
  )

  
  ##################################################################################################################################
  ############### Display the map areas as a pie chart <-- improve this
#   output$mappie <- renderPlot({
#     maparea <- mapareaInput()
#     slices <- maparea$Pixel_count
#     pct <- round(slices/sum(slices)*100)
#     lbls <- maparea$Map_value
#     lbls <- paste(lbls, pct)
#     lbls <- paste(lbls,"%",sep="") # ad % to labels
#     pie(slices, labels=lbls)
#   })
  
  ##################################################################################################################################
  ############### Export the computed areas as a table    
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('maparea_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(mapareaInput(),file,row.names = F)
    })
  
  
  ####################################################################################
  ####### Step 2 : compute sampling given expected accuracy ##########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Select classes to be included with High expected User's Accuracy 
  output$selectUI_cat_hi <- renderUI({
    maparea <- mapareaInput()
    
    categories <- as.list(unique(maparea[1]))
    print(categories)
    selectInput("cat_hi",
                label = h5(paste("Classes to include with high confidence (Expected UA = 0.9)")),
                choices = categories,
                multiple = TRUE
    )
  })
  
  ##################################################################################################################################
  ############### Select classes to be included with Low expected User's Accuracy 
  output$selectUI_cat_lo <- renderUI({
    maparea <- mapareaInput()
    categories <- as.list(unique(maparea[1]))
    print(categories)
    selectInput("cat_lo",
                label = h5(paste("Classes to include with low confidence (Expected UA = 0.5)")),
                choices = categories,
                multiple = TRUE
    )
  })
  
  ##################################################################################################################################
  ############### Compute sample size as a reactive variable
  strat_sample <- reactive({
    
    maparea <- mapareaInput()
    
    ############### Read the inputs from the dropdown menu
    list_categories_hi <- input$cat_hi
    list_categories_lo <- input$cat_lo
    
    list_categories <- list_categories_hi
    list_categories <- list_categories_lo
    
    list_categories <- append(list_categories_hi,list_categories_lo)
    
    ############### Select only samples in selected list
    df <- maparea[maparea$Map_value %in% list_categories,]
    sumofmapcategories <- sum(df[,2])
    
    ############### Add a column for Weight (wi) expected Users accuracy (eua)
    df$wi <- df[,2]/sumofmapcategories
    df$eua <- 0
    
    df[df$Map_value %in% list_categories_hi,]$eua <- expected_ua_hi
    df[df$Map_value %in% list_categories_lo,]$eua <- expected_ua_lo
    df$si <- sqrt(df$eua*(1-df$eua))
    
    ############### Add a column for Standard Error and Weighted SE
    df$si <- sqrt(df$eua*(1-df$eua))
    df$wisi <- df$wi*df$si
    
    ############### Compute overall sampling size
    sum.wi.si <- sum(df$wisi)
    overallsample <- (sum.wi.si/input$expected_overall_accuracy)^2
    
    ############### Get minimum sample size for each strata
    minimum_ssize <- input$minsample
    
    ############### Compute equal,proportional and adjusted sampling repartition
    df$equal <- overallsample/nrow(df)
    
    df$proportional <- df$wi*overallsample
    
    df$min[ df$proportional < minimum_ssize ] <- minimum_ssize
    df$adjprop <- df$Pixel_count/(sum(df$Pixel_count[df$proportional >= minimum_ssize]))
    df$adjusted <- df$adjprop*(overallsample-sum(df$min, na.rm=T))
    df$adjusted[df$adjusted < minimum_ssize] <- minimum_ssize
    print(df)
    write.csv(df[,c(1,2,11)],"data/output/sampling.csv",row.names=F)
    write.table(df[,c(1,2,11)],"data/output/manual_sampling.txt",row.names=F)
    ############### Compute the total sample size
    #print(class(df))
    #df<- datatable(df, class= 'hover')
    df
    
  })    
  
  ##################################################################################################################################
  ############### Display the results of sampling within the UI
  output$sample2 <- renderTable({
    df<- strat_sample()
    df["Total" ,] <- colSums(df)
    df<-df[,c(1,2,7,8,11)]
    names(df)<- c('Map value', 'Pixel count', 'Equal', 'Proportional', 'Adjusted')
    print(class(df))
    df
  })
  
  ##################################################################################################################################
  ############### Allow download of the file
  output$downloadData2 <- downloadHandler(
    filename = function(){
      paste(input$basename2, ".csv",sep="")},
    content  = function(file){
      write.csv(strat_sample(),file,row.names=F)}
  )

##################################################################################################################################
############### Select classes to be included with High expected User's Accuracy 
output$selectManualSampling <- renderUI({
  fileInput("manualSampling",
              label = h5(paste("Choose the file with manual sampling points"))#,
              #choices = list.files("data/ouput/",pattern=".txt")
  )
})
  #?fileInput
  ####################################################################################
  ####### Step 3 : Generating Sampling points               ##########################
  ####################################################################################
  
  
  ##################################################################################################################################
  ############### Generate points 
  
  all_points <- reactive({
    rp <- strat_sample()[,c(1,2,11)]
    if(input$IsManualSampling == T){
      rp<-read.table(paste("data/output/",input$manualSampling$name,sep=""),header = T)
    }
    print(rp)
    map <- lcmap()
    
    beginCluster()
    ############### Generate 10x times the number of points from overall sample
    withProgress(
      message= 'Generating random points ', 
      value = 0, 
      {
        setProgress(value=.1)
        rand_sample <- data.frame(sampleRandom(map,(sum(rp$adjusted)*
                                                      10+ log((sum(rp$Pixel_count)))),xy=TRUE))
      }
    )
    
    names(rand_sample) <- c("x_coord","y_coord","Map_value")
    rand_sample$id     <- row(rand_sample)[,1]
    
    rp2 <- merge(rp,data.frame(table(rand_sample$Map_value)),by.x="Map_value",by.y="Var1",all.x=T)  
    rp2[is.na(rp2)]<-0
    print('rp2')
    print(rp2)
    
    ############### Create the list of classes that need to be specifically sampled
    to_rtp <- rp2[rp2$Freq <  rp2$adjusted,]$Map_value 
    print(to_rtp)
    ############### Create the list of classes that are enough represented in the random sampling
    to_spl <- rp2[rp2$Freq >= rp2$adjusted,]$Map_value
    print(to_spl)
    ############### Sample points from the first class
    i = 1
    
    final <- rand_sample[
      rand_sample$id
      %in%
        sample(
          rand_sample[rand_sample$Map_value %in% c(to_spl[i],to_rtp[i]),]$id,
          rp2[rp2$Map_value %in% c(to_spl[i],to_rtp[i]),]$adjusted
        ),]
    print('final')
    print(final)
    print(length(to_spl))
    ############### Loop into the well represented classes, sample and append
    if(length(to_spl) > 1){
      for(i in 2:length(to_spl)){
        tmp <- rand_sample[
          rand_sample$id
          %in%
            sample(
              rand_sample[rand_sample$Map_value == to_spl[i],]$id,
              rp2[rp2$Map_value == to_spl[i],]$adjusted
            ),]
        final <- rbind(final,tmp)
      }
    }
    print(length(to_rtp))
    
    ############### Loop into the subrepresented classes, raster_to_point then append
    if(length(to_rtp) > 0){
      for(i in 1:length(to_rtp)){
        withProgress(
          message= paste('Convert raster to point for rare class ',to_rtp[i],sep=""), 
          value = 0, 
          {
            setProgress(value=.1)
            tmp_rtp <- as.data.frame(rasterToPoints(map,fun=function(rast){rast==to_rtp[i]}))
          }
        )
        
        names(tmp_rtp) <- c("x_coord","y_coord","Map_value")
        tmp_rtp$id<-row(tmp_rtp)[,1]
        sampling <- min(rp2[rp2$Map_value == to_rtp[i],]$adjusted,
                        rp2[rp2$Map_value == to_rtp[i],]$Pixel_count)
        
        tmp<-tmp_rtp[tmp_rtp$id 
                     %in% 
                       sample(tmp_rtp[tmp_rtp$Map_value == to_rtp[i],]$id,
                              sampling
                       ),
                     ]
        final <- rbind(final,tmp)                              
      }
    }
    #print('finalfinal')
    #print(final)
    endCluster()
    #testtest <- table(final$Map_value)
    #print(testtest)
    final
  })
  
  
  
  ##################################################################################################################################
  ############### Create vector layer with the points
  spdf <- reactive({
    points <- all_points()
    
    sp_df<-SpatialPointsDataFrame(
      coords=points[,c(1,2)],
      data=data.frame(points[,c(3)]),
      proj4string=CRS("+proj=longlat +datum=WGS84"))
    print("ok for points shapefiled")
    sp_df
  })
  
  # can make a drop down list to choose country for displaying shape outline of country boundaries from GADM
  #match the coordinate systems for the sample points and the boundaries
  ##################################################################################################################################
  ############### Select input for ISO codes
  
  
  
  ##################################################################################################################################
  ############### Display the points                     
  output$plotxy  <-  renderLeaflet({
    dfa<-spdf()
    #coordinates(dfa) <- ~x_coord+y_coord
    #print(length(unique(dfa$Map_value)))
    #       print(names(dfa))
    #       print(head(dfa))
    #print(class(dfa))
    #print(head(dfa))
    #print(names(dfa))
    names(dfa)<- 'Map_value'
    #print(names(dfa))
    #df <- all_points()
    #dfa <- as.data.frame(dfa)
    #dfa$Map_value <- factor(sample.int(length(unique(dfa$Map_value))), nrow(dfa), TRUE)
    factpal <- colorFactor("Spectral", dfa$Map_value)
    #print(head(factpal))
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data = dfa, color= ~factpal(Map_value),
                       fillOpacity = 1,
                       radius = 1
                       )
      
    m
    #             p <- ggplot(dfa, aes(x_coord, y_coord))
    #             p + geom_point(aes(colour = factor(Map_value)))
    #print('display sample points...')
  })
  #?leaflet
  ##################################################################################################################################
  ############### Create the Collect Earth file
  CEfile <- reactive({
    print("reading CEFile loop")
    ################ Get the points
    coord.spdf <- spdf()
    points <- all_points()
    coord <- points[,c(1,2)]
    class <- points[,3]
    nsamples <- nrow(coord)
    ID <- matrix(sample(1:nsamples , nsamples , replace=F),nrow = nsamples , ncol =1, dimnames= list(NULL,c("ID")))
    YCOORD <- coord[,2]
    XCOORD <- coord[,1]
    
    ################ Get the country boundaries and admin info
    country <-  input$countrycode
    print(country)
    
    
    withProgress(
      message= 'Downloading country data 1', 
      value = 0, 
      {
        setProgress(value=.1)
        country <- getData('ISO3',path='www/')[,1][getData('ISO3',path='www/')[,2]==country]
      })
    withProgress(
      message= 'Downloading country data 2', 
      value = 0, 
      {
        setProgress(value=.1)
        adm <- getData ('GADM',path='www/', country= country, level=1)
      })
    
    proj4string(coord.spdf) <-proj4string(adm)
    adm1 <- over(coord.spdf, adm)
    
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
    
    ################ Get the GFC information for the point / Set to zero         
    #   gfc_treecover <- raster(gfc, band=1)
    #   GFC_TREE_COVER <- extract(gfc_treecover, cbind(coord[,1], coord[,2]))
    #   rm(gfc_treecover)
    #   gfc_gain <- raster(gfc, band=3)
    #   GFC_FOREST_GAIN <- extract(gfc_gain, cbind(coord[,1], coord[,2]))
    #   rm(gfc_gain)
    #   gfc_loss <- raster(gfc, band=2)
    #   GFC_FOREST_LOSS <- extract(gfc_loss, cbind(coord[,1], coord[,2]))
    #   rm(gfc_loss)
    #   gfc_lossyear <- raster(gfc, band=4)
    #   GFC_FOREST_LOSS_YEAR <- extract(gfc_lossyear, cbind(coord[,1], coord[,2]))
    #   rm(gfc_lossyear)
    #   gfc_mask <- raster(gfc, band=5)
    #   GFC_DATA_MASK <- extract(gfc_mask, cbind(coord[,1], coord[,2]))
    #   rm(gfc_mask)
    
    m$GFC_TREE_COVER  <-0
    m$GFC_FOREST_GAIN <-0
    m$GFC_FOREST_LOSS <-0
    m$GFC_FOREST_LOSS_YEAR <-0
    m$GFC_DATA_MASK <-0
    m$class <- class
    write.csv(m,"data/output/response.csv",row.names=F)
    m
  })
  
  ##################################################################################################################################
  ############### Enable to download the CE file
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$basename,".csv",sep="")},
    content  = function(file){
      to_export <- CEfile()
      write.csv(to_export,file,row.names=FALSE)}
  )
  
  
  ################## Stop the shiny server
  ####################################################################################
  
  }

shinyApp(ui, server)