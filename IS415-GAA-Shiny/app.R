#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinycssloaders)

pacman::p_load(readxl, httr, jsonlite, maptools, sf, sfdep, raster, spatstat, spNetwork, rgdal, sp, tmap, tidyverse)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

busstop_sf <- st_read(dsn = "data/geospatial/busstop-022023-shp", layer="BusStop")
mpsz_sf <- st_read(dsn = "data/geospatial/MPSZ-2019-shp", layer="MPSZ-2019")
network_sf <- st_read(dsn = "data/geospatial/roadnetwork-022023-shp", layer="RoadSectionLine")
attr_sf <- st_read(dsn = "data/geospatial/tourism-shp", layer="TOURISM")
mrt_sf <- st_read(dsn = "data/geospatial/mrt-112022-shp", layer="Train_Station_Exit_Layer") 
hotel_sf <- st_read("data/geospatial/hotels-2021-kml/hotel-locations.kml")
sg_sf <- st_read(dsn = "data/geospatial/costal-shp", layer="CostalOutline")
mall_csv <- read_csv("data/geospatial/shoppingmalls-2019-csv/mall_coordinates_updated.csv")
mall_sf <- st_as_sf(mall_csv, coords = c("longitude", "latitude"), crs=4326)
airbnb_csv <- read_csv("data/geospatial/airbnb-2022-csv/listings.csv")
airbnb_sf <- st_as_sf(airbnb_csv, coords = c("longitude", "latitude"), crs=4326)
seveneles <- read_rds("data/geospatial/711stores-032023-xlsx/sevenele_rds.rds")
sevenele_sf <- st_as_sf(seveneles,
                        coords = c("longitude", 
                                   "latitude"),
                        crs=4326) %>%
  st_transform(crs = 3414)
unis <- read_rds("data/geospatial/universitiescolleges-2023-xlsx/unis_rds.rds")
unis_sf <- st_as_sf(unis,
                    coords = c("longitude", 
                               "latitude"),
                    crs=4326) %>%
  st_transform(crs = 3414)

attr_sf <- attr_sf %>% select(c(5))
busstop_sf <- busstop_sf %>% select(c(1))
hotel_sf <- hotel_sf %>% select(c(1))
mall_sf <- mall_sf %>% select(c(2))
mrt_sf <- mrt_sf %>% select(c(1))
sevenele_sf <- sevenele_sf %>% select(c(1))
unis_sf <- unis_sf %>% select(c(1))
airbnb_sf <- airbnb_sf %>% select(c(2,6,7,8))
network_sf <- network_sf %>% select(c(2))

hotel_sf <- st_zm(hotel_sf)
mpsz_sf <- st_make_valid(mpsz_sf)
sg_sf <- st_make_valid(sg_sf)

# with st_set_crs(), we can assign the appropriate ESPG Code
attr_sf <- st_set_crs(attr_sf, 3414)
busstop_sf <- st_set_crs(busstop_sf, 3414)
mrt_sf <- st_set_crs(mrt_sf, 3414)
network_sf <- st_set_crs(network_sf, 3414)
sg_sf <- st_set_crs(sg_sf, 3414)

# with st_transform(), we can change from one CRS to another
mpsz_sf <- st_transform(mpsz_sf, crs=3414)
hotel_sf <- st_transform(hotel_sf, crs=3414)
mall_sf <- st_transform(mall_sf, crs=3414)
airbnb_sf <- st_transform(airbnb_sf, crs=3414)

# sevenele_sf and unis_sf are in the correct CRS and ESPG code

airbnb <- as_Spatial(airbnb_sf)
mpsz <- as_Spatial(mpsz_sf)
sg <- as_Spatial(sg_sf)

airbnb_sp <- as(airbnb, "SpatialPoints")
sg_sp <- as(sg, "SpatialPolygons")

airbnb_ppp <- as(airbnb_sp, "ppp")
airbnb_ppp_jit <- rjitter(airbnb_ppp, 
                          retry=TRUE, 
                          nsim=1, 
                          drop=TRUE)

my_vars <- c("Airbnb", "Bus Stops", "MRT", "Tourist Attractions", "Hotels", "Malls", "7-11", "Universities")

# for SPPA K cross, L cross
attr_mp <- attr_sf
attr_mp$type <- "attraction"
attr_mp <- attr_mp %>% select(c(2,3))

busstop_mp <- busstop_sf
busstop_mp$type <- "busstop"
busstop_mp <- busstop_mp %>% select(c(2,3))

hotel_mp <- hotel_sf
hotel_mp$type <- "hotel"
hotel_mp <- hotel_mp %>% select(c(2,3))

mall_mp <- mall_sf
mall_mp$type <- "mall"
mall_mp <- mall_mp %>% select(c(2,3))

mrt_mp <- mrt_sf
mrt_mp$type <- "mrt"
mrt_mp <- mrt_mp %>% select(c(2,3))

sevenele_mp <- sevenele_sf
sevenele_mp$type <- "seveneleven"
sevenele_mp <- sevenele_mp %>% select(c(2,3))

unis_mp <- unis_sf
unis_mp$type <- "uni"
unis_mp <- unis_mp %>% select(c(2,3))

airbnb_mp <- airbnb_sf
airbnb_mp$type <- "airbnb"
airbnb_mp <- airbnb_mp %>% select(c(5,6))

all_sf <- do.call("rbind", list(attr_mp, busstop_mp, hotel_mp, mall_mp, mrt_mp, sevenele_mp, unis_mp, airbnb_mp))
all <- as_Spatial(all_sf)
all@data$type <- as.factor(all@data$type)
all_ppp <- as(all, "ppp")
all_ppp_jit <- rjitter(all_ppp, retry=TRUE, nsim=1, drop=TRUE)


# for NetSPPA
kallang_airbnb_sf <- filter(airbnb_sf, neighbourhood == "Kallang")
network_linestring <- read_rds("data/rds/network_linestring.rds")
lixels_rds <- read_rds("data/rds/lixels_rds.rds")
samples_rds <- read_rds("data/rds/samples_rds.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Navbar
  navbarPage("GeoNinjas",
             theme = bs_theme(bootswatch="lux"),
             tabPanel("Home",
                      imageOutput("logo"),
                      br(),
                      hr(),
                      br(),
                      h4(strong("Project Description")),
                      p(style="text-align: justify; font-size = 25px",
                        "Spatial point patterns analysis studies the distribution of the points, whether the distribution is random or clustered. This form of analysis can be very useful in the evaluation of events such as crime, traffic accidents, diseases, etc. Thus, we would be able to plan after the analysis and investigate whether there are any dependency relationships between different point distributions to make a comparison and conclusion.
                        "),
                      p(style="text-align: justify; font-size = 25px",
                        "Another kind of spatial point patterns analysis is called network constrained spatial point patterns analysis which allows us to analyse if the distribution of the spatial point events are affected by a network or whether the spatial point events occur alongside a network. 
                          To illustrate the functions of our Shiny application, we will use Airbnb as an example. 
                        "),
                      p(style="text-align: justify; font-size = 25px",
                        "For spatial point patterns analysis, we would like to find out if the Airbnb locations in Singapore are randomly distributed throughout the country and if not, where are the locations with higher concentrations of Airbnb. Also, at these locations of higher concentration, do the Airbnb locations co-exist with other point events like train stations, hotels, etc.? 
                        "),
                      p(style="text-align: justify; font-size = 25px",
                        "For network constrained spatial point patterns analysis, we would like to discover whether the distribution of the Airbnb locations are affected by the road network in Singapore. Through these analyses, we can investigate whether the distribution of Airbnb locations in Singapore are affected by point events or the road network.
                        "),
                      br(),
                      h4(strong("Project Motivation")),
                      p(style="text-align: justify; font-size = 25px",
                        "Geographical data is abundant online for users to use freely. However, many do not know what tool to use or know how to make use of those data since they are in different file formats like geojson, csv, shapefile and more.
                        "),
                      p(style="text-align: justify; font-size = 25px",
                        "The aim of our project is to create a Shiny web application that will enable users to upload their data and help them with the geographical analysis, in particular spatial point patterns analysis and analysis on geographical accessibility. As such, one does not need to be technically trained to do these types of analysis.
                        ")
             ),
             # tabPanel("Visualisation",
             #          titlePanel("Visualisation by Zone"),
             # 
             #          fluidRow(
             #            # Sidebar
             #            sidebarLayout(
             # 
             #              sidebarPanel(
             #                selectInput(
             #                  "PPPInput",
             #                  "Zone",
             #                  # label = ACTUAL backend input
             #                  choices = c("Central Region", "Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
             #                  selected = "Central Region",
             #                  multiple = FALSE
             #                )
             #              ),
             # 
             #              # Show a plot of the generated distribution
             #              mainPanel(
             #                tabsetPanel(type = "tabs",
             #                            tabPanel("Plot", withSpinner(plotOutput("PPPPlot", width="100%", height=400), type=2))
             #                            )
             #              )
             #            )
             #          )
             #  ),
             tabPanel("SPPA",
                      # Application title
                      titlePanel("Spatial Point Patterns Analysis"),
                      
                      fluidRow(
                        # Sidebar 
                        sidebarLayout(
                          
                          sidebarPanel(
                            selectInput(
                              "variableInput",
                              "Zone",
                              # label = ACTUAL backend input
                              choices = c("Central Region", "Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                              selected = "Central Region",
                              multiple = FALSE
                            ),
                            
                            ### K cross, L cross
                            conditionalPanel(
                              "input.tabSelected == 'KCross' || input.tabSelected == 'LCross'",
                              selectInput(
                                "SPPACrossFunc1",
                                "Variable Input A",
                                # label = ACTUAL backend input
                                choices = my_vars,
                                selected = "Airbnb",
                                multiple = FALSE
                              ),
                              selectInput(
                                "SPPACrossFunc2",
                                "Variable Input B",
                                # label = ACTUAL backend input
                                choices = my_vars,
                                selected = "MRT",
                                multiple = FALSE
                              )
                            ),
                            
                            ### K cross, L cross ^
                            
                            ### Func
                            conditionalPanel(
                              "input.tabSelected != 'KDE'",
                              numericInput(
                                "numSimulations",
                                "Number of Simulations (value from 1-999)",
                                value = 99,
                                min = 1,
                                max = 999
                              )
                            ),
                            ### Func ^
                            
                            ### KDE
                            conditionalPanel(
                              "input.tabSelected == 'KDE'",
                              selectInput(
                                "kernelSmoothingInput",
                                "Kernel Smoothing Input",
                                choices = c("Gaussian" = "gaussian",
                                            "Epanechnikov" = "epanechnikov",
                                            "Quartic" = "quartic",
                                            "Disc" = "disc"),
                                selected = "gaussian",
                                multiple = FALSE
                              ),
                              radioButtons(
                                "bandwidthMethod",
                                "Bandwidth Method",
                                choices = c("Auto",
                                            "Fixed",
                                            "Adaptive")
                              ),
                              conditionalPanel(
                                "input.bandwidthMethod == 'Auto'",
                                selectInput(
                                  "autobwInput",
                                  "Automatic Bandwidth Method",
                                  choices = c("bw.diggle",
                                              "bw.CvL",
                                              "bw.scott",
                                              "bw.ppl"),
                                  selected = "bw.diggle",
                                  multiple = FALSE
                                )
                              ),
                              conditionalPanel(
                                "input.bandwidthMethod == 'Fixed'",
                                sliderInput(
                                  "fixedbwInput",
                                  "Fixed Bandwidth Method (in km)",
                                  min = 0,
                                  max = 5,
                                  value = 2
                                )
                              )
                            ),
                            ### KDE ^
                            
                            actionButton("runAnalysis", "Run Analysis")
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Kernel Density Estimation", value = 'KDE', withSpinner(plotOutput("mapPlot", width="100%", height=400), type=2)),
                                        tabPanel("G Function", value = 'GFunction', withSpinner(plotOutput("GFunction", width="100%", height=400), type=2)),
                                        tabPanel("F Function", value = 'FFunction', withSpinner(plotOutput("FFunction", width="100%", height=400), type=2)),
                                        tabPanel("K Function", value = 'KFunction', withSpinner(plotOutput("KFunction", width="100%", height=400), type=2)),
                                        tabPanel("L Function", value = 'LFunction', withSpinner(plotOutput("LFunction", width="100%", height=400), type=2)),
                                        tabPanel("K Cross Function", value = 'KCross', withSpinner(plotOutput("KCross", width="100%", height=400), type=2)),
                                        tabPanel("L Cross Function", value = 'LCross', withSpinner(plotOutput("LCross", width="100%", height=400), type=2)),
                                        id = 'tabSelected')
                          )
                        )
                      ),
                      fluidRow(
                        br(),
                        textOutput("SPPAtext")
                      )
                      
                      
             ),
             tabPanel("NetSPPA",
                      # Application title
                      titlePanel("Network Constrained Spatial Point Patterns Analysis"),
                      
                      # Sidebar 
                      sidebarLayout(
                        
                        sidebarPanel(
                          conditionalPanel(
                            "input.NetSPPAtabSelected != 'crossKFunction'",
                            selectInput(
                              "NetSPPAvariableInput",
                              "Variable Input",
                              # label = ACTUAL backend input
                              choices = c("Airbnb"),
                              selected = "Airbnb",
                              multiple = FALSE
                            )
                          ),
                          
                          ### Func
                          conditionalPanel(
                            "input.NetSPPAtabSelected == 'KFunction'",
                            numericInput(
                              "NetSPPAnumSimulations",
                              "Number of Simulations (value from 1-999)",
                              value = 50,
                              min = 1,
                              max = 999
                            )
                          ),
                          ### Func ^
                          
                          ### KDE
                          conditionalPanel(
                            "input.NetSPPAtabSelected == 'KDE'",
                            selectInput(
                              "NetSPPAkernelSmoothingInput",
                              "Kernel Smoothing Input",
                              choices = c("Quartic" = "quartic",
                                          "Epanechnikov" = "epanechnikov",
                                          "Uniform" = "uniform",
                                          "Triangle" = "triangle",
                                          "Tricube" = "tricube",
                                          "Cosine" = "cosine",
                                          "Triweight" = "triweight",
                                          "Gaussian" = "gaussian",
                                          "Scaled Gaussian" = "scaled gaussian"
                              ),
                              selected = "quartic",
                              multiple = FALSE
                            ),
                            selectInput(
                              "NetSPPAmethod",
                              "Method",
                              choices = c("Simple" = "simple",
                                          "Discontinuous" = "discontinuous",
                                          "Continuous" = "continuous"),
                              selected = "simple",
                              multiple = FALSE
                            )
                          ),
                          ### KDE ^
                          
                          ### cross K
                          conditionalPanel(
                            "input.NetSPPAtabSelected == 'crossKFunction'",
                            selectInput(
                              "NetSPPAcrossK1",
                              "Variable Input A",
                              # label = ACTUAL backend input
                              choices = my_vars,
                              selected = "Airbnb",
                              multiple = FALSE
                            ),
                            selectInput(
                              "NetSPPAcrossK2",
                              "Variable Input B",
                              # label = ACTUAL backend input
                              choices = my_vars,
                              selected = "MRT",
                              multiple = FALSE
                            ),
                            numericInput(
                              "NetSPPAnumSimulations",
                              "Number of Simulations (value from 1-999)",
                              value = 50,
                              min = 1,
                              max = 999
                            )
                          ),
                          ### cross K ^
                          
                          actionButton("NetSPPArunAnalysis", "Run Analysis")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Kernel Density Estimation", value = 'KDE', withSpinner(plotOutput("NetKDEPlot", width="100%", height=400), type=2)),
                                      tabPanel("K Function", value = 'KFunction', withSpinner(plotOutput("NetSPPAKFunction", width="100%", height=400), type=2)),
                                      tabPanel("Network Cross K Function", value = "crossKFunction", withSpinner(plotOutput("crossKFunction", width="100%", height=400), type=2)),
                                      id = 'NetSPPAtabSelected')
                        )
                      )
             ),
             tabPanel("About Us")
             
  )
)



# Define server logic 
server <- function(input, output, session) {
  # use renderPlot() for STATIC map
  # PPPplot <- eventReactive(input$PPPInput, {
  # 
  #   zone <- switch(input$PPPInput,
  #                  "Central Region" = mpsz_sf[mpsz_sf$REGION_N == "CENTRAL REGION",],
  #                  "Kallang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "KALLANG",],
  #                  "Downtown Core" = mpsz_sf[mpsz_sf$PLN_AREA_N == "DOWNTOWN CORE",],
  #                  "Outram" = mpsz_sf[mpsz_sf$PLN_AREA_N == "OUTRAM",],
  #                  "Rochor" = mpsz_sf[mpsz_sf$PLN_AREA_N == "ROCHOR",],
  #                  "Jurong West" = mpsz_sf[mpsz_sf$PLN_AREA_N == "JURONG WEST",],
  #                  "Sembawang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "SEMBAWANG",],
  #                  "Pasir Ris" = mpsz_sf[mpsz_sf$PLN_AREA_N == "PASIR RIS",]
  #   )
  # 
  #   kl_owin <- zone %>%
  #     as('Spatial') %>%
  #     as('SpatialPolygons') %>%
  #     as('owin')
  #   all_ppp = all_ppp_jit[kl_owin]
  # 
  #   plot(all_ppp, cols=c("red", "orange", "darkgrey", "brown", "green", "violet", "blue", "black"))
  # },
  # ignoreNULL = FALSE
  # )
  # 
  # output$PPPPlot <- renderPlot({
  #   PPPplot()
  # })
  
  

  
  ######################## 1) SPPA ########################
  ### 1a) create KDE map
  KDEplot <- eventReactive(input$runAnalysis, {
    zone <- switch(input$variableInput,
                   "Central Region" = mpsz_sf[mpsz_sf$REGION_N == "CENTRAL REGION",],
                   "Kallang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "KALLANG",],
                   "Downtown Core" = mpsz_sf[mpsz_sf$PLN_AREA_N == "DOWNTOWN CORE",],
                   "Outram" = mpsz_sf[mpsz_sf$PLN_AREA_N == "OUTRAM",],
                   "Rochor" = mpsz_sf[mpsz_sf$PLN_AREA_N == "ROCHOR",],
                   "Jurong West" = mpsz_sf[mpsz_sf$PLN_AREA_N == "JURONG WEST",],
                   "Sembawang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "SEMBAWANG",],
                   "Pasir Ris" = mpsz_sf[mpsz_sf$PLN_AREA_N == "PASIR RIS",]
    )
    
    central_owin <- zone %>%
      as ('Spatial') %>%
      as ('SpatialPolygons') %>%
      as ('owin')
    airbnbSG_ppp = airbnb_ppp_jit[central_owin]
    airbnbSG_ppp.km <- rescale(airbnbSG_ppp, 1000, "km")
    
    if (input$bandwidthMethod == "Auto") {
      sigma <- switch(input$autobwInput,
                      "bw.diggle" = bw.diggle,
                      "bw.CvL" = bw.CvL,
                      "bw.scott" = bw.scott,
                      "bw.ppl" = bw.ppl
      )
      
      kde_airbnbSG.bw <- density(airbnbSG_ppp.km,
                                 sigma=sigma,
                                 edge=TRUE,
                                 kernel=input$kernelSmoothingInput)
      
    } else {
      if (input$bandwidthMethod == "Fixed") {
        kde_airbnbSG.bw <- density(airbnbSG_ppp.km,
                                   sigma=input$fixedbwInput,
                                   edge=TRUE,
                                   kernel=input$kernelSmoothingInput)
      } else {
        kde_airbnbSG.bw <- adaptive.density(airbnbSG_ppp.km,
                                            kernel=input$kernelSmoothingInput)
      }
    }
    gridded_kde_airbnbSG_bw <- as.SpatialGridDataFrame.im(kde_airbnbSG.bw)
    
    # convert grid output into raster
    kde_airbnbSG_bw_raster <- raster(gridded_kde_airbnbSG_bw)
    
    # assign projection systems
    projection(kde_airbnbSG_bw_raster) <- CRS("+init=EPSG:3414 +datum=WGS84 +units=km")
    
    mapex <- st_bbox(zone)
    
    tm_shape(zone) + 
      tm_borders(alpha=0.5) + 
      tm_shape(kde_airbnbSG_bw_raster, bbox=mapex) + 
      tm_raster("v") +
      tm_layout(legend.position = c("right", "bottom"), frame = FALSE) + 
      tm_view(set.zoom.limits = c(10,16))
  },
  ignoreNULL = FALSE
  )
  
  ### 1a) to OUTPUT KDE map
  output$mapPlot <- renderPlot({
    KDEplot()
  })
  
  ### 1b) to create SPPA CSR graphs for different functions
  soFunction <- eventReactive(input$runAnalysis, {
    zone <- switch(input$variableInput,
                   "Central Region" = mpsz_sf[mpsz_sf$REGION_N == "CENTRAL REGION",],
                   "Kallang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "KALLANG",],
                   "Downtown Core" = mpsz_sf[mpsz_sf$PLN_AREA_N == "DOWNTOWN CORE",],
                   "Outram" = mpsz_sf[mpsz_sf$PLN_AREA_N == "OUTRAM",],
                   "Rochor" = mpsz_sf[mpsz_sf$PLN_AREA_N == "ROCHOR",],
                   "Jurong West" = mpsz_sf[mpsz_sf$PLN_AREA_N == "JURONG WEST",],
                   "Sembawang" = mpsz_sf[mpsz_sf$PLN_AREA_N == "SEMBAWANG",],
                   "Pasir Ris" = mpsz_sf[mpsz_sf$PLN_AREA_N == "PASIR RIS",]
    )
    
    inputI <- switch(input$SPPACrossFunc1,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    
    inputJ <- switch(input$SPPACrossFunc2,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    central_owin <- zone %>%
      as ('Spatial') %>%
      as ('SpatialPolygons') %>%
      as ('owin')
    airbnbSG_ppp = airbnb_ppp_jit[central_owin]
    allSG_ppp = all_ppp_jit[central_owin]
    
    toplot <- switch(input$tabSelected,
                     "GFunction" = plot(envelope(airbnbSG_ppp, Gest, correction='all', nsim=input$numSimulations), main="Gest"),
                     "FFunction" = plot(envelope(airbnbSG_ppp, Fest, correction='all', nsim=input$numSimulations), main="Fest"),
                     "KFunction" = plot(envelope(airbnbSG_ppp, Kest, nsim=input$numSimulations, rank=1, glocal=TRUE), . - r ~ r, 
                                        xlab="d", ylab="K(d)-r", xlim=c(0,500), main="Kest"),
                     "LFunction" = plot(envelope(airbnbSG_ppp, Lest, nsim=input$numSimulations, rank=1, glocal=TRUE), . - r ~ r, 
                                        xlab="d", ylab="L(d)-r", xlim=c(0,500), main="Lest"),
                     "KCross" = plot(envelope(allSG_ppp, Kcross, i=inputI, j=inputJ, correction='border', nsim=input$numSimulations), xlab="distance(m)", xlim=c(0,500), main="Kcross"),
                     "LCross" = plot(envelope(allSG_ppp, Lcross, i=inputI, j=inputJ, correction='border', nsim=input$numSimulations), . - r ~ r, 
                                     xlab="distance(m)", xlim=c(0,500), main="Lcross"))
    
    toplot
  },
  ignoreNULL = FALSE
  )
  
  ### 1b) to OUTPUT G Function graph
  output$GFunction <- renderPlot({
    soFunction()
  })
  ### 1b) to OUTPUT F Function graph
  output$FFunction <- renderPlot({
    soFunction()
  })
  ### 1b) to OUTPUT K Function graph
  output$KFunction <- renderPlot({
    soFunction()
  })
  ### 1b) to OUTPUT L Function graph
  output$LFunction <- renderPlot({
    soFunction()
  })
  ### 1b) to OUTPUT K Cross Function graph
  output$KCross <- renderPlot({
    soFunction()
  })
  ### 1b) to OUTPUT L Cross Function graph
  output$LCross <- renderPlot({
    soFunction()
  })
  
  ######################## 1) SPPA ^ ########################
  
  ### Home Page logo
  output$logo <- renderImage({
    list(src = "images/logo.png",
         width = 650,
         height = 430,
         style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  
  ######################## NetSPPA reactive input ########################
  observe({
    if(!is.null(input$NetSPPAcrossK2))
      updateSelectInput(session, "NetSPPAcrossK1", 
                        choices = my_vars[!(my_vars %in% input$NetSPPAcrossK2)], 
                        selected = isolate(input$NetSPPAcrossK1) )
  })
  
  observe({
    if(!is.null(input$NetSPPAcrossK1))
      updateSelectInput(session, "NetSPPAcrossK2", 
                        choices = my_vars[!(my_vars %in% input$NetSPPAcrossK1)], 
                        selected = isolate(input$NetSPPAcrossK2) )
  })
  ######################## NetSPPA reactive input ^ ########################
  
  ######################## 2) NetSPPA ########################
  ### 2a) Create density map KDE
  densityMap <- eventReactive(input$NetSPPArunAnalysis, {
    var <- switch(input$NetSPPAvariableInput,
                  "Airbnb" = kallang_airbnb_sf
    )
    
    densities <- nkde(network_linestring, 
                      events = var,
                      w = rep(1,nrow(var)),
                      samples = samples_rds,
                      kernel_name = input$NetSPPAkernelSmoothingInput,
                      bw = 300, 
                      div= "bw", 
                      method = input$NetSPPAmethod, 
                      digits = 1, 
                      tol = 1,
                      grid_shape = c(1,1), 
                      max_depth = 8,
                      agg = 5, #we aggregate events within a 5m radius (faster calculation)
                      sparse = TRUE,
                      verbose = FALSE)
    
    samples_rds$density <- densities
    lixels_rds$density <- densities
    
    samples_rds$density <- samples_rds$density*1000
    lixels_rds$density <- lixels_rds$density*1000
    
    tmap_mode('view')
    tm_shape(lixels_rds)+
      tm_lines(col="density")+
      tm_shape(var)+
      tm_dots()
    
  },
  ignoreNULL = FALSE
  )
  
  ### 2a) Output NetKDE map
  output$NetKDEPlot <- renderPlot({
    densityMap()
  })
  
  ### 2b) create graph for K function
  NetSPPAkFun <- eventReactive(input$NetSPPArunAnalysis, {
    var <- switch(input$NetSPPAvariableInput,
                  "Airbnb" = kallang_airbnb_sf
                  
    )
    
    kfun_kallang_airbnb <- kfunctions(network_linestring, 
                                      var,
                                      start = 0, 
                                      end = 1000, 
                                      step = 50, 
                                      width = 50, 
                                      nsim = input$NetSPPAnumSimulations, 
                                      resolution = 50,
                                      verbose = FALSE, 
                                      conf_int = 0.05,
                                      agg = 40)
    
    plot(kfun_kallang_airbnb$plotk)
  },
  ignoreNULL = FALSE
  )
  
  ### 2b) output graph K function
  output$NetSPPAKFunction <- renderPlot({
    NetSPPAkFun()
  })
  
  
  ### 2c) create graph for cross K function
  crossKfun <- eventReactive(input$NetSPPArunAnalysis, {
    var1 <- switch(input$NetSPPAcrossK1,
                   "Airbnb" = kallang_airbnb_sf,
                   "Bus Stops" = busstop_sf, 
                   "MRT" = mrt_sf, 
                   "Tourist Attractions" = attr_sf, 
                   "Hotels" = hotel_sf, 
                   "Malls" = mall_sf, 
                   "7-11" = sevenele_sf, 
                   "Universities" = unis_sf
    )
    
    var2 <- switch(input$NetSPPAcrossK2,
                   "Airbnb" = kallang_airbnb_sf,
                   "Bus Stops" = busstop_sf, 
                   "MRT" = mrt_sf, 
                   "Tourist Attractions" = attr_sf, 
                   "Hotels" = hotel_sf, 
                   "Malls" = mall_sf, 
                   "7-11" = sevenele_sf, 
                   "Universities" = unis_sf
    )
    
    crossk_kallang <- cross_kfunctions(network_linestring, 
                                       var1, 
                                       var2, 
                                       start = 0, 
                                       end = 5000, 
                                       step = 50, 
                                       width = 1000, 
                                       nsim = input$NetSPPAnumSimulations, 
                                       verbose = FALSE, 
                                       agg = 100)
    
    plot(crossk_kallang$plotk)
  },
  ignoreNULL = FALSE
  )
  
  ### 2c) cross K function
  output$crossKFunction <- renderPlot({
    crossKfun()
  })
  
  ######################## 2) NetSPPA ^ ########################
  
}

# Run the application 
shinyApp(ui = ui, server = server)
