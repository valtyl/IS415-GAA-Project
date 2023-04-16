# THIS FILE

library(shiny)
library(bslib)
library(shinycssloaders)
library(readxl)
library(httr)
library(jsonlite)
library(maptools)
library(sf)
library(sfdep)
library(raster)
library(spatstat)
library(spNetwork)
library(rgdal)
library(sp)
library(tmap)
library(tidyverse)

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
busstop_sf <- st_intersection(busstop_sf, sg_sf)

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

kallang_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "KALLANG",]
kallang_owin <- kallang_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
kallang_airbnb_ppp <- airbnb_ppp_jit[kallang_owin]
kallang_airbnb_ppp.km <- rescale(kallang_airbnb_ppp, 1000, "km")

downtown_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "DOWNTOWN CORE",]
downtown_owin <- downtown_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
downtown_airbnb_ppp <- airbnb_ppp_jit[downtown_owin]
downtown_airbnb_ppp.km <- rescale(downtown_airbnb_ppp, 1000, "km")

outram_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "OUTRAM",]
outram_owin <- outram_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
outram_airbnb_ppp <- airbnb_ppp_jit[outram_owin]
outram_airbnb_ppp.km <- rescale(outram_airbnb_ppp, 1000, "km")

rochor_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "ROCHOR",]
rochor_owin <- rochor_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
rochor_airbnb_ppp <- airbnb_ppp_jit[rochor_owin]
rochor_airbnb_ppp.km <- rescale(rochor_airbnb_ppp, 1000, "km")

jurong_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "JURONG WEST",]
jurong_owin <- jurong_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
jurong_airbnb_ppp <- airbnb_ppp_jit[jurong_owin]
jurong_airbnb_ppp.km <- rescale(jurong_airbnb_ppp, 1000, "km")

sembawang_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "SEMBAWANG",]
sembawang_owin <- sembawang_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
sembawang_airbnb_ppp <- airbnb_ppp_jit[sembawang_owin]
sembawang_airbnb_ppp.km <- rescale(sembawang_airbnb_ppp, 1000, "km")

pasir_zone <- mpsz_sf[mpsz_sf$PLN_AREA_N == "PASIR RIS",]
pasir_owin <- pasir_zone %>% as ('Spatial') %>% as ('SpatialPolygons') %>% as ('owin')
pasir_airbnb_ppp <- airbnb_ppp_jit[pasir_owin]
pasir_airbnb_ppp.km <- rescale(pasir_airbnb_ppp, 1000, "km")


my_vars <- c("Airbnb", "Bus Stops", "MRT", "Tourist Attractions", "Hotels", "Malls", "7-11", "Universities")

# for SPPA K cross, L cross
attr_mp <- attr_sf
attr_mp$type <- "attraction"
attr_mp <- attr_mp %>% select(c(2,3))

busstop_mp <- busstop_sf
busstop_mp$type <- "busstop"
busstop_mp <- busstop_mp %>% select(c(6,7))

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

kallang_all_ppp <- all_ppp_jit[kallang_owin]
kallang_all_ppp_km <- rescale(kallang_all_ppp, 1000, "km")

downtown_all_ppp <- all_ppp_jit[downtown_owin]
downtown_all_ppp_km <- rescale(downtown_all_ppp, 1000, "km")

outram_all_ppp <- all_ppp_jit[outram_owin]
outram_all_ppp_km <- rescale(outram_all_ppp, 1000, "km")

rochor_all_ppp <- all_ppp_jit[rochor_owin]
rochor_all_ppp_km <- rescale(rochor_all_ppp, 1000, "km")

jurong_all_ppp <- all_ppp_jit[jurong_owin]
jurong_all_ppp_km <- rescale(jurong_all_ppp, 1000, "km")

sembawang_all_ppp <- all_ppp_jit[sembawang_owin]
sembawang_all_ppp_km <- rescale(sembawang_all_ppp, 1000, "km")

pasir_all_ppp <- all_ppp_jit[pasir_owin]
pasir_all_ppp_km <- rescale(pasir_all_ppp, 1000, "km")



# for NetSPPA
rochor_airbnb_sf <- filter(airbnb_sf, neighbourhood == "Rochor")
network_linestring <- read_rds("data/rds/rochor_network.rds")
lixels_rds <- read_rds("data/rds/rochor_lixels.rds")
samples_rds <- read_rds("data/rds/rochor_samples.rds")

rochor_attr <- read_rds("data/rds/rochor_attr.rds")
rochor_busstop <- read_rds("data/rds/rochor_busstop.rds")
rochor_hotel <- read_rds("data/rds/rochor_hotel.rds")
rochor_mall <- read_rds("data/rds/rochor_mall.rds")
rochor_mrt <- read_rds("data/rds/rochor_mrt.rds")
rochor_sevenele <- read_rds("data/rds/rochor_sevenele.rds")
rochor_unis <- read_rds("data/rds/rochor_unis.rds")


# UI
ui <- fluidPage(
  
  # Navbar
  navbarPage("GeoNinjas",
             theme = bs_theme(bootswatch="lux"),
             
             # Homepage Panel
             tabPanel("Home",
                      imageOutput("logo"),
                      br(),
                      hr(),
                      br(),
                      h4(strong("Project Description")),
                      p("Spatial point patterns analysis studies the distribution of the points, whether the distribution is random or clustered. This form of analysis can be very useful in the evaluation of events and we would be able to investigate whether there are any dependency relationships between different point distributions to make a comparison and conclusion."),
                      p("Another kind of spatial point patterns analysis is called network constrained spatial point patterns analysis which allows us to analyse if the distribution of the spatial point events are affected by a network or whether the spatial point events occur alongside a network. For our Shiny application, we will use Airbnbs as an example."),
                      p("For spatial point patterns analysis, we would like to find out if the Airbnb locations in Singapore are randomly distributed throughout the country and if not, where are the locations with higher concentrations of Airbnbs. Also, at these locations of higher concentration, do the Airbnb locations co-exist with other point events like train stations, hotels, etc.?"),
                      p("For network constrained spatial point patterns analysis, we would like to discover whether the distribution of the Airbnb locations are affected by the road network in Singapore. Through these analyses, we can investigate whether the distribution of Airbnb locations in Singapore are affected by point events or the road network."),
                      br(),
                      h4(strong("Project Motivation")),
                      p("Our team decided to develop an application that allows users to analyse Airbnb listings in Singapore and their relationships with other points of interest (MRTs, bus stops etc.) as there is a lack of such apps for Singapore Airbnb listings."),
                      p("After the COVID pandemic, with travelling becoming the norm and Singapore as one of the most popular cities to travel to in the world, Singapore Airbnb listing information would deem to be extremely valuable. Moreover, with the high cost of living in Singapore, many tourists would turn to cheaper accommodation options like Airbnb rentals."),
                      p("In order to provide a good accommodation experience for such tourists, it is crucial to analyse the current Airbnb listings in Singapore to study the regions where Airbnb listings are most prevalent and why."),
                      br(),
                      h4(strong("About our Application")),
                      p("Our application is focused on Airbnbs in Singapore and will assist users with two methods of Point Pattern Analysis:"),
                      tags$ul(
                        tags$li("Spatial Point Patterns Analysis (SPPA)"),
                        tags$li("Network-Constrained Point Patterns Analysis (NetSPPA)")
                      ),
                      br(),
                      p("For SPPA, users will be able to view the kernel density map of Airbnbs in different zones. Here are the zones chosen and the reasons why:"),
                      tags$ul(
                        tags$li("Kallang - Most Airbnbs (353 Airbnbs)"),
                        tags$li("Downtown Core - 2nd most Airbnbs (325 Airbnbs)"),
                        tags$li("Outram - 3rd most Airbnbs (255 Airbnbs)"),
                        tags$li("Rochor - 4th most Airbnbs (214 Airbnbs)"),
                        tags$li("Jurong West - High density of private rooms (43 Airbnbs)"),
                        tags$li("Sembawang - Medium density of shared and private rooms (28 Airbnbs)"),
                        tags$li("Pasir Ris - Small density of shared and private rooms (20 Airbnbs)")
                      ),
                      br(),
                      p("For NetSPPA, we will be focusing on the street network in Rochor. We chose Rochor as Rochor has a significant number of Airbnbs and each type of point events (Tourist Attractions, Bus Stops, Hotels, Shopping Malls, MRTs, 7-11s and Universities) are greater than 5, which will allow us to draw better statistical conclusions than the other zones with too little points. For example, Kallang only has 1 attraction and 1 university hence we will not be able to draw reliable statistical conclusions using Network Cross K-Function."),
                      br(),
                      p("To know more about how to use our application, ", tags$a(href="https://github.com/valtyl/IS415-GAA-Project/tree/master/others", "here"), " is our user guide!"),
                      br(),
                      h4(strong("Credits")),
                      imageOutput("smulogo"),
                      p("This project is done for IS415 Geospatial Analytics & Applications, a module in Singapore Management University with the guidance of Professor Kam Tin Seong."),
                      br(),
                      p("Done by:"),
                      tags$ul(
                        tags$li(tags$a(href="https://www.linkedin.com/in/valencia-tyl", "Tan Yan Lin, Valencia")),
                        tags$li(tags$a(href="https://www.linkedin.com/in/yashica-k", "Kumarapandian Yashicaramya")),
                        tags$li(tags$a(href="https://www.linkedin.com/in/derekpoh", "Derek Poh Yong Jie"))
                      ),
             ),
             
             # Visualisation Panel
             tabPanel("Visualisation",
                      titlePanel("Visualisation by Zone"),
                      
                      fluidRow(
                        # Sidebar
                        sidebarLayout(
                          
                          sidebarPanel(
                            selectInput(
                              "PPPInput",
                              "Zone",
                              # label = ACTUAL backend input
                              choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                              selected = "Kallang",
                              multiple = FALSE
                            )
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Plot",
                                       column(12,
                                              withSpinner(plotOutput("PPPPlot", width="100%", height=400), type=2)),
                                       p("Default zone is Kallang. To view the distribution of Airbnbs, Tourist Attractions, Bus Stops, Hotels, Shopping Malls, MRTs, 7-11s and Universities in another zone, simply select a different zone!"),
                                       p("A simple visualisation allows us to view the distribution of the point events, whether they are clustered, dispersed or at random with our eyes. However, if you would like to statistically prove the distribution, head over to the SPPA tab!")
                              )
                              
                            )
                          )
                        )
                      )
             ),
             tabPanel("Tmap",
                      titlePanel("Visualisation in whole SG"),
                      
                      withSpinner(tmapOutput("SGmap"), type=2),
                      
                      p("This map is for visualising Airbnbs, Tourist Attractions, Bus Stops, Hotels, Shopping Malls, MRTs, 7-11s and Universities in the whole of Singapore."),
                      p("Click on the layer to select and deselect the point events accordingly."),
                      p("Point events legend:"),
                      tags$ul(
                        tags$li("Airbnbs - airbnb_sf"),
                        tags$li("Tourist Attractions - attr_sf"),
                        tags$li("Bus Stops - busstop_sf"),
                        tags$li("Hotels - hotel_sf"),
                        tags$li("Shopping Malls - mall_sf"),
                        tags$li("MRTs - mrt_sf"),
                        tags$li("7-11s - sevenele_sf"),
                        tags$li("Universities - unis_sf")
                      ),
                      imageOutput("tmaplegend")
             ),
             tabPanel("SPPA",
                      # Application title
                      titlePanel("Spatial Point Patterns Analysis"),
                      
                      sidebarLayout(
                        sidebarPanel(fluid = TRUE, width = 3,
                                     
                                     # If KDE tabPanel is clicked, sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.SPPA_var === "SPPA Kernel Density Estimation"',
                                       selectInput(
                                         "SPPA_main_var",
                                         "Zone",
                                         choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                                         selected = "Kallang",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "SPPA_kernel",
                                         "Kernel Smoothing Input",
                                         choices = c("Gaussian" = "gaussian",
                                                     "Epanechnikov" = "epanechnikov",
                                                     "Quartic" = "quartic",
                                                     "Disc" = "disc"),
                                         selected = "gaussian",
                                         multiple = FALSE
                                       ),
                                       radioButtons(
                                         "SPPA_bandwidth_method",
                                         "Bandwidth Method",
                                         choices = c("Auto",
                                                     "Fixed",
                                                     "Adaptive"),
                                         selected = "Auto"
                                       ),
                                       conditionalPanel(
                                         condition = "input.SPPA_bandwidth_method == 'Auto'",
                                         selectInput(
                                           "SPPA_bw_auto_var",
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
                                         condition = "input.SPPA_bandwidth_method == 'Fixed'",
                                         sliderInput(
                                           "SPPA_bw_fixed_var",
                                           "Fixed Bandwidth Method (in km)",
                                           min = 0,
                                           max = 5,
                                           step = 0.1,
                                           value = 1
                                         )
                                       ),
                                       actionButton("SPPA_Run_KDE", "Run Analysis")
                                     ),
                                     
                                     # If G-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.SPPA_var === "SPPA G-Function"',
                                       selectInput(
                                         "SPPA_G_Main",
                                         "Zone",
                                         choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                                         selected = "Kallang",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "SPPA_G_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("SPPA_Run_Gfunc", "Run Analysis")
                                     ),
                                     
                                     # If F-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.SPPA_var === "SPPA F-Function"',
                                       selectInput(
                                         "SPPA_F_Main",
                                         "Zone",
                                         choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                                         selected = "Kallang",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "SPPA_F_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("SPPA_Run_Ffunc", "Run Analysis")
                                     ),
                                     
                                     # If Cross K-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.SPPA_var === "SPPA Cross K-Function"',
                                       selectInput(
                                         "SPPA_CrossK_Main",
                                         "Zone",
                                         choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                                         selected = "Kallang",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "SPPA_CrossK_V1",
                                         "Variable Input A",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "Airbnb",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "SPPA_CrossK_V2",
                                         "Variable Input B",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "MRT",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "SPPA_CrossK_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("SPPA_Run_Cross_Kfunc", "Run Analysis")
                                     ),
                                     
                                     # If Cross L-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.SPPA_var === "SPPA Cross L-Function"',
                                       selectInput(
                                         "SPPA_CrossL_Main",
                                         "Zone",
                                         choices = c("Kallang", "Downtown Core", "Outram", "Rochor", "Jurong West", "Sembawang", "Pasir Ris"),
                                         selected = "Kallang",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "SPPA_CrossL_V1",
                                         "Variable Input A",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "Airbnb",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "SPPA_CrossL_V2",
                                         "Variable Input B",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "MRT",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "SPPA_CrossL_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("SPPA_Run_Cross_Lfunc", "Run Analysis")
                                     )
                        ), # close sidebarPanel
                        
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    id = "SPPA_var",
                                    tabPanel("SPPA Kernel Density Estimation",
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default map to load.")),
                                                    p(em("Variable: Airbnbs in Kallang, Kernel: Gaussian, and Bandwidth Method: auto-bw.diggle are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(tmapOutput("SPPA_KDE_Map"), type=2),
                                                    tabsetPanel(
                                                      id = "SPPA_KDE_info",
                                                      tabPanel("About Spatial Kernel Density Estimation",
                                                               column(12,
                                                                      h4("What is Spatial Kernel Density Estimation?"),
                                                                      p("Kernel Density Estimation (KDE) is one of the most used density-based measures to estimate local density. It creates a grid in which each cell is assigned the density value of the kernel window centred on that cell. The density value is estimated by counting the number of objects/events in that kernel window."),
                                                                      h5("How to interpret the output?"),
                                                                      p("The v in the legend indicates the number of objects/events in the kernel window centred in each grid. The darker the colour of the area, the higher the intensity of points density in that area."),
                                                               ))))),
                                    
                                    tabPanel("SPPA G-Function",
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Variable: Airbnbs in Kallang and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("SPPA_G_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "SPPA_G_info",
                                                      tabPanel("About G-Function",
                                                               column(12,
                                                                      h4("What is G-Function?"),
                                                                      p("The G-function calculates the cumulative frequency distribution of the nearest neighbour distance of a point pattern."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("Null hypothesis: The Airbnbs in the Zone are randomly distributed."),
                                                                      p("1) If the observed G is above the envelope, it indicates that the Airbnbs in the Zone are clustered. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("2) If the observed G is below the envelope, it indicates that the Airbnbs in the Zone are dispersed. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("3) If the observed G is inside the envelope, it indicates that the Airbnbs in the Zone are randomly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                               ))))),
                                    
                                    tabPanel("SPPA F-Function", 
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Variable: Airbnbs in Kallang and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("SPPA_F_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "SPPA_F_info",
                                                      tabPanel("About F-Function",
                                                               column(12,
                                                                      h4("What is F-Function?"),
                                                                      p("The F-function first generates a few random points in the study area, and then it determines the minimum distance from each random point in P to any original points in the study area."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("Null hypothesis: The Airbnbs in the Zone are randomly distributed."),
                                                                      p("1) If the observed F is above the envelope, it indicates that the Airbnbs in the Zone are dispersed. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("2) If the observed F is below the envelope, it indicates that the Airbnbs in the Zone are clustered. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("3) If the observed F is inside the envelope, it indicates that the Airbnbs in the Zone are randomly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                               ))))),
                                    
                                    tabPanel("SPPA Cross K-Function", 
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Zone: Kallang, Main Variable: Airbnbs, Secondary Variable: MRTs and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("SPPA_Cross_K_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "SPPA_CrossK_info",
                                                      tabPanel("About Cross K-Function",
                                                               column(12,
                                                                      h4("What is Cross K-Function?"),
                                                                      p("Cross K-function measures the number of type A points up to a given distance from a type B point."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("Null hypothesis: The distribution of the two types of points are spatially independent."),
                                                                      p("1) If the observed K is above/below the envelope, for the distance where the observed K is above/below the envelope, it indicates that the two types of points are not spatially independent."),
                                                                      p("2) If the observed K is inside the envelope, for the distance where the observed K is inside the envelope, it indicates that the two types of points are spatially independent."),
                                                                      p("As long as the observed K lies within the envelope, we do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                                      p("If the observed K fully lies outside the envelope, we can reject the null hypothesis as the value is statistically significant."),
                                                               ))))),
                                    
                                    tabPanel("SPPA Cross L-Function", 
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Zone: Kallang, Main Variable: Airbnbs, Secondary Variable: MRTs and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("SPPA_Cross_L_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "SPPA_CrossL_info",
                                                      tabPanel("About Cross L-Function",
                                                               column(12,
                                                                      h4("What is Cross L-Function?"),
                                                                      p("Cross L-function measures the number of type A points up to a given distance from a type B point. Cross L-Function is the standardised version of Cross K-Function hence the dotted red line lies at 0."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("Null hypothesis: The distribution of the two types of points are spatially independent."),
                                                                      p("1) If the observed L is above/below the envelope, for the distance where the observed L is above/below the envelope, it indicates that the two types of points are not spatially independent."),
                                                                      p("2) If the observed L is inside the envelope, for the distance where the observed L is inside the envelope, it indicates that the two types of points are spatially independent."),
                                                                      p("As long as the observed L lies within the envelope, we do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                                      p("If the observed L fully lies outside the envelope, we can reject the null hypothesis as the value is statistically significant."),
                                                                      
                                                               ))))),
                                    
                                  ) # close tabsetPanel
                                  
                        ) # close mainPanel
                      )), # close SPPA tabPanel
             
             # NetSPPA Panel
             tabPanel("NetSPPA",
                      titlePanel("Network Constrained Spatial Point Patterns Analysis"),
                      sidebarLayout(
                        sidebarPanel(fluid = TRUE, width = 3,
                                     
                                     # If KDE tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.NetSPPA_var === "NetSPPA Kernel Density Estimation"',
                                       selectInput(
                                         "NetSPPA_main_var",
                                         "Variable Input",
                                         # label = ACTUAL backend input
                                         choices = c("Airbnb", "Tourist Attraction", "Bus Stop", "Hotel", "Mall", "MRT", "7-11", "University"),
                                         selected = "Airbnb",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "NetSPPA_kernel",
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
                                         "NetSPPA_method",
                                         "Method",
                                         choices = c("Simple" = "simple",
                                                     "Discontinuous" = "discontinuous",
                                                     "Continuous" = "continuous"),
                                         selected = "simple",
                                         multiple = FALSE
                                       ),
                                       actionButton("NetSPPA_Run_KDE", "Run Analysis")
                                     ),
                                     
                                     # If K-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.NetSPPA_var === "NetSPPA K-Function"',
                                       selectInput(
                                         "NetSPPA_K_Main",
                                         "Variable Input",
                                         # label = ACTUAL backend input
                                         choices = c("Airbnb", "Tourist Attraction", "Bus Stop", "Hotel", "Mall", "MRT", "7-11", "University"),
                                         selected = "Airbnb",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "NetSPPA_K_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("NetSPPA_Run_Kfunc", "Run Analysis")
                                     ),
                                     
                                     # If Cross K-Function tabPanel is clicked, the sidebarPanel below will be shown
                                     conditionalPanel(
                                       'input.NetSPPA_var === "NetSPPA Cross K-Function"',
                                       selectInput(
                                         "NetSPPA_CrossK_V1",
                                         "Variable Input A",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "Airbnb",
                                         multiple = FALSE
                                       ),
                                       selectInput(
                                         "NetSPPA_CrossK_V2",
                                         "Variable Input B",
                                         # label = ACTUAL backend input
                                         choices = my_vars,
                                         selected = "MRT",
                                         multiple = FALSE
                                       ),
                                       numericInput(
                                         "NetSPPA_CrossK_No_Simulations",
                                         "Number of Simulations (value from 1-999)",
                                         value = 99,
                                         step = 1,
                                         min = 1,
                                         max = 999
                                       ),
                                       actionButton("NetSPPA_Run_Cross_Kfunc", "Run Analysis")
                                     )
                                     
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    id = "NetSPPA_var",
                                    tabPanel("NetSPPA Kernel Density Estimation",
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default map to load.")),
                                                    p(em("Variable: Airbnbs in Rochor, Kernel: Quartic and Method: Simple are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(tmapOutput("NetSPPA_KDE_Map"), type=2),
                                                    tabsetPanel(
                                                      id = "NetSPPA_KDE_info",
                                                      tabPanel("About Network-Constrained Kernel Density Estimation",
                                                               column(12,
                                                                      h4("What is Network-Constrained Kernel Density Estimation?"),
                                                                      p("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a two-dimensional space, which is not suitable for analysing density of events occuring on a network. Therefore, the modified Network-Constrained Kernel Density Estimation is used to calculate density of events occuring along the edges of a network."),
                                                                      h5("How to interpret the output?"),
                                                                      p("The road segments of darker colour have relatively higher density of point events than the road segments of lighter colour."),
                                                               ))))),
                                    
                                    tabPanel("NetSPPA K-Function",
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Variable: Airbnbs in Rochor and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("NetSPPA_K_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "NetSPPA_K_info",
                                                      tabPanel("About K-Function",
                                                               column(12,
                                                                      h4("What is K-Function?"),
                                                                      p("K-function measures the number of events found up to a given distance of any particular event, and the graph helps illustrate the spatial dependence (clustering or dispersion) of point features over a wide range of distances (m)."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("Null hypothesis: The distribution of point events are uniformly distributed over the street network in Rochor"),
                                                                      p("1) If the empirical network K-function of the point events in Rochor is above the envelope, it indicates that the point events in Rochor are more clustered than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("2) If the empirical network K-function of the point events in Rochor is below the envelope, it indicates that the point events in Rochor are more dispersed than what we can expect from a random distribution. We can reject the null hypothesis as the value is statistically significant."),
                                                                      p("3) If the empirical network K-function of the point events in Rochor is inside the envelope, it indicates that the point events in Rochor are uniformly distributed. We do not have enough evidence to reject the null hypothesis as the value is not statistically significant."),
                                                               ))))),
                                    tabPanel("NetSPPA Cross K-Function", 
                                             column(12,
                                                    h6(strong("Note:")),
                                                    p(em("Please wait a short while for the default graph to load.")),
                                                    p(em("Main Variable: Airbnbs in Rochor, Secondary Variable: MRTs and Number of Simulations: 99 are used to plot the default map, select alternative choices and click on 'Run Analysis' to update the map.")),
                                                    withSpinner(plotOutput("NetSPPA_Cross_K_Function"), type=2),
                                                    tabsetPanel(
                                                      id = "NetSPPA_CrossK_info",
                                                      tabPanel("About Cross K-Function",
                                                               column(12,
                                                                      h4("What is Cross K-Function?"),
                                                                      p("An extension of K-function, the Cross K-function measures the number of main point events (A) around a set of secondary point events (B), and again the graph helps illustrates the spatial dependence (clustering or dispersion) of the point A features around point B features over a wide range of distances (m)."),
                                                                      h5("How to interpret the graph?"),
                                                                      p("1) If the empirical cross K-function is above the envelope, point A features tend to be clustered around point B features for the distances where the empirical cross K-function is above the envelope."),
                                                                      p("2) If the empirical cross K-function is below the envelope, point A features tend to be dispersed around point B features for the distances where the empirical cross K-function is below the envelope."),
                                                                      p("3) If the empirical cross K-function is within the envelope, point A features tend to be randomly located around point B features for the distances where the empirical cross K-function is within the envelope."),
                                                               )))))
                                    
                                  ) # close tabsetPanel
                        ) # close mainPanel
                      ) # close sidebarLayout
             ) # close NetSPPA Panel
  ) # close navbar Page
) # close fluid page

# Define server logic 
server <- function(input, output, session) {
  # use renderPlot() for STATIC map
  
  ######################## Visualisation ########################
  PPPplot <- eventReactive(input$PPPInput, {
    
    all_ppp <- switch(input$PPPInput,
                      "Kallang" = kallang_all_ppp,
                      "Downtown Core" = downtown_all_ppp,
                      "Outram" = outram_all_ppp,
                      "Rochor" = rochor_all_ppp,
                      "Jurong West" = jurong_all_ppp,
                      "Sembawang" = sembawang_all_ppp,
                      "Pasir Ris" = pasir_all_ppp
    )
    
    plot(all_ppp, cols=c("red", "orange", "darkgrey", "brown", "green", "violet", "blue", "black"))
  },
  ignoreNULL = FALSE
  )
  
  output$PPPPlot <- renderPlot({
    PPPplot()
  })
  ######################## Visualisation ^ ########################
  
  ######################## Tmap ########################
  SGmap_Function <- isolate(tm_shape(mpsz_sf) +
                              tm_borders(alpha = 0.5) +
                              tmap_options(check.and.fix = TRUE) +
                              tm_layout(legend.outside = TRUE, frame = FALSE, title='SG Map') +
                              tm_shape(airbnb_sf) +
                              tm_dots(col='red', size = 0.01, alpha = 1) +
                              tm_shape(attr_sf) +
                              tm_dots(col='orange', size = 0.01, alpha = 0.5) + 
                              tm_shape(busstop_sf) +
                              tm_dots(col='yellow', size = 0.01, alpha = 0.4) +
                              tm_shape(sevenele_sf) +
                              tm_dots(col='green', size = 0.01, alpha = 0.5) + 
                              tm_shape(mall_sf) +
                              tm_dots(col='blue', size = 0.01, alpha = 0.5) +
                              tm_shape(hotel_sf) +
                              tm_dots(col='purple', size = 0.01, alpha = 0.5) + 
                              tm_shape(unis_sf) +
                              tm_dots(col='black', size = 0.01, alpha = 0.5) +
                              tm_shape(mrt_sf) +
                              tm_dots(col='violet', size = 0.01, alpha = 0.5) +
                              tm_view(set.zoom.limits = c(11,16)))
  
  output$SGmap <- renderTmap({
    SGmap_Function
  })
  
  ######################## Tmap ^ ########################
  
  
  ######################## 1) SPPA ########################
  ### 1a) SPPA KDE map
  KDEplot <- eventReactive(input$SPPA_Run_KDE, {
    zone <- switch(input$SPPA_main_var,
                   "Kallang" = kallang_zone,
                   "Downtown Core" = downtown_zone,
                   "Outram" = outram_zone,
                   "Rochor" = rochor_zone,
                   "Jurong West" = jurong_zone,
                   "Sembawang" = sembawang_zone,
                   "Pasir Ris" = pasir_zone
    )
    
    central_owin <- zone %>%
      as ('Spatial') %>%
      as ('SpatialPolygons') %>%
      as ('owin')
    airbnbSG_ppp = airbnb_ppp_jit[central_owin]
    airbnbSG_ppp.km <- rescale(airbnbSG_ppp, 1000, "km")
    
    
    if (input$SPPA_bandwidth_method == "Auto") {
      sigma <- switch(input$SPPA_bw_auto_var,
                      "bw.diggle" = bw.diggle,
                      "bw.CvL" = bw.CvL,
                      "bw.scott" = bw.scott,
                      "bw.ppl" = bw.ppl
      )
      
      kde_airbnbSG.bw <- density(airbnbSG_ppp.km,
                                 sigma=sigma,
                                 edge=TRUE,
                                 kernel=input$SPPA_kernel)
      
    } else {
      if (input$SPPA_bandwidth_method == "Fixed") {
        kde_airbnbSG.bw <- density(airbnbSG_ppp.km,
                                   sigma=input$SPPA_bw_fix_var,
                                   edge=TRUE,
                                   kernel=input$SPPA_kernel)
      } else {
        kde_airbnbSG.bw <- adaptive.density(airbnbSG_ppp.km,
                                            kernel=input$SPPA_kernel)
      }
    }
    gridded_kde_airbnbSG_bw <- as.SpatialGridDataFrame.im(kde_airbnbSG.bw)
    
    # convert grid output into raster
    kde_airbnbSG_bw_raster <- raster(gridded_kde_airbnbSG_bw)
    
    # assign projection systems
    projection(kde_airbnbSG_bw_raster) <- CRS("+init=EPSG:3414 +datum=WGS84 +units=km")
    
    tmap_mode("view")
    tm_shape(zone) + 
      tm_borders(col = 'black',
                 lwd = 1,
                 alpha = 1.0) + 
      tm_shape(kde_airbnbSG_bw_raster) + 
      tm_raster("v") +
      tm_layout(legend.position = c("right", "bottom"), frame = FALSE, title="KDE") +
      tm_view(set.zoom.limits = c(13,16))
  },
  ignoreNULL = FALSE
  )
  
  ### 1a) to OUTPUT KDE map
  output$SPPA_KDE_Map <- renderTmap({
    KDEplot()
  })
  
  ### 1b) SPPA G Function 
  g_main <- reactive({
    if (input$SPPA_G_Main == "Kallang") {
      dataset <- kallang_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Downtown Core") {
      dataset <- downtown_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Outram") {
      dataset <- outram_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Rochor") {
      dataset <- rochor_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Jurong West") {
      dataset <- jurong_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Sembawang") {
      dataset <- sembawang_airbnb_ppp
    }
    else if (input$SPPA_G_Main == "Pasir Ris") {
      dataset <- pasir_airbnb_ppp
    }
    return(dataset)
  })
  
  G_Function <- eventReactive(input$SPPA_Run_Gfunc, {
    g_func.csr <- isolate(envelope(g_main(), 
                                   Gest, 
                                   correction='all',
                                   nsim = input$SPPA_G_No_Simulations))
    plot(g_func.csr, main="Gest")
  }, ignoreNULL = FALSE)
  
  output$SPPA_G_Function <- renderPlot({
    G_Function()
  })
  
  ### 1c) SPPA F Function Graph
  f_main <- reactive({
    if (input$SPPA_F_Main == "Kallang") {
      dataset <- kallang_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Downtown Core") {
      dataset <- downtown_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Outram") {
      dataset <- outram_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Rochor") {
      dataset <- rochor_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Jurong West") {
      dataset <- jurong_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Sembawang") {
      dataset <- sembawang_airbnb_ppp
    }
    else if (input$SPPA_F_Main == "Pasir Ris") {
      dataset <- pasir_airbnb_ppp
    }
    return(dataset)
  })
  
  F_Function <- eventReactive(input$SPPA_Run_Ffunc, {
    f_func.csr <- isolate(envelope(f_main(), 
                                   Fest, 
                                   correction='all',
                                   nsim = input$SPPA_F_No_Simulations))
    plot(f_func.csr, main="Fest")
  }, ignoreNULL = FALSE)
  
  output$SPPA_F_Function <- renderPlot({
    F_Function()
  })
  
  ### 1d) SPPA K Cross Function Graph
  Kcross_main <- reactive({
    if (input$SPPA_CrossK_Main == "Kallang") {
      dataset <- kallang_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Downtown Core") {
      dataset <- downtown_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Outram") {
      dataset <- outram_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Rochor") {
      dataset <- rochor_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Jurong West") {
      dataset <- jurong_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Sembawang") {
      dataset <- sembawang_all_ppp
    }
    else if (input$SPPA_CrossK_Main == "Pasir Ris") {
      dataset <- pasir_all_ppp
    }
    return(dataset)
  })
  
  Cross_K_Function <- eventReactive(input$SPPA_Run_Cross_Kfunc, {
    inputI <- switch(input$SPPA_CrossK_V1,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    
    inputJ <- switch(input$SPPA_CrossK_V2,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    crossk.csr <- isolate(envelope(Kcross_main(), 
                                   Kcross, 
                                   i=inputI, j=inputJ, 
                                   correction='border', 
                                   nsim=input$SPPA_CrossK_No_Simulations))
    
    plot(crossk.csr, xlab="distance(m)", xlim=c(0,1000), main="Kcross")
  }, ignoreNULL = FALSE)
  
  output$SPPA_Cross_K_Function <- renderPlot({
    Cross_K_Function()
  })
  
  
  ### 1e) to create L Cross Function Graph
  Lcross_main <- reactive({
    if (input$SPPA_CrossL_Main == "Kallang") {
      dataset <- kallang_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Downtown Core") {
      dataset <- downtown_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Outram") {
      dataset <- outram_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Rochor") {
      dataset <- rochor_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Jurong West") {
      dataset <- jurong_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Sembawang") {
      dataset <- sembawang_all_ppp
    }
    else if (input$SPPA_CrossL_Main == "Pasir Ris") {
      dataset <- pasir_all_ppp
    }
    return(dataset)
  })
  
  Cross_L_Function <- eventReactive(input$SPPA_Run_Cross_Lfunc, {
    inputI <- switch(input$SPPA_CrossL_V1,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    
    inputJ <- switch(input$SPPA_CrossL_V2,
                     "Airbnb" = "airbnb", 
                     "Bus Stops" = "busstop", 
                     "MRT" = "mrt", 
                     "Tourist Attractions" = "attraction", 
                     "Hotels" = "hotel", 
                     "Malls" = "mall", 
                     "7-11" = "seveneleven", 
                     "Universities" = "uni"
    )
    
    crossl.csr <- isolate(envelope(Lcross_main(), 
                                   Lcross, 
                                   i=inputI, j=inputJ, 
                                   correction='border', 
                                   nsim=input$SPPA_CrossK_No_Simulations))
    
    plot(crossl.csr, . -r ~ r, xlab="distance(m)", xlim=c(0,1000), main="Lcross")
  }, ignoreNULL = FALSE)
  
  output$SPPA_Cross_L_Function <- renderPlot({
    Cross_L_Function()
  })
  
  ######################## 1) SPPA ^ ########################
  
  ######################## images ########################
  
  ### Home Page logo
  output$logo <- renderImage({
    list(src = "images/logo.png",
         width = 650,
         height = 430,
         style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### SMU logo
  output$smulogo <- renderImage({
    list(src = "images/smu-logo.jpg",
         width = 670,
         height = 430,
         style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  ### tmap legend
  output$tmaplegend <- renderImage({
    list(src = "images/tmap-legend.png",
         width = 300,
         height = 40,
         style="display: block;")
  }, deleteFile = FALSE)
  
  ######################## images ^ ########################
  
  ######################## SPPA reactive input ########################
  observe({
    if(!is.null(input$SPPA_CrossL_V2))
      updateSelectInput(session, "SPPA_CrossL_V1", 
                        choices = my_vars[!(my_vars %in% input$SPPA_CrossL_V2)], 
                        selected = isolate(input$SPPA_CrossL_V1) )
  })
  
  observe({
    if(!is.null(input$SPPA_CrossL_V1))
      updateSelectInput(session, "SPPA_CrossL_V2", 
                        choices = my_vars[!(my_vars %in% input$SPPA_CrossL_V1)], 
                        selected = isolate(input$SPPA_CrossL_V2) )
  })
  
  observe({
    if(!is.null(input$SPPA_CrossK_V2))
      updateSelectInput(session, "SPPA_CrossK_V1", 
                        choices = my_vars[!(my_vars %in% input$SPPA_CrossK_V2)], 
                        selected = isolate(input$SPPA_CrossK_V1) )
  })
  
  observe({
    if(!is.null(input$SPPA_CrossK_V1))
      updateSelectInput(session, "SPPA_CrossK_V2", 
                        choices = my_vars[!(my_vars %in% input$SPPA_CrossK_V1)], 
                        selected = isolate(input$SPPA_CrossK_V2) )
  })
  ######################## SPPA reactive input ^ ########################
  
  ######################## NetSPPA reactive input ########################
  observe({
    if(!is.null(input$NetSPPA_CrossK_V2))
      updateSelectInput(session, "NetSPPA_CrossK_V1", 
                        choices = my_vars[!(my_vars %in% input$NetSPPA_CrossK_V2)], 
                        selected = isolate(input$NetSPPA_CrossK_V1) )
  })
  
  observe({
    if(!is.null(input$NetSPPA_CrossK_V1))
      updateSelectInput(session, "NetSPPA_CrossK_V2", 
                        choices = my_vars[!(my_vars %in% input$NetSPPA_CrossK_V1)], 
                        selected = isolate(input$NetSPPA_CrossK_V2) )
  })
  ######################## NetSPPA reactive input ^ ########################
  
  ######################## 2) NetSPPA ########################
  ### 2a) NetSPPA KDE Map
  kde_var <- reactive({
    if (input$NetSPPA_main_var == "Airbnb"){
      dataset <- rochor_airbnb_sf
    }
    else if (input$NetSPPA_main_var == "Tourist Attraction"){
      dataset <- rochor_attr
    }
    else if (input$NetSPPA_main_var == "Bus Stop"){
      dataset <- rochor_busstop
    }
    else if (input$NetSPPA_main_var == "Hotel"){
      dataset <- rochor_hotel
    }
    else if (input$NetSPPA_main_var == "Mall"){
      dataset <- rochor_mall
    }
    else if (input$NetSPPA_main_var == "MRT"){
      dataset <- rochor_mrt
    }
    else if (input$NetSPPA_main_var == "7-11"){
      dataset <- rochor_sevenele
    }
    else if (input$NetSPPA_main_var == "University"){
      dataset <- rochor_unis
    }
    return(dataset)
  })
  
  NetSPPA_KDE_Function <- eventReactive(input$NetSPPA_Run_KDE, {
    # isolate() is used to ensure that the code doesnt run unless the Action Button is clicked
    densities <- isolate(spNetwork::nkde(network_linestring, 
                                         events = kde_var(), #input$NetSPPA_main_var
                                         w = rep(1,nrow(kde_var())), #input$NetSPPA_main_var
                                         samples = samples_rds,
                                         kernel_name = input$NetSPPA_kernel,
                                         bw = 300, 
                                         div= "bw", 
                                         method = input$NetSPPA_method, 
                                         digits = 1, 
                                         tol = 1,
                                         grid_shape = c(1,1), 
                                         max_depth = 8,
                                         agg = 5, #we aggregate events within a 5m radius (faster calculation)
                                         sparse = TRUE,
                                         verbose = FALSE))
    
    samples_rds$density <- densities
    lixels_rds$density <- densities
    
    # rescaling to help the mapping
    samples_rds$density <- samples_rds$density*1000
    lixels_rds$density <- lixels_rds$density*1000
    tmap_mode('view')
    
    NetSPPA_KDE <- isolate(tm_shape(lixels_rds)+
                             tm_lines(col="density")+
                             tm_shape(kde_var())+
                             tm_dots(size=0.005) +
                             tm_layout(legend.outside = TRUE, frame = FALSE, title = "NetKDE") +
                             tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                                          basemaps.alpha = c(0.8, 0.8, 0.8)) +
                             tm_view(set.zoom.limits = c(15,16)))
  }, ignoreNULL = FALSE)
  
  output$NetSPPA_KDE_Map <- renderTmap({
    NetSPPA_KDE_Function()
  })
  
  ### 2b) NetSPPA K_Function
  k_main <- reactive({
    if (input$NetSPPA_K_Main == "Airbnb"){
      dataset <- rochor_airbnb_sf
    }
    else if (input$NetSPPA_K_Main == "Tourist Attraction"){
      dataset <- rochor_attr
    }
    else if (input$NetSPPA_K_Main == "Bus Stop"){
      dataset <- rochor_busstop
    }
    else if (input$NetSPPA_K_Main == "Hotel"){
      dataset <- rochor_hotel
    }
    else if (input$NetSPPA_K_Main == "Mall"){
      dataset <- rochor_mall
    }
    else if (input$NetSPPA_K_Main == "MRT"){
      dataset <- rochor_mrt
    }
    else if (input$NetSPPA_K_Main == "7-11"){
      dataset <- rochor_sevenele
    }
    else if (input$NetSPPA_K_Main == "University"){
      dataset <- rochor_unis
    }
    return(dataset)
  })
  
  NetSPPA_K_Graph <- eventReactive(input$NetSPPA_Run_Kfunc, {
    k_func <- isolate(kfunctions(network_linestring, 
                                 k_main(),
                                 start = 0, 
                                 end = 1000, 
                                 step = 50, 
                                 width = 50, 
                                 nsim = input$NetSPPA_K_No_Simulations, 
                                 resolution = 50,
                                 verbose = FALSE, 
                                 conf_int = 0.05,
                                 agg = 115))
    
    k_func$plotk
  }, ignoreNULL = FALSE)
  
  output$NetSPPA_K_Function <- renderPlot({
    NetSPPA_K_Graph()
  })
  
  
  ### 2c) NetSPPA Cross_K_Function
  NetSPPA_Cross_K_Graph <- eventReactive(input$NetSPPA_Run_Cross_Kfunc, {
    var1 <- switch(input$NetSPPA_CrossK_V1,
                   "Airbnb" = rochor_airbnb_sf,
                   "Bus Stops" = rochor_busstop, 
                   "MRT" = rochor_mrt, 
                   "Tourist Attractions" = rochor_attr, 
                   "Hotels" = rochor_hotel, 
                   "Malls" = rochor_mall, 
                   "7-11" = rochor_sevenele, 
                   "Universities" = rochor_unis
    )
    
    var2 <- switch(input$NetSPPA_CrossK_V2,
                   "Airbnb" = rochor_airbnb_sf,
                   "Bus Stops" = rochor_busstop, 
                   "MRT" = rochor_mrt, 
                   "Tourist Attractions" = rochor_attr, 
                   "Hotels" = rochor_hotel, 
                   "Malls" = rochor_mall, 
                   "7-11" = rochor_sevenele, 
                   "Universities" = rochor_unis
    )
    
    crossk <- isolate(cross_kfunctions(network_linestring, 
                                       var1,
                                       var2,
                                       start = 0, 
                                       end = 5000, 
                                       step = 50, 
                                       width = 1000, 
                                       nsim = input$NetSPPA_CrossK_No_Simulations, 
                                       resolution = 50,
                                       verbose = FALSE, 
                                       conf_int = 0.05,
                                       agg = 100))
    
    crossk$plotk
  }, ignoreNULL = FALSE)
  
  output$NetSPPA_Cross_K_Function <- renderPlot({
    NetSPPA_Cross_K_Graph()
  })
  
  ######################## 2) NetSPPA ^ ########################
  
}

# Run the application 
shinyApp(ui = ui, server = server)
