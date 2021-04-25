#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### Load packages
library(shiny)
library(shinyWidgets)
#library(shinythemes)
library(dashboardthemes)
library(shinydashboard)
library(sf)
library(measurements)
library(lwgeom)
library(leaflet)
#library(here)
library(tidyverse)

### Load the data

## Temple data
temples <- read_csv("ChurchofJesusChristTemples.csv")

# Filter out just USA temples and select the needed data
usa_temples <- temples %>% 
    filter(Country == "United States") %>% 
    select(Temple:Longitude, Address:Phone) %>% 
    st_as_sf(coords = c("Longitude", "Latitude")) %>% 
    st_set_crs(value = 4326) %>% 
    mutate(type = "Temples")

# usa_temple_circle <- st_buffer(usa_temples, dist = 1)



# dim(usa_temples)
# glimpse(usa_temples)

## Airport data
usa_airports <- read_csv("NfdcFacilities.csv")

# dim(usa_airports)
# glimpse(usa_airports)

## Use air traffic control tower as an initial proxy for major airport?
# usa_airports %>% count(ATCT)
# summary(usa_airports$LandAreaCoveredByAirport)

## Selected needed variables
usa_airport_atctY <- usa_airports %>% 
    filter(ATCT == "Y") %>% 
    select(State:FacilityName, ARPLatitude:ARPLongitudeS) %>%
    mutate(ARPLatitude = str_replace_all(ARPLatitude, "-", " "),
           ARPLatitude = str_remove(ARPLatitude, "N"),
           ARPLatitude = conv_unit(ARPLatitude, from = "deg_min_sec", to = "dec_deg"),
           ARPLongitude = str_replace_all(ARPLongitude, "-", " "),
           ARPLongitude = str_remove(ARPLongitude, "W"),
           ARPLongitude = paste0("-", ARPLongitude),
           ARPLongitude = conv_unit(ARPLongitude, from = "deg_min_sec", to = "dec_deg")) %>% 
    st_as_sf(coords = c("ARPLongitude", "ARPLatitude"), na.fail = FALSE) %>% 
    st_set_crs(value = 4326) %>% 
    mutate(type = "Airports")

#usa_airport_circles <- st_buffer(usa_airport_atctY, dist = 1)


temples_2_airports_dist <- st_distance(x = usa_temples, y = usa_airport_atctY) %>%
    as_tibble() %>% 
    mutate(across(.fns = ~str_remove(., " \\[m\\]") %>% as.numeric(.)),
           across(.cols = everything(), .fns = ~conv_unit(., from = "m", to = "mi"))) %>%
    set_names(usa_airport_atctY$FacilityName)
## temples are the rows, airports are the columns, so merge this back into temples

# usa_temples_dist <- usa_temples %>% 
#     bind_cols(temples_2_airports_dist)
# 
usa_temples_dist_long <- usa_temples %>%
    bind_cols(temples_2_airports_dist) %>%
    pivot_longer(-c(Temple:type)) %>% 
    ungroup() %>% 
    st_as_sf()

usa_temples_dist_min <- usa_temples %>% 
    bind_cols(temples_2_airports_dist) %>% 
    pivot_longer(-c(Temple:type)) %>% 
    group_by(Temple) %>% 
    filter(value == min(value)) %>% 
    ungroup() %>% 
    st_as_sf()

usa_temples_dist_75 <- usa_temples %>% 
    bind_cols(temples_2_airports_dist) %>% 
    pivot_longer(-c(Temple:type)) %>% 
    group_by(Temple) %>% 
    filter(value == min(value)) %>% 
    filter(value < 50) %>% 
    ungroup() %>% 
    st_as_sf()


# leaflet() %>% 
#     addTiles() %>% 
#     addAwesomeMarkers(data = usa_airport_atctY, popup = ~type) #%>% 
    # addPolygons(data = usa_airport_circles)






### Build the app parts.

## Header
app_header <- dashboardHeader(title = "LDS Temples and their Nearest Airport", titleWidth = 500)

## Sidebar
app_sidebar <- dashboardSidebar(
    ## What filters?
    
    ## Temples, Airports or both?
    # pickerInput(inputId = "type",
    #             label = "Airports or Temples?",
    #             choices = c("Airports", "Temples"),
    #             multiple = TRUE,
    #             selected = NULL),
    sliderInput(inputId = "distance",
                label = "Maximum Distance",
                min = 0,
                max = 100,
                value = c(25,50),
                step = 5,
                round = TRUE,
                dragRange = T),
    width = 250
)

## Body
app_body <- dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    fluidRow(
        box(leafletOutput("temple_airport_map"), width = 36)
    ),
    fluidRow(
        infoBoxOutput("temple_counter", width = 12)
    )
    
)


# Define UI for application that draws a leaflet map
ui <- dashboardPage(app_header, app_sidebar, app_body)


# Define server logic required to draw a leaflet map
server <- function(input, output) {
    
    
    
    output$temple_airport_map <- renderLeaflet({
        filter_data <- usa_temples_dist_min %>% 
            filter(between(value, input$distance[1], input$distance[2]))
        
        # temple_icon <- makeAwesomeIcon(
        #     icon = "place-of-worship",
        #     iconColor = "black",
        #     markerColor = "cadetblue"
        # )
        # draw the map
        leaflet(data = filter_data) %>% 
            addTiles() %>% 
            addAwesomeMarkers(popup = ~paste0("Temple: ", Temple,
                                                                          "<br>Nearest Airport: ", name,
                                                                          "<br>Distance (mi): ", round(value, 2)))
    })
    
    output$temple_counter <- renderInfoBox({
        temple_count_dat <- usa_temples_dist_min %>% 
            filter(between(value, input$distance[1], input$distance[2])) %>% 
            summarise(temple_count = n())
        infoBox(title = paste0("Number of LDS Temples within ", input$distance[1], " and ", input$distance[2], " miles of an airport"), value = temple_count_dat$temple_count, width = 12, icon = icon("place-of-worship", lib = "font-awesome"))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
