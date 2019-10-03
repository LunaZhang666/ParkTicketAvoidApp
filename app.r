# loading libraries
library(shiny)
library(tidyverse) # metapackage with lots of helpful functions
library(lubridate) # for working with dates
library(plotly) # for interactive plots
library(janitor)
library(leaflet)
library(colorRamps)
library(proj4)
library(validate)
library(htmlwidgets)
library(mapview)
library(shinyBS)


################# LOAD DATA ##################
park_cit <- read_csv("./park8cols.csv") %>%
  clean_names()
# vehicle_crime <- read_csv("./vehicle_crime_after_2014.csv") %>%
#   clean_names()
######################################################
# Define UI ----
ui <- bootstrapPage(
  uiOutput('modal'), # dialogue box
  tags$head(
    # Include custom CSS
    includeCSS("./style.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("distPlot", width = "80%", height = "100%"),
  absolutePanel(id = "controls", top = 0, right = 10,class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, left = "auto", bottom = "auto",
                width = 330, height = "auto",
                h3("Parking Citations in LA"),
                dateRangeInput(
                  inputId = "daterange", label = "Date range:",
                  start  = "2018-03-15",
                  end    = "2018-03-25",
                  min    = "2010-01-09",
                  max    = "2019-01-25"),
                textInput(
                  inputId = "locationIn", label = "Enter Street Name",
                  value = "",
                  placeholder = "CANYON DR"
                ),
                # select to show pinned points
                checkboxInput(
                  inputId = "keepPin", 
                  label = "pin", 
                  value = TRUE),
                plotOutput("barchart", height = 300)
  )
)

# pin
pin <- data.frame(
  id = as.character(0:99),
  lng = rep(200, times = 100),
  lat = rep(100, times = 100)
)
pin_cnt <- 0
# Global ID vector
ids <- pin$id
zoom_thred <- 14
zoom_prev <- 11
zoom_level <- 11
zoomed <- 0

# Define server logic ----
server <- function(input, output, session) {
  # read geojson
  LAPD <- geojsonio::geojson_read("json/lapd.geojson", what = "sp")
  neighborhood <- geojsonio::geojson_read("json/neighborhood.geojson", what = "sp")
  # updating data
  update_data <- reactive({
    selected_time_range <- park_cit %>% 
      filter(issue_date >= input$daterange[1] & issue_date <= input$daterange[2])
    top_violations <- selected_time_range %>% 
      group_by(violation_description) %>% 
      tally() %>% 
      arrange(-n) %>% 
      head(5)
    ##Filter last week's data only for top 5 violations
    top_violations_selected_time_range <- selected_time_range %>% 
      filter(violation_description %in% top_violations$violation_description)
    ##Filter data to keep only 500 most expensive issued tickets and filter out the ones without coordinates
    top_500_fines <- selected_time_range %>%
      arrange(-fine_amount) %>%
      head(5000)
    
    ##Convert latitude longitude from US Feet coordinates to normal lat lon
    #Create projection element to convert from US Feet coordinates to normal lat lon
    pj <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"
    #Add converted latitude longitude to top_500_fines dataframe
    x <- cbind(top_500_fines, data.frame(project(data.frame(top_500_fines$latitude, top_500_fines$longitude), proj = pj, inverse = TRUE)))
    names(x)[c(9, 10)] <- c('lon', 'lat') #Rename column names of converted longitude latitude
    return(x)
  })
  #get input location
  input_location <- reactive({
    x <- c("", "")
    if (input$locationIn != ""){
      match_row_num <- grep(input$locationIn, park_cit$location, ignore.case = TRUE, value = FALSE)[1]
      if (!is.na(match_row_num)){
        match_row <- park_cit[match_row_num,]
        pj <- "+proj=lcc +lat_1=34.03333333333333 +lat_2=35.46666666666667 +lat_0=33.5 +lon_0=-118 +x_0=2000000 +y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"
        match_row_conv <- x <- cbind(match_row, data.frame(project(data.frame(match_row$latitude, match_row$longitude), proj = pj, inverse = TRUE)))
        names(match_row_conv)[c(9, 10)] <- c('lon', 'lat') #Rename column names of converted longitude latitude
        x <- c(match_row_conv$lon,match_row_conv$lat)
      }
    }
    return(x)
  })
  # top10 violation barchart
  # get top 10
  top10_data <- reactive({
    selected_time_range <- park_cit %>% 
      filter(issue_date >= input$daterange[1] & issue_date <= input$daterange[2])
    x <- selected_time_range %>% 
      group_by(violation_description) %>% 
      tally() %>% 
      arrange(-n) %>% 
      head(10)
    return(x)
  })
  output$barchart <- renderPlot({
    top_violations <- top10_data()
    par(mar=c(5.1,1,4.1,2.1))
    bp <- barplot(top_violations$n, main = "Top 10 Violations",horiz=TRUE, col = rgb(1,0.6,0.6,0.5))
    text(y=bp+0.6, x=max(top_violations$n)/2, labels=top_violations$violation_description, pos=1, cex = 0.6)  
  })
  
  #Plot the map
  output$distPlot <- renderLeaflet({
    top_500_fines_conv <- update_data()
    
    # crime layer
    bins <- seq(8318, 15515, length.out = 5)
    pal <- colorBin("YlOrRd", domain = LAPD$crime_cnt, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d vehicle crimes in 5 years",
      LAPD$name, LAPD$crime_cnt
    ) %>% lapply(htmltools::HTML)
    
    map <- leaflet(data = LAPD, options = leafletOptions(doubleClickZoom= FALSE)) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        noWrap = TRUE,
        opacity = 0.5,
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')),
        group = "OSM (default)") %>%
      addPolygons(fillColor = ~pal(crime_cnt),
                  weight = 2,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = 'crime_layer') %>%
      setView(lng = mean(top_500_fines_conv$lon), lat = mean(top_500_fines_conv$lat), zoom = 10) %>%
      addLegend('bottomleft', pal = pal, values = ~crime_cnt, title = 'Crime count', group = 'crime_layer')
    
    # population density layer
    bins <- seq(0, 42611, length.out = 5)
    pal <- colorBin("PuRd", domain = neighborhood$population_density, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d people per square mi",
      neighborhood$name, neighborhood$population_density
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("distPlot", data = neighborhood) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        noWrap = TRUE,
        opacity = 0.5,
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')),
        group = "OSM (default)") %>%
      addPolygons(fillColor = ~pal(population_density),
                  weight = 2,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = 'population_layer') %>%
      addLegend('bottomleft', pal = pal, values = ~population_density, title = 'population density', group = 'population_layer')
    
    # tickets layer
    pal <- colorNumeric(palette = 'RdYlBu', domain = NULL, reverse = TRUE) #Create color palette
    
    leafletProxy("distPlot", data=top_500_fines_conv) %>%
      # base groups
      addTiles(options = providerTileOptions(noWrap = TRUE),
               group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addCircleMarkers(~lon, ~lat, color = ~pal(fine_amount), radius = 3,
                       #label = paste0(top_500_fines_conv$location," | ", top_500_fines_conv$issue_date, ' : ',
                       #              top_500_fines_conv$violation_description, ' | ',' $', top_500_fines_conv$fine_amount),
                       clusterOptions = markerClusterOptions(),
                       options = list(zIndex = 650),
                       group = 'tickets_layer') %>%
      addLegend('bottomright', pal = pal, values = ~fine_amount, title = 'Fine amounts', group = 'tickets_layer') %>%
      addScaleBar(position="bottomleft") %>%
      # Layers control
      addLayersControl(
        baseGroups = c('OSM (default)', 'Toner', 'Toner Lite'),
        overlayGroups = c('crime_layer','population_layer'),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      )
    
    # tackle zoom to
    zoomto <- input_location()
    
    if (is.numeric(zoomto[1])){
      zoomed <<- 1
      leafletProxy('distPlot') %>%
        clearGroup('tickets_layer')
      
      # tickets layer
      pal <- colorNumeric(palette = 'RdYlBu', domain = NULL, reverse = TRUE) #Create color palette
      
      leafletProxy('distPlot', data=top_500_fines_conv) %>%
        addTiles(options = providerTileOptions(noWrap = TRUE),
                 group = "OSM (default)") %>%
        addCircleMarkers(~lon, ~lat, color = ~pal(fine_amount), radius = 3,
                         label = paste0(top_500_fines_conv$location," | ", top_500_fines_conv$issue_date, ' : ',
                                        top_500_fines_conv$violation_description, ' | ',' $', top_500_fines_conv$fine_amount),
                         options = list(zIndex = 650), 
                         group = 'tickets_layer') %>%
        setView(lng = zoomto[1], lat = zoomto[2], zoom = 15)
    
    }
    else {
      # print(input$locationIn)
      if (input$locationIn != ""){
        text <- c('No street called "', input$locationIn, '". Please try again')
        text <- paste(text, collapse="")
        output$modal <- renderUI({
          bsModal(id='alert', title=text, trigger="locationIn", size = "large")
        })
        toggleModal(session,modalId='alert', toggle='toggle')
        updateTextInput(session,inputId = "locationIn", label = "Enter Street Name",
                        value = "", placeholder = "CANYON DR")
      }
    }
    
    map
  })
  
  # zoom in and out, change view
  observe({
    req(input$distPlot_zoom)
    zoom_level <- input$distPlot_zoom
    # print(zoom_level)
    # tickets data
    top_500_fines_conv <- update_data()
    
    if (zoomed == 0 && zoom_level >= zoom_thred && zoom_prev < zoom_thred){ # show markers
      # clear view
      leafletProxy('distPlot') %>%
        clearGroup('crime_layer') %>%
        clearGroup('tickets_layer') %>%
        clearGroup('population_layer')
      
      # crime layer
      bins <- seq(8318, 15515, length.out = 8)
      bins <- c(bins, Inf)
      pal <- colorBin("YlOrRd", domain = LAPD$crime_cnt, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%d vehicle crimes in 5 years",
        LAPD$name, LAPD$crime_cnt
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy('distPlot', data = LAPD) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          noWrap = TRUE,
          opacity = 0.5,
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')),
          group = "OSM (default)") %>%
        addPolygons(fillColor = ~pal(crime_cnt),
                    weight = 2,
                    opacity = 0.5,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.6,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = 'crime_layer') %>%
        setView(lng = mean(top_500_fines_conv$lon), lat = mean(top_500_fines_conv$lat), zoom = zoom_level)
      
      # population density layer
      bins <- seq(0, 42611, length.out = 5)
      pal <- colorBin("PuRd", domain = neighborhood$population_density, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%d people per square mi",
        neighborhood$name, neighborhood$population_density
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy("distPlot", data = neighborhood) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          noWrap = TRUE,
          opacity = 0.5,
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')),
          group = "OSM (default)") %>%
        addPolygons(fillColor = ~pal(population_density),
                    weight = 2,
                    opacity = 0.5,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.6,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = 'population_layer')
      
      # tickets layer
      pal <- colorNumeric(palette = 'RdYlBu', domain = NULL, reverse = TRUE) #Create color palette
      
      leafletProxy('distPlot', data=top_500_fines_conv) %>%
        addTiles(options = providerTileOptions(noWrap = TRUE),
                 group = "OSM (default)") %>%
        addCircleMarkers(~lon, ~lat, color = ~pal(fine_amount), radius = 3,
                         label = paste0(top_500_fines_conv$location," | ", top_500_fines_conv$issue_date, ' : ',
                                        top_500_fines_conv$violation_description, ' | ',' $', top_500_fines_conv$fine_amount),
                         options = list(zIndex = 650),
                         group = 'tickets_layer')
    }
    else if (zoomed == 0 && zoom_level < zoom_thred && zoom_prev >= zoom_thred){ # show marker in clusters
      # crime layer
      bins <- seq(8318, 15515, length.out = 8)
      bins <- c(bins, Inf)
      pal <- colorBin("YlOrRd", domain = LAPD$crime_cnt, bins = bins)
      
      leafletProxy('distPlot') %>%
        clearGroup('crime_layer') %>%
        clearGroup('tickets_layer')
      
      # crime layer
      bins <- seq(8318, 15515, length.out = 8)
      bins <- c(bins, Inf)
      pal <- colorBin("YlOrRd", domain = LAPD$crime_cnt, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%d vehicle crimes in 5 years",
        LAPD$name, LAPD$crime_cnt
      ) %>% lapply(htmltools::HTML)
      
      leafletProxy('distPlot', data = LAPD) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          noWrap = TRUE,
          opacity = 0.5,
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')),
          group = "OSM (default)") %>%
        addPolygons(fillColor = ~pal(crime_cnt),
                    weight = 2,
                    opacity = 0.5,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.6,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    group = 'crime_layer') %>%
        setView(lng = mean(top_500_fines_conv$lon), lat = mean(top_500_fines_conv$lat), zoom = zoom_level)
      
      # tickets layer
      pal <- colorNumeric(palette = 'RdYlBu', domain = NULL, reverse = TRUE) #Create color palette
      leafletProxy('distPlot', data=top_500_fines_conv) %>%
        addTiles(options = providerTileOptions(noWrap = TRUE),
                 group = "OSM (default)") %>%
        addCircleMarkers(~lon, ~lat, color = ~pal(fine_amount), radius = 3,
                         #label = paste0(top_500_fines_conv$location," | ", top_500_fines_conv$issue_date, ' : ',
                         #              top_500_fines_conv$violation_description, ' | ',' $', top_500_fines_conv$fine_amount),
                         clusterOptions = markerClusterOptions(),
                         options = list(zIndex = 650),
                         group = 'tickets_layer')
    }
    else if (zoomed == 1){
      zoomed <<- 2
    }
    else {
      zoomed <<- 0
    }
    print(zoomed)
    
    zoom_prev <<- zoom_level
  })

  ## Observe mouse clicks and add circles
  observeEvent(input$distPlot_click, {
    if (input$keepPin && pin_cnt <= 99){
      click <- input$distPlot_click
      clat <- click$lat
      clng <- click$lng
      
      pin_cnt <<- pin_cnt + 1
      pin$lng[pin_cnt] <<- clng
      pin$lat[pin_cnt] <<- clat
      
      # print(pin)
      # print(ids)
      
      # Analysis data and give suggestion
      top_500_fines_conv <- update_data()
      cnt_small <- 0
      cnt_large <- 0
      amount <- 0
      
      deg2rad <- function(deg) {(deg * pi) / (180)}
      
      haversine <- function(lon1, lon2, lat1, lat2){
        R <- 6371e3
        phi1 <- deg2rad(lat1)
        phi2 <- deg2rad(lat2)
        delta_phi <- deg2rad(lat2 - lat1)
        delta_lambda <- deg2rad(lon2 - lon1)
        a <- sin(delta_phi/2)^2 + cos(phi1)*cos(phi2)*sin(delta_lambda/2)^2
        c <- atan2(sqrt(a), sqrt(1-a))
        d <- R * c
        return (d * 2)
      }
      
      
      for (i in 1:lengths(top_500_fines_conv[1])){
        curr_amount <- top_500_fines_conv[i, 6]
        curr_lon <- top_500_fines_conv[i,9]
        curr_lat <- top_500_fines_conv[i, 10]
        # print(haversine(curr_lon, clng, curr_lat, clat))
        if (haversine(curr_lon, clng, curr_lat, clat) <= 5000){
          cnt_large <- cnt_large + 1
          if (haversine(curr_lon, clng, curr_lat, clat) <= 500){
            cnt_small <- cnt_small + 1
            amount <- amount + curr_amount
          }
        }
      }
      if (cnt_small != 0)
        amount <- amount / cnt_small
      # icons
      greenIcon <- makeIcon(
        iconUrl = "icons/happy.png",
        iconWidth = 48, iconHeight = 48,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      orangeIcon <- makeIcon(
        iconUrl = "icons/meh.png",
        iconWidth = 48, iconHeight = 48,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      redIcon <- makeIcon(
        iconUrl = "icons/frown.png",
        iconWidth = 48, iconHeight = 48,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      # add pin point on click
      cat("cnt_small", cnt_small, " cnt_large", cnt_large, "\n")
      if (cnt_small * 100 >= cnt_large && amount >= 100){
        leafletProxy('distPlot') %>%
          addCircles(layerId=pin_cnt, lng=clng, lat=clat, group='pin',
                     weight=3, radius=500, color='#f03b20', fillColor='none',
                     fillOpacity=0.7, opacity=1) %>%
          addMarkers(lng=clng, lat=clat, icon = redIcon, group = 'pin')
      }
      else if (cnt_small * 100 >= cnt_large || amount >= 100){
        leafletProxy('distPlot') %>%
          addCircles(layerId=pin_cnt, lng=clng, lat=clat, group='pin',
                     weight=3, radius=500, color='#feb24c', fillColor='none',
                     fillOpacity=0.7, opacity=1) %>%
          addMarkers(lng=clng, lat=clat, icon = orangeIcon, group = 'pin')
      }
      else {
        leafletProxy('distPlot') %>%
          addCircles(layerId=pin_cnt, lng=clng, lat=clat, group='pin',
                     weight=3, radius=500, color='#31a354', fillColor='none',
                     fillOpacity=0.7, opacity=1) %>%
          addMarkers(lng=clng, lat=clat, icon = greenIcon, group = 'pin')
      }
      
      location <- c("Current Location: (", clng, "\u00B0W , ", clat, "\u00B0N)")
      location <- paste(location, collapse="")
      if (cnt_small == 0){
        text <- "No parking tickets issued in this area"
        risk <- "Low: Safe to park here"
      }
      else {
        text <- c("Number of tickets: ", cnt_small, "\nAverage fine amount: ", amount)
        text <- paste(text, collapse=" ")
        risk <- "Potential Risks:"
        if (cnt_small * 100 >= cnt_large){
          risk <- paste(risk, "Ticket Frequency Warning: High frequency of tickets issue in this area", sep = "\n")
        }
        if (amount >= 100){
          risk <- paste(risk, "Fine Amount Warning: More than $100 each time", sep = "\n")
        }
      }
      
      output$modal <- renderUI({
        bsModal(id='popup', title=location, trigger="distPlot_click", size = "large",
                textAreaInput("text", label = h4("Tickets Nearby") ,
                              value = text,
                              width = "500px", height = "200px", resize = "both"),
                textAreaInput("text", label = h4("Parking Risk") , value = risk,
                              width = "500px", height = "200px", resize = "both")
        )
      })
      toggleModal(session,modalId='popup', toggle='Assessment') 
    }
  })
  
  # Remove points
  observeEvent(input$keepPin, {
    if (!input$keepPin){
      proxy <- leafletProxy('distPlot')
      # clear pins
      proxy %>% clearGroup(group = 'pin')
      # reset pins
      pin <<- data.frame(
        id = as.character(0:99),
        lng = rep(200, times = 100),
        lat = rep(100, times = 100)
      )
      pin_cnt <<- 0
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)