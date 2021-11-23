library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(sf)
library(sp)
library(randomcoloR)

shinyServer(function(input, output) {
  # Import Data from https://www.gov.uk/government/publications/renewable-energy-planning-database-monthly-extract
  
  re <- read.csv("data/renewable-energy-planning-database-march-2020.csv", 
                 stringsAsFactors = FALSE, skip = 1)
  
  latlong = "+init=epsg:4326"
  ukgrid = "+init=epsg:27700"
  
  re$X.coordinate <- as.numeric(re$X.coordinate <- gsub(",", "", re$X.coordinate, fixed = TRUE))
  re$Y.coordinate <- as.numeric(re$Y.coordinate <- gsub(",", "", re$Y.coordinate, fixed = TRUE))
  
  re$fulladdress <- paste0(re$Address, ", ", re$County, ", ", re$Region, ",", re$Post.Code)
  re <- filter(re, !is.na(X.coordinate))
  
  coordinates(re) <- c("X.coordinate", "Y.coordinate")
  
  re <- st_as_sf(re, ukgrid)
  st_crs(re) = ukgrid
  re <- st_transform(re, latlong)
  
  re_geo <- data.frame(st_coordinates(re))
  names(re_geo) <- c("Longitude", "Latitude")
  
  re <- bind_cols(re, re_geo)
  
  # create a color paletter for category type in the data file
  
  tech_type <- unique(re$Technology.Type)
  
  p <- distinctColorPalette(NROW(tech_type))
  
  pal <- colorFactor(palette = p, domain = tech_type)
  
  # create the leaflet map  
  output$re <- renderLeaflet({
    leaflet(re) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
      addProviderTiles('CartoDB.Positron') %>%
      addCircleMarkers(data = re, lat =  ~Latitude, lng =~Longitude, 
                       radius = 3, popup = paste0('<strong>Ref ID: </strong>',re$Ref.ID,
                                                  '<br><strong>Site Name:</strong> ', re$Site.Name,
                                                  '<br><strong>Technology Type:</strong> ', re$Technology.Type,
                                                  '<br><strong>Storage Type:</strong> ',re$Storage.Type,
                                                  '<br><strong>Installed Capacity:</strong> ',re$Installed.Capacity..MWelec.,
                                                  '<br><strong>Development Status:</strong> ',re$Development.Status,
                                                  '<br><strong>Operator/Applicant:</strong> ',re$Operator..or.Applicant.,
                                                  '<br><strong>Address:</strong> ', re$fulladdress,
                                                  '<br><strong>Operational:</strong> ',re$Operational), 
                       color = ~pal(Technology.Type),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=re$Technology.Type,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    re,filter = 'top',
    colnames = c("Old Ref ID", "Ref ID", "Record Last Updated", "Operator/Applicant", "Site Name",
                 "Technology Type", "Storage Type", "Storage Co Location", "Installed Capacity", 
                 "CHP Enabled", "RO Branding", "FiT Tariff", "CfD Capacity", "Turbine Capacity",
                 "No of Turbines", "Height of Turbines", "Mounting Type for Solar", "Development Status",
                 "Development Status - short", "Address", "County", "Postcode", "Planning Authority", 
                 "Planning Application Reference", "Appeal Reference", "Secretary of State Reference", 
                 "Type of Secretery of State Intervention", "Judicial Review", "Offshore Wind Round", 
                 "Planning Application Submitted", "Planning Application Withdrawn", "Planning Permission Refused",
                 "Appeal Lodged", "Appeal Withdrawn", "Appeal Refused", "Appeal Granted", "Planning Permission Granted",
                 "Secretery of State Intervened", "Secretery of State Refusal", "Secretery of State Granted", 
                 "Planning Permission Expired", "Under Construction", "Operational", "Full Address", "geometry", "Longitude", "Latitude")
  ))
  
  
})

