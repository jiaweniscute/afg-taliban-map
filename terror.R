# import libraries
library(leaflet)
library(rgdal)

# get terrorist data, downloaded from: https://www.start.umd.edu/gtd/ 
data <- read.csv(file=paste(getwd(),'/terrorist_dataset.csv',sep=''), header=TRUE, sep="\t", stringsAsFactors=FALSE)
df <- subset(data, select=c("iyear", "country_txt","city","longitude","latitude","success","attacktype1_txt","targtype1_txt","total_kill_wounded","gname","people_extent"))
df <- subset(df, iyear == 2017)
df_afg <- subset(df, country_txt=='Afghanistan')

# get district and province shape file, downloaded from: http://www.diva-gis.org/gdata 
afg_district <- rgdal::readOGR("shapefiles/afg_districts/afg_admbnda_adm2_agcho_20180522.shp")
afg_province <- rgdal::readOGR("shapefiles/provicinial-boundaries/admin2_poly_32.shp")

# get regions of control, from: https://www.fdd.org/analysis/visuals/2018/09/24/mapping-taliban-control-in-afghanistan/
afg_control <- read.csv('afg_districts.csv', stringsAsFactors = FALSE)

# Merge district shape file and regions of control 
afg_districts_control <- merge(x = afg_district, y = afg_control, by = c("ADM2_EN","ADM1_EN"), all = TRUE)
afg_districts_control$Control.Under <- as.character(afg_districts_control$Control.Under)

# popup text
afg_popup_text = paste("City:", afg_districts_control$ADM1_EN, "<br>",
                   "District:", afg_districts_control$ADM2_EN, "<br>",
                   "Control Under:", afg_districts_control$Control.Under,"<br>")

# palette settings
afg_control_colors <- c('#3956BC','#A4ADC7','#1A2868')
afg_control_pal <- colorFactor(
  palette = afg_control_colors, 
  domain = afg_districts_control$Control.Under)

afg_p <- leaflet(afg_districts_control) %>%
  # Set view
  setView(lng = 68.518338, lat = 34.225723, zoom = 6) %>% 
  # Black background
  addRectangles(
    lng1=47.618341, lat1=43.544165,
    lng2=91.036309, lat2=15.828719,
    fillColor = "#000000",
    stroke = FALSE,
    fillOpacity = 1
  ) %>% 
  # Region Control Polygons
  addPolygons(
    stroke = FALSE, 
    smoothFactor = 0.1,
    popup = afg_popup_text, 
    color = 'white',
    fillColor =~afg_control_pal(Control.Under), 
    fillOpacity = 1) %>% 
  # Province Polygons
  addPolygons(
    data = afg_province, 
    popup = afg_popup_text,
    color='#DFDFDF', 
    weight = 0.1, 
    fillOpacity = 0) %>% 
  # Terrorist Attacks Circles
  addCircleMarkers(
    data = df_afg,
    radius = 2,
    color = '#FF9600',
    fillOpacity = 1,
    stroke = FALSE
  ) %>% 
  # Add Legend
  addLegend("bottomright",
            colors= c('#FF9600', afg_control_colors),
            labels=c('Terrorist Attack', "Contested","Government","Taliban-Controlled"),
            opacity= 1) %>%
  addLayersControl(
    options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar()
  
afg_p

