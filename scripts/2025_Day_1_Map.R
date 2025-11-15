# Libraries -----------------------------
lapply(c('tidyverse','janitor','data.table',
      'sf','tmap','tmap.mapgl','osmdata'),
    require,
    character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages()
# Import Data -------------------------------
qonce_bbox <- osmdata::getbb("Qonce, South Africa")

qonce_places <- read_sf('./data/2025_Qonce_Places_Day_1.geojson')

qonce_places$place_details <- str_trim(qonce_places$place_details)

# Clean JSON -----------------------------------------------
remove_trails <- function(a_vec){
  new_vec <-gsub(pattern ="\\{|\\}",replacement = "",x=a_vec) |> 
  str_trim() |> 
  str_replace_all(pattern = '"',replacement ="'") 
  return(new_vec)
}

qonce_places$place_details <- qonce_places$place_details |> remove_trails()

qonce_places$primary_industry <- qonce_places$place_details |> 
  str_extract(pattern = "(?<=primary[[:punct:]]{2}\\s{1,2}[[:punct:]])\\w{1,}")

tmap_mode('mapbox')

qonce_places$primary_industry |> table()

qonce_eateries <- qonce_places[grepl("restaurant|eat|food",qonce_places$primary_industry),]

qonce_eateries$primary_industry <- qonce_eateries$primary_industry |> gsub(pattern = "_",replacement = " ") |> 
  str_to_sentence()


qonce_eateries <- left_join(qonce_eateries,qonce_eateries$primary_industry |> table() |> 
  as.data.frame() |> 
  rename(
    primary_industry = Var1,
    establishments = Freq
  ))

qonce_output <- qonce_eateries |> 
tm_shape()+
  tm_dots(
    'primary_industry',
    size = 'establishments',
    fill.scale = tm_scale_categorical(values = 'powerbi.high_contrast'),
    fill.legend = tm_legend(title = 'Industry'),
    size.legend = NULL
  )+
  tm_basemap(
    'mapbox.satellite_streets'
  )+
  tm_compass()+
  tm_minimap()

webshot2::webshot(url = "http://127.0.0.1:33633/bd7ee10c/index.html",
file = paste0("./output/2025_Day_1_Points_",Sys.Date(),".png"))
