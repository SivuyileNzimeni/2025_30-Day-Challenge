# Libraries -----------------------------
lapply(c('tidyverse','janitor','data.table',
      'sf','tmap','tmap.mapgl','osmdata'),
    require,
    character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages()
# Import Data -------------------------------
bfn_bbox <- osmdata::getbb("Bloemfontein, South Africa")

bfn_roads <- osmdata::opq(bbox = bfn_bbox) |> 
  osmdata::add_osm_features(
    features = list(
      'highway' = 'motorway',
      'highway' = 'trunk',
      'highway' = 'secondary',
      'highway' = 'tertiary',
      'highway' = 'residential',
      'highway' = 'motorway_link',
      'highway' = 'trunk_link',
      'highway' = 'secondary_link',
      'highway' = 'primary_link',
      'highway' = 'tertiary_link'
    )
  ) |> 
  osmdata::osmdata_sf()

tmap_mode("mapbox")


bloem_lines <- tm_shape(bbox = bfn_bbox)+
  tm_basemap('mapbox.dark')+
  tm_shape(bfn_roads$osm_lines |> 
  st_as_sf(crs=4326) |> 
  filter('highway'!= 'residential'))+
  tm_shape()+
    tm_lines(
      col = '#39FF14',
      col.legend = NULL
    )+
  tm_scalebar()+
  tm_compass()+
  tm_minimap()+
  tm_credits()


webshot2::webshot(url = "http://127.0.0.1:42745/9e0ef1b3/index.html",
file = paste0("./output/2025_Day_2_Lines_",Sys.Date(),".png"))
