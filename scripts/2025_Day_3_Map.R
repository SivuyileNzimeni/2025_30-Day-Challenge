# Libraries -----------------------------
lapply(c('tidyverse','janitor','data.table',
      'sf','tmap','tmap.mapgl','osmdata'),
    require,
    character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages()
# Import Map Data --------------------------
TSH_Wards <- as.data.table(read_sf('data/2020_MDB_Wards.geojson'))[CAT_B == 'TSH',.SD,.SDcols = c('WardID','WardLabel','Municipali','Province','CAT_B','geometry')] |> 
  st_as_sf(
    crs=4326
  )

TSH_Wards <- TSH_Wards |> 
  left_join(fread("data/TSH.csv")[BallotType =='Ward',
list(
  eligible_voters = sum(unique(RegisteredVoters)),
  votes = sum(TotalValidVotes))
  ,.(Ward)] |>
  mutate(
    turn_out = (100/eligible_voters)*votes
  ) |> 
  rename(
    WardID = Ward
  ) |> 
  mutate(
    ballot_type = 'Ward',
    WardID = str_extract(WardID,"(?<=Ward )\\d{1,}")
  )
  ) |> 
  clean_names()


tmap_mode('mapbox')

TSH_Wards |> 
tm_shape()+
  tm_polygons(
    fill = 'turn_out',
    fill.scale = tm_scale_continuous()
  )
# LISA using https://sfdep.josiahparry.com/articles/basics-of-sfdep
library(sfdep)

TSH_Sf <- TSH_Wards |> 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    local_moran = local_moran(turn_out,
      nb = st_contiguity(geometry),
      wt = st_weights(nb),
      nsim=800)
  ) |> 
  unnest(local_moran) |> 
  rename(local_moran_pysal = pysal)

TSH_Expanded <- TSH_Sf |> 
  left_join(TSH_Wards |> 
   mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    local_gc = local_gstar_perm(turn_out,
      nb = st_contiguity(geometry),
      wt = st_weights(nb),
      nsim=800)
  ) |> 
  unnest(local_gc) |> 
  rename(local_gc_cluster = cluster) |> 
  select(ward_id,local_gc_cluster) |> 
  st_drop_geometry()
)

TSH_Expanded$local_gc_cluster |> table()
TSH_Expanded$local_moran_pysal |> table()

tmap_mode('plot')


compare_palette <- c("#A71930","#0f204b")

tmap_mode('mapbox')

aggregate(turn_out ~ local_gc_cluster,data=TSH_Expanded,mean)

TSH_Expanded |> 
  st_as_sf( crs=4326
  ) |> 
  tm_shape()+
  tm_basemap('mapbox.satellite_streets')+
  tm_polygons(
    fill ='local_gc_cluster',
    fill.scale = tm_scale_categorical(n.max =2,values = compare_palette),
    fill_alpha = 0.7,
    fill.legend =  tm_legend('Cluster')
  )+
  tm_borders("white")+
  tm_scalebar()+
  tm_compass()+
  tm_minimap()+
  tm_credits()

# http://127.0.0.1:42745/ffa9c1eb/index.html

webshot2::webshot(url = "http://127.0.0.1:42745/ffa9c1eb/index.html",
file = paste0("./output/2025_Day_3_Points_",Sys.Date(),".png"))
