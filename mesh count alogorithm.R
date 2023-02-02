library(tidyverse)

library(sf)

# read in
boundaries <- st_read("Regional boundaries\\regional-council-2022-generalised.shp", quiet = TRUE)
my.clip.province <- boundaries %>% filter(REGC2022_1 == "Otago Region")
  
catch.2193 <- my.clip.province %>%
  st_transform(crs = 2193)

# make grid
grid <- st_make_grid(
  catch.2193,
  cellsize = c(10000, 10000),
  crs = 2193,
  square = TRUE
) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)


# eDNA wrangle
eDNA

eDNA_my_prov_sf  <- eDNA %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# fix dates
eDNA_my_prov_sf$collection_date <- ymd(eDNA_my_prov_sf $collection_date )

# unique coord
eDNA_coord <- st_coordinates(eDNA_my_prov_sf) %>% as.data.frame()
eDNA_coord$xy <- paste(eDNA_coord$X, eDNA_coord$Y)
eDNA_my_prov_sf <- cbind(eDNA_my_prov_sf, eDNA_coord$xy)
eDNA_my_prov_sf

# group by coord$xy and date
eDNA.counts <- eDNA_my_prov_sf %>%
  group_by(eDNA_coord.xy,  uid) %>% 
  tally() 

# count intersections with grid and line
my.intersects <- st_intersects(grid, eDNA.counts)



# make raster
grid$counted <- lengths(my.intersects)
grid$counted <- ifelse(grid$counted == 0, NA, grid$counted)
# grid <- na.omit(grid)

str(grid)

ggplot()+
  geom_sf(data = grid, aes(fill = counted))

# palette function

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Reds",
  domain = grid$counted)

leaflet() %>%
  # add different provider tiles
  addProviderTiles("Esri.WorldImagery",
                   # give the layer a name
                   group = "World") %>%
  addPolygons(
    data = grid,
    weight = 1,
    fillOpacity = 0.5,
    color = ~pal(counted)
  )