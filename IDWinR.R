library(tidyverse) 
library(sf)
library(gstat)
library(stars) |> suppressPackageStartupMessages()

# read data
no2 <- read_csv("Dissertation/for_idw/known_dataNC6.csv", show_col_types = FALSE)

crs <- st_crs("EPSG:3857")
st_as_sf(no2, crs = "OGC:CRS84", coords = c("POINT_X", "POINT_Y")) |>
  st_transform(crs) -> no2.sf

# create grid /boundary
read_sf("Dissertation/data/sz/shenzhen_census_community.shp") |> st_transform(crs) -> de
ggplot() + geom_sf(data = de) 


st_bbox(de) |>
  st_as_stars(dx = 400) |>
  st_crop(de) -> grd
grd

# idw
i <- idw(avg_time~1, no2.sf, grd)

ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)

# idw results to sf
st_as_sf(i, crs = "OGC:CRS84", coords = c("x", "y")) |>
  st_transform(crs) -> i.sf

ggplot() + geom_sf(data = i.sf, mapping = aes(col = var1.pred))

# unknown data
unknown_no2 <- read_csv("Dissertation/for_idw/unknown_dataNC6.csv", show_col_types = FALSE)
crs <- st_crs("EPSG:3857")
st_as_sf(unknown_no2, crs = "OGC:CRS84", coords = c("POINT_X", "POINT_Y")) |>
  st_transform(crs) -> unknown_no2.sf

ggplot() + geom_sf(data = unknown_no2.sf)


out <- st_intersection(unknown_no2.sf, i.sf)

st_write(out, "Dissertation/for_idw/idw/out3.csv")

#------------------------------------

# create grid /boundary
read_sf("Dissertation/data/sz/shenzhen_census_community.shp") |> st_transform(crs) -> de
ggplot() + geom_sf(data = de) 
st_bbox(de) |>
  st_as_stars(dx = 400) |>
  st_crop(de) -> grd
grd
# grd is the parameter


# to a function
my_diw <- function(known_data, unknown_data, sz_boundary, living_nc){
  # known data
  no2 <- known_data
  crs <- st_crs("EPSG:3857")
  st_as_sf(no2, crs = "OGC:CRS84", coords = c("POINT_X", "POINT_Y")) |>
    st_transform(crs) -> no2.sf
  
  # idw
  i <- idw(avg_time~1, no2.sf, sz_boundary)
  
  # idw results to sf
  st_as_sf(i, crs = "OGC:CRS84", coords = c("x", "y")) |>
    st_transform(crs) -> i.sf
  
  # unknown data
  unknown_no2 <- unknown_data
  crs <- st_crs("EPSG:3857")
  st_as_sf(unknown_no2, crs = "OGC:CRS84", coords = c("POINT_X", "POINT_Y")) |>
    st_transform(crs) -> unknown_no2.sf
  
  # spatial intersection
  out <- st_intersection(unknown_no2.sf, i.sf)
  
  path<- paste('Dissertation/for_idw/idw/idwforNC',as.character(living_nc), '.csv',sep = "")
  st_write(out, path)
}

# ------
livingNC <- read_csv("Dissertation/for_idw/livingNC.csv", show_col_types = FALSE)

for (n in 1:577){
  current_livingNC <-livingNC[n,2]
  path <-paste('Dissertation/for_idw/known_dataNC',as.character(current_livingNC), '.csv',sep = "")
  known_data <-read_csv(path)
  path <-paste('Dissertation/for_idw/unknown_dataNC',as.character(current_livingNC), '.csv',sep = "")
  unknown_data<-read_csv(path)
  my_diw(known_data,unknown_data,grd,current_livingNC)
  
}



