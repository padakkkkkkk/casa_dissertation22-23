
library(sf)
library(mapview)
library(spdep)

library(tidyr)

library(tidyverse)

cuencas <- read_sf("Dissertation/data/sz/shenzhen_census_community.shp")


cuencas_mat <- nb2mat(poly2nb(cuencas), style = "B")
cuencas_mat[1:10, 1:10]

mydata <- tibble::rowid_to_column(dataframe_data, "ID")

df_long <- pivot_longer(mydata, cols = "ID", names_to = 'id2', values_to = 'contiguity')

mydata %>%
  pivot_longer(!ID, names_to = 'id2', values_to = "contiguity") ->df_long


st_write(df_long, "Dissertation/for_luti/contiguity.csv")