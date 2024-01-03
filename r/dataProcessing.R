library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(sp)
library(spdep)
library(Matrix)
library(INLA)

setwd("/Users/MarkusTraetli/Desktop/markus-2023/r") #MAKE SURE THIS IS CORRECT
stopifnot(file.exists("../data/26Feb_05Jan_Madrid_Muni_Combined.csv")) #Make sure the files exist
options(dplyr.summarise.inform = FALSE)

############################################################################################
#### Read the covid-19 case counts and mobility data. Format to suitable data frames / matrices. Prepare data for models
############################################################################################

############
#### Madrid Covid case counts

# Start by reading data and reformating by pivot
madrid_cases_region = read.csv("../data/26Feb_05Jan_Madrid_Muni_Combined.csv") #Read csv. Path is relative, so check that the working directory is set appropriately
madrid_cases_region = select(madrid_cases_region, MunicipalityName, MunicipalityCode,TotalPopulation, Lat, Lon, starts_with("day")) #Select columns
madrid_cases_region = pivot_longer(madrid_cases_region, #Pivot the table. Change from columns day_1,... to rows. Observations go to column cases instead. Get separate day observations for each muncipality
                            cols = starts_with("day"),
                            names_to = "time",
                            values_to = "cases",
                            values_drop_na = TRUE)

# Make dataframe aggragated on time
madrid_cases_date = madrid_cases_region #Make unaggragated dataframe with aggregated case count over time,
madrid_cases_date = ungroup(madrid_cases_date) 
madrid_cases_date = mutate(madrid_cases_date, time = as.numeric(str_sub(time, start=5))) 
madrid_cases_date = group_by(madrid_cases_date, time) %>% summarise(cases=sum(cases))
madrid_cases_date = filter(madrid_cases_date, time < 97)
time_idx = c(seq.Date(from = as.Date("2020/02/26"), by="day", to= as.Date("2020/05/31")))
madrid_cases_date$time = rep(time_idx, nrow(madrid_cases_date)/length(time_idx))

# Make dataframe aggregated on muncipality
madrid_cases_region = mutate(madrid_cases_region, time = as.numeric(str_sub(time, start=5))) #Remove "day_" from day_x
madrid_cases_region = filter(madrid_cases_region, time <= 95) #restrict cases to before June 1, 95 is number of days between Feb 26 and May 31 (inclusive), 2020 was a leap year
madrid_cases_region = group_by(madrid_cases_region, MunicipalityName, TotalPopulation, MunicipalityCode, Lat, Lon) %>% #Group by and aggregate case counts based on muncipality_code. Keep MuncipalityName, TotalPopulation, Lat and Lon as is.
  summarise(cases = sum(cases)) %>%
  arrange(MunicipalityCode) %>% 
  ungroup()
madrid_cases_region = mutate(madrid_cases_region, cases_per_thousand = (cases/TotalPopulation)*1000)

############
#### Madrid polygon for plotting map
muni <- st_read("../data/madrid_mobility/boundary") #fix this file path at some point?

attributes(muni)$proj4string = sp::CRS("+init=epsg:4326")
muni$natcode[which(muni$nameunit=="Madrid")] <- "34132828000" #This municipality was given wrong code, so fix
muni$MunicipalityCode = as.numeric(str_sub(muni$natcode, start=7))
muni = muni %>% arrange(MunicipalityCode)
madrid_sf = left_join(x = muni, y = madrid_cases_region, by="MunicipalityCode")


############
#### Madrid mobility data
files <- list.files(path = "../data/madrid_mobility/Madrid",pattern=".csv")[1:92] #only take movement up until end of first wave

madrid_mobility <- matrix(0, nrow= 179, ncol =179)
madrid_mobility_week1 <- matrix(0, nrow= 179, ncol =179)
total_madrid <- vector(length = length(files))

# sum up mobility data
for (i in 1:length(files)){
  tmp <- read.csv(paste0("../data/madrid_mobility/Madrid/", files[i]) )
  madrid_mobility <- madrid_mobility + tmp[,-1]
  total_madrid[i] <- sum(tmp[,-1])
}
for (i in 1:7){
  tmp <- read.csv(paste0("../data/madrid_mobility/Madrid/", files[i]) )
  madrid_mobility_week1 <- madrid_mobility_week1 + tmp[,-1]
}

############
#### Madrid fixed and random effects

#Prepare neighbor graph
n_regions = length(madrid_sf$natcode)
neighbor_mat <- spdep::poly2nb(st_make_valid(muni), row.names = muni$MunicipalityCode)
nb2INLA("../data/neighbors_madrid.txt", nb = neighbor_mat)

#Indices for random effects
madrid_sf$theta = 1:n_regions
madrid_sf$phi = 1:n_regions
madrid_sf$gamma = 1:n_regions

#Fixed effect
prop_travel_in_region = diag(madrid_mobility_week1 %>% as.matrix)/madrid_sf$TotalPopulation
beta = rep(0, n_regions)
for (i in 1:n_regions) {
  beta[i] = (prop_travel_in_region[i] - mean(prop_travel_in_region[-i]))/sqrt(var(prop_travel_in_region[-i]))
}
madrid_sf$beta = beta

#Prepare mobility graph
mobility_structure_madrid = as.matrix(madrid_mobility_week1)
diag(mobility_structure_madrid) = 0
mobility_structure_madrid[mobility_structure_madrid < 0.5] = 0
mobility_structure_madrid = mobility_structure_madrid + t(mobility_structure_madrid)
diag(mobility_structure_madrid) = -rowSums(mobility_structure_madrid)
mobility_structure_madrid = -mobility_structure_madrid
mobility_structure_madrid = Matrix(mobility_structure_madrid, sparse = TRUE)
mobility_structure_madrid = inla.scale.model(mobility_structure_madrid, constr = list(A= matrix(1,1,n_regions), e = 0))

############
#### Castilla y Leon case counts
castilla_cases_region = read.csv("../data/01March_08Jan_CYL_combined.csv") #Read csv. Path is relative, so check that the working directory is set appropriately
castilla_cases_region = select(castilla_cases_region, hzone_name, hzone_code, total_pop, lat, lon, starts_with("infected_day") ) #Select columns
castilla_cases_region = pivot_longer(castilla_cases_region, #Pivot the table. Change from columns day_1,... to rows. Observations go to column cases instead. Get separate day observations for each muncipality
                            cols = starts_with("infected_day"),
                            names_to = "time",
                            values_to = "cases",
                            values_drop_na = TRUE)

# Make dataframe aggragated on time
castilla_cases_date = castilla_cases_region #Make unaggragated dataframe with aggregated case count over time,
castilla_cases_date = ungroup(castilla_cases_date) 
castilla_cases_date = mutate(castilla_cases_date, time = as.numeric(str_sub(time, start=14))) 
castilla_cases_date = group_by(castilla_cases_date, time) %>% summarise(cases=sum(cases))
castilla_cases_date = filter(castilla_cases_date, time <= 92)
time_idx = c(seq.Date(from = as.Date("2020/03/01"), by="day", to = as.Date("2020/05/31")))
castilla_cases_date$time = rep(time_idx, nrow(castilla_cases_date)/length(time_idx))

#Make dataframe aggregated on region
castilla_cases_region = mutate(castilla_cases_region, time = as.numeric(str_sub(time, start=14))) #Remove "infected_day_" from "_infected_day_x"
castilla_cases_region = filter(castilla_cases_region, time <= 92) #restrict cases to before June 1, 95 is number of days between March 01 and May 31 (inclusive)
castilla_cases_region = mutate(castilla_cases_region, hzone_code=ifelse(hzone_code %in% c(170813,170814), 170811, hzone_code )) # codes 170813, 170814 are collapsed
castilla_cases_region = group_by(castilla_cases_region, hzone_code) %>% #Group by and aggregate case counts based on muncipality_code. Keep MuncipalityName, TotalPopulation, Lat and Lon as is.
  summarise(cases = sum(cases), lat=first(lat), lon=first(lon), hzone_name=first(hzone_name), total_pop=sum(unique(total_pop)) ) %>%
  arrange(hzone_code) %>% 
  ungroup() 
castilla_cases_region = mutate(castilla_cases_region, cases_per_thousand = (cases/total_pop)*1000)

############
#### Castilla y Leon mobility data
files <- list.files(path = "../data/castilla_mobility/Castilla Y Leon",pattern=".csv")[1:92] #only take movement up until end of first wave

castilla_mobility <- matrix(0, nrow = 245, ncol = 245)
castilla_mobility_week1 <- matrix(0, nrow= 245, ncol =245)
total_castilla <- vector(length = length(files))

# sum up mobility data
for (i in 1:length(files)){
  tmp <- read.csv(paste0("../data/castilla_mobility/Castilla Y Leon/", files[i]) )
  castilla_mobility <- castilla_mobility + tmp[,-1]
  total_castilla[i] <- sum(tmp[,-1])
}
for (i in 1:7){
  tmp <- read.csv(paste0("../data/castilla_mobility/Castilla Y Leon/", files[i]) )
  castilla_mobility_week1 <- castilla_mobility_week1 + tmp[,-1]
}

############
#### Castilla y Leon polygon for plotting on map
muni <- st_read("../data/castilla_mobility/boundary") #fix this file path at some point?
attributes(muni)$proj4string = sp::CRS("+init=epsg:4326")
muni$hzone_code = as.numeric(muni$hzcode)
muni = muni %>% arrange(hzone_code)
castilla_sf = left_join(x = muni, y = castilla_cases_region, by = "hzone_code")

############
#### Castilla y Leon fixed and random effects

# Create neighbor matrix for the region
n_regions = length(castilla_sf$hzone_name)
neighbor_mat <- spdep::poly2nb(muni, row.names = muni$natcode)
neighbor_mat_cop = nb2listw(neighbor_mat, zero.policy = T)
neighbor_mat = nb2mat(neighbor_mat, zero.policy = T) #Transform to matrix so we can edit and adjust for disjoint region
island_zone = match("CONDADO DE TREVIÑO", castilla_sf$hzone_name) #Get index for the disjoint region
to_connect_zone = match("MIRANDA OESTE", castilla_sf$hzone_name) #Index to connect to

#Fix islands row and in neighbor matrix. Only rows need to sum to 1
island_row = rep(0.0, n_regions)
island_row[to_connect_zone] = 1.0
neighbor_mat[island_zone,] = island_row

#Fix the zone connected to's rows in neighbor matrix. Only rows need to sum to 1
connected_row = neighbor_mat[to_connect_zone,]
n_neigh = sum(connected_row != 0) + 1
connected_row[connected_row != 0] = 1/n_neigh
connected_row[island_zone] = 1/n_neigh
neighbor_mat[to_connect_zone,] = connected_row

#Transform to text file
neighbor_mat = mat2listw(neighbor_mat, style = neighbor_mat_cop$style)
nb2INLA("../data/neighbors_castilla.txt", nb = neighbor_mat$neighbours)

#Prepare indices for the random effects of the model.
castilla_sf$phi = 1:n_regions
castilla_sf$theta = 1:n_regions
castilla_sf$gamma = 1:n_regions

#Prepare the fixed effect
prop_travel_in_region = diag(castilla_mobility_week1 %>% as.matrix)/castilla_sf$total_pop
beta = rep(0, n_regions)
for (i in 1:n_regions) {
  beta[i] = (prop_travel_in_region[i] - mean(prop_travel_in_region[-i]))/sqrt(var(prop_travel_in_region[-i]))
}
castilla_sf$beta = beta

#Prepare the mobility structure graph for the mobility data random effect
mobility_structure_castilla = as.matrix(castilla_mobility_week1)
diag(mobility_structure_castilla) = 0
mobility_structure_castilla[mobility_structure_castilla < 0.5] = 0
mobility_structure_castilla = -mobility_structure_castilla - t(mobility_structure_castilla)
diag(mobility_structure_castilla) = -colSums(mobility_structure_castilla)
mobility_structure_castilla = Matrix(mobility_structure_castilla, sparse = TRUE)
mobility_structure_castilla = inla.scale.model(mobility_structure_castilla, constr = list(A= matrix(1,1,n_regions), e = 0))

############################################################################################
#### Save data to RData object
############################################################################################

total_madrid = as.data.frame(total_madrid) #Convert so they can be saved
total_castilla = as.data.frame(total_castilla)
rm(tmp, time_idx, beta, i, n_regions, prop_travel_in_region, files, muni, neighbor_mat) #Remove from environment as they are no longer necessary
save(list = c("castilla_cases_date","castilla_cases_region","castilla_mobility","castilla_mobility_week1","castilla_sf","mobility_structure_castilla", "total_castilla",
              "madrid_cases_date","madrid_cases_region","madrid_mobility","madrid_mobility_week1","madrid_sf", "mobility_structure_madrid", "total_madrid"),
     file = "../data/processed_case_and_mobility.RData") #Save


















