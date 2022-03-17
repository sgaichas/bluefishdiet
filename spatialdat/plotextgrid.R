# plot the extrapolation grid, overlay with NEAMAP

# from survdat plot_data_area function 
# and sf documentation https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

library(ggplot2)

theme_set(theme_bw())

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

sites <- data.frame(longitude = northwest_atlantic_grid$Lon, 
                    latitude = northwest_atlantic_grid$Lat)

ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size=0.5) +
  coord_sf(xlim = c(-78, -66), ylim = c(35, 45), expand = FALSE)


sites <- st_as_sf(sites, coords = c("longitude","latitude"), 
                  crs = 4326, agr = "constant")

ggplot(data = ecodata::coast) +
  geom_sf() +
  geom_sf(data = sites, size = 0.05) +
  coord_sf(xlim = c(-78, -66), ylim = c(35, 45), expand = FALSE)
  #coord_sf(xlim = c(-77, -75), ylim = c(35, 37), expand = FALSE)

bfstrata <- c(3020, 3050, 3080, 3110, 3140, 3170, 3200, 3230, 
              3260, 3290, 3320, 3350, 3380, 3410, 3440, 3450, 3460)

bfgrid <- northwest_atlantic_grid %>%
  filter(stratum_number %in% bfstrata)

inshorest <- northwest_atlantic_grid %>%
  filter(stratum_number>2999 & stratum_number<3999) %>%
  select(stratum_number) %>%
  distinct() %>%
  as_vector() %>%
  unname() 

inshoregrid <- northwest_atlantic_grid %>%
  filter(stratum_number>2999 & stratum_number<3999)

offshoregrid <- northwest_atlantic_grid %>%
  filter(stratum_number<3000)

MABGBgrid <- northwest_atlantic_grid %>%
  filter(EPU %in% c("Mid_Atlantic_Bight", "Georges_Bank"))

#preset EPUs dont go all the way inshore, neeed to redefine

bfoffshore <- c(1010, 1730, 1690, 1650, 1050, 1090, 1230)

bfoffshoregrid <- northwest_atlantic_grid %>%
  filter(stratum_number %in% bfoffshore)

ggplot(data = ecodata::coast) +
  geom_sf() + 
  geom_point(data = northwest_atlantic_grid, aes(x = Lon, y = Lat), size=0.05, alpha=0.1) +
  geom_point(data = offshoregrid, aes(x = Lon, y = Lat), size=0.05, colour = "orange") +
  geom_point(data = inshoregrid, aes(x = Lon, y = Lat), size=0.05, colour = "yellow") +
  geom_point(data = bfgrid, aes(x = Lon, y = Lat), size=0.05, colour = "blue") +
  geom_point(data = bfoffshoregrid, aes(x = Lon, y = Lat), size=0.05, colour = "blue", alpha=0.1) +
  coord_sf(xlim = c(-78, -65.5), ylim = c(35, 45))
  