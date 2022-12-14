# Meetup BelgradeR
# 2020-12-14
# Autor: Petar Bursac i Milutin Pejovic

# Ucitavanje osnovnih paketa
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(viridis)

# Ucitavanje paketa za rad sa prostornim podacima
library(sf)
library(raster)
library(terra)
library(stars)
library(mapview)
library(ggspatial)
library(RStoolbox)

mapviewOptions(fgb = FALSE) 

# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/intro.html

# Primer 1 - vekotrski podaci
# ------------------------------------------------------------------------------

# Uictavanje podataka
# ------------------------------------------------------------------------------

# OpenStreetMap podaci za deo podrucja Grada Beograda
# Geofabrik baza podataka - https://download.geofabrik.de/

tacke <- sf::st_read(dsn = "Data_demo/Example_1/OSM_Beograd.gpkg", layer = "Tacke") %>% dplyr::select(-name)
linije <- sf::st_read(dsn = "Data_demo/Example_1/OSM_Beograd.gpkg", layer = "Putevi") %>% dplyr::select(-name)
poligoni <- sf::st_read(dsn = "Data_demo/Example_1/OSM_Beograd.gpkg", layer = "Objekti") %>% dplyr::select(-name)

# Dataframe sa entitetima gde su koordinate date kao dve kolone (lon, lat)
# ------------------------------------------------------------------------------
tacke_df <- tacke %>% 
  st_transform(., 4326) %>% 
  dplyr::mutate(lon = st_coordinates(.)[, 1],
                lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  as.data.frame()

head(tacke_df, 10)

# CRS - Coordinat Reference System reprezentacija
# ------------------------------------------------------------------------------

# Baza dostupnih EPSG kodova - https://epsg.io/

crs_wgs84 <- st_crs(4326) # WGS84 elipsoidni koordinatni sistem ima EPSG kod 4326
class(crs_wgs84) 
crs_wgs84 # kod crs objekta uočavaju se dve stvari: EPSG kod i WKT2 string
cat(crs_wgs84$wkt) # direktan pristup wkt elementu crs objekta
crs_wgs84$epsg # direktan pristup epsg elementu crs objekta
crs_wgs84$proj4string # direktan pristup proj4string elementu crs objekta

# Kreiranje sf objekta i konverzija u koordinatni sistem u projekciji
# ------------------------------------------------------------------------------
tacke <- st_as_sf(tacke_df, coords = c("lon", "lat"), crs = 4326)
st_crs(32634)
tacke %<>% st_transform(32634)

# Pregled dostupnih podataka
tacke
linije
poligoni

# Geo-vizuelizacija 
# ------------------------------------------------------------------------------

# Base plot funkcija
# ------------------------------------------------------------------------------

plot(tacke)
plot(linije)
plot(poligoni)

# ggplot 
# ------------------------------------------------------------------------------

# Plot settings
my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 13),
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "none",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5),
      legend.title=element_text(size=11), 
      legend.text=element_text(size=9)
    )
}
theme_set(my_theme())

tac_map <- ggplot() +
  geom_sf(data = tacke,
          aes(color = fclass)) +
  labs(# title = "Spatial distribution of points of interest across Belgrade",
    col = "fclass: ",
    xlab = "Longitude [°]",
    ylab = "Latitude [°]")+
  my_theme() +
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "none")+
  coord_sf(datum = sf::st_crs(32634))+
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

  
tac_map

ggplot() +
  geom_sf(data = linije)

lin_map <- ggplot() +
  geom_sf(data = linije,
          aes(color = fclass)) +
  labs(# title = "Spatial distribution of roads across Belgarde",
    col = "fclass: ",
    xlab = "Longitude [°]",
    ylab = "Latitude [°]")+
  #my_theme() +
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  coord_sf(datum = sf::st_crs(32634))+
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)

lin_map

poligoni %<>% dplyr::mutate(type = case_when(is.na(type) ~ "unclassified",
                                             !is.na(type) ~ type))

pol_map <- ggplot() +
  geom_sf(data = poligoni,
          aes(fill = type)) +
  labs(# title = "Spatial distribution of objects across Belgrade",
    col = "type: ",
    xlab = "Longitude [°]",
    ylab = "Latitude [°]")+
  my_theme() +
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom")+
  coord_sf(datum = sf::st_crs(32634))+
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)


pol_map

gridExtra::grid.arrange(tac_map + theme(legend.position = "none"), 
                        lin_map + theme(legend.position = "none"), 
                        pol_map + theme(legend.position = "none"),
                        ncol = 2)


# mapview - interaktivna web karta
# ------------------------------------------------------------------------------

mapview(tacke) + 
  mapview(linije, zcol = "fclass") + 
  mapview(poligoni, zcol = "type")


# Primer 2 - rasterski podaci
# ------------------------------------------------------------------------------

# Primer kreiranja rastera
# ------------------------------------------------------------------------------

# Putem paketa "raster"

extent(7440500, 7475000, 4948000, 4976500) # xmin, xmax, ymin, ymax
podrucje_beograda <- raster(extent(7440500, 7475000, 4948000, 4976500), res = 100)
values(podrucje_beograda) <- rnorm(ncell(podrucje_beograda)) 
summary(values(podrucje_beograda))

podrucje_beograda # CRS:NA

plot(podrucje_beograda)

crs(podrucje_beograda) <- 3909
crs(podrucje_beograda) <- "EPSG:3909"
crs(podrucje_beograda) <- st_crs(3909)$wkt # a WKT string
crs(podrucje_beograda) <- CRS(SRS_string = "EPSG:3909") # an sp CRS object

cat(wkt(podrucje_beograda))


# Putem paketa "terra"

podrucje_beograda <- rast(xmin = 7440500, xmax = 7475000, ymin = 4948000, ymax = 4976500, resolution = 100)
values(podrucje_beograda) <- rnorm(ncell(podrucje_beograda)) 
podrucje_beograda # CRS:NA
terra::crs(podrucje_beograda) <- "epsg:3909"
plot(podrucje_beograda)


mapview::mapview(stars::st_as_stars(podrucje_beograda), 
                 na.color = "transparent", 
                 trim = TRUE, 
                 layer.name = "Random field")


# Multispektralni satelitski podaci
# ------------------------------------------------------------------------------

# Učitati 3 multispektralna satelitska snimka (3 epohe), sa više dostupnih kanala.
# Kreirati raster stack kao kombinaciju sve tri epohe.
# Vizualizovati podatke u formi kolor kompozita i falš kolor kompozita.
# Sračunati vegetacioni indeks za sve tri epohe na nivou piksela.
# Kreirati histogram NDVI indeksa.


files.ms <- list.files("Data_demo/Example_1/Multispektralni_snimci/", 
                    full.names = TRUE,
                    pattern = ".tif")

files.ms

# Putem paketa "raster"
rasStack <- lapply(files.ms, stack)
rasStack

# Putem paketa "terra"
spatStack <- rast(files.ms)

# cat(wkt(rasStack[[1]]))
# 
# for(i in 1:length(rasStack)){
#   crs(rasStack[[i]]) <- 32634
# }

# Funkcija za vizuelizaciju podataka u formi kolor kompozita i falš kolor kompozita:
# ------------------------------------------------------------------------------

files.names <- list.files("Data_demo/Example_1/Multispektralni_snimci/", full.names = F,
                          pattern = ".tif")
files.names %<>% stringr::str_remove(., pattern = ".tif")
files.names

plotS2parcel <- function(listS2parc = listS2parc, files.names = files.names, id = 1){
  
  c.plot <- ggRGB(listS2parc[[id]], r = 3, g = 2, b = 1) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  f.plot <- ggRGB(listS2parc[[id]], r = 7, g = 3, b = 2) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Falsh color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  gridCF <- grid.arrange(c.plot, f.plot, ncol = 1, nrow = 2)
  return(gridCF)
}

id1 <- plotS2parcel(listS2parc = rasStack, files.names = files.names, id = 1)
id2 <- plotS2parcel(listS2parc = rasStack, files.names = files.names, id = 2)
id3 <- plotS2parcel(listS2parc = rasStack, files.names = files.names, id = 3)


# Racunanje NDVI vegetacionog indeksa
# ------------------------------------------------------------------------------

# Pogledati: https://www.indexdatabase.de/
  
# (NIR - Red) / (NIR + Red)
# Kanali: NIR = B08, i = 7 i RED = BO2, i = 3;

# Funkcija za računanje NDVI indeksa nad svim snimcima: 

rasListNDVI <- list()
for(i in 1:length(rasStack)){
  rasListNDVI[[i]] <- (rasStack[[i]][[7]] - rasStack[[i]][[3]]) / (rasStack[[i]][[7]] + rasStack[[i]][[3]])
}

rasListNDVI

names(rasListNDVI) <- paste(files.names, "_NDVI", sep = "")
names(rasListNDVI)

plotS2parcel <- function(listS2parc = listS2parc, listS2parcNDVI = listS2parcNDVI, files.names = files.names, id = 1){
  c.plot <- ggRGB(listS2parc[[id]], r = 3, g = 2, b = 1) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  f.plot <- ggRGB(listS2parc[[id]], r = 7, g = 3, b = 2) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Falsh color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  star_pred <- st_as_stars(listS2parcNDVI[[id]])
  
  ndvi.plot <- ggplot()+
    geom_stars(data = star_pred)+
    scale_fill_viridis(option = "D", na.value = NA, name = "NDVI: ")+
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - NDVI",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank(), 
          legend.position = "bottom")
  
  df <- as.data.frame(listS2parcNDVI[[id]])
  hist.plot <- ggplot(data=df, aes(layer)) + 
    geom_histogram(binwidth = 0.005,
                   col="red", 
                   aes(y = ..density..,
                       fill = ..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red")+
    stat_function(fun = dnorm, 
                  color = "black",
                  size = 1.5,
                  args = list(mean = mean(df$layer), sd = sd(df$layer)))+
    labs(x = "NDVI values", y = "Count",
         title = "Sentinel 2 satellite image - NDVI histogram",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank(), 
          legend.position = "bottom")+
    annotate("text",x = 0.5, y = 5.5, label = paste("Mean:",round(mean(df$layer),digits = 2)))+
    annotate("text",x = 0.5, y = 5, label = paste("SD:",round(sd(df$layer),digits = 2)))
  
  gridCF <- grid.arrange(c.plot, f.plot, ndvi.plot, hist.plot, ncol = 2)
  return(gridCF)
}

id1 <- plotS2parcel(listS2parc = rasStack, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 1)
id2 <- plotS2parcel(listS2parc = rasStack, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 2)
id3 <- plotS2parcel(listS2parc = rasStack, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 3)


mv1 <- mapview(rasListNDVI[[1]], layer.name = files.names[1], na.color = "transparent", trim = TRUE, na.label = NA)
mv2 <- mapview(rasListNDVI[[2]], layer.name = files.names[2], na.color = "transparent", trim = TRUE, na.label = NA)
mv3 <- mapview(rasListNDVI[[3]], layer.name = files.names[3], na.color = "transparent", trim = TRUE, na.label = NA)


library(leafsync)

sync(mv1, mv2, mv3, ncol = 2)


# # Kao objekat terra paketa
# # ------------------------------------------------------------------------------
# 
# ndvi.terra <- rast(rasListNDVI[[1]])
# plot(ndvi.terra)
# 


# Mapedit
# ------------------------------------------------------------------------------

library(mapedit)

kartirani_entiteti <- mapedit::editMap()
kartirani_entiteti

mapview::mapview(kartirani_entiteti)



