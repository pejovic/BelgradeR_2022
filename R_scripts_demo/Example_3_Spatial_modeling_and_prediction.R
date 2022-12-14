# Meetup BelgradeR
# 2020-12-14
# Autor: Petar Bursac i Milutin Pejovic

rm(list = ls())

# ucitavanje osnovnih paketa
library(raster)
library(caret)
library(mapview)
library(sf)
library(randomForest)
library(ranger)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tuneRanger)
library(mlr)
library(tidymodels)
library(classInt)
library(ggspatial)
library(reshape2) 

set.seed(2022)

mapviewOptions(fgb = FALSE)

# Funkcije
# ------------------------------------------------------------------------------

nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

nmae_func <-  function(obs, pred, type = "sd") {
  
  mae <- MAE(pred = pred, obs = obs)
  if (type == "sd") nmae <- mae/sd(obs)
  if (type == "mean") nmae <- mae/mean(obs)
  if (type == "maxmin") nmae <- mae/ (max(obs) - min(obs))
  if (type == "iq") nmae <- mae/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nmae <- round(nmae, 3)
  return(nmae)
  
}

# ------------------------------------------------------------------------------


trainSites <- readxl::read_xlsx("Data_demo/Example_3/soc.points.cleaned.xlsx", col_names = TRUE) %>% 
  as.data.frame()

trainSites %<>% dplyr::rename(
  SOC = SOC_procenat,
  prcp_ann = prcp_ann_2020_wgs84,
  slp_ann = slp_ann_2020_wgs84,
  tmax_ann = tmax_ann_2020_wgs84,
  tmean_ann = tmean_ann_2020_wgs84,
  tmin_ann = tmin_ann_2020_wgs84
)

dim(trainSites)

gran_sf <- sf::st_read("Data_demo/Example_3/Granica_Vojvodine.gpkg")


# Geovizuelizacija
# ------------------------------------------------------------------------------

tac_sf <- st_as_sf(trainSites, coords = c("lon", "lat"), crs = 4326)
tac_sf %<>% dplyr::select(ID, SOC) %>%
  dplyr::rename(`SOC [%]` = SOC) 

pal1 <- viridisLite::viridis(6, direction = -1)
# pal2 <- viridisLite::inferno(6, direction = 1)
# pal3 <- viridisLite::magma(6, direction = 1)
# pal4 <- rev(RColorBrewer::brewer.pal(6, "Spectral"))
# pal5 <- RColorBrewer::brewer.pal(6, "Pastel2")


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
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5),
      legend.title=element_text(size=11),
      legend.text=element_text(size=9)
    )
}

classes.oc <- classIntervals(tac_sf$`SOC [%]`, n = 6, style = "fisher")

tac_sf <- tac_sf %>%
  mutate(percent_class_oc = cut(`SOC [%]`, classes.oc$brks, include.lowest = T,dig.lab=5))


tac_map <- ggplot() +
  geom_sf(data = tac_sf,
          aes(color = percent_class_oc)) +
  scale_color_manual(values = pal1,
                     name = "SOC [%]") +
  labs(# title = "Map - spatial position of point samples [LUCAS database]",
    # subtitle = "Attribute: OC - Organic Carbon",
    xlab = "Longitude [°]",
    ylab = "Latitude [°]")+
  theme(panel.grid = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="bottom")+
  geom_sf(data = gran_sf, color = "black", fill = NA) +
  # geom_sf(data = cbound_c12, color = "red", fill = NA) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(crs = 4326)+
  my_theme()

tac_map

mapview(tac_sf, zcol = "SOC [%]", layer.name = "SOC [%]") + mapview(gran_sf, col.regions = "lightblue", legend = FALSE)

hist.soc <- ggplot(data = tac_sf, aes(`SOC [%]`)) +
  geom_histogram(#breaks=seq(0, 5, by =0.001),
    #col="cyan",
    bins = 50,
    aes(fill=..count..)) +
  # labs(title="Histogram of SOC values") +
  labs(x= "SOC [%]", y="Count") +
  scale_fill_gradient("Count", low = "blue", high = "orange") +
  theme_bw()


hist.soc

# Korelacija predikotra
# ------------------------------------------------------------------------------

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- round(cor(trainSites %>% dplyr::select(-ID)),2)

# Reorder the correlation matrix
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  # labs(title = "") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() 

gg1 <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4.5) +
  theme(
    plot.title = element_text(size = 14, face = "bold", colour = "black" ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 12, face = "bold", colour = "black"),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

gg1


# Podela podataka na trening i test skup
# ------------------------------------------------------------------------------

set.seed(124)
 
# soc_split <- initial_split(trainSites, prop = 0.70, strata = SOC)
# soc_train <- training(soc_split)
# soc_test  <-  testing(soc_split)

soc_split <- sample(nrow(trainSites), 3/4 * nrow(trainSites))
soc_train <- trainSites[soc_split, ] %>% as.data.frame() # 903 
soc_test <- trainSites[-soc_split, ] %>% as.data.frame() # 301

soc_fun <- as.formula(paste("SOC", paste(names(soc_train)[c(3:16)], collapse = "+"), sep = "~"))

# Trening hiperparametara
# -----------------------------------------------

rf.task <- makeRegrTask(data = soc_train %>% as.data.frame(), target = "SOC")

estimateTimeTuneRanger(rf.task, iters = 50 , num.threads = 15, num.trees = 1000)

tune.model <- tuneRanger(rf.task, 
                         measure = list(rrse),  
                         num.trees = 1000,
                         num.threads = 15, 
                         iters = 150, 
                         iters.warmup = 30)
tune.model$model


# Model
# ------------------------------------------------------------------------------

rf <- ranger(soc_fun, 
             data = soc_train, 
             num.threads = 15, 
             keep.inbag = TRUE, 
             mtry = 5, 
             num.trees = 1000, 
             min.node.size = 2, 
             sample.fraction = 0.8506121, 
             importance = 'impurity')
rf

pred.soc <- predict(rf, data = soc_test)

# Ocena performansi modela na test setu
# ------------------------------------------------------------------------------

# test setu
df_acc_test <- data_frame(obs = soc_test$SOC, pred = pred.soc$predictions)

caret::R2(pred = df_acc_test$pred, obs = df_acc_test$obs) 
nrmse_func(obs = df_acc_test$obs, pred = df_acc_test$pred, type = "mean") 
nmae_func(obs = df_acc_test$obs, pred = df_acc_test$pred, type = "mean") 
caret::RMSE(pred = df_acc_test$pred, obs = df_acc_test$obs) 


# Variable importance
# ------------------------------------------------------------------------------

df.imp <- rf$variable.importance %>%
  as.data.frame() %>%
  dplyr::rename(importance = ".") %>%
  tibble::rownames_to_column(., "variable") %>% as.data.frame()


# df.imp$variable[df.imp$variable == "prcp_ann_2020_wgs84"] <- "prcp_ann"
# df.imp$variable[df.imp$variable == "slp_ann_2020_wgs84"] <- "slp_ann"
# df.imp$variable[df.imp$variable == "tmax_ann_2020_wgs84"] <- "tmax_ann"
# df.imp$variable[df.imp$variable == "tmean_ann_2020_wgs84"] <- "tmean_ann"
# df.imp$variable[df.imp$variable == "tmin_ann_2020_wgs84"] <- "tmin_ann"

imp.rf <- ggplot(df.imp, aes(x = reorder(variable, importance),
                             y = importance,
                             fill = importance)) +
  geom_bar(stat="identity", position="dodge")+
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  # ggtitle("Variable Importance") +
  guides(fill=F) +
  scale_fill_gradient(low="blue", high="red")

imp.rf


# Predikcija
# ------------------------------------------------------------------------------


theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Karta organskog ugljenika

pal_soc <- c("#440154", "#471467", "#472677", "#453781", "#3f4689", "#39558b", "#32638d", "#2c708e", "#287d8e", "#228a8e", "#1f968b", "#20a386", "#29af7f", "#3cbc74", "#56c766", "#73d055", "#94d840", "#b8df29", "#dce319", "#fde725")
cut_soc <- c(1.24, 1.45, 1.47, 1.48, 1.49, 1.5, 1.50746647977831, 1.51358362722399, 1.52123006153109, 1.52734720897677, 1.53499364328387, 1.54264007759096, 1.5533450856209, 1.57016724109652, 1.62, 1.7, 1.75, 1.78, 1.81, 2.78)

rast_pred_soc <- raster::raster("Data_demo/Example_3/SOC_Vojvodina_g_kg_CERESpredicition.tif")
pred_spdf_soc <- as(rast_pred_soc, "SpatialPixelsDataFrame")

soc_rast <- as.data.frame(pred_spdf_soc) %>%
  rename(`SOC [g/kg]` = `SOC_Vojvodina_g_kg_CERESpredicition`) %>%
  mutate(ValueCut = cut(`SOC [g/kg]`, breaks = cut_soc))

karta_soc <- ggplot() +
  geom_raster(data = soc_rast, aes(x = x,
                                   y = y,
                                   fill = ValueCut)) +
  scale_fill_manual(values = pal_soc,  name = "SOC [g/kg]")+
  theme_map() +
  theme(legend.position = "right") +
  #edit legends
  guides(fill = guide_legend(reverse = TRUE))


karta_soc


mapview(rast_pred_soc, layer.name = "SOC", na.color = "transparent", trim = TRUE, na.label = NA)

# Karta zaliha organskog ugljenika - SOC Stock

pal_stock <- c("#404040", "#5a5a5a", "#737373", "#8d8d8d", "#a7a7a7", "#bebebe", "#cccccc", "#dbdbdb", "#eaeaea", "#f8f8f8", "#fef6f2", "#fce3d8", "#fad0bd", "#f7bda3", "#f5aa89", "#ee8b72", "#e5685e", "#dc4549", "#d32234", "#ca0020")
cut_stock <- c(11, 14.6, 15.1, 15.4, 15.6, 15.8, 15.9, 16.1, 16.3, 16.5, 16.6, 16.8, 17, 17.2, 17.3, 17.6, 17.8, 18.3, 19.1, 29)


rast_pred <- raster::raster("Data_demo/Example_3/SOC_Stock_Vojvodina_kg_m2_CERESpredicition.tif")
pred_spdf <- as(rast_pred, "SpatialPixelsDataFrame")

stock_rast <- as.data.frame(pred_spdf) %>%
  rename(`SOC Stock [kg/m2]` = `SOC_Stock_Vojvodina_kg_m2_CERESpredicition`) %>%
  mutate(ValueCut=cut(`SOC Stock [kg/m2]`, breaks = cut_stock))

karta_soc_stock <- ggplot() +
  geom_raster(data = stock_rast, aes(x = x,
                                     y = y,
                                     fill = ValueCut)) +# alpha = value,
  #scale_alpha(name = "", range = c(0.5,1))  +
  # scale_fill_distiller(palette = "BrBG", direction = -1) +
  # scale_fill_viridis(option = "H", direction = 1) + # D
  scale_fill_manual(values = pal_stock,  name = "SOC Stock [kg/m2]")+
  theme_map() +
  theme(legend.position = "right") +
  #edit legends
  guides(fill = guide_legend(reverse = TRUE))

karta_soc_stock


mapview(rast_pred, layer.name = "SOC Stock", na.color = "transparent", trim = TRUE, na.label = NA)


