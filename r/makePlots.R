library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(sf)
library(sp)
library(geosphere)
library(ggspatial)
library(tables)
library(ggnewscale)
library(Hmisc)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(xtable)
library(visNetwork)

setwd("/Users/MarkusTraetli/Desktop/markus-2023/r") #MAKE SURE THIS IS CORRECT
options(dplyr.summarise.inform = FALSE)

if(!file.exists("../data/inference_results.RData")){
  source("inference.R")
} else {
  load("../data/processed_case_and_mobility.RData")
  load("../data/inference_results.RData") 
}
# The following data frames are loaded
# castilla_cases_region & madrid_cases_region : sum of case counts for each municipality in the two regions. 
# castilla_cases_time & madrid_cases_time : sum of case counts over time for entire region
# castilla_mobility & madrid_mobility : mobility data over entire duration
# castilla_mobility_week1 & madrid_mobility_week1 : mobility data over the first week
# total_castilla & total_madrid : total amount of travel in each region

############################################################################################
#### Figures showing case counts over time in Madrid and Castilla y Leon
############################################################################################

dates = seq.Date(from = as.Date("2020-03-01"), to = as.Date("2020-05-31"), by = "day") #Dates for plot

#### Daily trips in Madrid
par(mfcol=c(2,2), oma=c(0,0,0,0), mar = c(2,4,0.5,0.5), cex = 1.2)
plot(dates, total_madrid$total_madrid/1000000, type = "l", ylab = "Daily Trips (millions)", xlab = "", xaxt = "n")
axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
abline(v = as.Date("2020-05-11"), col = "blue", lty =2)
legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue"), lty=c(2,2), bg = "white")

#### Daily cases in Madrid
plot(madrid_cases_date$time, madrid_cases_date$cases, type = "l",col =, ylab = "Daily Cases", xlab = "", xaxt="n")
axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue", "purple"), lty=c(2,2), bg = "white")

#### Daily trips in Castilla y Leon
plot(dates, total_castilla$total_castilla/1000000, type = "l", ylab = "Daily Trips (millions)", xlab = "", xaxt = "n")
axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue"), lty=c(2,2), bg = "white")

#### Daily cases in Castilla y Leon
plot(castilla_cases_date$time, castilla_cases_date$cases, type = "l",col =, ylab = "Daily Cases", xlab = "", xaxt = "n")
axis(1, at = c(as.Date("2020-03-01"), as.Date("2020-04-01"), as.Date("2020-05-01"), as.Date("2020-06-01")), labels = c("March", "April", "May", "June"))
abline(v = as.Date("2020-03-14"), col = "red", lty = 2)
abline(v = as.Date("2020-05-11"), col = "blue", lty = 2)
legend("topright",legend = c("lockdown start","lockdown lifted"), col=c("red","blue", "purple"), lty=c(2,2), bg = "white")




############################################################################################
#### Figures showing case counts
############################################################################################

###########
# Function for case count plot in heat map style
case_count_plot <- function(sf_data){
  scale_col = heat.colors(30, rev=TRUE) #Divide color gradient into 30 
  scale = scale_col[c(1,7,10,15,18,22,26,30)] #Select color scale to be more red
  #scale = heat.colors(8, rev= TRUE)
  ggplot(data = sf_data) + 
    geom_sf(aes(fill = `cases_per_thousand`), #Plots cases per thousand
            alpha = 1,
            color="black") + 
    annotation_scale(width_hint = 0.25, pad_x = unit(0.8,"cm"), pad_y = unit(-0.003, "cm")) + #Length scale in bottom left
    ggspatial::annotation_north_arrow( #North arrow in bottom left
      style = ggspatial::north_arrow_orienteering(),
      location = "bl",
      pad_x = unit(0, "cm"),
      height = unit(0.55, "cm"),
      width = unit(0.55, "cm")) +
    theme(axis.title.x = element_blank(), #Remove axis and background grid
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size = 17)) +
    guides(fill=guide_legend(title=NULL,
                             reverse = TRUE,
                             label.position = "right",
                             )) + #Remove colorbar title
    binned_scale( #Scaling the color
      aesthetics = "fill",
      scale_name = "gradientn",
      palette = function(x) c(scale),
      labels = function(x){x},
      breaks = c(0,2,5,10,20,30,50,100),
      limits= c(0,100),
      guide = "colorscale")
}

###########
# Castilla y Leon

#Plot using the function above
pdf("../figures/Castilla_case_counts.pdf", width = 6, height = 4)
case_count_plot(castilla_sf)
dev.off()


###########
# Madrid
#Plot using the function above
pdf("../figures/Madrid_case_counts.pdf", width = 6, height = 4)
case_count_plot(madrid_sf)
dev.off()

############################################################################################
#### Figures showing mobility data
############################################################################################

#########
#Function for plotting mobility data
mobility_plot <- function(sf_data, fill, do_bar = FALSE, do_annotate = F){
  scale = terrain.colors(15, rev = TRUE)
  plot_out = ggplot(data = sf_data) + 
    geom_sf(aes(fill = fill), #Plots cases per thousand
            alpha = 0.9,
            color="black", 
            show.legend = do_bar) +
    theme(axis.title.x = element_blank(), #Remove axis and background grid
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size = 17)) +
    guides(fill=guide_legend(title=NULL, reverse = TRUE, label.position = "right")) +  #Remove colorbar title
    binned_scale( #Scaling the color
      aesthetics = "fill",
      scale_name = "stepsn",
      palette =  function(x) c(scale),
      labels = c("0", "10", "50", "100", "500", "1k", "5k", "10k", "50k", "100k", "500k", "1M", "5M", "10M", "50M"),
      breaks = c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000),
      limits = c(0,50000000),
      guide = "colorsteps")
  if(do_annotate){
    plot_out = plot_out + 
      annotation_scale(width_hint = 0.25, pad_x = unit(0.8,"cm"), pad_y = unit(-0.003, "cm")) + #Length scale in bottom left
      ggspatial::annotation_north_arrow( #North arrow in bottom left
        style = ggspatial::north_arrow_orienteering(),
        location = "bl",
        pad_x = unit(0, "cm"),
        height = unit(0.55, "cm"),
        width = unit(0.55, "cm"))  
  }
  plot_out
}


########
#Castilla 

#Attatch mobility data to sf_data
castilla_sf$within <- diag(castilla_mobility_week1 %>% as.matrix)
castilla_mobility_week1_tmp <- castilla_mobility_week1- diag(diag(castilla_mobility_week1 %>% as.matrix))
castilla_sf$from <- rowSums(castilla_mobility_week1_tmp)
castilla_sf$to<-colSums(castilla_mobility_week1_tmp)

pdf("../figures/castilla_to.pdf", width = 5, height = 4)
mobility_plot(castilla_sf, castilla_sf$to)
dev.off()
pdf("../figures/castilla_from.pdf", width = 7, height = 6)
mobility_plot(castilla_sf, castilla_sf$from, do_bar=TRUE)
dev.off()
pdf("../figures/castilla_within.pdf", width = 5, height = 4)
mobility_plot(castilla_sf, castilla_sf$within, do_annotate = T)
dev.off()

########
#Madrid 

#Attatch mobility data to sf_data
madrid_sf$within <- diag(madrid_mobility_week1 %>% as.matrix)
madrid_mobility_week1_tmp <- madrid_mobility_week1- diag(diag(madrid_mobility_week1 %>% as.matrix))
madrid_sf$from <- rowSums(madrid_mobility_week1_tmp)
madrid_sf$to<-colSums(madrid_mobility_week1_tmp)

pdf("../figures/madrid_to.pdf", width = 5, height = 4)
mobility_plot(madrid_sf, madrid_sf$to)
dev.off()
pdf("../figures/madrid_from.pdf", width = 5, height = 4)
mobility_plot(madrid_sf, madrid_sf$from)
dev.off()
pdf("../figures/madrid_within.pdf", width = 5, height = 4)
mobility_plot(madrid_sf, madrid_sf$within, do_annotate = T)
dev.off()

############################################################################################
#### Figures showing mobility matrix and neighbor matrix
############################################################################################


#Castilla
neighb_mat = as.matrix(inla.graph2matrix(inla.read.graph("../data/neighbors_castilla.txt")))
diag(neighb_mat) = 0
sorting_order = order(rowSums(neighb_mat), decreasing = T)
n_neighb = rowSums(neighb_mat)[sorting_order]
neighb_mat = neighb_mat[sorting_order, sorting_order]
neighb_mat = neighb_mat != 0
diag(neighb_mat) = NA
neighb_mat = melt(neighb_mat)
neighb_mat$value2 = rep(NA, length(neighb_mat$value))
neighb_mat$value2[neighb_mat$Var1 == neighb_mat$Var2] = n_neighb

p1 = ggplot(data = neighb_mat) + 
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_manual(na.value = "transparent", values = c("FALSE"="transparent","TRUE" = "black"), labels = c("", "Are neighbors")) +
  scale_y_reverse() +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_raster(aes(x = Var2, y = Var1, fill = value2)) +
  scale_fill_gradientn(na.value = "transparent",  colours = c("red", "green") ) +
  scale_y_reverse() + 
  labs(fill = "Neighbors") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(NULL) + 
  ylab(NULL)

mob_mat = as.matrix(mobility_structure_castilla)
colnames(mob_mat) = rownames(mob_mat)
mob_mat = -as.matrix(mob_mat)
diag(mob_mat) = 0
movement = rowSums(mob_mat)[sorting_order]
diag(mob_mat) = NA
mob_mat = mob_mat[sorting_order, sorting_order]
mob_mat[mob_mat == 0] = NA
mob_mat = melt(mob_mat)
mob_mat$value2 = rep(NA, length(mob_mat$value))
mob_mat$value2[mob_mat$Var1 == mob_mat$Var2] = movement


p2 = ggplot(data = mob_mat) + 
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_gradientn(na.value = "transparent", colours = c("white", "black")) +
  scale_y_reverse() +
  labs(fill = "Movement") +
  new_scale_fill() +
  geom_raster(aes(x = Var2, y = Var1, fill = value2)) +
  scale_fill_gradientn(na.value = "transparent",colours = c("red", "green") ) +
  scale_y_reverse() + 
  labs(fill = "Total Movement") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(NULL) + 
  ylab(NULL)


#Madrid
neighb_mat = as.matrix(inla.graph2matrix(inla.read.graph("../data/neighbors_madrid.txt")))
diag(neighb_mat) = 0
sorting_order = order(rowSums(neighb_mat), decreasing = T)
n_neighb = rowSums(neighb_mat)[sorting_order]
neighb_mat = neighb_mat[sorting_order, sorting_order]
neighb_mat = neighb_mat != 0
diag(neighb_mat) = NA
neighb_mat = melt(neighb_mat)
neighb_mat$value2 = rep(NA, length(neighb_mat$value))
neighb_mat$value2[neighb_mat$Var1 == neighb_mat$Var2] = n_neighb

p3 = ggplot(data = neighb_mat) + 
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_manual(na.value = "transparent", values = c("FALSE"="transparent","TRUE" = "black"), labels = c("", "Are neighbors")) +
  scale_y_reverse() +
  labs(fill = NULL) +
  new_scale_fill() +
  geom_raster(aes(x = Var2, y = Var1, fill = value2)) +
  scale_fill_gradientn(na.value = "transparent",  colours = c("red", "green") ) +
  scale_y_reverse() + 
  labs(fill = "Neighbors") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(NULL) + 
  ylab(NULL)

mob_mat = as.matrix(mobility_structure_madrid)
colnames(mob_mat) = rownames(mob_mat)
mob_mat = -as.matrix(mob_mat)
diag(mob_mat) = 0
movement = rowSums(mob_mat)[sorting_order]
diag(mob_mat) = NA
mob_mat = mob_mat[sorting_order, sorting_order]
mob_mat[mob_mat == 0] = NA
mob_mat = melt(mob_mat)
mob_mat$value2 = rep(NA, length(mob_mat$value))
mob_mat$value2[mob_mat$Var1 == mob_mat$Var2] = movement

p4 = ggplot(data = mob_mat) + 
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_gradientn(na.value = "transparent", colours = c("white", "black")) +
  scale_y_reverse() +
  labs(fill = "Movement") +
  new_scale_fill() +
  geom_raster(aes(x = Var2, y = Var1, fill = value2)) +
  scale_fill_gradientn(na.value = "transparent",colours = c("red", "green") ) +
  scale_y_reverse() + 
  labs(fill = "Total Movement") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(NULL) + 
  ylab(NULL)

pdf("../figures/matrix_plot_castilla.pdf", height = 5, width = 12)
ggarrange(p1,p2, nrow = 1, ncol = 2)
dev.off()
pdf("../figures/matrix_plot_madrid.pdf", height = 5, width = 12)
ggarrange(p3,p4, nrow = 1, ncol = 2)
dev.off()

############################################################################################
#### Figures showing results in INLA
############################################################################################

########
# Function for plotting
result_plot <- function(sf_data, fill, which_scale, scale_to_max = F, scale_to_min = F, scale_round = T){
  scale_col = heat.colors(30, rev=TRUE) #Divide color gradient into 30 
  #scale = scale_col[c(1,9,13,15,18,22,26,30)] #Select color scale to be more red
  scale = heat.colors(8, rev = T)
  if (which_scale == 0){
    labels = c(0,2,5,10,20,30,50,100)
    breaks = c(0,2,5,10,20,30,50,100)
    limits = c(0,100)
  }
  else if (which_scale == 1){
    labels = c(-2.5, -1, -0.5, -0.25, 0.25, 0.5, 1, 2.5)
    breaks = c(-2.5, -1, -0.5, -0.25, 0.25, 0.5, 1, 2.5)
    limits = c(-2.5, 2.5)
  }
  else if (which_scale == 2){
    labels = c(-5, -2.5, 1, -0.25, 0.25, 1, 2.5, 5)
    breaks = c(-5, -2.5, 1, -0.25, 0.25, 1, 2.5, 5)
    limits = c(-5,5)
  }
  else if (which_scale == 3){
    labels = c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1.0)
    breaks = c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1.0)
    limits = c(0,1.0)
  }
  else if( scale_to_max || scale_to_min ){
    if (scale_to_max) {maximum = max(fill)} else {maximum = 0}
    if (scale_to_min) {minimum = min(fill)} else {minimum = 0}
    if(scale_round == T){
      maximum = ceil(maximum*10)/10
      minimum = floor(minimum*10)/10
    }
    breaks = seq(from = minimum, to = maximum, length.out = 8)
    labels = signif(breaks, digits = 2)
    limits = c(minimum, maximum)
  }
  
  #scale = heat.colors(8, rev= TRUE)
  print(ggplot(data = sf_data) + 
    geom_sf(aes(fill = fill), #Plots cases per thousand
            alpha = 1,
            color="black") + 
    theme(axis.title.x = element_blank(), #Remove axis and background grid
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0,0,0,0), "inches"),
          legend.text = element_text(size = 17)) +
    guides(fill=guide_legend(title=NULL, reverse = TRUE, label.position = "right")) + #Remove colorbar title
    binned_scale( #Scaling the color
      aesthetics = "fill",
      scale_name = "gradientn",
      palette = function(x) c(scale),
      labels = labels,
      breaks = breaks,
      limits= limits,
      guide = "colorscale"))
}

########
# Plotting the results in madrid

#Full model
current_model = res_full_madrid$inla
pdf("../figures/madrid_full_phi.pdf")
result_plot(madrid_sf, current_model$summary.random$phi$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/madrid_full_phi_sd.pdf")
result_plot(madrid_sf, current_model$summary.random$phi$sd, scale_to_max = T, scale_to_min = F, which_scale = -1)
dev.off()
pdf("../figures/madrid_full_theta.pdf")
result_plot(madrid_sf, current_model$summary.random$theta$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/madrid_full_theta_sd.pdf")
result_plot(madrid_sf, current_model$summary.random$theta$sd, scale_to_max = T, scale_to_min = F, which_scale = -1)
dev.off()
pdf("../figures/madrid_full_gamma.pdf")
result_plot(madrid_sf, current_model$summary.random$gamma$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/madrid_full_gamma_sd.pdf")
result_plot(madrid_sf, current_model$summary.random$gamma$sd, scale_to_max = T, scale_to_min = F, which_scale = -1) 
dev.off()

########
# Plotting the results in castilla

#Full model
current_model = res_full_castilla$inla
pdf("../figures/castilla_full_phi.pdf")
result_plot(castilla_sf, current_model$summary.random$phi$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/castilla_full_phi_sd.pdf")
result_plot(castilla_sf, current_model$summary.random$phi$sd, scale_to_max = T, scale_to_min = F, which_scale = -1)
dev.off()
pdf("../figures/castilla_full_theta.pdf")
result_plot(castilla_sf, current_model$summary.random$theta$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/castilla_full_theta_sd.pdf")
result_plot(castilla_sf, current_model$summary.random$theta$sd, scale_to_max = T, scale_to_min = F, which_scale = -1)
dev.off()
pdf("../figures/castilla_full_gamma.pdf")
result_plot(castilla_sf, current_model$summary.random$gamma$"0.5quant", scale_to_max = T, scale_to_min = T, which_scale = -1)
dev.off()
pdf("../figures/castilla_full_gamma_sd.pdf")
result_plot(castilla_sf, current_model$summary.random$gamma$sd, scale_to_max = T, scale_to_min = F, which_scale = -1) 
dev.off()

#####
# Plotting posterior density of the rhos in the model
rho_histogram = function(hist_data, legend_pos = "l", ylim = F, do_sigma = T){
  effects = colnames(hist_data)
  n_effects = length(effects)
  if ("sigma" %in% colnames(hist_data)){
    effects = colnames(hist_data)[-n_effects] # We dont care about sigma here
    n_effects = n_effects - 1
  } 
  if (any(startsWith(effects,"normal."))){
    label = paste("$\\rho_{\\", str_split_i(effects[1], "_",2), "}$", sep = "")
    effects = gsub("rho_phi", "", effects) # Map the effect to explanatory names
    effects = gsub("rho_gamma", "", effects)
    effects = gsub("rho_theta", "", effects)
    effects = gsub("normal.", "Joint-Dir ", effects)
    effects = gsub("shrink.", "Joint-PC ", effects)
  }
  else{
    effects = gsub("rho_phi", "Neighbor", effects) # Map the effect to explanatory names
    effects = gsub("rho_gamma", "Movement", effects)
    effects = gsub("rho_theta", "Unstructured", effects)
    label = "$\\rho$"
  }
  colnames(hist_data)[1:n_effects] = effects
  rho = c()
  group = c()
  for (i in 1:n_effects) {
    rho = c(rho, hist_data[effects[i]][,1])
    group = c(group, rep(effects[i], length(hist_data[effects[i]][,1])))
  }
  plot_data = data.frame(rho = rho, 
                         group = group)
  
  #Place legend left or right 
  if (legend_pos == "l"){
    legend_position = c(0.25,0.75)
  }
  else if (legend_pos == "r"){
    legend_position = c(0.75, 0.75)
  }
  else {
    legend_position = c(0.5, 0.75)
  }
  
  #Use print for saving the plot with pdf()
  p = ggplot(plot_data, mapping = aes(x = rho, fill = group)) + 
    geom_density(position = "identity", alpha = 0.4) +
    theme_bw() + 
    theme(strip.text.x = element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "white"),
          legend.background = element_rect(fill = "transparent"),
          legend.position=legend_position,
          legend.title = element_blank(),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          ) +
    xlab(latex2exp::TeX(label)) + 
    xlim(0,1)
  if(ylim){
    p = p + coord_cartesian(ylim = c(0, ylim))
  }
  print(p)
}

# For the full models we compare within the model
pdf("../figures/castilla_full_rhos.pdf")
rho_histogram(castilla_inference_results$full$hist_data, legend_pos = "m")
dev.off()
pdf("../figures/madrid_full_rhos.pdf")
rho_histogram(madrid_inference_results$full$hist_data, legend_pos = "m")
dev.off()

# For the movement and neighbor we compare cross model
castilla_plot_data = merge(castilla_inference_results$movement$hist_data$rho_gamma, castilla_inference_results$neighbor$hist_data$rho_phi, by = 0)[c(2,3)]
colnames(castilla_plot_data) = c("rho_gamma", "rho_phi")
pdf("../figures/castilla_simple_rhos.pdf")
rho_histogram(castilla_plot_data)
dev.off()

madrid_plot_data = merge(madrid_inference_results$movement$hist_data$rho_gamma, madrid_inference_results$neighbor$hist_data$rho_phi, by = 0)[c(2,3)]
colnames(madrid_plot_data) = c("rho_gamma", "rho_phi")
pdf("../figures/madrid_simple_rhos.pdf")
rho_histogram(madrid_plot_data, legend_pos = "l")
dev.off()

#Now for shrinkage
# For the full models we compare within the model
pdf("../figures/castilla_shrink_rhos.pdf")
rho_histogram(castilla_inference_results$shrink$hist_data, legend_pos = "m")
dev.off()
pdf("../figures/madrid_shrink_rhos.pdf")
rho_histogram(madrid_inference_results$shrink$hist_data, legend_pos = "m")
dev.off()

#############
#Plots for comparing the posterior densities
#Compare each component of shrinkage with the full
all_hist_data_castilla = data.frame(shrink = castilla_inference_results$shrink$hist_data,
                                  normal = castilla_inference_results$full$hist_data)
p1 = rho_histogram(all_hist_data_castilla[,c("normal.rho_gamma","shrink.rho_gamma")], legend_pos = "l")
p2 = rho_histogram(all_hist_data_castilla[,c("normal.rho_phi","shrink.rho_phi")], legend_pos = "r")
p3 = rho_histogram(all_hist_data_castilla[,c("normal.rho_theta","shrink.rho_theta")], legend_pos = "r")
pdf("../figures/castilla_rho_comparison.pdf", width = 15, height = 5)
ggarrange(p1,p2,p3, ncol = 3, nrow = 1)
dev.off()

all_hist_data_madrid = data.frame(shrink = madrid_inference_results$shrink$hist_data,
                                    normal = madrid_inference_results$full$hist_data)
p1 = rho_histogram(all_hist_data_madrid[,c("normal.rho_gamma","shrink.rho_gamma")], legend_pos = "l")
p2 = rho_histogram(all_hist_data_madrid[,c("normal.rho_phi","shrink.rho_phi")], legend_pos = "r")
p3 = rho_histogram(all_hist_data_madrid[,c("normal.rho_theta","shrink.rho_theta")], legend_pos = "r")
pdf("../figures/madrid_rho_comparison.pdf", width = 15, height = 5)
ggarrange(p1,p2,p3, ncol = 3, nrow = 1)
dev.off()

# Now compare mu, beta and sigma in the two models
#Madrid
len_normal = length(all_hist_data_madrid$normal.sigma)
len_shrink = length(all_hist_data_madrid$shrink.sigma)
sigma_data_madrid = data.frame(value = c(all_hist_data_madrid$normal.sigma, all_hist_data_madrid$shrink.sigma),
                               group = c(rep("Joint-Dir", len_normal), rep("Joint-PC", len_shrink)))

length_full = length(res_full_madrid$inla$marginals.fixed$`(Intercept)`[,1])
length_fixed = length(res_full_madrid_shrink$inla$marginals.fixed$`(Intercept)`[,1])
intercept_madrid = data.frame(x = c(res_full_madrid$inla$marginals.fixed$`(Intercept)`[,1], res_full_madrid_shrink$inla$marginals.fixed$`(Intercept)`[,1]),
                              y = c(res_full_madrid$inla$marginals.fixed$`(Intercept)`[,2], res_full_madrid_shrink$inla$marginals.fixed$`(Intercept)`[,2]),
                              group = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_fixed)))

length_full = length(res_full_madrid$inla$marginals.fixed$beta[,1])
length_fixed = length(res_full_madrid_shrink$inla$marginals.fixed$beta[,1])
beta_madrid = data.frame(x = c(res_full_madrid$inla$marginals.fixed$beta[,1], res_full_madrid_shrink$inla$marginals.fixed$beta[,1]),
                         y = c(res_full_madrid$inla$marginals.fixed$beta[,2], res_full_madrid_shrink$inla$marginals.fixed$beta[,2]),
                         group = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_fixed)))


#pdf("../figures/compare_intercept_madrid.pdf", width = 5, height = 5)
p1 = ggplot(data = intercept_madrid, aes(x = x, y= y, fill = group)) +
  geom_area(alpha = 0.4, position = "identity") + 
  geom_line() +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.25, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\mu$")) +
  ylab("density")
#dev.off()

#pdf("../figures/compare_beta_madrid.pdf", width = 5, height = 5)
p2 = ggplot(data = beta_madrid, aes(x = x, y= y, fill = group)) +
  geom_area(alpha = 0.4, position = "identity") + 
  geom_line() +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.25, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\beta$")) +
  ylab("density")
#dev.off()

#pdf("../figures/compare_sigma_madrid.pdf", width = 5, height = 5)
p3 = ggplot(data = sigma_data_madrid, aes(x = value, fill = group)) + geom_density(position = "identity", alpha = 0.4) +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.75, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\sigma$"))
pdf("../figures/compare_fixed_madrid.pdf", width = 15, height = 5)
ggarrange(p1,p2,p3, ncol = 3)
dev.off()



#Castilla
len_normal = length(all_hist_data_castilla$normal.sigma)
len_shrink = length(all_hist_data_castilla$shrink.sigma)
sigma_data_castilla = data.frame(value = c(all_hist_data_castilla$normal.sigma, all_hist_data_castilla$shrink.sigma),
                               group = c(rep("Joint-Dir", len_normal), rep("Joint-PC", len_shrink)))

length_full = length(res_full_castilla$inla$marginals.fixed$beta[,1])
length_fixed = length(res_full_castilla_shrink$inla$marginals.fixed$beta[,1])
beta_castilla = data.frame(x = c(res_full_castilla$inla$marginals.fixed$beta[,1], res_full_castilla_shrink$inla$marginals.fixed$beta[,1]),
                           y = c(res_full_castilla$inla$marginals.fixed$beta[,2], res_full_castilla_shrink$inla$marginals.fixed$beta[,2]),
                           group = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_fixed)))

length_full = length(res_full_castilla$inla$marginals.fixed$`(Intercept)`[,1])
length_fixed = length(res_full_castilla_shrink$inla$marginals.fixed$`(Intercept)`[,1])
intercept_castilla = data.frame(x = c(res_full_castilla$inla$marginals.fixed$`(Intercept)`[,1], res_full_castilla_shrink$inla$marginals.fixed$`(Intercept)`[,1]),
                                y = c(res_full_castilla$inla$marginals.fixed$`(Intercept)`[,2], res_full_castilla_shrink$inla$marginals.fixed$`(Intercept)`[,2]),
                                group = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_fixed)))

p1 = ggplot(data = intercept_castilla, aes(x = x, y= y, fill = group)) +
  geom_area(alpha = 0.4, position = "identity") + 
  geom_line() +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.25, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\mu$")) +
  ylab("density")


p2 = ggplot(data = beta_castilla, aes(x = x, y= y, fill = group)) +
  geom_area(alpha = 0.4, position = "identity") + 
  geom_line() +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.25, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\beta$")) +
  ylab("density")


p3 = ggplot(data = sigma_data_castilla, aes(x = value, fill = group)) + geom_density(position = "identity", alpha = 0.4) +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.position=c(0.75, 0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\sigma$"))

pdf("../figures/compare_fixed_castilla.pdf", width = 15, height = 5)
ggarrange(p1,p2,p3, ncol = 3)
dev.off()

#Plots the posterios marginals of the standard deviations of each component in a joint model
std_plot = function(result_full, result_shrink){
  
  #First for gamma
  full = inla.tmarginal(function(x){1/sqrt(x)}, result_full$marginals.hyperpar[[1]])
  shrink = inla.tmarginal(function(x){1/sqrt(x)}, result_shrink$marginals.hyperpar[[1]])
  length_full = length(full[,1])
  length_shrink = length(shrink[,1])
  temp_dat = data.frame(x = c(full[,1], shrink[,1]),
                         y = c(full[,2], shrink[,2]),
                         model = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_shrink)))
  p1 = ggplot(data = temp_dat, aes(x=x, y = y, fill = model)) + 
    geom_line() +
    geom_area(alpha = 0.4, position = "identity") + 
    theme_bw() + xlab(latex2exp::TeX("$\\sigma_\\gamma$")) + 
    ylab("density") + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          strip.text.x = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "black"),
          legend.position=c(0.75, 0.75),
          legend.title = element_blank())
  
  #First for gamma
  full = inla.tmarginal(function(x){1/sqrt(x)}, result_full$marginals.hyperpar[[2]])
  shrink = inla.tmarginal(function(x){1/sqrt(x)}, result_shrink$marginals.hyperpar[[2]])
  length_full = length(full[,1])
  length_shrink = length(shrink[,1])
  temp_dat = data.frame(x = c(full[,1], shrink[,1]),
                        y = c(full[,2], shrink[,2]),
                        model = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_shrink)))
  p2 = ggplot(data = temp_dat, aes(x=x, y = y, fill = model)) + 
    geom_line() +
    geom_area(alpha = 0.4, position = "identity") + 
    ylab("density") + 
    theme_bw() + xlab(latex2exp::TeX("$\\sigma_\\phi$")) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          strip.text.x = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "black"),
          legend.position=c(0.75, 0.75),
          legend.title = element_blank())
  
  #Then for theta
  full = inla.tmarginal(function(x){1/sqrt(x)}, result_full$marginals.hyperpar[[3]])
  shrink = inla.tmarginal(function(x){1/sqrt(x)}, result_shrink$marginals.hyperpar[[3]])
  length_full = length(full[,1])
  length_shrink = length(shrink[,1])
  temp_dat = data.frame(x = c(full[,1], shrink[,1]),
                         y = c(full[,2], shrink[,2]),
                         model = c(rep("Joint-Dir", length_full), rep("Joint-PC", length_shrink)))
  p3 = ggplot(data = temp_dat, aes(x=x, y = y, fill = model)) + 
    geom_line() +
    geom_area(alpha = 0.4, position = "identity") + 
    ylab("density") + 
    theme_bw() + xlab(latex2exp::TeX("$\\sigma_\\theta$")) + 
    theme(axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 17),
          strip.text.x = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(colour = "black"),
          legend.position=c(0.75, 0.75),
          legend.title = element_blank())
  
  #Arrange and plot
  p = ggarrange(p1,p2,p3, ncol = 3)
  print(p)
}
pdf("../figures/compare_sigma_component_castilla.pdf", width = 15, height = 5)
std_plot(res_full_castilla$inla, res_full_castilla_shrink$inla)
dev.off()

pdf("../figures/compare_sigma_component_madrid.pdf", width = 15, height = 5)
std_plot(res_full_madrid$inla, res_full_madrid_shrink$inla)
dev.off()

##########
# PLotting of priors

# Plot of the priors used full model
prior_data_V = prior_data[1:201,]
prior_data_phi = prior_data[202:302,]
prior_data_gamma = prior_data[303:403,]
colnames(prior_data_gamma) = c("rho", "density", "param")
colnames(prior_data_phi) = c("rho", "density", "param")
colnames(prior_data_V) = c("V", "density", "param")

#Gamma and phi are identical, plot only one
pdf("../figures/prior_gamma_phi.pdf")
print(ggplot(prior_data_gamma, aes(x = rho, y = density)) +
  geom_path() +
  theme_bw() +
  theme(panel.background = element_blank(),
  axis.title.x = element_text(size = 25),
  axis.title.y = element_text(size = 17),
  axis.text = element_text(size = 17),
  legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\rho$"))) 
dev.off()

# For V, the variance
pdf("../figures/prior_variance.pdf")
print(ggplot(prior_data_V, aes(x = V, y = density)) +
  geom_path() +
  theme_bw() +
  theme(panel.background = element_blank(), 
  axis.title.x = element_text(size = 25),
  axis.title.y = element_text(size = 17),
  axis.text = element_text(size = 17),
  legend.text = element_text(size = 17)) +
  xlab(latex2exp::TeX("$\\sigma$")))
dev.off()




#plots for priors used shrinkage model
prior_data_bot = prior_data_shrink[prior_data_shrink$param == 'omega[frac("gamma", "gamma,phi")]', ]
prior_data_top = prior_data_shrink[prior_data_shrink$param == 'omega[frac("theta", "gamma,phi,theta")]', ]


#Plot The two priors in the shrinkage model
pdf("../figures/prior_top_level.pdf", height = 7, width = 5)
print(ggplot(prior_data_top, aes(x = x, y = y)) +
        geom_path() +
        theme_bw() +
        theme(panel.background = element_blank(), 
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 17),
              axis.text = element_text(size = 17),
              legend.text = element_text(size = 17)) +
        xlab(latex2exp::TeX("$\\frac{\\rho_\\theta}{\\rho_\\gamma+\\rho_\\phi+\\rho_\\theta}$")) +
        ylab("density")) 
dev.off()

#Plot The two priors in the shrinkage model
pdf("../figures/prior_bot_level.pdf", width = 5, height = 7)
print(ggplot(prior_data_bot, aes(x = x, y = y)) +
        geom_path() +
        theme_bw() +
        theme(panel.background = element_blank(), 
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 17),
              axis.text = element_text(size = 17),
              legend.text = element_text(size = 17)) +
        xlab(latex2exp::TeX("$\\frac{\\rho_\\gamma}{\\rho_\\gamma+\\rho_\\phi}$"))+
        ylab("density"))
dev.off()

#Take screenshots of the trees instead as that is higher resolution than the saving options. 
visExport(plot_tree_structure(full_prior, nodenames = list("gamma_phi_theta"="Case counts", "gamma"="Movement", "phi"="Neighbor", "theta"="Independent")), type = "png", name = "mmp_tree_structure_full")
visExport(plot_tree_structure(shrink_prior, nodenames = c("theta_gamma_phi"="Case counts", "theta"="Unstructured", "gamma_phi"="Structured", "gamma"="Movement", "phi"="Neighbor")), type = "png", name = "mmp_tree_structure_shrink")

#########
#Violin plot for the regions

#Castilla

# Compare the spatial effects
n_reg_c = length(castilla_sf$fid)                                  
speff_castilla = data.frame(parameter = c(rep(rep("gamma", n_reg_c), 2), rep(rep("phi", n_reg_c), 2), rep(rep("theta", n_reg_c), 2)),
                            model = rep(c(rep("Joint-Dir", n_reg_c), rep("Joint-PC", n_reg_c)), 3),
                            value = rep(0, 6*n_reg_c))

speff_castilla$value[speff_castilla$parameter == "gamma" & speff_castilla$model == "Joint-Dir"] = res_full_castilla$inla$summary.random$gamma$"0.5quant"
speff_castilla$value[speff_castilla$parameter == "gamma" & speff_castilla$model == "Joint-PC"] = res_full_castilla_shrink$inla$summary.random$gamma$"0.5quant"
speff_castilla$value[speff_castilla$parameter == "phi" & speff_castilla$model == "Joint-Dir"] = res_full_castilla$inla$summary.random$phi$"0.5quant"
speff_castilla$value[speff_castilla$parameter == "phi" & speff_castilla$model == "Joint-PC"] = res_full_castilla_shrink$inla$summary.random$phi$"0.5quant"
speff_castilla$value[speff_castilla$parameter == "theta" & speff_castilla$model == "Joint-Dir"] = res_full_castilla$inla$summary.random$theta$"0.5quant"
speff_castilla$value[speff_castilla$parameter == "theta" & speff_castilla$model == "Joint-PC"] = res_full_castilla_shrink$inla$summary.random$theta$"0.5quant"

p1 = ggplot(data = speff_castilla[speff_castilla$parameter == "gamma",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\gamma$")) + 
  xlab(NULL) + 
  guides(fill="none")
p2 = ggplot(data = speff_castilla[speff_castilla$parameter == "phi",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\phi$")) + 
  xlab(NULL) + 
  guides(fill="none")
p3 = ggplot(data = speff_castilla[speff_castilla$parameter == "theta",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\theta$")) + 
  xlab(NULL) + 
  guides(fill="none")
pdf("../figures/violin_castilla.pdf", height = 5, width = 15)
ggarrange(p1,p2,p3,ncol=3)
dev.off()


# Madrid
n_reg_m = length(madrid_sf$natlevel)
speff_madrid = data.frame(parameter = c(rep(rep("gamma", n_reg_m), 2), rep(rep("phi", n_reg_m), 2), rep(rep("theta", n_reg_m), 2)),
                            model = rep(c(rep("Joint-Dir", n_reg_m), rep("Joint-PC", n_reg_m)), 3),
                            value = rep(0, 6*n_reg_m))


speff_madrid$value[speff_madrid$parameter == "gamma" & speff_madrid$model == "Joint-Dir"] = res_full_madrid$inla$summary.random$gamma$"0.5quant"
speff_madrid$value[speff_madrid$parameter == "gamma" & speff_madrid$model == "Joint-PC"] = res_full_madrid_shrink$inla$summary.random$gamma$"0.5quant"
speff_madrid$value[speff_madrid$parameter == "phi" & speff_madrid$model == "Joint-Dir"] = res_full_madrid$inla$summary.random$phi$"0.5quant"
speff_madrid$value[speff_madrid$parameter == "phi" & speff_madrid$model == "Joint-PC"] = res_full_madrid_shrink$inla$summary.random$phi$"0.5quant"
speff_madrid$value[speff_madrid$parameter == "theta" & speff_madrid$model == "Joint-Dir"] = res_full_madrid$inla$summary.random$theta$"0.5quant"
speff_madrid$value[speff_madrid$parameter == "theta" & speff_madrid$model == "Joint-PC"] = res_full_madrid_shrink$inla$summary.random$theta$"0.5quant"

p1 = ggplot(data = speff_madrid[speff_madrid$parameter == "gamma",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\gamma$")) + 
  xlab(NULL) + 
  guides(fill="none")
p2 = ggplot(data = speff_madrid[speff_madrid$parameter == "phi",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\phi$")) + 
  xlab(NULL) + 
  guides(fill="none")
p3 = ggplot(data = speff_madrid[speff_madrid$parameter == "theta",], aes(x = model, y = value, fill = model)) + 
  geom_violin(trim = F) + 
  theme_bw() +
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25, angle = 0, vjust = 0.5),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.position=c(0.5, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ylab(latex2exp::TeX("$\\theta$")) + 
  xlab(NULL) + 
  guides(fill="none")
pdf("../figures/violin_madrid.pdf", height = 5, width = 15)
ggarrange(p1,p2,p3,ncol=3)
dev.off()







# 
# 
# txt_size = 30 
# p1 = ggplot(data_spatial_effects_castilla) + 
#   geom_point(aes(x = gamma, y = gamma_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) + ylab(latex2exp::TeX("$\\gamma_{PC}$")) + xlab(latex2exp::TeX("$\\gamma_{Dirichlet}$"))
# p2 = ggplot(data_spatial_effects_castilla) + 
#   geom_point(aes(x = phi, y = phi_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) + ylab(latex2exp::TeX("$\\phi_{PC}$")) + xlab(latex2exp::TeX("$\\phi_{Dirichlet}$"))
# p3 = ggplot(data_spatial_effects_castilla) + geom_point(aes(x = theta, y = theta_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) +  ylab(latex2exp::TeX("$\\theta_{PC}$")) + xlab(latex2exp::TeX("$\\theta_{Dirichlet}$")) 
# pdf("../figures/compare_effects_castilla.pdf", height = 5, width = 15)
# ggarrange(p1,p2,p3, ncol=3,nrow=1)
# dev.off()
# 
# 
# p1 = ggplot(data_spatial_effects_madrid) + geom_point(aes(x = gamma, y = gamma_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) + ylab(latex2exp::TeX("$\\gamma_{PC}$")) + xlab(latex2exp::TeX("$\\gamma_{Dirichlet}$"))
# p2 = ggplot(data_spatial_effects_madrid) + geom_point(aes(x = phi, y = phi_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) + ylab(latex2exp::TeX("$\\phi_{PC}$")) + xlab(latex2exp::TeX("$\\phi_{Dirichlet}$"))
# p3 = ggplot(data_spatial_effects_madrid) + geom_point(aes(x = theta, y = theta_shrink), color = "black", shape = 20) + 
#   geom_abline(slope = 1, intercept = 0, color = "red", lty="dashed") + 
#   theme_bw() + theme(text = element_text(size = txt_size)) + ylab(latex2exp::TeX("$\\theta_{PC}$")) + xlab(latex2exp::TeX("$\\theta_{Dirichlet}$"))
# pdf("../figures/compare_effects_madrid.pdf", height = 5, width = 15)
# ggarrange(p1,p2,p3, ncol=3,nrow=1)
# dev.off()


############################################################################################
#### Tables and constants
############################################################################################
########
#### Full model results into a tex-table
n_digits = 2
full_model_table_madrid = madrid_inference_results$full$inference_results
full_model_table_madrid = round(full_model_table_madrid, digits = n_digits) #Round to two digits
full_model_table_madrid[] = sapply(full_model_table_madrid, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included
full_madrid = paste(full_model_table_madrid$median," (",full_model_table_madrid$"0.025quant",", ",full_model_table_madrid$"0.975quant",")", sep="")

full_model_table_castilla = castilla_inference_results$full$inference_results
full_model_table_castilla = round(full_model_table_castilla, digits = n_digits) #Round to two digits
full_model_table_castilla[] = sapply(full_model_table_castilla, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included
full_castilla = paste(full_model_table_castilla$median," (",full_model_table_castilla$"0.025quant",", ",full_model_table_castilla$"0.975quant",")", sep="")


#Start formating the table for the full model with dirichlet prior
table_full = data.frame(matrix(0, nrow = 8, ncol = 4)) #Create empty table
table_full[table_full==0] = ""
table_full[1,] = c("", "Parameter", "Madrid", "Castilla-Leon") #Change text
table_full[2,] = c("", "", "Est (95\\% CrI)", "Est (95\\% CrI)") 
table_full[3,1:2] = c("", "Movement ($\\gamma$)")
table_full[4,1:2] = c("$\\pmb \\rho$", "Neighbor ($\\phi$)")
table_full[5,1:2] = c("", "Independent ($\\theta$)")
table_full[6,1:2] = c("", "$\\mu$")
table_full[7,1:2] = c("", "$\\beta$")
table_full[8,1:2] = c("", "$\\sigma$")

#Rhos for madrid
table_full[3,3] =  paste(full_model_table_madrid["rho_gamma","median"],
                         " (",
                         paste(full_model_table_madrid["rho_gamma","0.025quant"],", ", full_model_table_madrid["rho_gamma","0.975quant"], sep = ""),
                         ")", sep="")
table_full[4,3] =  paste(full_model_table_madrid["rho_phi","median"],
                         " (",
                         paste(full_model_table_madrid["rho_phi","0.025quant"],", ", full_model_table_madrid["rho_phi","0.975quant"], sep = ""),
                         ")", sep="")
table_full[5,3] =  paste(full_model_table_madrid["rho_theta","median"],
                         " (",
                         paste(full_model_table_madrid["rho_theta","0.025quant"],", ", full_model_table_madrid["rho_theta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[6,3] =  paste(full_model_table_madrid["(Intercept)","median"],
                         " (",
                         paste(full_model_table_madrid["(Intercept)","0.025quant"],", ", full_model_table_madrid["(Intercept)","0.975quant"], sep = ""),
                         ")", sep="")
table_full[7,3] =  paste(full_model_table_madrid["beta","median"],
                         " (",
                         paste(full_model_table_madrid["beta","0.025quant"],", ", full_model_table_madrid["beta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[8,3] =  paste(full_model_table_madrid["sigma","median"],
                         " (",
                         paste(full_model_table_madrid["sigma","0.025quant"],", ", full_model_table_madrid["sigma","0.975quant"], sep = ""),
                         ")", sep="")

#Castila
table_full[3,4] =  paste(full_model_table_castilla["rho_gamma","median"],
                         " (",
                         paste(full_model_table_castilla["rho_gamma","0.025quant"],", ", full_model_table_castilla["rho_gamma","0.975quant"], sep = ""),
                         ")", sep="")
table_full[4,4] =  paste(full_model_table_castilla["rho_phi","median"],
                         " (",
                         paste(full_model_table_castilla["rho_phi","0.025quant"],", ", full_model_table_castilla["rho_phi","0.975quant"], sep = ""),
                         ")", sep="")
table_full[5,4] =  paste(full_model_table_castilla["rho_theta","median"],
                         " (",
                         paste(full_model_table_castilla["rho_theta","0.025quant"],", ", full_model_table_castilla["rho_theta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[6,4] =  paste(full_model_table_castilla["(Intercept)","median"],
                         " (",
                         paste(full_model_table_castilla["(Intercept)","0.025quant"],", ", full_model_table_castilla["(Intercept)","0.975quant"], sep = ""),
                         ")", sep="")
table_full[7,4] =  paste(full_model_table_castilla["beta","median"],
                         " (",
                         paste(full_model_table_castilla["beta","0.025quant"],", ", full_model_table_castilla["beta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[8,4] =  paste(full_model_table_castilla["sigma","median"],
                         " (",
                         paste(full_model_table_castilla["sigma","0.025quant"],", ", full_model_table_castilla["sigma","0.975quant"], sep = ""),
                         ")", sep="")

caption = "Posterior medians along with 95\\% equi-tailed for $\\pmb \\rho$ in the reparameterized BYM model introduced in Section \\ref{Section:Reparameterized movement augmented BYM} using both the movement and adjacency data. The effect within region mobility spatial covariate is denoted by $\\beta$, the intercept by $\\mu$ and standard deviation by $\\sigma$."
label = "table:full-model-results"
table_full = xtable(table_full, align = "XXXXX", caption = caption, label = label)

#save table
print(table_full,
      sanitize.text.function = function(x){x},
      include.rownames = F,
      include.colnames = F,
      tabular.environment = "tabularx",
      width="\\textwidth",
      hline.after = c(-1,2,5,8),
      size ="small", 
      booktabs = T,
      file = "../figures/full_table.tex")

#Shrinked model
n_digits = 2
full_model_table_madrid_shrink = madrid_inference_results$shrink$inference_results
full_model_table_madrid_shrink = round(full_model_table_madrid_shrink, digits = n_digits) #Round to two digits
full_model_table_madrid_shrink[] = sapply(full_model_table_madrid_shrink, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included
full_madrid_shrink = paste(full_model_table_madrid_shrink$median," (",full_model_table_madrid_shrink$"0.025quant",", ",full_model_table_madrid_shrink$"0.975quant",")", sep="")

full_model_table_castilla_shrink = castilla_inference_results$shrink$inference_results
full_model_table_castilla_shrink = round(full_model_table_castilla_shrink, digits = n_digits) #Round to two digits
full_model_table_castilla_shrink[] = sapply(full_model_table_castilla_shrink, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included
full_castilla_shrink = paste(full_model_table_castilla_shrink$median," (",full_model_table_castilla_shrink$"0.025quant",", ",full_model_table_castilla_shrink$"0.975quant",")", sep="")


#Start formating the table for the model with pc prior
table_full = data.frame(matrix(0, nrow = 8, ncol = 4)) #Create empty table
table_full[table_full==0] = ""
table_full[1,] = c("", "Parameter", "Madrid", "Castilla-Leon") #Change text
table_full[2,] = c("", "", "Est (95\\% CrI)", "Est (95\\% CrI)") 
table_full[3,1:2] = c("", "Movement ($\\gamma$)")
table_full[4,1:2] = c("$\\pmb \\rho$", "Neighbor ($\\phi$)")
table_full[5,1:2] = c("", "Independent ($\\theta$)")
table_full[6,1:2] = c("", "$\\mu$")
table_full[7,1:2] = c("", "$\\beta$")
table_full[8,1:2] = c("", "$\\sigma$")

#Rhos for madrid
table_full[3,3] =  paste(full_model_table_madrid_shrink["rho_gamma","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["rho_gamma","0.025quant"],", ", full_model_table_madrid_shrink["rho_gamma","0.975quant"], sep = ""),
                         ")", sep="")
table_full[4,3] =  paste(full_model_table_madrid_shrink["rho_phi","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["rho_phi","0.025quant"],", ", full_model_table_madrid_shrink["rho_phi","0.975quant"], sep = ""),
                         ")", sep="")
table_full[5,3] =  paste(full_model_table_madrid_shrink["rho_theta","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["rho_theta","0.025quant"],", ", full_model_table_madrid_shrink["rho_theta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[6,3] =  paste(full_model_table_madrid_shrink["(Intercept)","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["(Intercept)","0.025quant"],", ", full_model_table_madrid_shrink["(Intercept)","0.975quant"], sep = ""),
                         ")", sep="")
table_full[7,3] =  paste(full_model_table_madrid_shrink["beta","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["beta","0.025quant"],", ", full_model_table_madrid_shrink["beta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[8,3] =  paste(full_model_table_madrid_shrink["sigma","median"],
                         " (",
                         paste(full_model_table_madrid_shrink["sigma","0.025quant"],", ", full_model_table_madrid_shrink["sigma","0.975quant"], sep = ""),
                         ")", sep="")

#Castila
table_full[3,4] =  paste(full_model_table_castilla_shrink["rho_gamma","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["rho_gamma","0.025quant"],", ", full_model_table_castilla_shrink["rho_gamma","0.975quant"], sep = ""),
                         ")", sep="")
table_full[4,4] =  paste(full_model_table_castilla_shrink["rho_phi","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["rho_phi","0.025quant"],", ", full_model_table_castilla_shrink["rho_phi","0.975quant"], sep = ""),
                         ")", sep="")
table_full[5,4] =  paste(full_model_table_castilla_shrink["rho_theta","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["rho_theta","0.025quant"],", ", full_model_table_castilla_shrink["rho_theta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[6,4] =  paste(full_model_table_castilla_shrink["(Intercept)","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["(Intercept)","0.025quant"],", ", full_model_table_castilla_shrink["(Intercept)","0.975quant"], sep = ""),
                         ")", sep="")
table_full[7,4] =  paste(full_model_table_castilla_shrink["beta","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["beta","0.025quant"],", ", full_model_table_castilla_shrink["beta","0.975quant"], sep = ""),
                         ")", sep="")
table_full[8,4] =  paste(full_model_table_castilla_shrink["sigma","median"],
                         " (",
                         paste(full_model_table_castilla_shrink["sigma","0.025quant"],", ", full_model_table_castilla_shrink["sigma","0.975quant"], sep = ""),
                         ")", sep="")

caption = "Posterior medians along with 95\\% equi-tailed for $\\pmb \\rho$ in the reparameterized BYM model using both the movement and adjacency data with a PC prior structure on the distribution of variance to the components rather than a Dirichlet structure. The effect within region mobility spatial covariate is denoted by $\\beta$, the intercept by $\\mu$ and the standard deviation by $\\sigma$."
label = "table:full-model-results-shrink"
table_full = xtable(table_full, align = "XXXXX", caption = caption, label = label)

#save table
print(table_full,
      sanitize.text.function = function(x){x},
      include.rownames = F,
      include.colnames = F,
      tabular.environment = "tabularx",
      width="\\textwidth",
      hline.after = c(-1,2,5,8),
      size ="small", 
      booktabs = T,
      file = "../figures/full_table_shrink.tex")



#######
# neighbor and movement results into table

n_digits = 2
movement_model_table_madrid = madrid_inference_results$movement$inference_results
movement_model_table_madrid = round(movement_model_table_madrid, digits = n_digits) #Round to two digits
movement_model_table_madrid[] = sapply(movement_model_table_madrid, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included

movement_model_table_castilla = castilla_inference_results$movement$inference_results
movement_model_table_castilla = round(movement_model_table_castilla, digits = n_digits) #Round to two digits
movement_model_table_castilla[] = sapply(movement_model_table_castilla, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included

neighbor_model_table_madrid = madrid_inference_results$neighbor$inference_results
neighbor_model_table_madrid = round(neighbor_model_table_madrid, digits = n_digits) #Round to two digits
neighbor_model_table_madrid[] = sapply(neighbor_model_table_madrid, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included

neighbor_model_table_castilla = castilla_inference_results$neighbor$inference_results
neighbor_model_table_castilla = round(neighbor_model_table_castilla, digits = n_digits) #Round to two digits
neighbor_model_table_castilla[] = sapply(neighbor_model_table_castilla, sprintf, fmt = "%01.2f") #Make sure trailing 0 is included


table_simple = data.frame(matrix(0, nrow = 10, ncol = 4)) #Create empty table
table_simple[table_simple==0] = ""
table_simple[1,] = c("Parameter", "Model", "Madrid", "Castilla-Leon") #Change text
table_simple[2,] = c("", "", "Est (95\\% CrI)", "Est (95\\% CrI)") 
table_simple[3,1:2] = c("$\\rho$", "\\hspace{-0.7cm}BYM2 movement")
table_simple[4,1:2] = c("", "\\hspace{-0.7cm}BYM2 neighbor")
table_simple[5,1:2] = c("$\\mu$", "\\hspace{-0.7cm}BYM2 movement")
table_simple[6,1:2] = c("", "\\hspace{-0.7cm}BYM2 neighbor")
table_simple[7,1:2] = c("$\\beta$", "\\hspace{-0.7cm}BYM2 movement")
table_simple[8,1:2] = c("", "\\hspace{-0.7cm}BYM2 neighbor")
table_simple[9,1:2] = c("$\\sigma$", "\\hspace{-0.7cm}BYM2 movement")
table_simple[10,1:2] = c("", "\\hspace{-0.7cm}BYM2 neighbor")


#Rhos for neigbor madrid
table_simple[4,3] =  paste(neighbor_model_table_madrid["rho_phi","median"],
                           " (",
                           paste(neighbor_model_table_madrid["rho_phi","0.025quant"],", ", neighbor_model_table_madrid["rho_phi","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[6,3] =  paste(neighbor_model_table_madrid["(Intercept)","median"],
                           " (",
                           paste(neighbor_model_table_madrid["(Intercept)","0.025quant"],", ", neighbor_model_table_madrid["(Intercept)","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[8,3] =  paste(neighbor_model_table_madrid["beta","median"],
                           " (",
                           paste(neighbor_model_table_madrid["beta","0.025quant"],", ", neighbor_model_table_madrid["beta","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[10,3] =  paste(neighbor_model_table_madrid["sigma","median"],
                            " (",
                            paste(neighbor_model_table_madrid["sigma","0.025quant"],", ", neighbor_model_table_madrid["sigma","0.975quant"], sep = ""),
                            ")", sep="")

#rhos for movement madrid
table_simple[3,3] =  paste(movement_model_table_madrid["rho_gamma","median"],
                           " (",
                           paste(movement_model_table_madrid["rho_gamma","0.025quant"],", ", movement_model_table_madrid["rho_gamma","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[5,3] =  paste(movement_model_table_madrid["(Intercept)","median"],
                           " (",
                           paste(movement_model_table_madrid["(Intercept)","0.025quant"],", ", movement_model_table_madrid["(Intercept)","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[7,3] =  paste(movement_model_table_madrid["beta","median"],
                           " (",
                           paste(movement_model_table_madrid["beta","0.025quant"],", ", movement_model_table_madrid["beta","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[9,3] =  paste(movement_model_table_madrid["sigma","median"],
                           " (",
                           paste(movement_model_table_madrid["sigma","0.025quant"],", ", movement_model_table_madrid["sigma","0.975quant"], sep = ""),
                           ")", sep="")

#Rhos for neigbor castilla
table_simple[4,4] =  paste(neighbor_model_table_castilla["rho_phi","median"],
                           " (",
                           paste(neighbor_model_table_castilla["rho_phi","0.025quant"],", ", neighbor_model_table_castilla["rho_phi","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[6,4] =  paste(neighbor_model_table_castilla["(Intercept)","median"],
                           " (",
                           paste(neighbor_model_table_castilla["(Intercept)","0.025quant"],", ", neighbor_model_table_castilla["(Intercept)","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[8,4] =  paste(neighbor_model_table_castilla["beta","median"],
                           " (",
                           paste(neighbor_model_table_castilla["beta","0.025quant"],", ", neighbor_model_table_castilla["beta","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[10,4] =  paste(neighbor_model_table_castilla["sigma","median"],
                            " (",
                            paste(neighbor_model_table_castilla["sigma","0.025quant"],", ", neighbor_model_table_castilla["sigma","0.975quant"], sep = ""),
                            ")", sep="")

#rhos for movement castilla
table_simple[3,4] =  paste(movement_model_table_castilla["rho_gamma","median"],
                           " (",
                           paste(movement_model_table_castilla["rho_gamma","0.025quant"],", ", movement_model_table_castilla["rho_gamma","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[5,4] =  paste(movement_model_table_castilla["(Intercept)","median"],
                           " (",
                           paste(movement_model_table_castilla["(Intercept)","0.025quant"],", ", movement_model_table_castilla["(Intercept)","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[7,4] =  paste(movement_model_table_castilla["beta","median"],
                           " (",
                           paste(movement_model_table_castilla["beta","0.025quant"],", ", movement_model_table_castilla["beta","0.975quant"], sep = ""),
                           ")", sep="")
table_simple[9,4] =  paste(movement_model_table_castilla["sigma","median"],
                           " (",
                           paste(movement_model_table_castilla["sigma","0.025quant"],", ", movement_model_table_castilla["sigma","0.975quant"], sep = ""),
                           ")", sep="")


caption = "Posterior medians along with 95\\% equi-tailed for $\\rho$ in BYM2 models using the movement and adjacency data in separate models. Both models use the within-region spatial covariate with corresponding coefficient $\\beta$. The intercept is denoted by $\\mu$, and the standard deviation by $\\sigma$."
label = "table:simple-models-results"
table_simple = xtable(table_simple, align = "XXXXX", caption = caption, label = label)

#save table
print(table_simple,
      sanitize.text.function = function(x){x},
      include.rownames = F,
      include.colnames = F,
      tabular.environment = "tabularx",
      width="\\textwidth",
      hline.after = c(-1,2,4,6,8,10),
      size ="small", 
      booktabs = T,
      file = "../figures/simple_table.tex")



########
# Information criterion table
#caption = 'The computed information criteria and logarithmic scores for the fitted models discussed in the report. The most favorable model for each criterion/score in each region is highlighted in bold font. '
caption = 'The computed information criteria and logarithmic scores for the fitted models discussed in the report.'
label = "table:model-scores"

mydf = data.frame(criterion = rep(c(rep("1",4),rep("2",4),rep("3",4)),2),
                  model = rep(c("1","2","3","4"), 6),
                  value = 1:24,
                  region = c(rep("1",12),rep("2",12)))

mydf$value[mydf$region=="1" & mydf$criterion == "1"] = madrid_inference_results$scores$dic
mydf$value[mydf$region=="1" & mydf$criterion == "2"] = madrid_inference_results$scores$waic
mydf$value[mydf$region=="1" & mydf$criterion == "3"] = madrid_inference_results$scores$cpo
mydf$value[mydf$region=="2" & mydf$criterion == "1"] = castilla_inference_results$scores$dic
mydf$value[mydf$region=="2" & mydf$criterion == "2"] = castilla_inference_results$scores$waic
mydf$value[mydf$region=="2" & mydf$criterion == "3"] = castilla_inference_results$scores$cpo

do_boldfont = F
if(do_boldfont){
  #Get the smallest value for each criterion in each region before rounding.
  min_indices = matrix(rep(0,6), nrow = 2, ncol = 3)
  for (region in 1:2) {
    for (criterion in 1:3) {
      min_score_ind = which.min(mydf$value[mydf$region == region & mydf$criterion == criterion])
      min_indices[region, criterion] = min_score_ind
    }
  }
  
  #Rounding
  mydf$value[mydf$criterion == "3"] = round(mydf$value[mydf$criterion == "3"], 1)
  mydf$value[mydf$criterion != "3"] = round(mydf$value[mydf$criterion != "3"])
  
  #Highlight smallest value
  mydf$value = as.character(mydf$value)
  for (region in 1:2) {
    for (criterion in 1:3) {
      val = mydf$value[mydf$region == region & mydf$criterion == criterion][min_indices[region, criterion]]
      mydf$value[mydf$region == region & mydf$criterion == criterion][min_indices[region, criterion]] = paste("\\textbf{", val , "}", sep="")
    }
  }
} else {
  #Rounding
  mydf$value[mydf$criterion == "3"] = round(mydf$value[mydf$criterion == "3"], 1)
  mydf$value[mydf$criterion != "3"] = round(mydf$value[mydf$criterion != "3"])
  mydf$value = as.character(mydf$value)
}

#Transform to late table
latex_tabular <- latexTable(tabular((Heading()*RowFactor(criterion, levelnames = c("DIC","WAIC","CPO"), space = 0, spacing = 0)*
                  Heading(name = "Model", character.only = T)*RowFactor(model, levelnames = c("BYM2 neighbor", "BYM2 movement", "Joint-Dirichlet", "Joint-PC"), name = "Model", space = 0, spacing = 0)) ~
                  (Heading()*Factor(region, levelnames = c("Madrid", "Castilla-Leon"))*
                  Heading()*value*Heading()*identity),
                data = mydf), table.envir = "table"
                )

# #Sub some latex code for better format 
latex_tabular = gsub("WAIC", "\\hline WAIC", latex_tabular, fixed = T)
latex_tabular = gsub("CPO", "\\hline CPO", latex_tabular, fixed = T)
latex_tabular = gsub("\\begin{tabular}{llcc}", "\\begin{tabularx}{\\textwidth}{cXXX}", latex_tabular, fixed=T)
latex_tabular = gsub("\\end{tabular}", "\\end{tabularx}\\endgroup", latex_tabular, fixed=T)
latex_tabular = gsub("\\begin{table}", "\\begin{table}[h!]\n\\centering", latex_tabular, fixed=T)
latex_tabular = gsub("\\centering", "\\centering\n\\begingroup\\footnotesize\\setstretch{1.2}", latex_tabular, fixed=T)
latex_tabular = gsub("\\hfill", "\\midrule", latex_tabular, fixed=T)
latex_tabular = gsub("\\endgroup", paste("\\endgroup\n \\caption{",caption,"} \\label{", label,"}", sep =""), latex_tabular, fixed = T)
latex_tabular = gsub("\\multicolumn{1}{c}{Castilla-Leon}", "\\hspace{-0.5cm}Castilla-Leon", latex_tabular, fixed = T)

#save table
cat(latex_tabular, file = "../figures/scores.tex")


#######
#Posterior probabilities of rho_gamma > rho_phi in the full models
cat(round(as.numeric(madrid_inference_results$full$posterior_prob[2]),2),
      file = "../figures/posterior_prob_madrid.tex")
cat(round(as.numeric(castilla_inference_results$full$posterior_prob[2]),2),
      file = "../figures/posterior_prob_castilla.tex")


#Posterior probabilities of rho_gamma > rho_phi in the shrinkage models
cat(round(as.numeric(madrid_inference_results$shrink$posterior_prob[2]),2),
    file = "../figures/posterior_prob_madrid_shrink.tex")
cat(round(as.numeric(castilla_inference_results$shrink$posterior_prob[2]),2),
    file = "../figures/posterior_prob_castilla_shrink.tex")






