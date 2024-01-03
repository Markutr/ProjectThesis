library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(sf)
library(sp)
library(spdep)
library(geosphere)
library(ggspatial)
library(makemyprior)
library(matrixcalc)
library(bayestestR)

setwd("/Users/MarkusTraetli/Desktop/markus-2023/r") #MAKE SURE THIS IS CORRECT
options(dplyr.summarise.inform = FALSE)

#Load data. Runs the data processing script if the RData file does not exist
if( !file.exists("../data/processed_case_and_mobility.RData") ){
  source("dataProcessing.R")
  } else {
  load("../data/processed_case_and_mobility.RData") 
  }

#change line to fix bug. Needs to be done when loading makemyprior
#Change line 12 to: if (no_neighb > 0) mat[ind, all_lines[[ind + 1]][c(-1, -2)]] <- -1
#trace(makemyprior:::make_besag_prec_mat, edit = T) 

# The following data frames are loaded
# castilla_cases_region & madrid_cases_region : sum of case counts for each municipality in the two regions. 
# castilla_cases_time & madrid_cases_time : sum of case counts over time for entire region
# castilla_mobility & madrid_mobility : mobility data over entire duration
# castilla_mobility_week1 & madrid_mobility_week1 : mobility data over the first week
# total_castilla & total_madrid : total amount of travel in each region

############################################################################################
#### functions 
############################################################################################
#Check the sum to zero constraints.
verify_sum_to_zero = function(result, tol = 0.05, n_samples = 100){
  
  #Extract info
  n_effects = length(result$summary.random)
  effect_names = names(result$summary.random)
  
  #Sample the model to check
  samples = inla.posterior.sample(n_samples, result)
  
  #Verify for each effect
  for (i in 1:n_effects) {
    vals = rep(0,n_samples)
    for (j in 1:n_samples) {
      sample_names = rownames(samples[[j]]$latent)
      mask = str_starts(sample_names, effect_names[i])
      stopifnot(sum(mask) == length(result$summary.linear.predictor$mean)) 
      vals[j] = sum(samples[[j]]$latent[str_starts(sample_names, effect_names[i])])
    }
    #Give warning if above a treshold
    if(mean(abs(vals)) > tol) {
      print(paste("Sum to zero failed for component", effect_names[i], "with mean abosulute sum of", mean(abs(vals)), "Samples:"))
      print(vals)
    }
  }
}


# The function extracts the results from the inla to compute estimates of rho
extract_res_inla = function(result, n_samples = 10000, return_hist_data = T, check_sum0 = F, check_samples = 100){
  
  #Check the sum to zero constraints
  if (check_sum0){
    verify_sum_to_zero(result, n_samples = check_samples)
  }
  n_fixed = ifelse(is.na(nrow(result$summary.fixed)), 0, nrow(result$summary.fixed))
  stopifnot(n_fixed > 0)
  
  posterior_samples = inla.hyperpar.sample(n_samples, result) #Sample the posterior
  effect_names = word(colnames(posterior_samples), start = -1) # Extract names
  stopifnot(length(effect_names) > 0) # Need random effect to work
  rho_samples = data.frame(matrix(ncol = length(effect_names), nrow = n_samples))
  colnames(rho_samples) = effect_names
  
  #Transform precision to variance
  for (i in 1:n_samples) {
    rho_samples[i,] = as.numeric(1/posterior_samples[i,])
  }
  sigma = sqrt(rowSums(rho_samples)) # Square root total marginal variance
  rho_samples = rho_samples/rowSums(rho_samples) #Calculate proportion of variance in each effect
  
  #Create dataframe to store the results
  inference_results = data.frame(matrix(ncol = 3, nrow = 1 + n_fixed + length(effect_names)))
  colnames(inference_results) = c("median", "0.025quant", "0.975quant")
  
  #These first rows are the fixed effects
  for (i in 1:n_fixed) {
    inference_results[i, ] = result$summary.fixed[c("0.5quant", "0.025quant", "0.975quant")][i,]
  }
  
  rownames(inference_results)[1:n_fixed] = rownames(result$summary.fixed)
  
  for (i in 1:length(effect_names)) {
    effect = effect_names[i]
    cred_int = ci(rho_samples[effect], method = "ETI")
    inference_results[n_fixed + i,] = c(median(rho_samples[effect][,1]), cred_int$CI_low, cred_int$CI_high)
  }
  
  rownames(inference_results)[(n_fixed + 1):(n_fixed + length(effect_names))] = paste0("rho_", effect_names)
  
  
  #Create equi-tailed CI
  cred_int_sigma = ci(sigma, method ="ETI")
  inference_results[n_fixed + length(effect_names) + 1,] = c(median(sigma), cred_int_sigma$CI_low, cred_int_sigma$CI_high)
  rownames(inference_results)[n_fixed + length(effect_names) + 1] = "sigma"
  
  # Include the samples in the returned object. For making posterior densities
  if (return_hist_data){
    hist_data = data.frame(matrix(ncol = length(effect_names) + 1, nrow = n_samples))
    colnames(hist_data)[1:length(effect_names)] = effect_names
    colnames(hist_data)[length(effect_names)+1] = "sigma"
    for (i in 1:length(effect_names)) {
      hist_data[effect_names[i]][,1] = rho_samples[,i]
    }
    colnames(hist_data)[1:length(effect_names)] = paste0("rho_", effect_names)
    hist_data$sigma = as.numeric(sigma)
    inference_results = list(inference_results = inference_results, hist_data = hist_data)
  }
  
  # Calculate posterior probabilities 
  inference_results$posterior_prob = c(paste("Posterior probability of", effect_names[1], ">",effect_names[2], sep =" "), mean(ifelse(rho_samples[,1] > rho_samples[,2], 1, 0)))
  
  return(inference_results)
}



############################################################################################
##### Castilla y Leon
############################################################################################


#Define formulas for different models. Use makemyprior to define movement and full.
formula_neighbor = formula(cases ~ beta + 
                             mc(phi, model = "besag", graph = "../data/neighbors_castilla.txt", scale.model = T, constr = T) +
                             mc(theta, model = "iid"))

formula_movement = formula(cases ~ beta + 
                             mc(gamma, model = "generic0", Cmatrix = mobility_structure_castilla, constr = T, rankdef = 1) +
                             mc(theta, model = "iid"))


formula_full = formula(cases ~ beta +
                             mc(gamma, model = "generic0", Cmatrix = mobility_structure_castilla, constr = T, rankdef = 1) +         
                             mc(phi, model="besag", graph = "../data/neighbors_castilla.txt", scale.model = T, constr = T) +
                             mc(theta, model="iid")) 

prior_neighbor_castilla = make_prior(formula_neighbor, data = castilla_sf, family = "poisson",
                                 prior = list(
                                   tree = "s1 = (theta, phi)",
                                   V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                                 intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                 covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_movement_castilla = make_prior(formula_movement, data = castilla_sf, family = "poisson",
                                 prior = list(
                                   tree = "s1 = (theta, gamma)",
                                   V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                                 intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                 covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_full_castilla = make_prior(formula_full, data = castilla_sf, family = "poisson",
                          prior = list(
                            tree = "s1 = (theta, phi, gamma)",
                            V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                          intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                          covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_full_castilla_shrink = make_prior(formula_full, data = castilla_sf, family = "poisson",
                                 prior = list(
                                   tree = "s1 = (phi, gamma); s2 = (theta, s1)",
                                 V = list(s2 = list(prior = "hn", param=1)),
                                 w = list(s2 =list(prior = "pc1", param = c(0.6)), s1 = list(prior = "pcM", param = c(0.25,0.8)))),
                                 intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                 covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

# Settings for INLA
inla_compute_params = control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE)
inla_params = list(int.strategy = "grid")

#Obtain results
res_neighbor_castilla = inference_inla(prior_neighbor_castilla, E = castilla_sf$total_pop, control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_movement_castilla = inference_inla(prior_movement_castilla, E = castilla_sf$total_pop, control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_full_castilla = inference_inla(prior_full_castilla, E = castilla_sf$total_pop, control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_full_castilla_shrink = inference_inla(prior_full_castilla_shrink, E = castilla_sf$total_pop, control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")



#Recompute CPO
res_neighbor_castilla$inla = inla.cpo(res_neighbor_castilla$inla)
res_movement_castilla$inla = inla.cpo(res_movement_castilla$inla)
res_full_castilla$inla = inla.cpo(res_full_castilla$inla)
res_full_castilla_shrink$inla = inla.cpo(res_full_castilla_shrink$inla)

#Make empty list to fill in 
castilla_inference_results = list(neighbor = extract_res_inla(res_neighbor_castilla$inla),
                                movement = extract_res_inla(res_movement_castilla$inla),
                                full = extract_res_inla(res_full_castilla$inla), 
                                shrink = extract_res_inla(res_full_castilla_shrink$inla))


#Calculate the dic, waic and cpo scores for each model
castilla_scores = data.frame(matrix(ncol = 3, nrow = 4))
colnames(castilla_scores) = c("dic", "waic", "cpo")
rownames(castilla_scores) = c("neighbor", "movement", "complete", "shrinkage")
castilla_scores[1,] = c(res_neighbor_castilla$inla$dic$dic, res_neighbor_castilla$inla$waic$waic, -mean(log(res_neighbor_castilla$inla$cpo$cpo)))
castilla_scores[2,] = c(res_movement_castilla$inla$dic$dic, res_movement_castilla$inla$waic$waic, -mean(log(res_movement_castilla$inla$cpo$cpo)))
castilla_scores[3,] = c(res_full_castilla$inla$dic$dic, res_full_castilla$inla$waic$waic, -mean(log(res_full_castilla$inla$cpo$cpo)))
castilla_scores[4,] = c(res_full_castilla_shrink$inla$dic$dic, res_full_castilla_shrink$inla$waic$waic, -mean(log(res_full_castilla_shrink$inla$cpo$cpo)))


castilla_inference_results$scores = castilla_scores

############################################################################################
##### Madrid
############################################################################################

#Define formulas for different models
formula_neighbor = formula(cases ~ beta + 
                             mc(phi, model = "besag", graph = "../data/neighbors_madrid.txt", scale.model = T, constr = T) +
                             mc(theta, model = "iid"))

formula_movement = formula(cases ~ beta + 
                             mc(gamma, model = "generic0", Cmatrix = mobility_structure_madrid, constr = T, rankdef = 1) +
                             mc(theta, model = "iid"))

formula_full = formula(cases ~ beta +
                         mc(gamma, model = "generic0", Cmatrix = mobility_structure_madrid, constr = T, rankdef = 1) +         
                         mc(phi, model="besag", graph = "../data/neighbors_madrid.txt", scale.model = T, constr = T) +
                         mc(theta, model="iid")) 

prior_neighbor_madrid = make_prior(formula_neighbor, data = madrid_sf, family = "poisson",
                                     prior = list(
                                       tree = "s1 = (theta, phi)",
                                       V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                                     intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                     covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_movement_madrid = make_prior(formula_movement, data = madrid_sf, family = "poisson",
                                     prior = list(
                                       tree = "s1 = (theta, gamma)",
                                       V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                                     intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                     covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_full_madrid = make_prior(formula_full, data = madrid_sf, family = "poisson",
                                 prior = list(
                                   tree = "s1 = (theta, phi, gamma)",
                                   V = list(s1 =list(prior = "hn", param = 1))), #Variance has closest thing to half normal(0,1) 
                                 intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                 covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

prior_full_madrid_shrink = make_prior(formula_full, data = madrid_sf, family = "poisson",
                                        prior = list(
                                          tree = "s1 = (phi, gamma); s2 = (theta, s1)",
                                          V = list(s2 = list(prior = "hn", param=1)),
                                          w = list(s2 = list(prior = "pc1", param = c(0.6)), s1 = list(prior = "pcM", param = c(0.25,0.8)))),
                                        intercept_prior = c(0,1), #Intercept prior is normal(0,1)
                                        covariate_prior = list(beta = c(0,1))) #covariate prior is normal(0,1)

# prior_movement_shrink_madrid = make_prior(formula_movement, data = madrid_sf, family = "poisson",
#                                           prior = list(
#                                             tree = "s1 = (theta, gamma)",
#                                             V = list(s1 = list(prior = "hn", param = 1)),
#                                             w = list(s1 = list(prior = "pc1", param = 0.6))),
#                                           intercept_prior = c(0,1), #Intercept prior is normal(0,1)
#                                           covariate_prior = list(beta = c(0,1)))

#Save for plotting of tree structures
shrink_prior = prior_full_madrid_shrink
full_prior = prior_full_madrid

#Save the plot data for the priors for own plots
prior_data = as.data.frame(plot_prior(prior_full_madrid)$data)
prior_data_shrink = as.data.frame(plot_prior(prior_full_madrid_shrink)$data)

# Settings for INLA
inla_compute_params = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE)
inla_params = list(int.strategy = "grid")

#Obtain results
res_neighbor_madrid = inference_inla(prior_neighbor_madrid, E = as.numeric(madrid_sf$TotalPopulation), control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_movement_madrid = inference_inla(prior_movement_madrid, E = as.numeric(madrid_sf$TotalPopulation), control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_full_madrid = inference_inla(prior_full_madrid, E = as.numeric(madrid_sf$TotalPopulation), control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
res_full_madrid_shrink = inference_inla(prior_full_madrid_shrink, E = as.numeric(madrid_sf$TotalPopulation), control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")
#res_movement_madrid_shrink = inference_inla(prior_movement_shrink_madrid, E = as.numeric(madrid_sf$TotalPopulation), control.compute = inla_compute_params, control.inla = inla_params, inla.mode = "classic")

#Recompute CPO
res_neighbor_madrid$inla = inla.cpo(res_neighbor_madrid$inla)
res_movement_madrid$inla = inla.cpo(res_movement_madrid$inla)
res_full_madrid$inla = inla.cpo(res_full_madrid$inla)
res_full_madrid_shrink$inla = inla.cpo(res_full_madrid_shrink$inla)


# Extract desired results from the inla results.
madrid_inference_results = list(neighbor = extract_res_inla(res_neighbor_madrid$inla),
                         movement = extract_res_inla(res_movement_madrid$inla),
                         full = extract_res_inla(res_full_madrid$inla),
                         shrink = extract_res_inla(res_full_madrid_shrink$inla))

#Calculate the dic, waic and cpo scores for each model
madrid_scores = data.frame(matrix(ncol = 3, nrow = 4))
colnames(madrid_scores) = c("dic", "waic", "cpo")
rownames(madrid_scores) = c("neighbor", "movement", "complete", "shrinked")
madrid_scores[1,] = c(res_neighbor_madrid$inla$dic$dic, res_neighbor_madrid$inla$waic$waic, -mean(log(res_neighbor_madrid$inla$cpo$cpo)))
madrid_scores[2,] = c(res_movement_madrid$inla$dic$dic, res_movement_madrid$inla$waic$waic, -mean(log(res_movement_madrid$inla$cpo$cpo)))
madrid_scores[3,] = c(res_full_madrid$inla$dic$dic, res_full_madrid$inla$waic$waic, -mean(log(res_full_madrid$inla$cpo$cpo)))
madrid_scores[4,] = c(res_full_madrid_shrink$inla$dic$dic, res_full_madrid_shrink$inla$waic$waic, -mean(log(res_full_madrid_shrink$inla$cpo$cpo)))

#Transfer to result object
madrid_inference_results$scores = madrid_scores

############
#### Save results for plotting

save(list = c("res_neighbor_castilla", "res_movement_castilla", "res_full_castilla", "res_full_castilla_shrink",
              "res_neighbor_madrid", "res_movement_madrid", "res_full_madrid", "res_full_madrid_shrink",
              "madrid_inference_results", "castilla_inference_results", "prior_data", "prior_data_shrink", "shrink_prior","full_prior"),
     file = "../data/inference_results.RData") #Save


