library(decisionSupport)
library(dplyr)
library(extrafont)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggstance)
library(tidyverse)

Calluna_Simulation <- function(x, varnames){
  
  # Number of plants in the whole production area
  original_plant_number <- production_area * plants_per_ha
  
  # Define risky months (May to August)                
  W <- weather_arguments_for_infection <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
  
  # Define months where detection rate is increased (May to June for monitoring plan) 
  MPD <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0) 
  
  # Infection risk for each month under uncertainty
  risk_per_month <- vv(infection_risk, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  # Factor settings in % so that they will not be higher or lower than 1
  # Calculate risks and factors for normal preventive spraying
  fungus_probability_N <- vv(fungus_probability_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_N <- vv(detection_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_N <- vv(disease_expansion_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_N <- vv(fungus_fight_effect_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  # Calculate risks and factors for reduced preventive spraying
  fungus_probability_R <- vv(fungus_probability_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_R <- vv(detection_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_R <- vv(disease_expansion_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_R <- vv(fungus_fight_effect_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  # Calculate risks and factors for monitoring plan + normal preventive spraying
  fungus_probability_MPN <- vv(fungus_probability_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_MPN <- vv(detection_factor_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_MPN <- vv(disease_expansion_factor_N, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_MPN <- vv(fungus_fight_effect_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  # Calculate risks and factors for monitoring plan + reduced preventive spraying
  fungus_probability_MPR <- vv(fungus_probability_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  detection_factor_MPR <- vv(detection_factor_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  disease_expansion_factor_MPR <- vv(disease_expansion_factor_R, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  fungus_fight_effect_MPR <- vv(fungus_fight_effect_MP, var_CV, 12)* W %>%
    sapply(., function(x) max(c(min(c(1,x)),0)))
  
  # Normal and reduced preventive spraying costs are calculated per year
  costs_yearly_prophy_application_N <- round((number_yearly_prophy_application_N),digits=0) * 
    cost_one_prophy_application * production_area
  
  costs_yearly_prophy_application_R <- round((number_yearly_prophy_application_R),digits=0) * 
    cost_one_prophy_application * production_area
  
  # Normal and reduced preventive spraying costs + monitoring plan are calculated per year
  costs_yearly_prophy_application_MPN <- round((number_yearly_prophy_application_N),digits=0) * 
    cost_one_prophy_application * production_area
  
  costs_yearly_prophy_application_MPR <- round((number_yearly_prophy_application_R),digits=0) * 
    cost_one_prophy_application * production_area
  
  # Split number of preventive applications per year in accidentally selected applications per month
  
  need.random.integers <- function(a,b,n,k){
    # Finds n random integers in range a:b which sum to k
    if (n*b<k) stop("k too large")
    if (n*a>k) stop("k too small")
    while(TRUE){
      x <- sample(1:(k - n*a),n-1, replace = TRUE)
      x <- sort(x)
      x <- c(x,k-n*a) - c(0,x)
      if(max(x) <= b-a) return(a+x)}}
  
  # Number of preventive applications per month for normal preventive spraying
  
  effect_application_N <- need.random.integers(0, round((number_yearly_prophy_application_N), digits = 0),
                                               4, round((number_yearly_prophy_application_N), digits = 0))* W
  
  # Define effect of normal preventive spraying (potential to reduce fungus onset)
  
  effect_application_N <- effect_application_N %>%
    replace(., effect_application_N==0, effect_no_prophy_application) %>%
    replace(., effect_application_N==1, effect_one_prophy_application) %>%
    replace(., effect_application_N==2, effect_two_prophy_application) %>%
    replace(., effect_application_N==3, effect_three_prophy_application) %>%
    replace(., effect_application_N==4, effect_four_prophy_application) %>%
    replace(., effect_application_N==5, effect_five_prophy_application) %>%
    replace(., effect_application_N==6, effect_six_prophy_application) %>%
    replace(., effect_application_N==7, effect_seven_prophy_application) %>%
    replace(., effect_application_N>=8, effect_eight_prophy_application) * W
  
  
  # Number of preventive applications per month for normal preventive spraying
  
  effect_application_R <- need.random.integers(0, round((number_yearly_prophy_application_R), digits = 0),
                                               4, round((number_yearly_prophy_application_R), digits = 0))* W
  
  # Define effect of reduced preventive spraying (potential to reduce fungus onset)
  
  effect_application_R <- effect_application_R %>%
    replace(., effect_application_R==0, effect_no_prophy_application) %>%
    replace(., effect_application_R==1, effect_one_prophy_application) %>%
    replace(., effect_application_R==2, effect_two_prophy_application) %>%
    replace(., effect_application_R==3, effect_three_prophy_application) %>%
    replace(., effect_application_R==4, effect_four_prophy_application) %>%
    replace(., effect_application_R==5, effect_five_prophy_application) %>%
    replace(., effect_application_R==6, effect_six_prophy_application) %>%
    replace(., effect_application_R==7, effect_seven_prophy_application) %>%
    replace(., effect_application_R>=8, effect_eight_prophy_application) * W
  
  # Define effect of normal and reduced preventive spraying for monitoring plan (potential to reduce fungus onset)
  effect_application_MPN <- effect_application_N
  effect_application_MPR <- effect_application_R
  
  # Simulate all probabilities of infection, effect of prophylactic applications,
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for normal preventive spraying
  
  infected_plant_number_N <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_N <- W
  symptomatic_plants_N <- W
  detected_plants_after_monitoring_N <- W
  healthy_plants_after_fungus_fight_N <- W
  getting_again_sick_plants_N <- W
  
  for (i in 2:length(infected_plant_number_N)){ 
    infected_plant_number_N[i] <- infected_plant_number_N[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_N[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_N)) {
    not_infected_plants_after_prophy_N[j] <- not_infected_plants_after_prophy_N[j-1] + effect_application_N[j]*
      (infected_plant_number_N[j] - not_infected_plants_after_prophy_N[j-1])}
  
  still_infected_plants_N <- infected_plant_number_N - not_infected_plants_after_prophy_N
  
  for (k in 2:length(symptomatic_plants_N)) {
    symptomatic_plants_N[k] <- symptomatic_plants_N[k-1] + fungus_probability_N[k]*
      (still_infected_plants_N[k] - symptomatic_plants_N[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_N)) {
    detected_plants_after_monitoring_N[l] <- detected_plants_after_monitoring_N[l-1] + detection_factor_N[l]*
      (symptomatic_plants_N[l] - detected_plants_after_monitoring_N[l-1])}
  
  symptomatic_plants_after_monitoring_N <- symptomatic_plants_N - detected_plants_after_monitoring_N
  
  for (m in 2:length(getting_again_sick_plants_N)) {
    getting_again_sick_plants_N[m] <- getting_again_sick_plants_N[m-1] + disease_expansion_factor_N[m]*
      (symptomatic_plants_after_monitoring_N[m] - getting_again_sick_plants_N[m-1])}
  
  all_symptomatic_plants_N <- symptomatic_plants_after_monitoring_N + getting_again_sick_plants_N
  
  for (n in 2:length(healthy_plants_after_fungus_fight_N)) {
    healthy_plants_after_fungus_fight_N[n] <- healthy_plants_after_fungus_fight_N[n-1] + fungus_fight_effect_N[n]*
      (all_symptomatic_plants_N[n] - healthy_plants_after_fungus_fight_N[n-1])}
  
  final_fungus_infected_plants_N <- all_symptomatic_plants_N[12] - healthy_plants_after_fungus_fight_N[12] #now it is always positive
  
  direct_plant_losses_N <- detected_plants_after_monitoring_N[12]
  
  actual_saleable_Callunas_N <- original_plant_number - (final_fungus_infected_plants_N + direct_plant_losses_N)
  
  # Simulate all probabilities of infection, effect of prophylactic applications,
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for reduced preventive spraying
  
  infected_plant_number_R <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_R <- W
  symptomatic_plants_R <- W
  detected_plants_after_monitoring_R <- W
  healthy_plants_after_fungus_fight_R <- W
  getting_again_sick_plants_R <- W
  
  for (i in 2:length(infected_plant_number_R)){
    infected_plant_number_R[i] <- infected_plant_number_R[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_R[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_R)) {
    not_infected_plants_after_prophy_R[j] <- not_infected_plants_after_prophy_R[j-1] + effect_application_R[j]*
      (infected_plant_number_R[j] - not_infected_plants_after_prophy_R[j-1])}
  
  still_infected_plants_R <- infected_plant_number_R - not_infected_plants_after_prophy_R
  
  for (k in 2:length(symptomatic_plants_R)) {
    symptomatic_plants_R[k] <- symptomatic_plants_R[k-1] + fungus_probability_R[k]*
      (still_infected_plants_R[k] - symptomatic_plants_R[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_R)) {
    detected_plants_after_monitoring_R[l] <- detected_plants_after_monitoring_R[l-1] + detection_factor_R[l]*
      (symptomatic_plants_R[l] - detected_plants_after_monitoring_R[l-1])}
  
  symptomatic_plants_after_monitoring_R <- symptomatic_plants_R - detected_plants_after_monitoring_R
  
  for (m in 2:length(getting_again_sick_plants_R)) {
    getting_again_sick_plants_R[m] <- getting_again_sick_plants_R[m-1] + disease_expansion_factor_R[m]*
      (symptomatic_plants_after_monitoring_R[m] - getting_again_sick_plants_R[m-1])}
  
  all_symptomatic_plants_R <- symptomatic_plants_after_monitoring_R + getting_again_sick_plants_R
  
  for (n in 2:length(healthy_plants_after_fungus_fight_R)) {
    healthy_plants_after_fungus_fight_R[n] <- healthy_plants_after_fungus_fight_R[n-1] + fungus_fight_effect_R[n]*
      (all_symptomatic_plants_R[n] - healthy_plants_after_fungus_fight_R[n-1])}
  
  final_fungus_infected_plants_R <- all_symptomatic_plants_R[12] - healthy_plants_after_fungus_fight_R[12]
  
  direct_plant_losses_R <- detected_plants_after_monitoring_R[12]
  
  actual_saleable_Callunas_R <- original_plant_number - (final_fungus_infected_plants_R + direct_plant_losses_R)
  
  # Simulate all probabilities of infection, effect of prophylactic applications,
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for normal preventive spraying + monitoring plan
  
  infected_plant_number_MPN <- original_plant_number * risk_per_month
  
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_MPN <- W
  symptomatic_plants_MPN <- W
  detected_plants_after_monitoring_MPN <- MPD
  healthy_plants_after_fungus_fight_MPN <- W
  getting_again_sick_plants_MPN <- W
  
  for (i in 2:length(infected_plant_number_MPN)){ 
    infected_plant_number_MPN[i] <- infected_plant_number_MPN[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_MPN[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_MPN)) {
    not_infected_plants_after_prophy_MPN[j] <- not_infected_plants_after_prophy_MPN[j-1] + effect_application_MPN[j]*
      (infected_plant_number_MPN[j] - not_infected_plants_after_prophy_MPN[j-1])}
  
  still_infected_plants_MPN <- infected_plant_number_MPN - not_infected_plants_after_prophy_MPN
  
  for (k in 2:length(symptomatic_plants_MPN)) {
    symptomatic_plants_MPN[k] <- symptomatic_plants_MPN[k-1] + fungus_probability_MPN[k]*
      (still_infected_plants_MPN[k] - symptomatic_plants_MPN[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_MPN)) {
    detected_plants_after_monitoring_MPN[l] <- detected_plants_after_monitoring_MPN[l-1] + detection_factor_MPN[l]*
      (symptomatic_plants_MPN[l] - detected_plants_after_monitoring_MPN[l-1])}
  
  symptomatic_plants_after_monitoring_MPN <- symptomatic_plants_MPN - detected_plants_after_monitoring_MPN
  
  for (m in 2:length(getting_again_sick_plants_MPN)) {
    getting_again_sick_plants_MPN[m] <- getting_again_sick_plants_MPN[m-1] + disease_expansion_factor_MPN[m]*
      (symptomatic_plants_after_monitoring_MPN[m] - getting_again_sick_plants_MPN[m-1])}
  
  all_symptomatic_plants_MPN <- symptomatic_plants_after_monitoring_MPN + getting_again_sick_plants_MPN
  
  for (n in 2:length(healthy_plants_after_fungus_fight_MPN)) {
    healthy_plants_after_fungus_fight_MPN[n] <- healthy_plants_after_fungus_fight_MPN[n-1] + fungus_fight_effect_MPN[n]*
      (all_symptomatic_plants_MPN[n] - healthy_plants_after_fungus_fight_MPN[n-1])}
  
  final_fungus_infected_plants_MPN <- all_symptomatic_plants_MPN[12] - healthy_plants_after_fungus_fight_MPN[12]
  
  direct_plant_losses_MPN <- detected_plants_after_monitoring_MPN[12]
  
  actual_saleable_Callunas_MPN <- original_plant_number - (final_fungus_infected_plants_MPN + direct_plant_losses_MPN)
  
  # Simulate all probabilities of infection, effect of prophylactic applications,
  # number of symptomatic plants, monitoring and detection of sick plants and reinfection 
  # to get number of detected, not saleable and healthy plants for reduced preventive spraying + monitoring plan
  
  infected_plant_number_MPR <- original_plant_number * risk_per_month
  
  # All used vectors need to have the same length
  not_infected_plants_after_prophy_MPR <- W
  symptomatic_plants_MPR <- W
  detected_plants_after_monitoring_MPR <- MPD
  healthy_plants_after_fungus_fight_MPR <- W
  getting_again_sick_plants_MPR <- W
  
  for (i in 2:length(infected_plant_number_MPR)){ 
    infected_plant_number_MPR[i] <- infected_plant_number_MPR[i-1] + risk_per_month[i]*
      (original_plant_number - infected_plant_number_MPR[i-1])}
  
  for (j in 2:length(not_infected_plants_after_prophy_MPR)) {
    not_infected_plants_after_prophy_MPR[j] <- not_infected_plants_after_prophy_MPR[j-1] + effect_application_MPR[j]*
      (infected_plant_number_MPR[j] - not_infected_plants_after_prophy_MPR[j-1])}
  
  still_infected_plants_MPR <- infected_plant_number_MPR - not_infected_plants_after_prophy_MPR
  
  for (k in 2:length(symptomatic_plants_MPR)) {
    symptomatic_plants_MPR[k] <- symptomatic_plants_MPR[k-1] + fungus_probability_MPR[k]*
      (still_infected_plants_MPR[k] - symptomatic_plants_MPR[k-1])}
  
  for (l in 2:length(detected_plants_after_monitoring_MPR)) {
    detected_plants_after_monitoring_MPR[l] <- detected_plants_after_monitoring_MPR[l-1] + detection_factor_MPR[l]*
      (symptomatic_plants_MPR[l] - detected_plants_after_monitoring_MPR[l-1])}
  
  symptomatic_plants_after_monitoring_MPR <- symptomatic_plants_MPR - detected_plants_after_monitoring_MPR
  
  for (m in 2:length(getting_again_sick_plants_MPR)) {
    getting_again_sick_plants_MPR[m] <- getting_again_sick_plants_MPR[m-1] + disease_expansion_factor_MPR[m]*
      (symptomatic_plants_after_monitoring_MPR[m] - getting_again_sick_plants_MPR[m-1])}
  
  all_symptomatic_plants_MPR <- symptomatic_plants_after_monitoring_MPR + getting_again_sick_plants_MPR
  
  for (n in 2:length(healthy_plants_after_fungus_fight_MPR)) {
    healthy_plants_after_fungus_fight_MPR[n] <- healthy_plants_after_fungus_fight_MPR[n-1] + fungus_fight_effect_MPR[n]*
      (all_symptomatic_plants_MPR[n] - healthy_plants_after_fungus_fight_MPR[n-1])}
  
  final_fungus_infected_plants_MPR <- all_symptomatic_plants_MPR[12] - healthy_plants_after_fungus_fight_MPR[12]
  
  direct_plant_losses_MPR <- detected_plants_after_monitoring_MPR[12]
  
  actual_saleable_Callunas_MPR <- original_plant_number - (final_fungus_infected_plants_MPR + direct_plant_losses_MPR)
  
  # Simulation of expected value for price premium Calluna plants
  value_of_saleable_more_sustainable_Calluna <- value_of_saleable_Calluna +
    chance_event(chance_higher_price_sustainable, price_premium_sustainable, 0)
  
  # Simulation of final monetary value for normal preventive spraying concerning
  # saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_N <- actual_saleable_Callunas_N * value_of_saleable_Calluna
  value_of_sorted_out_plants_N <- direct_plant_losses_N * value_sorted_out_Calluna
  value_of_not_saleable_plants_N <- final_fungus_infected_plants_N * value_not_saleable_Calluna
  
  # Simulation of final monetary value for reduced preventive spraying concerning
  # saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_R <- actual_saleable_Callunas_R * value_of_saleable_more_sustainable_Calluna
  value_of_sorted_out_plants_R <- direct_plant_losses_R * value_sorted_out_Calluna
  value_of_not_saleable_plants_R <- final_fungus_infected_plants_R * value_not_saleable_Calluna
  
  # Simulation of final monetary value for normal and reduced preventive spraying + monitoring plan
  # concerning saleable, sorted out and not saleable plants after cultivation time
  value_saleable_plants_MPN <- actual_saleable_Callunas_MPN * value_of_saleable_Calluna
  value_of_sorted_out_plants_MPN <- direct_plant_losses_MPN * value_sorted_out_Calluna
  value_of_not_saleable_plants_MPN <- final_fungus_infected_plants_MPN * value_not_saleable_Calluna
  
  value_saleable_plants_MPR <- actual_saleable_Callunas_MPR * value_of_saleable_more_sustainable_Calluna
  value_of_sorted_out_plants_MPR <- direct_plant_losses_MPR * value_sorted_out_Calluna
  value_of_not_saleable_plants_MPR <- final_fungus_infected_plants_MPR * value_not_saleable_Calluna
  
  # List up yearly costs for whole production area for normal preventive spraying
  costs_per_year_N <- (value_of_one_new_plant * original_plant_number) +
    costs_yearly_prophy_application_N +
    costs_normal_fertilizer * production_area +
    (costs_monitoring_per_ha_month * sum(W) * production_area) +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_N * production_area
  
  
  # List up the benefits for normal preventive spraying
  benefits_per_year_N <- value_saleable_plants_N
  
  
  # List up staff costs
  costs_staff <- ifelse(production_area > threshold_big_area_more_staff, costs_staff + 
                          costs_more_staff, costs_staff)
  
  # List up yearly costs for reduced preventive spraying
  costs_per_year_R <- (value_of_one_new_plant * original_plant_number) +
    costs_yearly_prophy_application_R +
    (costs_normal_fertilizer + fertilizer_adjustment) * production_area +
    (costs_monitoring_per_ha_month * sum(W) * production_area) +
    additional_costs_more_monitoring_per_ha * production_area +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_R * production_area
  
  # List up the benefits for reduced preventive spraying
  benefits_per_year_R <- value_saleable_plants_R
  
  # List up yearly costs for whole production area for normal preventive spraying + monitoring plan
  costs_per_year_MPN <- (value_of_one_new_plant * original_plant_number) +
    (direct_plant_losses_MPN * amount_of_samples_MP * costs_per_sample_MP) +
    costs_yearly_prophy_application_MPN +
    costs_normal_fertilizer * production_area +
    (costs_monitoring_plan_per_ha * production_area) +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_N * production_area
  
  # List up the benefits for normal preventive spraying + monitoring plan
  benefits_per_year_MPN <- value_saleable_plants_MPN + (savings_due_to_MP * production_area)
  
  # List up yearly costs for reduced preventive spraying + monitoring plan
  costs_per_year_MPR <- (value_of_one_new_plant * original_plant_number) +
    (direct_plant_losses_MPR * amount_of_samples_MP * costs_per_sample_MP) +
    costs_yearly_prophy_application_MPR +
    (costs_normal_fertilizer + fertilizer_adjustment) * production_area +
    (costs_monitoring_plan_per_ha * production_area) +
    additional_costs_more_monitoring_per_ha * production_area +
    costs_staff +
    yearly_costs_of_direct_fungus_fight_R * production_area
  
  # List up the benefits for reduced preventive spraying + monitoring plan
  benefits_per_year_MPR <- value_saleable_plants_MPR + (savings_due_to_MP * production_area)
  
  #### Benefits and outputs ####
  
  # All outcomes: NORMAL (N), REDUCE (R), WATCH (MPN) and WATCHREDUCE (MPR)
  cashflow_N <- benefits_per_year_N - costs_per_year_N
  cashflow_R <- benefits_per_year_R - costs_per_year_R
  cashflow_MPN <- benefits_per_year_MPN - costs_per_year_MPN
  cashflow_MPR <- benefits_per_year_MPR - costs_per_year_MPR
  
  # All outcomes in comparison with NORMAL: SPRAYLESS, WATCHMORESPRAYLESS, WATCHMORE
  # are visualized in the next sections to plot probability distributions, EVPI and VIP analyses
  comp_R_N<-(cashflow_R-cashflow_N)/production_area
  comp_MPR_N<-(cashflow_MPR-cashflow_N)/production_area
  comp_MPN_N<-(cashflow_MPN-cashflow_N)/production_area
  comp_MPR_MPN<-(cashflow_MPR-cashflow_MPN)/production_area
  
  return(list(comp_R_N=comp_R_N,
              comp_MPR_N=comp_MPR_N,
              comp_MPN_N=comp_MPN_N,
              comp_MPR_MPN=comp_MPR_MPN,
              cashflow_N=cashflow_N,
              cashflow_R=cashflow_R,
              cashflow_MPR=cashflow_MPR,
              cashflow_MPN=cashflow_MPN))}


input_table <- "Input_file.csv"
legend_file <- "Legend_file.csv"
results_folder <- "Results_low_prophy"
figures_folder <- "EVPI_tables_low_prophy"

make_variables <- function(est,n=1)
{ x <- random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir = .GlobalEnv)}
make_variables(estimate_read_csv(input_table))
dir.create(figures_folder)

decisionSupport(input_table, #input file with estimates
                results_folder, #output folder
                write_table = TRUE, Calluna_Simulation, 10000,
                functionSyntax = "plainNames")

outvars <- list("comp_R_N","comp_MPR_N","comp_MPN_N","comp_MPR_MPN","cashflow_N","cashflow_R","cashflow_MPR","cashflow_MPN")
legend_table <- read.csv(legend_file)

labels <- list("comp_R_N","comp_MPR_N","comp_MPN_N","comp_MPR_MPN","cashflow_N","cashflow_R","cashflow_MPR","cashflow_MPN")

MC_file <- read.csv(paste(results_folder,"/mcSimulationResults.csv",sep = ""))


mc_EVPI <- MC_file[,-grep("cashflow",colnames(MC_file))]
mc_EVPI <- MC_file
multi_EVPI(mc = mc_EVPI,"comp_R_N",write_table = TRUE,outfolder = figures_folder)