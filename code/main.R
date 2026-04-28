# First read in the data and rename/transform variables
# Note, that you need a "data", "figures", and "code" folder in your project

# We will use the here library here
library(here)

#source("code/install_packages.R") # install all needed packages
source_path <- here::here("code", "read_data.R")
source(source_path) # read and wrangle data

# Make first descriptive and simulation plots
# Note, that you need a figures folder in your project
source_path <- here::here("code", "make_descriptive_figures.R")
source(source_path) # create descriptive figures
source_path <- here::here("code", "graphs_bias.R")
source(source_path) # create publication bias diagrams

# Run estimations
# Note, that you need a output folder in your project
code_path <- here::here("code", "intercept_only_models.R")
source(code_path) # create publication bias diagrams
code_path <- here::here("code", "pubbias_models.R")
source(code_path) # publication bias models
code_path <- here::here("code", "benchmark_models.R")
source(code_path) # full models
code_path <- here::here("code", "sensitivity_models.R") 
source(code_path) # sensitivity models to check for imputed errors
# variations in specifications and international versus domestic tourism
code_path <- here::here("code", "sensitivity_models2.R")
source(code_path) # Additional sensitivity models to check for 
# estimation with weights, different time periods and one
# observation per primary paper
code_path <- here::here("code", "sensitivity_models_sim.R")
source(code_path) # Checking the effect of different 
# priors (running from 0.25 to 10 for the standard deviation)
# of the beta parameters and intercept.

# make tables
# Note, that you need a output folder in your project
code_path <- here::here("code", "make_table_intercept_only.r")
source(code_path) # create tables for intercept only and publication bias models
code_path <- here::here("code", "make_tables.R")
source(code_path) # create tables for full models 

# Create ex-post plots and analyses
# Note, that you need a figures folder in your project
code_path <- here::here("code", "predict_mean.R")
source(code_path) # mean effect size is not the constant
code_path <- here::here("code", "make_figure_time.R")
source(code_path) # create time trends
code_path <- here::here("code", "international_domestic_time.R")
source(code_path) # Estimate and create Figure 6
code_path <- here::here("code", "dist_sim.R")
source(code_path) # Create simulation of total amount of tourists in distance