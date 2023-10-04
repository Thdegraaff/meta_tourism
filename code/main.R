# First read in the data and rename/transform variables
# Note, that you need a data and code folder in your project
source("code/install_packages.R") # install and read all needed packages
source("code/read_data.R") # read and wrangle data

# Make first descriptive and simulation plots
# Note, that you need a figures folder in your project
source("code/make_descriptive_figures.R") # create descriptives figures
source("code/graphs_bias.R") # create publication bias diagrams

# Run estimations
# Note, that you need a output folder in your project
source("code/intercept_only_models.R") # intercept only models
source("code/pubbias_models.R") # publication bias models
source("code/benchmark_models.R") # full models

# make tables
# Note, that you need a output folder in your project
source("code/make_table_intercept_only.R") # create tables for intercept only and publication bias models
source("code/make_tables.R") # create tables for full models 

# Create ex-post plots and analyses
# Note, that you need a figures folder in your project
source("code/predict_mean.R") # mean effect size is not the constant
source("code/make_figure_time.R") # create time trends
# source("code/compare_intercept.R") # to assess differences between effect sizes of two models
source("code/international_domestic_time.R") # time trends in international and domestic tourism
source("code/dist_sim.R") # Create simulation of total amount of tourists in distance