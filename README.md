# meta_tourism

## Summary

This is the repository for the paper **Going the distance? A meta-analysis of the deterring effect of distance in tourism** co-authored by Elisa Panzera and Henri L. F. de Groot. 

## Requirements

You need *R* and preferably *RStudio* to run this code (the latter is only needed for the internal 
file structure as it uses a project). Moreover, one needs to install *Stan* to estimate the meta-regression models by Markov Chain
Monte Carlo sampling.

## Setup

You need to create a subfolder `output` for yourself. Namely: estimation results will be written to 
this folder as well as the output tables (in `.tex` format). The file `install_packages.R` can be used to install and 
read all necessary packages; however, they are included in every script file as well, so that every `R` file can be used as 
stand-alone, provided the needed input data or estimation results is already generated. 

## Contents

### Code

All code needed for output as produced in the paper (Tables and Figures). 
If you want to run all output consecutively you need to run `main.R`.

### Data

Main Data is to be found in `data_updated_RR2.xlsx` with some ancilliary data
and earlier versions of the database. `read_data.R` transforms this to an R data file.

### Figures

Figures as can be produced by the code. The `dag.tex` and `dag_2.tex` files create
Directed Acyclical Graphs in pdf to be used in the paper and the appendix.

### Output

Statistical output and tables in `.tex` format. Estimations create here large `.RData` files.
