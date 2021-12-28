# Transfer Learning Analysis for Landscape-Aware Algorithm Selection

This repository contains the source code associated with the paper "Transfer Learning Analysis for Landscape-Aware Algorithm Selection", which analyzed the results of a transfer learning machine learning model for automatic algorithm selection using Exploratory Landscape Analysis.

## Requirements
The majority of the source code is written in R, and requires the following libraries.
* robustbase
* stringr
* randomForest
* R.matlab
* ggplot2
* ggcorplot




The shapley calculation is written in python, and requires the following libraries:
* scikit-learn
* shap
* pandas


## Structure
The repository contains the following folders

### landscape_features
Contains all of the raw landscape feature data that is used for training and testing the machine learning models. Due to the large number of files, the features are stored as a compressed archive.

The files are named as {iter}_{function_number}\_samples\_{algorithm}\_250\_.mat.RDS, where iter is the iteration of the sampling, function\_number is the ID of the function on which the features were calculated, and algorithm is the best performing algorithm (the prediction class).

The landscape features are saved as plain R objects, and can be loaded from R using the function readRDS.

Further, the processed folder contains two files containing the preprocessed features that can be used directly for machine learning.


### machine_learning
Contains the code used to train and test the machine learning model, with each individual model as its own R file.


### shapley
Contains the code used for the calculation of the shapley values.

### shapley_plots
Contains the visualizations of the shapley values.

### visualization
Contains the code for the visualization of the data.






