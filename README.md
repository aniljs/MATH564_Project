# MATH564_Project
Applied Statistics Project
Predicting loan defaults using binary-outcome models
December 2016
  
  The first goal of the project was to model the probability that a borrower with a given socio-economic profile might default on a loan. To this end, we utilized two separate logistic regression models to estimate loan default probabilities: the first model was based on the Akaike information criterion (AIC) and the second model was based on the Bayesian Information Criterion (BIC). The second aim of the project was to compare the predictive efficacy of the each of the two models by comparing their classification accuracy on outcomes of a test dataset. The following is a brief overview of the tasks involved in the project and the final outcome:

Cleaning and Feature selection (using R):
  1. The data for the project was extracted from the Lending Club. The dataset was cleaned and formatted thoroughly so as to make it appropriate for statistical analysis, this also included preliminary feature selection.
  2. The dataset was divided into a training-set (80% of dataset) and a test-set (20% of dataset). 
  3. Final model selection was performed using the training-set based on the AIC and BIC via (bidirectional) stepwise logistic regression. 

Model Diagnostics and Classification Accuracy (using R):
  1. Both models preformed well on the training dataset in terms of model diagnostics (error rate, expected proportion of correct predictions (ePCP), deviance, and the receiver operating characteristic curve (ROC Curve). However, the AIC model nominally outperformed the BIC model on all of the indicators, albeit only marginally.
  2. In terms of classification of outcomes on the test-set, the AIC model had an accuracy of 97.59% while the BIC model had an accuracy of 97.51%.less


Team members

  Ashim Bhattarai
  
  Rodrigo Tejeida Estrada
  
  Sardhendu Mishra
  
  Anil Simon

  Master of Data Science Candidates, Illinois Institute of Technology
