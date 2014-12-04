# Utility functions common in machine learning

sigmoid <- Vectorize(function(x) 1 / (1 + exp(-x)))