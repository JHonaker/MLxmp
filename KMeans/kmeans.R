# From the Wikipedia article on k-means clustering:
#
# k-means clustering is a method of vector quantization,
# originally from signal processing, that is popular for
# cluster analysis in data mining. k-means clustering aims
# to partition n observations into k clusters in which each
# observation belongs to the cluster with the nearest mean,
# serving as a prototype of the cluster. 

# To initialize we usually pick k random points. Since there is no
# guarantee that we will converge to the global maximum, more than one
# run with different starting points are used.
initialization.step <- function(x) {

}

# In the assignment step we can reuse a lot of the code from the
# k-nearest neighbors algorithm. We will flip the algorithm, instead of
# using the data that we have to classify new things, we will use the data
# to estimate the classifications. We assign each of the 
# k means their own class and then use the 1-NN of the dataset to 
# determine membership of the data points to each mean.
assignment.step <- function(x) {

}

# Then, we use these k new sets to calculate the new means, and repeat
# the assignment step until convergence.
update.step <- function(x) {

}