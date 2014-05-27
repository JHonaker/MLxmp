# Note: This implementation only works with numerical data.

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
# We will use the Forgy initialization algorithm:
# 	Choose k random points from the data set,
# 	and use them as the initialization points.
initialization.step <- function(k, dataset) {
	dataset[sample(1:nrow(dataset), k), ]
}

# In the assignment step we can reuse a lot of the code from the
# k-nearest neighbors algorithm. We will flip the algorithm, instead of
# using the data that we have to classify new things, we will use the data
# to estimate the classifications. We assign each of the 
# k means their own class and then use the 1-NN of the dataset to 
# determine membership of the data points to each mean.
source('../KNearestNeighbors/knn.R')

assignment.step <- function(dataset, means) {
	means <- cbind(1:nrow(means), means)
	knn(1, dataset, means)
}

# Then, we use these k new sets to calculate the new means, and repeat
# the assignment step until convergence.
update.step <- function(dataset, sets) {
	do.call('rbind', lapply(1:max(sets), function(k) {
			(1 / nrow(dataset[sets == k, ])) *
				colSums(dataset[sets == k,])
		}))
}

kmeans <- function(k, dataset, max.iters=100, tol=0.0001, ret.history=FALSE) {

    mean.mat <- array(NA, dim=c(k, ncol(dataset), max.iters))
    dataset <- as.matrix(dataset)

    iter <- 1
    mean.mat[, , iter] <- initialization.step(k, dataset)
    diff <- tol + 1
    while (iter < max.iters & diff > tol) {
        mean.mat[, , iter + 1] <- update.step(dataset,
            assignment.step(dataset, mean.mat[, , iter]))

        iter <- iter + 1
        diff <- sum(abs(mean.mat[, , iter] - mean.mat[, , iter - 1]))
    } 

    if (ret.history == TRUE) {
        return(mean.mat[, , 1:iter])
    } else {
        return(mean.mat[, , iter])
    }
}

# EXAMPLE:
example.2.clusters <- function(max.iters=100, tol=0.01) {
	mean.mat <- array(NA, c(2, 2, max.iters))

	dat <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           		 matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

	iter <- 1
	diff <- tol + 1
	mean.mat[, , iter] <- initialization.step(2, dat)
	while (iter <= max.iters && diff > tol) {
		mean.mat[, , iter + 1] <- update.step(dat,
			assignment.step(dat, mean.mat[, , iter]))
		
		iter <- iter + 1
		diff <- sum(abs(mean.mat[, , iter] - mean.mat[, , iter - 1]))
	}

	mean.mat[, , 1:iter]
}

example.iris <- function() {
    max.iters=100

    data(iris)
    test.iris <- iris[, 1:4]
    actual.means <- do.call('rbind', by(iris[, 1:4], iris$Species, colMeans))

    test.means <- kmeans(3, test.iris)
    print("Actual Means")
    print(actual.means)
    print("Trained Means")
    print(test.means)
}