# This is an implementation of the k-nearest neighbors classification algorithm
# with various metrics for nearness and classification functions

# This is the workhorse function. It will take the nearness/classification
# functions as arguments and return the classification of the various train
# points.
knn <- function(k, test.points, dataset,
				metric=euclidean,
				voting.fun=most.frq) {

	# Returns an ordered data frame of the dataset by distance
	nearest.points <- function(point) {
		t <- data.frame(dataset,
			distance=apply(dataset, 1, metric, point))
		t[with(t, order(distance)), ]
	}

	# TODO: Apply over test.points
	nearest.points(c(1,1))

	# Figure out how to input classifications.
}	

euclidean <- function(to, from) {
	sqrt( sum( (from - to)^2 ) )
}

# TODO: most.frq voting function

# TODO: Other voting functions and metrics
#		like the Hamming distance