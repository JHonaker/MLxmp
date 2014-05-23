# This is an implementation of the k-nearest neighbors classification algorithm
# with various metrics for nearness and classification functions

# This is the workhorse function. It will take the nearness/classification
# functions as arguments and return the classification of the various train
# points.
knn <- function(k, test.points, dataset, class.col=1,
				metric=euclidean,
				voting.fun=most.frq,
				prob=FALSE, smooth=0.5) {

	if (!is.numeric(k) | k < 1) stop('K must be a number > 0')

	k <- floor(k)

	# Returns an ordered data frame of the dataset by distance
	nearest.points <- function(point) {
		t <- data.frame(dataset,
						distance=apply(dataset[, -class.col], 1,
										metric, point))
		# Return the dataset sorted by the distance metric (ascending)
		t[order(t$distance), ]
	}

	# TODO: Apply over test.points
	apply(test.points, 1, function(point) {
			NNs <- nearest.points(point)[1:k, ]
			cls <- voting.fun(NNs[, class.col], NNs[, ncol(NNs)])
			print(cls)
			if (prob) {
				s <- smooth
				Ni <- sum(NNs[, class.col] == cls)
				print(Ni)
				C <- length(unique(dataset[, class.col]))
				print(C)
				return((Ni + s) / (k + C * s))
			} else {
				return(cls)
			}
		})
}	

# Distance metrics
euclidean <- function(to, from) {
	sqrt( sum( (from - to)^2 ) )
}

# The hamming distance is one metric that can be used on non-numeric
# data. It is defined as the number of positions, in two strings of
# length n, that are different from one another.
hamming.dist <- function(to, from) {
	if (nchar(to) != nchar(from))
		stop("The two strings must be of equal length.")
		
	to <- unlist(strsplit(to, split=''))
	from <- unlist(strsplit(from, split=''))

	sum(to != from)
}

# Reports the most frequent classification.
# Code taken from: http://stackoverflow.com/a/8189441/2985170
most.frq <- function(nbr.class, nbr.distance) {
	uniq <- unique(nbr.class)
	uniq[which.max(tabulate(match(nbr.class, uniq)))]
}


# TODO: Other voting functions and metrics


# TODO: Write example script at bottom.