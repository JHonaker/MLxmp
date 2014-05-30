# ID3 or 'Iterative Dichotomizer 3' is and algorithm for creating
# a decision tree from a dataset. It is fairly simple, and we will
# use this as an introduction to decision tree learning.

# NOTE: We will limit our implementation to categorical/factor variables
#		This is to keep the clarity and simplicity of this 
#		intellectual exercise. It can be easily extended to deal with
# 		continuous variables by implementing a discretization method
#		for continuous variables. A popular method is k-means classification.

# First, while not strictly necessary, we will create two R class types:
# Tree and Node for easier handling of the tree-structure.

# Tree:
# Tree will be used in a recursive structure that contians a either
# another tree or a node at each branch. Notice that we use a list
# for the branches, so that each tree can contain an arbitrary number
# of brances.
tree <- function(root, branches) {
	if (class(branches) == 'node') return(branches)

	new_tree <- list(branches)
	names(new_tree) <- root
	structure(new_tree, class='tree')
}
# Node:
# Node is the used for the terminal location in the trees. Each branch
# that contains a node will signify that the algorithm has either:
# 	1. Every element in the subset belongs to the same class
#	2. There are no more attributes to be selected
#	3. There are no more examples in the subset
node <- function(val) {
	structure(as.character(val), class='node')
}

# Entropy: H(S) - a measure of uncertainty in the set S
# H(S) = - sum(p(x) * log2(p(x)) for each subset x of S
entropy <- function(S) {
	if (!is.factor(S)) S <- as.factor(S)

	p <- prop.table(table(S))

	-sum(sapply(levels(S),
		function(name) p[name] * log2(p[name]))
	)
}

# ID3: 	The meat of the algorithm
#		Recursively builds a tree data structure that contians the 
#		decision tree
ID3 <- function(dataset, target_attr,
					attributes=setdiff(names(dataset), target_attr)) {
	# If there are no attributes left to classify with,
	# return the most common class left in the dataset
	# as a best approximation.
	if (length(attributes) <= 0) {
		# DEBUG: print("attributes ran out")
		return(node(most.frq(dataset[, target_attr])))
	}

	# If there is only one classification left, return a
	# node with that classification as the answer
	if (length(unique(dataset[, target_attr])) == 1) {
		# DEBUG: print('one class left')
		return(node(unique(dataset[, target_attr])[1]))
	}

	best_attr <- attributes[which.min(sapply(attributes, entropy))]
	rem_attrs <- setdiff(attributes, best_attr)
	split_dataset <- split(dataset, dataset[, best_attr])
	branches <- lapply(seq_along(split_dataset), function(i) {
			name <- names(split_dataset)[i]
			branch <- split_dataset[[i]]
			
			if (nrow(branch) == 0) node(most.frq(dataset[, target_attr]))
			else tree(root=name, branches=	ID3(branch[, union(target_attr,
																rem_attrs), drop=FALSE],
												target_attr,
												rem_attrs))
			})
	names(branches) <- names(split_dataset)
	id3_tree <- tree(root=best_attr, branches=branches)

	id3_tree
}

# Utility functions: REFACTOR ALL BELOW TO COMMON BASE
# Reports the most frequent factor
# Code taken from: http://stackoverflow.com/a/8189441/2985170
most.frq <- function(nbr.class, nbr.distance) {
	uniq <- unique(nbr.class)
	uniq[which.max(tabulate(match(nbr.class, uniq)))]
}