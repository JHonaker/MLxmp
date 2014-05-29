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
tree <- function(val, branch.list) {

}
# Node:
# Node is the used for the terminal location in the trees. Each branch
# that contains a node will signify that the algorithm has either:
# 	1. Every element in the subset belongs to the same class
#	2. There are no more attributes to be selected
#	3. There are no more examples in the subset
node <- function(val) {

}