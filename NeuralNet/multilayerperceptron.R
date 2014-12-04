source('../utility.R')

layer = function(inputs = 2, nodes = 3) {
	weights = matrix(runif((inputs + 1) * nodes),
		ncol = inputs + 1,
		nrow = nodes)
	class(weights) <- "nnetlayer"

	weights
}

nnet <- function(inputs, hiddenlayers, hiddennodes, outputs) {
	layers <- list()

	layers[[1]] <- layer(inputs, hiddennodes)
	if (hiddenlayers != 0) {
		for (i in 1:hiddenlayers) {
			layers[[i + 1]] <- layer(hiddennodes, hiddennodes)
		}
	}
	layers[[hiddenlayers + 2]] <- layer(hiddennodes, outputs)

	class(layers) <- "nnet"

	layers
}

predict.nnet = function(nnet, inputs, activation.fun = sigmoid) {
	feedforward = function(input, network) {
		if (length(network) == 0) input
		else feedforward(network[[1]] %*% activation.fun(c(1, input)), network[-1])
	}

	feedforward(inputs, nnet)
}

backprop = function(nnet, inputs, outputs) {

}