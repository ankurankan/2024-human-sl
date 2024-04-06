library(dagitty)
library(dplyr)


compute_multinom_p <- function(y_values) {
    y_exp <- apply(y_values, exp, MARGIN = 2)
    y_exp <- cbind(1, y_exp)
    den <- 1 + rowSums(y_exp)
    p <- c()
    for (i in 1:ncol(y_exp)) {
        p <- cbind(p, y_exp[, i] / den)
    }
    return(p)
}

# Iterates over topological ordering and generates data with fixed effect.
mixed_data_gen_multinom <- function(n = 1000, dag = dagitty("dag{X <- Z -> Y}"), var_types = NA, effect = 0.2, k = 4, error_var = 1) {
    # Step 1: Compute the topological ordering
    topo_order <- topologicalOrdering(dag)
    # Step 2: If var_types is not provided, generate random ones.
    if (any(is.na(var_types))) {
        var_types <- list()
        for (var in names(topo_order)) {
            var_types[[var]] <- sample(c("cont", "cat", "ord"), 1)
        }
    }
    var_order <- vector(length = length(topo_order))

    disconnected_count <- 0
    for (var in names(topo_order)) {
        if (topo_order[[var]] == 0) {
            disconnected_count <- disconnected_count + 1
            var_order[disconnected_count] <- var
        } else {
            var_order[topo_order[[var]]] <- var
        }
    }

    data <- as.data.frame(matrix(nrow = n, ncol = length(var_order)))
    colnames(data) <- var_order

    # Step 2: Iterate over topological order and generate data.
    for (var in var_order) {
        var_parents <- dagitty::parents(dag, var)
        # Step 2.1: If var has no parents generate random data.
        if (length(var_parents) == 0) {
            if (var_types[var] == "cont") {
                data[, var] <- rnorm(n, 0, error_var)
            } else if (var_types[var] == "cat") {
                data[, var] <- factor(sample(1:k, n, replace = T), ordered = F)
            } else if (var_types[var] == "ord") {
                data[, var] <- factor(sample(1:k, n, replace = T), ordered = T)
            }
        }
        # Step 2.2: If they have parents, use parents data to generate values using linear model.
        else {
	    # Step 2.2.1: Define the error for each variable type.
            if ((var_types[var] == "cont") | (var_types[var] == "ord")) {
                effect_rows <- 1
                values <- rnorm(n, 0, error_var)
            } else {
                effect_rows <- k - 1
                values <- matrix(rnorm(n * (k - 1), 0, error_var), nrow = n)
            }

	    # Step 2.2.2: Preprocess the covariates. If discrete, dummy encode them using
	    #             bipolar encoding, continuous and ordinal stay as they are.
            type_parents <- var_types[var_parents]
            cat_parents <- type_parents[type_parents == "cat"]
            if ((length(cat_parents)) != 0) {
                for (pa in names(cat_parents)) {
            	    effect_matrix <- matrix(runif((k - 1) * effect_rows, min = -0.5, max = 0.5), ncol = k - 1, nrow = effect_rows)
                    tryCatch(
                        {
                            bipolar <- table(seq_len(n), data[, pa])
                            bipolar[bipolar == 0] <- -1
                            # values <- values + table(seq_len(n), data[, pa])[, -k, drop=F] %*% t(effect_matrix)
                            values <- values + bipolar[, -k, drop = F] %*% t(effect_matrix)
                        },
                        error = function(x) {
                            print("One of the parents doesn't have k categories.")
                            ncat <- ncol(bipolar[, -k, drop = F])
                            values <- values + bipolar[, -k, drop = F] %*% t(effect_matrix)[1:ncat, ]
                        }
                    )
                }
            }
            rest_parents <- var_parents[!var_parents %in% names(cat_parents)]
            if (length(rest_parents) > 0) {
                values <- values + apply(data[, rest_parents, drop = F], MARGIN = 2, FUN = as.numeric) %*% t(matrix(runif(effect_rows * length(rest_parents), min = -0.5, max = 0.5), nrow = effect_rows, ncol = length(rest_parents)))
            }

	    # values <- (values^2)
	    # values <- values/sd(values)

	    # Step 2.2.3: Postprocess the values depending on the target variable type.
	    # 	          For discrete variables, apply a multinomial model, for ordinal
	    #             variable a proportional log-odd model, and continuous remains 
	    #             the same.
            if (var_types[var] == "cont") {
                data[, var] <- values
            } else if (var_types[var] == "cat") {
                data[, var] <- factor(apply(compute_multinom_p(values), MARGIN = 1, function(pr) {
                    sample(1:k, size = 1, prob = pr)
                }), ordered = F)
            } else if (var_types[var] == "ord") {
                intercept <- as.matrix(seq(1, -1, length.out = k - 1))
                # values <- values / max(values)
                logodds <- apply(intercept, MARGIN = 1, function(x) x + values)
                cum_probs <- as.data.frame(cbind(1, apply(logodds, MARGIN = 2, function(x) exp(x) / (1 + exp(x)))))
                probs <- as.data.frame(cbind(cum_probs[-ncol(cum_probs)] - cum_probs[-1], cum_probs[ncol(cum_probs)]))
                data[, var] <- factor(apply(probs, MARGIN = 1, function(pr) sample(1:k, size = 1, prob = pr)), ordered = T)
            }
        }
    }
    
    # Step 3: Return the generated data along with the variable types.
    return(list(d = data, var_types = var_types))
}
