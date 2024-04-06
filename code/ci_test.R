sqrtinv <- function(mA) {
  ei <- eigen(mA)
  d <- ei$values
  d <- (d + abs(d)) / 2
  d2 <- 1 / sqrt(d)
  d2[d == 0] <- 0
  return(ei$vectors %*% diag(d2) %*% t(ei$vectors))
}

pred2res <- function( pred ){
	setNames( c( 0, cumsum( pred )[-length(pred)]) - 
		c( rev( cumsum( rev( pred ) ) ), 0 )[-1], names(pred) )
}

p2r <- function( x, preddf ){
	present.levels <- colnames( preddf )
	missing.levels <-  setdiff( levels( x ), present.levels )
	if( length( missing.levels ) > 0 ){
		for( xx in missing.levels ){
			preddf <- cbind( preddf, 0 )
		}
		colnames( preddf ) <- c( present.levels, missing.levels )
		preddf <- preddf[ , levels( x ) ]
	}
	as.matrix( apply( preddf, 1, pred2res ), rownames=colnames(preddf) )[
			cbind( x, seq_along( x ) )
		]
}

residual.test <- function(x, y, x_parents, y_parents, predictor, test) {
  # Random Forest test v1. Return the p-value for the independence
  # test X _|_ Y | Z
  #
  # @param x: vector
  # @param y: vector
  # @param z: vector or data.frame
  # @param predictor: 'glm' or 'rf'
  # @param test: 'q3' or 'pillai'

  n <- length(x)

  # Step 1: Determine the variable type of x
  if (all(class(x) == "factor")){
    x <- droplevels(x)
    k <- length(levels(x))

    stopifnot(k > 1)
    tx <- table(seq_len(n), x)[, -k, drop = FALSE]

    x_type <- "cat"
  }
  else if(all(class(x) == c("ordered", "factor"))){
    x <- droplevels(x)
    k <- 1 
    tx <- x

    x_type <- "ord"
  }
  else{
    k <- 1
    tx <- x

    x_type <- "cont"
  }

  # Step 2: Determine the variable type of y
  if (all(class(y) == "factor")){
    y <- droplevels(y)
    r <- length(levels(y))

    stopifnot(r > 1)
    ty <- table(seq_len(n), y)[, -r, drop = FALSE]

    y_type <- "cat"
  }
  else if(all(class(y) == c("ordered", "factor"))){
    y <- droplevels(y)
    r <- 1 
    ty <- y

    y_type <- "ord"
  }
  else{
    r <- 1
    ty <- y

    y_type <- "cont"
  }

  # Step 3: Train an estimator x~z
  if (any(x_type == c("ord", "cat"))){
    x_prob <- TRUE
  }
  else{
    x_prob <- FALSE
  }

  if (predictor == 'rf'){
      forest.x <- ranger::ranger(as.formula(paste("x ~", paste(colnames(z), collapse = "+"))),
        data.frame(x, x_parents),
        num.trees = 50,
        probability = x_prob
      )
      predict.x <- predict(forest.x, data = data.frame(x_parents))$predictions
  }
  else if (predictor == 'glm'){
      if (x_type == 'cont'){
	      lm.x <- lm(as.formula(paste("x ~", paste(colnames(x_parents), collapse = "+"))), data=data.frame(x, z))
	      predict.x <- predict(lm.x, data.frame(x_parents))
      }
      else if (x_type == 'ord'){
	      # prop.x <- VGAM::vglm(as.formula(paste("x ~", paste(colnames(z), collapse="+"))), data=data.frame(x, z), family=VGAM::propodds, model=TRUE)
    	      # predict.x <- as.data.frame(predict(prop.x, newdata=data.frame(z), type='response'))
	      prop.x <- ordinal::clm(as.formula(paste("x ~", paste(colnames(x_parents), collapse="+"))), data=data.frame(x, x_parents))
	      predict.x <- predict(prop.x, data.frame(x_parents))$fit
      }
      else{
      	      glm.x <- nnet::multinom(as.formula(paste("x ~", paste(colnames(x_parents), collapse="+"))), data=data.frame(x, x_parents), trace=F)
      	      predict.x <- as.data.frame(predict(glm.x, data.frame(x_parents), type='probs'))
      	      if (ncol(predict.x) == 1){
      	              predict.x = 1-predict.x
      	      }
      }
  }

  # Step 4: Train an estimator y~z
  if (any(y_type == c("ord", "cat"))){
    y_prob <- TRUE
  }
  else{
    y_prob <- FALSE
  }

  if (predictor == 'rf'){
      # predict.y <- matrix(0, ncol = k, nrow = n, dimnames = list(NULL, levels(x)))
      forest.y <- ranger::ranger(as.formula(paste("y ~", paste(colnames(y_parents), collapse = "+"))),
        data.frame(y, y_parents),
        num.trees = 50, probability = y_prob
      )
      predict.y <- predict(forest.y, data = data.frame(y_parents))$predictions

  }
  else if (predictor == 'glm'){
      if (y_type == 'cont'){
	      lm.y <- lm(as.formula(paste("y ~", paste(colnames(y_parents), collapse = "+"))), data=data.frame(y, y_parents))
	      predict.y <- predict(lm.y, data.frame(y_parents))
      }
      else if (y_type == 'ord'){
	      prop.y <- ordinal::clm(as.formula(paste("y ~", paste(colnames(y_parents), collapse="+"))), data=data.frame(y, y_parents))
	      predict.y <- predict(prop.y, data.frame(y_parents))$fit
	      # prop.y <- VGAM::vglm(as.formula(paste("y ~", paste(colnames(z), collapse="+"))), data=data.frame(y, z), family=VGAM::propodds, model=TRUE)
    	      # predict.y <- as.data.frame(predict(prop.y, newdata=data.frame(z), type='response'))
      }
      else{
      	      glm.y <- nnet::multinom(as.formula(paste("y ~", paste(colnames(y_parents), collapse="+"))), data=data.frame(y, y_parents), trace=F)
      	      predict.y <- as.data.frame(predict(glm.y, data.frame(y_parents), type='probs'))
      	      if (ncol(predict.y) == 1){
      	              predict.y = 1-predict.y
      	      }
      }
  }

  # Step 5: Compute the residuals.
  if (x_type == 'cont'){
	Rx <- as.matrix(x) - as.matrix(predict.x)
  }
  else if (x_type == 'ord'){
	Rx <- as.matrix(p2r(x, predict.x))
  }
  else if (x_type == 'cat'){
  	Rx <- matrix(0, nrow=n, ncol = (k-1))
  	for (i in 1:(k-1)){
  	      Rx[, i] <- (tx[, i] - predict.x[, i])
  	}
  }
  if (y_type == 'cont'){
	Ry <- as.matrix(y) - as.matrix(predict.y)
  }
  else if(y_type == 'ord'){
	Ry <- as.matrix(p2r(y, predict.y))
  }
  else if (y_type == 'cat'){
  	Ry <- matrix(0, nrow=n, ncol = (r-1))
  	for (i in 1:(r-1)){
  		Ry[, i] <- (ty[, i] - predict.y[, i])
  	}
  }

  # Step 6: Compute the test statistics
  can <- cancor(Rx, Ry)
  return(prod(can$cor))
}

likelihood.test <- function(x, y, z, dataset){
    d <- dataset %>% dplyr::select(-c(x))
    if (is.numeric(dataset[, x])){
      res <- MXM::testIndMMReg(
		target=dataset[, x],
		dataset=d,
		xIndex=which(y == colnames(d)),
		csIndex=as.vector(sapply(z, function(t){which(t == colnames(d))}))
		)
      return(10^res$pvalue)
    }
    else if (is.ordered(dataset[, x])){
      res <- MXM::testIndOrdinal(
		target=dataset[, x],
		dataset=d,
		xIndex=which(y == colnames(d)),
		csIndex=as.vector(sapply(z, function(t){which(t == colnames(d))})),
		)
      return(10^res$pvalue)
    }
    else{
      	res <- MXM::testIndMultinom(
		target=dataset[, x],
		dataset=d,
		xIndex=which(y == colnames(d)),
		csIndex=as.vector(sapply(z, function(t){which(t == colnames(d))}))
		)
      return(10^res$pvalue)
    }
}

ci.test <- function(x, y, z, d, test){
	if (test == 'glm_pillai'){
		return(residual.test(d[, x], d[, y], d[z], 'glm', 'pillai'))
	}
	else if (test == 'rf_pillai'){
		return(residual.test(d[, x], d[, y], d[z], 'rf', 'pillai'))
	}
	else if (test == 'glm_q3'){
		return(residual.test(d[, x], d[, y], d[z], 'glm', 'q3'))
	}
	else if (test == 'rf_q3'){
		return(residual.test(d[, x], d[, y], d[z], 'rf', 'q3'))
	}
	else if (test == 'rf_arht'){
		return(residual.test(d[, x], d[, y], d[z], 'rf', 'q3_arht'))
	}
	else if (test == 'mxm'){
		return(likelihood.test(x, y, z, d))
	}
	else if (test == 'mc_mi'){
		return(bnlearn::ci.test(x, y, z, data=d, test='mc-mi')$p.value)
	}
	else if (test == 'mi_cg'){
		return(bnlearn::ci.test(x, y, z, data=d, test='mi-cg')$p.value)
	}
	else if (test == 'mc_mi_g'){
		return(bnlearn::ci.test(x, y, z, data=d, test='mc-mi-g')$p.value)
	}
	else if (test == 'chi_sq'){
		return(bnlearn::ci.test(x, y, z, data=d, test='mi')$p.value)
	}
}

cond_effects <- function(x, y, z, d){
	return(residual.test(d[, x], d[, y], d[z], 'glm', 'pillai'))
}
