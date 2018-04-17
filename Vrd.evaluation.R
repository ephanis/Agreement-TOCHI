#===========================================================================================
# Evaluation of the Type I Error of the Vrd test (Vatavu & Wobbrock, 2015)
# Zipfian distributions
# Author: Theophanis Tsandilas
#======================================================================================

source("Vrd.R")

################################################################################################
################################################################################################
# Code for simulating the creation of pairs of samples
# (comparison between agreement rates for two only referents) with specific AR levels
################################################################################################
################################################################################################

# It requires the zipfR package for the Zip-Mandelbrot distribution
#library("zipfR") # Check http://www.r-bloggers.com/the-zipf-and-zipf-mandelbrot-distributions/

# init: errors <- rep(0, L)
typeI.simulation <- function(errors, createFunction, N, params, alpha = .05){
    L <- length(errors)
    
    # 1st random sample of size N (1st referent)
    samples1 <- mapply(createFunction, rep(N, L), params)
    
    # 2nd random sample of size N (2nd referent)
    samples2 <- mapply(createFunction, rep(N, L), params)
    
    for(i in 1:L){
        mat <- cbind(samples1[, i], samples2[, i])
        pairs <- agreementPairs(mat)
        
        # Uncomment the following line (and comment the previous one) to apply Cochran's Q test on n - 1 independent pairs of participants as exaplained in the article
        #pairs <- agreementPairs.independent(mat)
        
        pvalue <- p.vrd.vatavu(pairs)
        
        if(pvalue < alpha) errors[i] <- errors[i] + 1
        
        #tryCatch({
        #    test <- vrd(pairs)
            
        #    if(pvalue(test) < alpha) errors[i] <- errors[i] + 1
        #}, error = function(e) 1)
        
    }
    
    errors
}


typeI.simulation.random <- function(errors, createFunction, N, params, alpha = .05){
    L <- length(errors)
    
    # 1st random sample of size N (1st referent)
    samples1 <- mapply(createFunction, rep(N, L), params)
    
    # 2nd random sample of size N (2nd referent)
    samples2 <- mapply(createFunction, rep(N, L), params)
    
    for(i in 1:L){
        mat <- cbind(samples1[, i], samples2[, i])
        pairs <- agreementPairs(mat)
        
        # Uncomment the following line (and comment the previous one) to apply Cochran's Q test on n - 1 independent pairs of participants as exaplained in the article
        #pairs <- agreementPairs.independent(mat)
        print(sum(pairs[,1]))
        
        if(sum(pairs[,1]) != sum(pairs[,2])){
            if(runif(1) > .5) errors[i] <- errors[i] + 1
            
        }
        
        #tryCatch({
        #    test <- vrd(pairs)
        
        #    if(pvalue(test) < alpha) errors[i] <- errors[i] + 1
        #}, error = function(e) 1)
        
    }
    
    errors
}


to.percent <- function(x){
    sprintf("%.0f", 100*x)
}


plot.error <- function(errors, labels, alpha =.05, axis = TRUE){
    x <- barplot(errors, names = labels, ylim = c(0,1), axes = FALSE, col = rgb(.93,.8,.5))
    
    segments(0, alpha, x[length(x)] + .75, alpha, col = "red")
    text(x[length(x)], alpha + .06, paste0(alpha*100, "%"), col = "red")
    
    y <- as.matrix(errors)
    text(x, errors + .06, labels=to.percent(y), col = "black")

    if(axis){
        axis(2, at=pretty(seq(0, 1, 0.5)), lab=pretty(seq(.1, .9, by = .1))*100, las=1)
    }
}

