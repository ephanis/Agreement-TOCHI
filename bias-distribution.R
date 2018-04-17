
#===========================================================================
#===========================================================================
# Script used to plot theoretical bias distribution functions
#
# Theophanis Tsandilas
#===========================================================================
#===========================================================================

rm(list=ls()) # Clean up R's memory

library("zipfR") # Check http://www.r-bloggers.com/the-zipf-and-zipf-mandelbrot-distributions/
#source("agreement.CI.R")
source("agreement.coefficients.R")


emptyframe <- function(nrows, ncols, colprefix) {
    as.data.frame(setNames(replicate(ncols,character(nrows), simplify = F), paste(colprefix, seq(1:ncols), sep="")))
}

# Create a population from a Zipf distribution
zipf <- function(N, beta){
    ZM <- lnre("zm", alpha = 1/2, B = beta)
    zmsample <- rlnre(ZM, n = N)
    
    zmsample
}

# Create a population of size N from a normal frequency distribution with mean = 1 and std. dev = sd
# P is the size of the original population - any large enough value will suffice
normal <- function(N, sd, P = 10000){
    sample <- sample(c(1:P), N, replace = TRUE, prob=dnorm(mean=1,sd=sd,c(1:P)))
    
    sample
}


zipf.plot <- function(beta, nsamples, N, ymax = .8, color = rgb(0.3, 0.3, 0.8)){
    sample <- zipf(nsamples, beta)
    
    freq <- table(sample)
    total <- sum(freq)
    
    freq <- head(freq, n = N) / total
    
    plot(freq, xlim = c(1, N), ylim=c(0, ymax), col = color, cex=1,  lwd=1, xaxt='n', yaxt='n', ann=FALSE, bty="n",  type = "o")
    
    freq
}


normal.plot <- function(sd, nsamples, N, ymax = .8, color = rgb(0.3, 0.3, 0.8)){
    sample <- normal(nsamples, sd)
    
    freq <- table(sample)
    total <- sum(freq)
    
    freq <- head(freq, n = N) / total
    
    plot(freq, xlim = c(1, N), ylim=c(0, ymax), col = color, cex=1,  lwd=1, xaxt='n', yaxt='n', ann=FALSE, bty="n", type = "o",
    ylab = "Probability")
    
    freq
}



plot.distribution <- function(agr.samples, N = 40, min = 0, max = 1.0, axis = TRUE){
    binsize = 1 / N
    
    samples <- table(seq(from = min, to = max, by = binsize)) * 0
    
    agr.samples <- round(N * agr.samples)/N
    freq <- table(unlist(agr.samples))
    # names(freq) <- as.numeric(names(freq)) / N
    
    samples[names(freq)] <- freq[names(freq)]
    
    samples <- t(samples / sum(samples))
    
    #points <- barplot(samples, col = rgb(.8,.8,.95), ylim=range(c(0, 1)), axisnames = FALSE, axes = FALSE)
    
    points <- barplot(samples, col = rgb(.93,.8,.5), ylim=range(c(0, 1)), axisnames = FALSE, axes = FALSE, add = TRUE)
    
    #####
    size <- length(points)
    filtered <- points[seq(1, size, size/(10*(max-min)))]
    
    filtered <- append(filtered, points[size])
    
    axis(1, at = filtered, labels = seq(min, max, .1))
    
    filtered <- filtered + (filtered[2] - filtered[1]) /2
    axis(1, at = filtered, labels = NA, tck = -0.02)
    
    if(axis){
        axis(2, at=pretty(seq(0, 1, 0.5)), lab=pretty(seq(.1, .9, by = .1))*100, las=1)
    }
    #####
    
    samples
}


plot.distribution_ <- function(agr.samples, N = 40, min = 0, max = 1.0, axis = TRUE){
    binsize = 1 / N

    samples <- table(seq(from = min, to = max, by = binsize)) * 0
    
    agr.samples <- round(N * agr.samples)/N
    freq <- table(unlist(agr.samples))
    # names(freq) <- as.numeric(names(freq)) / N
    
    samples[names(freq)] <- freq[names(freq)]
    
    samples <- t(samples / sum(samples))
    
    par(lwd = .2)
    points <- barplot(samples, col = rgb(.86,.86,.95), ylim=range(c(0, 1)), axisnames = FALSE, axes = FALSE)
    
    #####
    size <- length(points)
    filtered <- points[seq(1, size, size/(10*(max-min)))]
    
    filtered <- append(filtered, points[size])
    
    #axis(1, at = filtered, labels = seq(min, max, .1))
    
    filtered <- filtered + (filtered[2] - filtered[1]) /2
    #axis(1, at = filtered, labels = NA, tck = -0.02)
    
    #if(axis){
    #    axis(2, at=pretty(seq(0, 1, 0.5)), lab=pretty(seq(.1, .9, by = .1))*100, las=1)
    #}
    #####
    
    samples
}


simulation <- function(values, nparticipants, nreferents, distr, par, f){
    data <- emptyframe(nreferents, nparticipants, "P")
    
    for(i in 1:nparticipants){
        data[i] <- distr(nreferents, par)
    }
    
    c(values, f(data))
}

