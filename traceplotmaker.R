### Makes the traceplot in README.md
rm(list=ls())

library(overture)

set.seed(38299)

LogP <- function(x) dnorm(x, 0, 1, log=TRUE) # Target distribution

f <- function(x, s) {
    x.prop <- x + rnorm(1, 0, s)
    if(AcceptProposal(LogP(x), LogP(x.prop))) {
        x <- x.prop
    }

    return(x)
}

s.start <- 0.1
g <- Amwg(f, s.start, batch.size=25)

n.save <- 10000
Mcmc <- InitMcmc(n.save)
y <- 0
x <- 0
samples <- Mcmc({
    x <- g(x)
})

col1 <- rgb(255, 15, 133, maxColorValue=255)
col2 <- rgb(68, 204, 17, maxColorValue=255)
colfunc <- colorRampPalette(c(col1, col2))
png("traceplot.png", height=120, width=120*5)
    par(mar=c(0.1, 0.1, 0.1, 0.1))
    plot(1:n.save, samples$x, main="Traceplots", xlab="Iteration",
         ylab="Value", xaxt='n', yaxt='n', ann=FALSE, col=colfunc(n.save),
         pch=20, bty='n')
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1))
