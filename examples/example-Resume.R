# Setup the MCMC
n.iter <- 5
SampleX <- function(x) x + 1
backing.path <- tempfile()
dir.create(backing.path)
x <- 0
interrupt.mcmc <- TRUE
Mcmc <- InitMcmc(n.iter, backing.path=backing.path)

# Interrupt the MCMC during the third iteration
try({
    samps <- Mcmc({
        x <- SampleX(x)
        if(x==3 && interrupt.mcmc) break
    })
}, silent=TRUE)

# The sampling is incomplete
samps <- LoadMcmc(backing.path)
samps$x[,]
rm(samps)

# Resume the MCMC
interrupt.mcmc <- FALSE
samps <- Resume(backing.path)

# All samples are available
samps$x[,]
