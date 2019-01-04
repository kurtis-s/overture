# Run a file-backed MCMC
backing.path <- tempfile()
dir.create(backing.path)
mcmc <- InitMcmc(1000, backing.path=backing.path)
samples <- mcmc({
    x <- rnorm(1)
})
rm(samples)

# Load the saved samples
loaded.samples <- LoadMcmc(backing.path)
hist(loaded.samples$x[,], main="Samples", xlab="x")
