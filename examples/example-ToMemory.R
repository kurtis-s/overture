# Run a file-backed MCMC
backing.path <- tempfile()
dir.create(backing.path)
mcmc <- InitMcmc(1000, backing.path=backing.path)
samples <- mcmc({
    x <- rnorm(1)
    y <- rnorm(2)
})

# Convert to standard in-memory R matrices
samples.in.memory <- ToMemory(samples)

is.matrix(samples.in.memory$x)
is.matrix(samples.in.memory$y)
bigmemory::is.big.matrix(samples.in.memory$x)
bigmemory::is.big.matrix(samples.in.memory$y)
