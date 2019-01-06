\dontrun{
SampleSomething <- function() {
    Sys.sleep(0.3)
    rnorm(1)
}

backing.path <- "/some/backing/path"
slow.mcmc <- InitMcmc(1000, backing.path=backing.path)
slow.mcmc({
    x <- SampleSomething()
})

### In another process, while the MCMC is still running
backing.path <- "/some/backing/path"
samples.so.far <- Peek(backing.path)
samples.so.far$x[,]
}
