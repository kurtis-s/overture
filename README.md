# overature
[![Build Status](https://travis-ci.org/kurtis-s/overature.svg?branch=master)](https://travis-ci.org/kurtis-s/overature)

`overature` makes writing Markov chain Monte Carlo (MCMC) samplers simpler.  With overature you can:

* **Write less code** `overature` eliminates boilerplate code, looping through sampling functions and saving the results automatically.
* **Easily recover from interruptions** Samples can be saved on-disk as the MCMC runs, so it's easy to resume an MCMC if something goes wrong.
* **Run more chains in parallel** Saving samples on-disk results in a dramatically smaller memory footprint for high-dimensional models, allowing more chains to be run when available RAM is limited.
* **Monitor chain progress** Completed samples can be viewed in another R process while the MCMC is still running.

## Usage
Using `overature` is easy:
#### 1. Write the sampling functions
```r
SamplerForX <- function(x) {
    x + 1
}

SamplerForY <- function(y) {
    y * y
}
```
#### 2.  Initialize the MCMC
```r
mcmc <- InitMcmc(3) # Run the chain for 3 iterations
```
#### 3.  Set initial values for the chain
```r
x <- c(0, 10) # Initial value for x
y <- 2 # Initial value for y
```
#### 4.  Run the MCMC
```r
samples <- mcmc({
    x <- SamplerForX(x)
    y <- SamplerForY(y)
})
```
#### 5.  Analyze the results
```r
> samples$x
     [,1] [,2]
[1,]    1   11
[2,]    2   12
[3,]    3   13
> samples$y
     [,1]
[1,]    4
[2,]   16
[3,]  256
```
More examples are given in the package documentation.

## Installation
After installing [devtools](https://github.com/r-lib/devtools) run:
```r
library(devtools)
install_github("kurtis-s/overature")
```
