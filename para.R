
# Packages ----------------------------------------------------------------

library(foreach)
library(doParallel)
library(iterators)


# Load function -----------------------------------------------------------

source("estimator.R")


# Sim setup ---------------------------------------------------------------

theta.true  = 3
sd          = 1
lambda.grid = c(0.1, 0.5, 0.8)
n.iter      = 10000


# Parallel stuff ----------------------------------------------------------

registerDoParallel(cores = detectCores())


# Main sim function -------------------------------------------------------

sim.fun = function(theta.true, x, lambda){
  t.ml = theta.ml(x)
  t.lam = theta.lam(x, lambda)
  out = c(t.ml, t.lam,
          mse(t.ml, theta.true),
          mse(t.lam, theta.true))
  return(out)
}

# Trying out --------------------------------------------------------------

sim.fun(theta.true, rnorm(1, mean = theta.true, sd = sd), lambda.grid[1])

# Basic for loop & timing -------------------------------------------------

time0 = Sys.time()

nopar.res = matrix(NA, nrow = n.iter, ncol = 4)
set.seed(123)

for(idx in 1:n.iter){
  x = rnorm(1, mean = theta.true, sd = sd)
  nopar.res[idx, ] = sim.fun(theta.true, x, lambda.grid[1])
}

time.nopar = Sys.time() - time0


# Parallel Loop -----------------------------------------------------------

time0 = Sys.time()

par1 = foreach(idx = 1:n.iter, 
              .combine = rbind) %dopar% {
                x = rnorm(1, mean = theta.true, sd = sd)
                sim.fun(theta.true, x, lambda.grid[1])
              } 

time.nopar = Sys.time() - time0
time.nopar
