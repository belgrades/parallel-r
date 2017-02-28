theta.ml = function(x) x

theta.lam = function(x, lambda) lambda*x

mse = function(theta, theta.hat) (theta-theta.hat)^2



simulation= function(theta, sigma, n.iter, lambda){
  theta = 4
  sigma = 2
  n.iter = 1000
  lambda = 0.5
  x = rnorm(n.iter, mean = theta, sd = sigma)
  theta.ml = x
  theta.lambda = lambda*x
  mse.ml = sum((theta.ml - rep(theta, n.iter))^2)
  mse.lambda = sum((theta.lambda - rep(theta, n.iter))^2)
  
}
