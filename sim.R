# Create a function that will create a dataset from a specific baseline hazard function

set.seed(123123)

# N = sample size    
# lambda = scale parameter in exponential, weibull, "cox" (> 0)
# gamma = shape parameter in weibull (> 0)
# alpha = shape parameter in "cox" (-inf to inf)
# beta = fixed treatment effect parameter

generate_data = function(N, beta, distribution, lambda, gamma, alpha) {
  
  #Inverse survival function from baseline hazard functions to obtain event times
  exponential = function(u, x, lambda, beta) {
    time = -log(u) / (exp(x * beta) * lambda) }
  
  weibull = function(u, x, lambda, gamma, beta) {
    time = ( -log(u) / (exp(x * beta) * lambda) ) ^ (1 / gamma) }
  
  cox = function(u, x, lambda, alpha, beta) {
    time = (1/alpha) * log( 1 - (alpha * log(u) / (lambda * exp(beta * x))) ) }
  
  #treatment assignment
  x = 1 * (runif(10) < 0.5)
  
  #generate event times
  u = runif(N)
  
  if (distribution == "exponential") {
    time = exponential(u, x, lambda, beta) } 
  else if (distribution == "weibull") {
    time = weibull(u, x, lambda, gamma, beta) }
  else { 
    time = cox(u, x, lambda, alpha, beta) } 
  
  #censoring time
  C = rexp(N, lambda)
  
  # follow-up times and event indicators
  observed_time = pmin(time, C)
  status = 1 * (time <= C)
  
  # data set
  survival_data = data.frame(id = 1:N,
                             time = time,
                             status = status,
                             x = x)
}