# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# Team Member Contributions
# Ziyi Wang        Q1-3
# Aishah ALmubarak Q4-5
# Yizhen Ge        Q6-7


# Purpose of this script:
#estimate changepoints in poisson count data using a smoothed lasso penalty
#the model allows the mean count to change at unknown time points
#the penalty controls changes in the mean function

# What the script does:
#simulate poisson counts with two known changepoints
#define the objective function, gradient and hessian
#check the derivatives using finite differences
#fit the model using bfgs and compare fitted means with true means

#step1
#simulate 100 independent poisson counts with two known changepoints
#the mean changes from 10 to 2 at time 30 and from 2 to 7 at time 70
#the simulated counts are used to test the estimation method

set.seed(12345)
n <- 100
time <- 1:n
tau_true <- c(1, 30, 70, n+1)
m_true <- length(tau_true)-2
theta_true <- c(10, 2, 7)

#assign a constant poisson mean to each segment
mu_true <- numeric(n)
for(i in seq_along(theta_true)){
  mu_true[tau_true[i]:(tau_true[i+1]-1)] <- theta_true[i]
}

#generate random counts using the true mean function
y <- rpois(n, lambda=mu_true)

#show the simulated counts, true means and changepoint positions
plot(time, y, pch=16, col="grey",
     xlab="Time", ylab="Count",
     main="Simulated Poisson changepoint data")
lines(time, mu_true, col="black", lwd=2, type="s")
abline(v=tau_true[-c(1,length(tau_true))], lty="dashed")

legend("topright",
       legend=c("Observed counts", "True mean", "Changepoints"),
       col=c("grey", "black", "black"),
       pch=c(16, NA, NA),
       lty=c(NA, 1, 2),
       bty="n")


#STEP2
#calculate the smoothed lasso-penalised negative log-likelihood

#beta contains the initial log mean and subsequent changes in log mean
#beta[1] is the initial log mean, and beta[k] is the change at time k
#zero changes indicate that the mean remains constant

#y contains the observed poisson counts

#lambda controls the penalty strength and eps controls smoothing

#return a single objective value to be minimised

poission_lasso_objective <- function(beta, y, lambda=1, eps=1e-6){
  
  #recover positive poisson means from cumulative log changes
  eta <- cumsum(beta)
  mu <- exp(eta)
  
  #measure how well the model explains the observed counts
  neg_loglik <- -sum(dpois(y, lambda=mu, log=TRUE))
  
  #penalise changes in log mean, excluding the initial level
  penalty <- lambda*sum(sqrt(beta[-1]^2+eps))
  
  #combine data fit and penalty into one objective value
  objective <- neg_loglik+penalty
  
  return(objective)
}


#calculate the gradient of the penalised objective function
#use the same inputs as the objective function
#return one first derivative for each element of beta

poission_lasso_grad <- function(beta, y, lambda=1, eps=1e-6){
  
  #recover the poisson means from the current parameters
  eta <- cumsum(beta)
  mu <- exp(eta)
  
  #each beta affects its own and all later means
  #sum residuals backwards to obtain the likelihood gradient
  grad_loglik <- rev(cumsum(rev(mu-y)))
  
  #calculate the derivative of the smoothed penalty
  #the initial log mean is not penalised
  grad_penalty <- numeric(length(beta))
  grad_penalty[-1] <- lambda*beta[-1]/sqrt(beta[-1]^2+eps)
  
  #combine the likelihood and penalty gradients
  grad <- grad_loglik+grad_penalty
  
  return(grad)
}


#calculate the hessian of the penalised objective function
#use the same inputs as the objective function
#return a square matrix of second derivatives with respect to beta

poission_lasso_hess <- function(beta, y, lambda=1, eps=1e-6){
  
  #recover the poisson means from the current parameters
  n <- length(beta)
  eta <- cumsum(beta)
  mu <- exp(eta)
  
  #construct a matrix linking changes in log mean to each time point
  #each row includes the initial level and all earlier changes
  X <- matrix(0, n, n)
  X[lower.tri(X, diag=TRUE)] <- 1
  
  #calculate the curvature of the negative log-likelihood
  hess_loglik <- t(X) %*% (mu*X)
  
  #calculate the second derivatives of the smoothed penalty
  #the penalty contributes only to the diagonal
  hess_penalty <- numeric(n)
  hess_penalty[-1] <- lambda*eps/(beta[-1]^2+eps)^(3/2)
  
  #combine both parts into the full hessian matrix
  hess <- hess_loglik+diag(hess_penalty)
  
  return(hess)
}


#check the analytical gradient using finite differences
#change each beta slightly and compare objective values
#the numerical and analytical gradients should be close

beta_test <- c(log(mean(y)), rep(0, n-1))
step <- 1e-5
grad_fd <- numeric(n)

for(i in 1:n){
  beta1 <- beta_test
  beta2 <- beta_test
  
  beta1[i] <- beta1[i]+step
  beta2[i] <- beta2[i]-step
  
  objective1 <- poission_lasso_objective(beta1, y)
  objective2 <- poission_lasso_objective(beta2, y)
  
  grad_fd[i] <- (objective1-objective2)/(2*step)
}


#report the largest difference between the two gradients
max(abs(grad_fd-poission_lasso_grad(beta_test, y)))

#check the analytical hessian using finite differences
#change each beta and measure the change in the gradient
#the numerical and analytical hessians should be close

grad0 <- poission_lasso_grad(beta_test, y)
hess_fd <- matrix(0, n, n)

for(i in 1:n){
  beta1 <- beta_test
  beta2 <- beta_test
  
  beta1[i] <- beta1[i]+step
  beta2[i] <- beta2[i]-step
  
  grad1 <- poission_lasso_grad(beta1, y)
  grad2 <- poission_lasso_grad(beta2, y)
  
  hess_fd[i,] <- (grad1-grad2)/(2*step)
}

#report the largest difference between the two hessians
max(abs(hess_fd-poission_lasso_hess(beta_test, y)))



#step3
#fit the poisson changepoint model using bfgs
#start with a constant mean equal to the observed sample mean
#use lambda=1 as an initial penalty value

beta_init <- c(log(mean(y)), rep(0, n-1))

fit <- optim(
  par=beta_init,
  fn=poission_lasso_objective,
  gr=poission_lasso_grad,
  y=y,
  lambda=1,
  method="BFGS",
  control=list(maxit=10000, reltol=1e-9)
)

#check whether the optimisation converged successfully
#convergence code 0 indicates normal termination
fit$convergence

if(fit$convergence != 0){
  stop("Optimisation did not converge")
}

#recover the fitted mean function from the estimated parameters
beta_hat <- fit$par
mu_hat <- exp(cumsum(beta_hat))

#compare the estimated means with the known true means
#check whether the fitted model captures the main changes in the data
plot(time, y, pch=16, col="grey",
     xlab="Time", ylab="Count",
     main="Observed data and fitted mean")

lines(time, mu_true, col="black", lwd=2, type="s")
lines(time, mu_hat, col="red", lwd=2, type="s")

legend("topright",
       legend=c("Observed counts", "True mean", "Fitted mean"),
       col=c("grey", "black", "red"),
       pch=c(16, NA, NA),
       lty=c(NA, 1, 1),
       bty="n")
