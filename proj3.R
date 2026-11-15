# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# Team Member Contributions
# Ziyi Wang        Q1-3
# Aishah ALmubarak Q4-5
# Yizhen Ge        Q6-7


## Purpose of this script:
# estimate changepoints in poisson count data using a smoothed lasso
# penalty. the mean count is modelled as a baseline plus a change at
# each time step, on the log scale; the penalty shrinks most changes
# to (near) zero so that only genuine changepoints remain.

# What the script does:
# 1. simulate 100 poisson counts with two known changepoints
# 2. define the penalised objective, its gradient and hessian, and
#    check the derivatives by finite differencing
# 3. fit the model by bfgs at lambda = 1 and compare with the truth
# 4. choose lambda by minimising the modified bic over a grid, using
#    the effective degrees of freedom to count changepoints
# 5. refit at the best lambda, estimate the number of changepoints
#    and locate them at the largest changes, and plot the fit
# 6. assess uncertainty by parametric bootstrap: resimulate from the
#    fitted mean, refit B = 200 times, and bias-correct the mean and
#    its 95% confidence bands
# 7. produce a final plot of the data, true mean, fitted mean, the
#    bias-corrected mean and its 95% confidence limits

#STEP 1
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


#STEP 2
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

# retest at a point with non-zero changes so the penalty terms are
# exercised too. 
beta_test2 <- c(log(mean(y)), 0.3*sin(1:(n-1)))

grad_fd2 <- numeric(n)
for(i in 1:n){
  beta1 <- beta_test2
  beta2 <- beta_test2
  beta1[i] <- beta1[i]+step
  beta2[i] <- beta2[i]-step
  grad_fd2[i] <- (poission_lasso_objective(beta1, y) -
                    poission_lasso_objective(beta2, y))/(2*step)
}
# largest gradient difference at a non-zero beta (should be ~0)
max(abs(grad_fd2-poission_lasso_grad(beta_test2, y)))

hess_fd2 <- matrix(0, n, n)
for(i in 1:n){
  beta1 <- beta_test2
  beta2 <- beta_test2
  beta1[i] <- beta1[i]+step
  beta2[i] <- beta2[i]-step
  hess_fd2[i,] <- (poission_lasso_grad(beta1, y) -
                     poission_lasso_grad(beta2, y))/(2*step)
}
# largest hessian difference is small next to the entries (hundreds/1000s)
max(abs(hess_fd2-poission_lasso_hess(beta_test2, y)))


#STEP 3
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
# with lambda = 1 the penalty is too weak, so the fitted mean zig-zags
# and suggests changes at many time points. Noticeably, it broadly follows
# the time varying trend in the data: high up to ~30, low from ~30 to ~70, then
# medium, which confirms the model and optimiser are working.

# STEP 4: 
# choosing the penalty lambda by mBIC
# lambda = 1 in step 3 was arbitrary, but lambda sets the price of every
# jump and it decides how many changepoints the model finds. We choose it
# by minimizing mbic = 3*m*log(n) - 2*loglik over a grid of log lambda:
# (-1, 5, by = 0.05).
# Since the smoothed penalty never sets a beta exactly to zero, the number
# of changepoints is measured by edf = trace(H_lambda^{-1} H_0) - 1.

fit_lasso <- function(beta0, y, lambda) {
  # fit the model at one lambda by BFGS, using the analytic gradient.
  # y and lambda are passed on to fn and gr via optim's '...' argument.
  fit <- optim(beta0, poission_lasso_objective, gr=poission_lasso_grad,
               y=y, lambda=lambda, method="BFGS",
               control=list(maxit=10000, reltol=1e-9))
  g <- poission_lasso_grad(fit$par, y, lambda=lambda) # grad at optimum
  H <- poission_lasso_hess(fit$par, y, lambda=lambda) # Hessian at optimum
  # conditions for a minimum: gradient 'close enough' to zero, and
  # Hessian positive definite (tested by whether a Cholesky works)
  R <- try(chol(H), silent=TRUE)
  list(beta=fit$par, conv=fit$convergence, counts=fit$counts,
       grad=max(abs(g))/lambda, # gradient relative to the penalty pull
       pd=!inherits(R, "try-error"), H=H, R=R)
} # fit_lasso

edf_lasso <- function(fit, y) {
  # effective degrees of freedom, trace(H_lambda^{-1} H_0), at a fitted beta.
  # H_lambda^{-1} is formed from its Cholesky factor, as chol2inv(chol(H)).
  H_0 <- poission_lasso_hess(fit$beta, y, lambda=0) # likelihood only
  sum(diag(chol2inv(fit$R) %*% H_0))
} # edf_lasso

log_lambda <- seq(-1, 5, by=0.05)   # grid of log penalty values
lambda <- exp(log_lambda)           # penalty values
n_lambdas <- length(lambda)

# one row of results per penalty value
grid_res <- data.frame(log_lambda=log_lambda, lambda=lambda, conv=NA,
                       grad=NA, pd=NA, edf=NA, m_changes=NA, loglik=NA,
                       mbic=NA)
beta_store <- matrix(NA, n_lambdas, n) # fitted beta for each lambda

beta_start <- c(log(mean(y)), rep(0, n-1)) # constant mean to start

for (j in 1:n_lambdas) { # loop over penalty values
  # (a) fit, warm starting from the previous lambda's solution
  fit_j <- fit_lasso(beta_start, y, lambda[j])
  beta_store[j, ] <- beta_start <- fit_j$beta
  grid_res$conv[j] <- fit_j$conv   # 0 means optim reports convergence
  grid_res$grad[j] <- fit_j$grad   # should be close to zero
  grid_res$pd[j] <- fit_j$pd       # TRUE if genuine minimum
  
  # (b) effective degrees of freedom
  grid_res$edf[j] <- edf_lasso(fit_j, y)
  
  # (c) unpenalised log likelihood (on the log scale), m and mbic
  mu_j <- exp(cumsum(fit_j$beta))
  grid_res$loglik[j] <- sum(dpois(y, mu_j, log=TRUE))
  grid_res$m_changes[j] <- grid_res$edf[j] - 1 # remove baseline beta[1]
  grid_res$mbic[j] <- 3*grid_res$m_changes[j]*log(n) -
    2*grid_res$loglik[j]
} # end of loop over penalty values

# verify convergence over the whole grid
all(grid_res$conv == 0) # optim's convergence code
max(grid_res$grad)      # largest relative gradient at an optimum
all(grid_res$pd)        # every Hessian positive definite

best <- which.min(grid_res$mbic) # penalty minimising mbic
lambda_best <- lambda[best]
# summary of the selected penalty: lambda, convergence, edf, m and mbic
round(grid_res[best, ], 3)

# mbic either side of the minimum, to see how well defined it is
round(grid_res[(best-3):(best+3), c("log_lambda","m_changes","mbic")], 2)

# best is the global minimiser over the grid, the rows either side
# of it confirm it is a clear minimum and not a flat region


# STEP 5:
# fit model and changepoint locations
# refit at the penalty chosen by mBIC, estimate the number of changepoints
# as the floor of the effective number, and place them at the largest
# estimated jumps |beta_k|, k >= 2.

# refit at lambda_best, starting from the solution stored in step 4
fit_best <- fit_lasso(beta_store[best, ], y, lambda_best)
# verify the values:
# conv should be 0, grad close to 0, pd =1 positive Hessian
c(conv=fit_best$conv, grad=fit_best$grad, pd=fit_best$pd) 

beta_best <- fit_best$beta
mu_best <- exp(cumsum(beta_best)) # fitted mean function

# effective and estimated number of changepoints
m_tilde <- edf_lasso(fit_best, y) - 1 # -1 to exclude beta 1
m_hat <- floor(m_tilde)
c(m_tilde=m_tilde, m_hat=m_hat)

# rank the jumps by size, excluding the baseline beta[1]; adding 1
# converts positions in beta[-1] back to time points
jump_order <- order(abs(beta_best[-1]), decreasing=TRUE) + 1
tau_hat <- sort(jump_order[1:m_hat])

# estimated changepoint locations:
tau_hat

# size of each detected jump and the factor it multiplies the mean by
round(data.frame(time=tau_hat, beta=beta_best[tau_hat],
                 factor=exp(beta_best[tau_hat])), 3)

# true and fitted level within each true segment
seg <- cut(time, breaks=tau_true, right=FALSE)
round(cbind(true=theta_true, fitted=tapply(mu_best, seg, mean)), 2)

# plot the data, true mean and fitted mean, with the detected 
# changepoints
plot(time, y, pch=16, col="grey", xlab="Time", ylab="Count",
     ylim=c(0, 1.4*max(y)), main="Fitted mean at the mBIC-selected penalty")
lines(time, mu_true, type="s", lwd=2)
lines(time, mu_best, type="s", lwd=2, col="red")
segments(tau_hat, 0, tau_hat, max(y), lty="dotted", col="red")
legend("top", ncol=2, bty="n", cex=0.85,
       legend=c("observed counts", "true mean", "fitted mean",
                "estimated changepoints"),
       pch=c(16, NA, NA, NA), lty=c(NA, "solid", "solid", "dotted"),
       lwd=c(NA, 2, 2, 1), col=c("grey", "black", "red", "red"))

# we notice from the plot:
# the drop at time 30 is detected cleanly, as one large change that
# multiplies the mean by about 0.32. the rise near time 70 is detected
# also, but split across times 69, 70 and 72 instead of one jump. this is
# because the counts there rise gradually, and the lasso penalty charges
# for the total size of the changes, so several small steps cost about
# the same as one big step. as a result m_hat = 4 overestimates the true
# m = 2, although every detected time lies at or next to a real change.
# the fitted levels (9.79, 3.10, 6.64) are also pulled towards the
# overall mean compared with the truth (10, 2, 7), because the penalty
# is shrinking every change towards zero.


#STEP 6:Parametric Bootstrap
#assess uncertainty in the fitted mean function
set.seed(2026)
B <- 200

#store the fitted mean from each bootstrap sample
mu_boot <- matrix(NA_real_, nrow = n, ncol = B)

#simulate and refit the model for each bootstrap sample

for(b in 1:B){
  #parametric resampling
  #generate new poisson observations using the fitted mean
  y_sim <- rpois(n, lambda = mu_best)
  #refit the model using the same penalty parameter
  fit_sim <- fit_lasso(beta_best, y_sim, lambda_best)
  #check convergence before storing the result
  if(fit_sim$conv != 0){
    stop("Bootstrap optimisation didn't converge")
  }
  #calculate and store the fitted mean function
  mu_boot[,b] <- exp(cumsum(fit_sim$beta))
}

#calculate the average fitted mean across bootstrap samples
mu_bar <- rowMeans(mu_boot)

#compare the bootstrap average with the original fitted mean

#save the current graphical settings
old_par <- par(no.readonly=TRUE)

#create two separate plotting areas:
#the upper one for the main plot, the lower one for the legend
layout(matrix(c(1, 2), nrow=2), heights=c(5, 1))

#main plot
par(mar=c(4, 4, 3, 2))

plot(time, y,
     pch=16, col="grey",
     xlab="Time", ylab="Poisson count",
     ylim=range(c(0, y, mu_true, mu_best, mu_bar), finite=TRUE),
     main="Bootstrap mean comparison")

#add the true, fitted and bootstrap mean functions
lines(time, mu_true, type="s", col="black", lwd=2)
lines(time, mu_best, type="s", col="red", lwd=2)
lines(time, mu_bar, type="s", col="blue", lwd=2)

#separate legend area
par(mar=c(0, 0, 0, 0))
plot.new()

legend("center",
       legend=c("Observed counts", "True mean",
                "Fitted mean", "Bootstrap mean"),
       col=c("grey", "black", "red", "blue"),
       pch=c(16, NA, NA, NA),
       lty=c(0, 1, 1, 1),
       ncol=2,
       cex=0.8,
       bty="n")

#restore the original graphical settings
par(old_par)
#estimate bootstrap bias and correct the fitted mean
bias_boot <- mu_bar - mu_best
mu_bc <- mu_best - bias_boot

#calculate pointwise 95% bootstrap percentile intervals
qL <- apply(mu_boot, 1, quantile, probs=0.025)
qU <- apply(mu_boot, 1, quantile, probs=0.975)

#bias-correct the 95% confidence interval limits
qL_bc <- 2*mu_best - qU
qU_bc <- 2*mu_best - qL

#STEP 7: Final plot
#show the observations, fitted means and confidence intervals

#save the current graphical settings
old_par <- par(no.readonly=TRUE)

#reate two separate plotting areas
layout(matrix(c(1, 2), nrow=2), heights=c(5, 1))

#set the range of the vertical axis
y_limits <- range(c(0, y, mu_true, mu_best,
                    mu_bc, qL_bc, qU_bc), finite=TRUE)

#plot the original observations
par(mar=c(4, 4, 3, 2))
plot(time, y,
     pch=16, col="grey",
     xlab="Time", ylab="Poisson count",
     ylim=y_limits,
     main="Poisson changepoint estimation with 95% confidence intervals")

#add the true and fitted mean functions
lines(time, mu_true, type="s", col="black", lwd=2)
lines(time, mu_best, type="s", col="red", lwd=2)

#add the bias-corrected mean function
lines(time, mu_bc, type="s", col="blue", lwd=2)

#add the bias-corrected 95% confidence limits
lines(time, qL_bc, type="s", col="darkgreen", lty=2)
lines(time, qU_bc, type="s", col="darkgreen", lty=2)

#create a separate area for the legend
par(mar=c(0, 0, 0, 0))
plot.new()

#add the legend 
legend("center",
       legend=c("Observed counts", "True mean",
                "Fitted mean", "Bias-corrected mean",
                "95% confidence limits"),
       col=c("grey", "black", "red", "blue", "darkgreen"),
       pch=c(16, NA, NA, NA, NA),
       lty=c(0, 1, 1, 1, 2),
       ncol=3,
       cex=0.8,
       bty="n")

par(old_par)

