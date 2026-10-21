# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# Purpose of this code:
#  With this code we want to simulate throwing a coin with probability p.
#  Two players each pick a pattern of k outcomes and the coin is tossed,
#  the player with the first patter to appear wins.


# Q1
# function returns position at which pattern a first appears in toss stream x,
# returns the last position of the first matching k-window, or NA if we have no match.
firsthit <- function(x,a){
  k <- length(a)
  n <- length(x)
  if (n < k)
   return(NA)
  
  for (start in 1:(n-k+1)){
  if (all(x[start:(start+k-1)]==a)) {
   endplace= start+k-1
   return(endplace)
   }
  }
  return(NA)
}

# Shared function for duel and pwin.
# Runs nreps duels between patterns a and b at once, each row of X is a duel.
# Tosses are added in blocks of 20, only to rows where neither pattern has appeared yet.
# Returns a matrix with 2 columns winner (1 = a, 0 = b) and tosses for the position of the win.

sim_duels <- function(a, b, p, nreps) {
  k      <- length(a)
  block  <- 20
  winner <- rep(NA, nreps)
  tosses <- rep(NA, nreps)
  X      <- matrix(0, nreps, 0)                  

  while (any(is.na(winner))) {                   
    # new block: random tosses for unfinished duels, zeros for finished ones
    undecided <- is.na(winner)
    new <- matrix(0, nreps, block)
    new[undecided, ] <- rbinom(sum(undecided) * block, 1, p)
    X <- cbind(X, new)

    # check only windows ending in the new block
    for (m in max(k, ncol(X) - block + 1):ncol(X)) {
      hitA <- is.na(winner)
      hitB <- is.na(winner)
      for (j in 1:k) {
        hitA <- hitA & X[, m - k + j] == a[j]
        hitB <- hitB & X[, m - k + j] == b[j]
      }
      winner[hitA] <- 1; tosses[hitA] <- m
      winner[hitB] <- 0; tosses[hitB] <- m
    }
  }
  cbind(winner = winner, tosses = tosses)
}

#Q2
# simulates one duel between patterns a and b on a shared coin stream.
# Uses sim_duels with one stream of tosses.
# Returns c(winner, tosses): winner 1 for a 0 if b, and the number of tosses the duel took.

duel <- function(a, b, p = 0.5) {
  results <- sim_duels(a, b, p, nreps = 1)   # table with one row
  one_duel <- results[1, ]                   # take that row
  return(one_duel)
}

#Q3
# returns the estimated probability that pattern a appears before b
# the proportion of nreps duels won by a.

pwin <- function(a, b, p = 0.5, nreps = 10000) {
  results <- sim_duels(a, b, p, nreps)       # table: one row per duel
  wins    <- results[, "winner"]             # winner column: 1s and 0s
  return(mean(wins))                         # proportion of 1s
}

#Q4 writing function ewait(a,p=0.5)

ewait <- function(a,p=0.5){
  k <- length(a)
  expected_wait <- 0
  for (j in 1:k) {
    if( all(a[1:j]==a[(k-j+1):k]))
    {
      h_j <- sum(a[1:j])
      prob <- p^h_j*(1-p)^(j-h_j)
      expected_wait <- 1/prob + expected_wait
    }
  }
  return(expected_wait)
}

# Monte Carlo estimation


mc_ewait <- function(a,p=0.5,nreps=10000){
  
  waits <- replicate(nreps,{
    x <- integer(0)
    hit<- NA_integer_
    
    while(is.na(hit)){
      x <- c(x, rbinom(10, size = 1, prob=p))
      hit <- firsthit(x,a)
    }
    hit
  })
  return(mean(waits))
}

# Test different patterns, including a biased coin (p = 0.7).

cat("Pattern 11, p = 0.5:\n")
cat("Exact:", ewait(c(1,1), p=0.5),
    "Monte Carlo:", mc_ewait(c(1,1), p=0.5), "\n")

cat("Pattern 10, p = 0.7:\n")
cat("Exact:", ewait(c(1,0), p=0.7),
    "Monte Carlo:", mc_ewait(c(1,0), p=0.7), "\n")

cat("Pattern 101, p = 0.7:\n")
cat("Exact:", ewait(c(1,0,1), p=0.7),
    "Monte Carlo:", mc_ewait(c(1,0,1), p=0.7), "\n")

#Q5 writing function pmat(k,p=0.5,nrep=10000)
pmat <- function(k, p = 0.5, nreps = 10000){
  n_patterns <- 2^k
  # Generate all binary patterns
  # Patterns are ordered by increasing binary value (000, 001, ..., 111).
  # Rows and columns of M follow this ordering.
  patterns <- matrix(0, nrow = n_patterns, ncol = k)
  
  for (i in 0:(n_patterns-1)) {
    for (j in 1:k) {
      patterns[i+1, j] <- (i %/% 2^(k-j)) %% 2
    }
  }
  #create win probability matrix
  M <- matrix(NA_real_, nrow = n_patterns, ncol = n_patterns)
  #Calculate pairwise win probabilities
  for (i in 1:n_patterns){
    for (j in 1:n_patterns){
      if (i!=j){
        a<-patterns[i,]
        b<-patterns[j,]
        
        M[i, j] <- pwin(a, b, p, nreps)
      }
    }
  }
  
  return(M)
}

#Q5(a): Best reply for each pattern
cat("Q5(a):Pattern index  Best reply index  Win probability\n")
M <- pmat(k = 3, p = 0.5, nreps = 10000)
for (j in 1:ncol(M)){
  best_index <- which.max(M[,j])
  best_prob  <- max(M[,j], na.rm = TRUE)
  
  cat(j, best_index, round(best_prob, 4), "\n")
}


#Q5(b) find the evenly matched pairs
#find win probabilities within 0.02 of 0.5.
cat("Q5(b): Evenly matched pairs\n")
pairs <- which(abs(M - 0.5) < 0.02 & upper.tri(M), arr.ind = TRUE)

for (i in 1:nrow(pairs)) {
  a <- pairs[i, 1]
  b <- pairs[i, 2]
  
  cat(a, b, round(M[a, b], 4), "\n")
}

#find an explicit nontransitive cycle
#use 0.55 as a threshold for identifying clear wins.
wins <- M > 0.55
#no 3-pattern cycle was found, so we check for a 4-pattern cycle.
for (i in 1:nrow(M)) {
  for (j in 1:nrow(M)) {
    for (h in 1:nrow(M)) {
      for (l in 1:nrow(M)) {
        
        if (length(unique(c(i, j, h, l))) == 4) {
          if (wins[i,j] && wins[j,h] &&
              wins[h,l] && wins[l,i]) {
            
            cat(i, "->", j, "->", h, "->", l, "->", i, "\n")
            
          }
        }
        
      }
    }
  }
}
#result:2 -> 4 -> 7 -> 5 -> 2 
#       4 -> 7 -> 5 -> 2 -> 4 
#       5 -> 2 -> 4 -> 7 -> 5 
#       7 -> 5 -> 2 -> 4 -> 7 
M[2,4]
M[4,7]
M[7,5]
M[5,2]

#Q5(c) Find two patterns with the same expected waiting time where one clearly beats the other
cat("Q5(c): Expected waiting times\n")
patterns <- matrix(0, nrow = 8, ncol = 3)

for (i in 0:7) {
  for (j in 1:3) {
    patterns[i+1, j] <- (i %/% 2^(3-j)) %% 2
  }
} 
for (i in 1:8) {
  cat(i, ewait(patterns[i, ], p = 0.5), "\n")
}
#result:1 14 ;2 8 ;3 10 ;4 8 ;5 8 ;6 10 ;7 8 ;8 14 
#chose M[5, 2]
#result:0.775 which is clearly above 0.5
#Both patterns compete on the same sequence of tosses, so their
#occurrences are dependent rather than independent.



#Q6
#show the distribution of duel lengths and the variability of win probability estimates.

#compare patterns 100 and 001, which have the same expected waiting time.
#simulate 10000 duels and record how many tosses each duel takes.
results <- sim_duels(c(1,0,0), c(0,0,1), 0.5, 10000)
duel_times <- results[, "tosses"]

#plot the distribution of duel lengths and mark the median.
freq <- table(duel_times)
prob <- as.numeric(freq) / sum(freq)
times <- as.numeric(names(freq))
med <- median(duel_times)

plot(times, prob, type = "h", lwd = 2,
   xlab = "Number of tosses", ylab = "Proportion of duels",
   main = "Distribution of duel lengths")
abline(v = med, col = "red", lty = 2)
text(med, max(prob) * 0.9, paste("Median =", med), pos = 4)

cat("Q6: Duel lengths\n")
cat("Mean:", mean(duel_times), "Median:", med, "\n")
#according to the graph
#the distribution is right-skewed, with most duels ending after a few tosses.
#the median is 5, while the mean is about 6.58 due to some longer duels.


#repeat pwin 50 times and see how the estimated win probability changes.
#use the best reply to 111 found in Q5(a)
best <- which.max(M[, 8])
a <- patterns[best, ]
b <- c(1,1,1)

estimates <- replicate(50, pwin(a, b, p = 0.5, nreps = 2000))
ref <- mean(estimates)

#plot the estimates and their mean as a reference.
plot(estimates, type = "b", pch = 16,
     xlab = "Simulation run", ylab = "Estimated win probability",
     main = "Variability of win probability estimates")
abline(h = ref, col = "red", lty = 2)
text(25, ref, paste("Mean =", round(ref, 3)), pos = 3)

cat("Q6: Win probability estimates\n")
cat("Best reply to 111:", paste(a, collapse = ""), "\n")
cat("Mean:", round(ref, 4), "\n")
cat("SD:", round(sd(estimates), 4), "\n")
cat("Range:", round(range(estimates), 4), "\n")
#the best reply to 111 is 011
#the 50 estimates are centred around 0.874, with a standard deviation of 0.0082.
#the small variation suggests that the estimated win probability is fairly stable.




# Q7
#investigate how coin bias changes the best replies and non-transitive cycles

for (p in c(0.25, 0.7)) {
  M_bias <- pmat(k = 3, p = p, nreps = 10000)
  
  cat("\nQ7: p =", p, "\n")
  cat("Pattern  Best reply  Win probability\n")
  
#find the pattern with the highest probability of beating each opponent
for (j in 1:8) {
  best <- which.max(M_bias[, j])
  cat(paste(patterns[j, ], collapse = ""),
      paste(patterns[best, ], collapse = ""),
      round(M_bias[best, j], 4), "\n")
}
  
#search for a cycle where every pattern beats the next with probability above 0.55
wins <- M_bias > 0.55
wins[is.na(wins)] <- FALSE
found <- FALSE
  
#check three-pattern cycles first
for (i in 1:8) {
  for (j in 1:8) {
  for (h in 1:8) {
    if (!found && length(unique(c(i,j,h))) == 3 && wins[i,j] && wins[j,h] && wins[h,i]) {
      cycle <- c(i,j,h,i)
      found <- TRUE
  }
  }
  }
}
  
#if no three-pattern cycle is found, check four-pattern cycles
if (!found) {
  for (i in 1:8) {
  for (j in 1:8) {
  for (h in 1:8) {
  for (l in 1:8) {
    if (!found && length(unique(c(i,j,h,l))) == 4 && wins[i,j] && wins[j,h] &&  wins[h,l] && wins[l,i]) {
     cycle <- c(i,j,h,l,i)
     found <- TRUE
  }
  }
  }
  }
  }
}
  
#print the first cycle found and the probability of each win
if (found) {
  cat("Non-transitive cycle:\n")
  cat(paste(cycle, collapse = " -> "), "\n")
    
for (j in 1:(length(cycle)-1)) {
  cat(paste(patterns[cycle[j], ], collapse = ""),"beats",
      paste(patterns[cycle[j+1], ], collapse = ""),"with probability",
      round(M_bias[cycle[j], cycle[j+1]], 4), "\n")
  }
  } else {
    cat("No cycle of length 3 or 4 found above 0.55.\n")
  }
}
#we can see that coin bias changes the best replies and their win probabilities.
#at p=0.25 and p=0.7, three-pattern non-transitive cycles were found.
#the patterns in these cycles differ, showing that non-transitivity remains under coin bias.

#summary
#patterns with the same expected waiting time can have very different win probabilities.
#the patterns compete on the same coin stream, so their first-hit times are dependent.
#therefore, ranking patterns by their isolated waiting times can be misleading.
