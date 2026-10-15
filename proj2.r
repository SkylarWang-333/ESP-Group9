# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# Purpose of this code:
#  With this code we want to simulate throwing a coin with possibility p.
#  Two players each pick a pattern of k outcomes, and the coin is tossed
#  until one of the patterns appears; that player wins.


# Q1

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

#Q2

duel <- function(a,b, p=0.5){
  x=c()
  
  while (TRUE){
    nextnum=sample(x = c(1,0), size = 1, replace = TRUE, prob = c(p,1-p))
    x=c(x,nextnum)
    
    match_a=firsthit(x,a)
    if (!is.na(match_a))
     return(c(1,match_a))
    
    match_b=firsthit(x,b)
    if (!is.na(match_b))
     return(c(0,match_b))

  }
}

# Q3

pwin <- function(a,b,p=0.5,nreps=10000){
  x=c()
  for (i in 1:nreps){
    winner=duel(a,b,p)
  x=c(x,winner[1])
  }
  return(mean(x))  
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
  for (j in 1:k) {
    patterns[i+1, j] <- (i %/% 2^(k-j)) %% 2
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
