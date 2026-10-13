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
     return(c(1,match_a,x))
    
    match_b=firsthit(x,b)
    if (!is.na(match_b))
     return(c(0,match_b,x))

  }
}

a=c(0,0,0)
b=c(1,1,1)      
     
duel(a,b)  
       