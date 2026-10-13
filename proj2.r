# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# Purpose of this code:
#  With this code we want to simulate throwing a coin with possibility p.
#  Two players each pick a pattern of k outcomes, and the coin is tossed
#  until one of the patterns appears; that player wins.

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


