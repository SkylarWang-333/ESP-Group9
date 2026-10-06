# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2975970
getwd()

#loading data
music_raw <- readLines("/Users/aishamubarak/Desktop/MSc/Semester 1/EStat_programming/Practical-1/palestrina.txt")

## Q4
#inspecting the data
head(music_raw,n=20L)

# preprocessing function

preprocess <- function(music){
  # a) remove first 3 rows
  music <-(music[-(1:3)])
  # b) we only want to keep lines that don't start with #
  music <- music[!startsWith(music,"#")]
  # c) asign a vector to save the position of the blanks to add || 
  blank <- which(music =="")
  islast_line <- blank -1
  # add " ||" to each end of line
  music[islast_line]<- paste0(music[islast_line]," ||")
  # add " |" to the rest
  notlast_line <- setdiff(seq_along(music), c(islast_line, blank))
  music[notlast_line] <- paste0(music[notlast_line]," |")
  
  # splitting the measures into notes on " "
  music <- strsplit(music," ")
  music <- unlist(music)
  
  # return processed data
  music
}
# check results match 
music_clean<- preprocess(music=music_raw)
tail(music_clean, n = 10)

## Q5

# Create refrence for unique values
notes <- unique(music_clean)
tokens <- match(music_clean, notes)

# verify lengths
length(music_clean)==length(tokens)

##Q6

make_matrix <- function(tokens, end, mlag = 4) {
  
  # Get the total number of tokens
  n <- length(tokens)
  
  # Create an empty matrix for consecutive token sequences
  M <- matrix(0,nrow = n - mlag, ncol = mlag + 1)
  
  # Fill each column with a shifted section of tokens
  for (i in 1:(mlag + 1)) {
    M[, i] <- tokens[i:(n - mlag + i - 1)]
  }
  
  # Identify rows where || occurs before the final column
  bad_rows <- apply(
    M[, 1:mlag, drop = FALSE],
    1,
    function(x) any(x == end)
  )
  
  # Remove sequences that cross between two pieces of music
  M <- M[!bad_rows, , drop = FALSE]
  
  return(M)
}

M <- make_matrix(tokens = tokens, end = which(notes==" || "), mlag = 4)
M


## Q7

next_note <- function(key, M, w=rep(1, ncol(M)-1 )) {
    
    # Get the maximum lag from M
    mlag <- ncol(M) - 1
    
    # If key is too long, keep only the most recent tokens
    if (length(key) > mlag) {
      key <- tail(key, mlag)
    }
    
    # Create empty vectors for possible next tokens and their weights
    u <- c()
    p <- c()
    
    # Check the key and its reduced versions
    for (k in length(key):1) {
      
      # Keep the last k tokens
      current_key <- tail(key, k)
      
      # Find the first column of M to be matched
      mc <- mlag - k + 1
      
      # Find differences between current_key and each row of M
      ii <- colSums(
        !(t(M[, mc:mlag, drop = FALSE]) == current_key)
      )
      
      # Find rows that completely match
      matches <- which(ii == 0)
      
      # Collect possible next tokens and their weights
      if (length(matches) > 0) {
        
        next_tokens <- M[matches, mlag + 1]
        
        u <- c(u, next_tokens)
        
        current_p <- rep(
          w[mc] / length(matches),
          length(matches)
        )
        
        p <- c(p, current_p)
      }
    }
    
    # No usable matches
    if (length(u) == 0 || sum(p) == 0) {
      
      next_token <- sample(M[, mlag + 1], size = 1)
      
      # Only one possible next token
    } else if (length(u) == 1) {
      
      next_token <- u[1]
      
      # Two or more possible next tokens
    } else {
      
      next_token <- sample(u, size = 1, prob = p)
    }
    
    return(next_token)
  }
  
  
generate_music(start, M, w, notes)

