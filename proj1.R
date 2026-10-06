# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2945970

# getwd()

# Overview:
# This file builds a simple Markov prediction model for music and
# uses it to simulate new music in the style of Giovanni Palestrina. The
# training data (palestrina.txt) is used to predict the next musical note.

## Q4
#loading data
music_raw <- readLines("palestrina.txt")


#inspecting the data
head(music_raw,n=20L)

# Preprocess function
# Description: this function turns the raw text data into a vector of notes the model can use to predict from.
# in a couple of steps: 1- remove the first 3 rows, 2- remove lines that start with #. 3- add || to the last line of the 
# piece and | for the rest of the lines. 4- split the measures into notes and combine into a vector.
# inputs: music text file
# output: cleaned vector

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
  
  # removing blank lines
  music <- music[music != " "]
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
# Create a dictionary for unique values, then create tokens to replace the each note with it's position in the dictionary.
# because comparing numbers is faster than comparing strings(notes).

notes <- unique(music_clean)
tokens <- match(music_clean, notes)

# verify lengths
length(music_clean)==length(tokens)

##Q6
# Make matrix function
# Description: Build the training matrix, arrange the data into n-mlag rows and mlag+1 columns. mlag is the maximum number of notes
# model considers to predict the next note. if the row contains || in the middle we must remove it beacuse it marks the end of the piece 
# so the notes that follow shouldn't be predicted.
# inputs: tokens (the text data converted into numbers), end : || (what signifies end of piece), mlag: maximum number of notes to consider for prediction. 
# output: M matrix

make_matrix <- function(tokens, end, mlag = 4) {
  
  # Get the total number of tokens
  n <- length(tokens)
  
  # Create an empty matrix for consecutive token sequences
  M <- matrix(0,nrow = n - mlag, ncol = mlag + 1)
  
  # Fill each column with a shifted section of tokens
  for (i in 1:(mlag + 1)) {
    M[, i] <- tokens[i:(n - mlag + i - 1)]
  }
  
  # Identify rows where || occur before the final column
  bad_rows <- apply(
    M[, 1:mlag, drop = FALSE],
    1,
    function(x) any(x == end)
  )
  
  # Remove sequences that cross between two pieces of music
  M <- M[!bad_rows, , drop = FALSE]
  
  return(M)
}

M <- make_matrix(tokens = tokens, end = which(notes=="||"), mlag = 4)


## Q7
# Description: in this function we are predicting the next note using our M matrix. The model finds
# rows of M that match the key, and the shorter versions of the key and assigns the matching notes their probability.
# If there are no usable matches, sample a token from the final column of M.
# if the key is too long we use the last m-lag notes.
# inputs: key, a vector containing a token sequence for which the next note is to be generated,
# M, the matrix produced by make_matrix, and w a vector of mixture weights
# outputs: next note token.

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


#test
test_key <- M[1, 1:4]
test_key
next_token=next_note(key=test_key,M=M)
next_token
notes[next_token]

#Q8
# Description: this function generates a sequence by repeatedly calling the previous function next_note using all
# notes generated so far as the key, and adding each new note to the end. until one of these conditions are met:
# 1- we get a double bar (||), 2- we get 8 single bars (|), 3- if the length of the sequence is 200.
# Inputs: start a single token ID to initiate the simulation, M the matrix generated by make_matrx,
# w a vector of mixing weights, and notes the vector of note names.
# Output: the generated music sequence.
generate_music <- function(start, M, notes, w = NULL) {
  
  # Get the maximum lag from M
  mlag <- ncol(M) - 1
  
  # Set default weights to all 1s
  if (is.null(w)) {
    w <- rep(1, mlag)
  }
  
  # Get the token IDs for the bar-line and double bar-line
  bar_token <- which(notes == "|")
  end_token <- which(notes == "||")
  
  # Start the generated sequence with the starting token
  generated <- start
  
  # Keep generating notes until a stopping condition is met
  while (TRUE) {
    
    # Generate the next token using the sequence generated so far
    new_token <- next_note(
      key = generated,
      M = M,
      w = w
    )
    
    # Add the new token to the generated sequence
    generated <- c(generated, new_token)
    
    # Stop if the double bar-line has been generated
    if (new_token == end_token) {
      break
    }
    
    # Stop if eight bar-lines have been generated
    if (sum(generated == bar_token) >= 8) {
      break
    }
    
    # Stop if the sequence has reached 200 tokens
    if (length(generated) >= 200) {
      break
    }
  }
  
  # Convert token IDs back to note names
  generated_notes <- notes[generated]
  
  # Join all notes into one character string
  music_string <- paste(generated_notes, collapse = " ")
  
  return(music_string)
}

#test
sim_music <- generate_music(
  start = 20,
  M = M,
  notes = notes,
  w = c(1, 1, 1, 1)
)

sim_music

music_w1 <- generate_music(
  start = 20,
  M = M,
  notes = notes,
  w = c(1, 1, 1, 1)
)

music_w2 <- generate_music(
  start = 20,
  M = M,
  notes = notes,
  w = c(1, 0, 0, 0)
)

music_w3 <- generate_music(
  start = 20,
  M = M,
  notes = notes,
  w = c(0, 0, 0, 1)
)

music_w1
music_w2
music_w3
#comment:
# All three weights run without error and produce a valid music string.
# The three generated music sequences are performed differently. The melody of w1 is rich and changeable, including different note time values and pauses. 
# There are more pauses in w2, which sounds slow. 
# The melody of w3 is complex and changeable, with more short notes and obvious changes in pitch. 
# The prediction model uses the previous notes as lagged information to predict the next note.
# The larger lag can capture a longer range of patterns while the smaller lag mainly reflects the local patterns between adjacent notes.
# this will affect the repetition  and change of the generated music.
# in the 3 examples, we can see different notes, pauses, and time values, which reflects different lad dependencied used by the model.

#play music
source("play_music.r")
play(str_to_waveform(sim_music, bpm = 400))

# Team Member Contributions
# Aishah ALmubarak: Completed Questions 4–5 and provided detailed comments and descriptions explaining the code.
# Ziyi Wang: Completed Questions 6–8 and implemented the functions for constructing the model and generating music.
# Yizhen Ge: Independently completed the task for cross-checking, and analysed the results under the three different weight settings.

# AI situation: We used ChatGPT to help us understand basic R syntax and how to implement our ideas in R. We first worked out the logic of the code and explained it to ChatGPT, then asked it to show us how the relevant R functions and syntax could be used. We wrote the code ourselves based on this guidance. When our code produced errors, we provided the error messages to ChatGPT and asked it to explain the causes and help us debug the code. This helped us better understand how to translate programming logic into R code and how to identify and fix errors.