# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2975970


#loading data
music_raw <- readLines("palestrina.txt")

music_raw <- readLines("/Users/aishamubarak/Desktop/MSc/Semester 1/EStat_programming/Practical-1/palestrina.txt")
#inspecting the data
head(music_raw,n=20L)

# preprocessing function
preprocess <- function(music)
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
