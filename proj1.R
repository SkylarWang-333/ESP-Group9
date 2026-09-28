# Aishah ALmubarak   S2957708
# Ziyi Wang          S2975970


#read file palestrina .txt
setwd("/Users/aishamubarak/proj1-test") ## comment out of submitted 

#loading data
music_raw <- readLines("~/Downloads/Practical-1/palestrina.txt")

#inspecting the data
head(music_raw,n=20L)

# preprocessing function
preprocess <- function(music)
