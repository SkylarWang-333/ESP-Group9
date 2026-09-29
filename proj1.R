# Aishah ALmubarak   S2957708
# Yizhen Ge          S3027690
# Ziyi Wang          S2975970


#loading data
music_raw <- readLines("palestrina.txt")

#inspecting the data
head(music_raw,n=20L)

# preprocessing function
preprocess <- function(music)
