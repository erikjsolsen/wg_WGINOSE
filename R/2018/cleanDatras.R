#script for cleaning DATRAS 2018 files
#by I Umar
#modified by E Olsen

# count spaces between headers (source: https://www.reddit.com/r/rstats/comments/2th8ic/function_to_count_the_number_of_white_spaces/)
countSpaces <- function(x){
  counter <- 0
  coll <- numeric()
  vec <- strsplit(x," ")[[1]]
  for(i in 1:length(vec)){
    if (vec[i]==""){
      counter <- counter+1
    }
    else{
      if (counter!=0) coll <- c(coll,counter)
      counter <- 1
    }
  }
  coll
}

library(readr)

file <- "WGINOSE_Age.csv"

# Read the whole file
lines <- readLines(file)

# Extract header and calculate the column widths
headers <- unlist(strsplit(lines[1], " +"))
lenHead <- as.numeric(sapply(headers, nchar))
lenSpaces <- countSpaces(lines[1])
lenSpaces[length(lenSpaces)+1] <- 1
headerWidths <- lenHead + lenSpaces

# Read into table
x <- read_fwf(file, fwf_widths(headerWidths,headers), skip=1, col_types=cols(.default = col_character()))

# Write output
write.csv(x, paste0(file,"_fixed.csv"),row.names=F, quote=F)
