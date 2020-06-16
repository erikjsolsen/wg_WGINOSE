library(readr)

fixDatras <- function(file) {
  # count spaces between headers (source: https://www.reddit.com/r/rstats/
  # comments/2th8ic/function_to_count_the_number_of_white_spaces/)
  countSpaces <- function(x) {
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


  # Read the whole file
  lines <- readLines(file)

  # Extract header and calculate the column widths
  headers <- unlist(strsplit(lines[1], " +"))
  lenHead <- as.numeric(sapply(headers, nchar))
  lenSpaces <- countSpaces(lines[1])
  lenSpaces[length(lenSpaces)+1] <- 1
  headerWidths <- lenHead + lenSpaces

  # Read into table
  x <- read_fwf(file, fwf_widths(headerWidths,headers), skip=1,
    col_types=cols(.default = col_character()))
  initRow <- nrow(x)

  # Remove invalid data
  x <- x[!is.na(x$Ship),]
  finRow <- nrow(x)

  # Output filename
  outFile <- paste0(gsub(pattern = "\\.csv$", "", basename(file)), "_fixed.csv")

  # Write output
  write.csv(x, outFile, row.names=F, quote = F)
  print(paste("Created", outFile, "file, with", finRow, "rows (", initRow-finRow, "row(s) removed )"))

  return(x)
}

files <- list("WGINOSE_LNGT.csv", "WGINOSE_Age.csv")

result <- lapply(files, fixDatras)
