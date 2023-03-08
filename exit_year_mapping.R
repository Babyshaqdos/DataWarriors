#Read the file in
df <- read.csv("C:\\Users\\nicks\\Downloads\\MockData_DW.csv")


#Get the exit date column
exit_date <- df['Exit.Date.Status']


#Add a new column to dataframe to indicate whether patient completed program or not
completed <- rep("a", nrow(df))
df <- cbind(df, completed)





mapYears <- function(df) {
  for(i in 1:ncol(df)){
    checkComplete(df$Exit.Date.Status[i], i, df)
    temp_string <- gsub(".*/(\\d+)\\D*$", "\\1", df$Exit.Date.Status[i])
    print(temp_string)
    print(checkYear(temp_string))
  }
}





#Function that takes a a string representing a year and returns it in 4 digits if necessary
checkYear <- function(string){
  if (nchar(string) == 4) {
    return(string)
  }
  if (as.integer(string) <= 23) {
    out_string <- paste("20", string, sep = "")
    return(out_string)
  } else {
    out_string <- paste("19", string, sep = "")
    return(out_string)
  }
}



#Function that takes the exit date string and searches for SLE, AC, or C to determine completion status
checkComplete <- function(string, row, df){
  if (grepl("SLE", string) == TRUE){
    df[as.integer(row), "completed"] <<- 'completed'
    return()
  }
  if (grepl("AC|C", string) == TRUE) {
    df[as.integer(row), "completed"] <<- "semi-completed"
    return()
  }
  df[as.integer(row), "completed"] <<- "incomplete"
  return()
}










