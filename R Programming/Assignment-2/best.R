best <- function(state, outcome){
  readdata <- read.csv("outcome-of-care-measures.csv", header = T, colClasses="character", na.strings=c("Not Available", "NA"))
  df <- data.frame(readdata)
  df_sub<- df[, c(2,7,11, 17,23)] ##Shrink data frame to respective columns, locations represented by a number 
  colnames(df_sub)<- c("Hospital", "State", "HA", "HF", "Pn") ##Rename the column names 
  df_sub <- df_sub[df_sub$State==state,] ##Select the specific state from the State column
  
  list_states <- unique(as.character(df$State)) ##Gather all the states and store them into a variable
  count_states <- length(list_states)  ##Count the length of the list of states and store into a variable
  for(i in 1:count_states){  ##loop as many times equal to the length of the list of the states
    x <- list_states[i]     ##set variable to a state, where 'i' is the location of that specific state
    if (x == state){break}}   ##if there is a match in the list of states to our own desired state then end loop
  if (x != state){stop("invalid state")}  ##if the variable 'x' does not match our desired state then stop code
  
  if (outcome == "heart attack"){
    ##Given the state, display the hospital name where the mortality rate for heart attack is lowest
    df_sub$Hospital[which.min(df_sub$HA)]
  }
  else if (outcome == "heart failure"){ 
    df_sub$Hospital[which.min(df_sub$HF)]
  }
  else if (outcome == "pneumonia"){ 
    df_sub$Hospital[which.min(df_sub$Pn)]
  }

}

## best("MD","pneumonia")
## best("CA","heart attack")
## best("AL","heart failure")
## best("MDE","heart failure")  ##should show as error because 'MDE' is not a valid state
