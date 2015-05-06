rankhospital <- function(state, outcome, num){
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
  
  if (outcome == "heart attack" & num == "best"){
    ##Given the state, display the hospital name where the mortality rate for heart attack is lowest
    df_sub$Hospital[which.min(df_sub$HA)] 
  }
  else if (outcome == "heart attack" & num == "worst"){
    ##Given the state, display the hospital name where the mortality rate for heart attack is highest
    df_sub$Hospital[which.max(df_sub$HA)]
  }
  else if (outcome == "heart attack" & num != "best" & num != "worst"){
  ##Given the state, display the hospital name where the mortality rate for heart attack is equal to the "num" varialbe
  df_sub <- df_sub[order(df_sub$HA),]
  df_sub <- df_sub[num,]
  df_sub$Hospital
}
  else if (outcome == "heart failure" & num == "best"){ 
    df_sub$Hospital[which.min(df_sub$HF)]
  }
  else if (outcome == "heart failure" & num == "worst"){ 
    df_sub$Hospital[which.max(df_sub$HF)]
  }
  else if (outcome == "heart failure" & num != "best" & num != "worst"){
  df_sub <- df_sub[order(df_sub$HF),]
  df_sub <- df_sub[num,]
  df_sub$Hospital
  }
  else if (outcome == "pneumonia" & num == "best"){ 
    df_sub$Hospital[which.min(df_sub$Pn)]
  }
  else if (outcome == "pneumonia" & num == "worst"){ 
    df_sub$Hospital[which.max(df_sub$Pn)]
  }
  else if (outcome == "pneumonia" & num != "best" & num != "worst"){
    df_sub <- df_sub[order(df_sub$Pn),]
    df_sub <- df_sub[num,]
    df_sub$Hospital
  }
else{
  stop("invalid outcome")
}
}

## rankhospital("MD","pneumonia","best")
## rankhospital("CA","heart attack","worst")
## rankhospital("AL","heart failure",3)
## rankhospital("MDE","heart failure",7)  ##should show as error because 'MDE' is not a valid state
