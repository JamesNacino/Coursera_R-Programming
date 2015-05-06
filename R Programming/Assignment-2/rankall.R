rankall <- function(outcome, num){
  readdata <- read.csv("outcome-of-care-measures.csv", header = T, colClasses="character", na.strings=c("Not Available", "NA"))
  df <- data.frame(readdata)
  df_sub<- df[, c(2,7,11, 17,23)] ##Shrink data frame to respective columns, locations represented by a number 
  colnames(df_sub)<- c("Hospital", "State", "HA", "HF", "Pn") ##Rename the column names  
  df_sub <- df_sub[order(df_sub$State,df_sub$HA),] ##Select the specific state from the State column
 

  for (state i)
  
  
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
    df_sub<- df_sub[, c(1,2)]
    df_sub
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
    df_sub <- df_sub[order(df_sub$State),]
    df_sub <- df_sub[1:num,]
    df_sub[, c(1,2)] 
  }
  else{
    stop("invalid outcome")
  }
}

## rankhospital("MD","pneumonia","best")
## rankhospital("CA","heart attack","worst")
## rankhospital("AL","heart failure",3)
## rankhospital("MDE","heart failure",7)  ##should show as error because 'MDE' is not a valid state
