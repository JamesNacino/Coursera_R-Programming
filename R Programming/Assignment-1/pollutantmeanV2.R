pollutantmean <- function(directory, pollutant, id = 1:332)
{
  myfiles <- list.files(directory, full.names=TRUE)
  mydataframe <- data.frame()
  for (i in id)
  {
    mydataframe <- rbind(mydataframe, read.csv(myfiles[i]))
    #print(files[i])
  }
  

  if (pollutant == 'nitrate')
  {
    valdf = mydataframe[!is.na(mydataframe$nitrate),]
    mean(valdf[, pollutant])
  }
  else if (pollutant == "sulfate")
  {
    valdf = mydataframe[!is.na(mydataframe$sulfate),]
    mean(valdf[, pollutant])
  }
  
}