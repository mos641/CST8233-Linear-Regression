# Lab 7
# Linear Regression

# function for linear regression
linReg <- function(){
  numN <- 5
  xVals <- c(20.5, 32.7, 51, 73.2, 95.7)
  yVals <- c(765, 826, 873, 942, 1032)
  options(digits=6)
  
  # # ask for input
  # numN <- readline(prompt = "Please enter the number of data pairs: ")
  # numN <- as.integer(numN)
  # 
  # # create vectors
  # xVals <- vector(mode = "numeric", length = numN)
  # yVals <- vector(mode = "numeric", length = numN)
  # 
  # inputNum <- 0
  # i <- 1
  # print("Enter the x-axis values:", quote = FALSE)
  # while (i <= numN){
  #   inputNum <- readline()
  #   inputNum <- as.numeric(inputNum)
  #   xVals[i] <- inputNum
  #   i <- i + 1
  # }
  # i <- 1
  # print("Enter the y-axis values:", quote = FALSE)
  # while (i <= numN){
  #   inputNum <- readline()
  #   inputNum <- as.numeric(inputNum)
  #   yVals[i] <- inputNum
  #   i <- i + 1
  # }
  
  # variables for calculation
  i <- 1
  xSum <- 0
  ySum <- 0
  xySum <- 0
  xSqSum <- 0
  
  # calculate different values for a0 and a1
  while (i <= numN){
    xSum <- xSum + xVals[i]
    ySum <- ySum + yVals[i]
    xySum <- xySum + (xVals[i] * yVals[i])
    xSqSum <- xSqSum + (xVals[i] * xVals[i])
    
    i <- i + 1
  }
  
  # calculate constants a0 and a1
  a1 <- ((numN * xySum) - (xSum * ySum))/((numN * xSqSum) - (xSum * xSum))
  a0 <- (ySum / numN) - (a1 * (xSum / numN))
  
  # print the equation
  print("The best linear fit is of the form:", quote = FALSE)
  print(paste("y =", a1, "x +", a0), quote = FALSE)
  
  # calculated fitted and residual sum
  yFit <- vector(mode = "numeric", length = numN)
  rss <- 0
  i <- 1
  while (i <= numN){
    yFit[i] <- (a0 + (a1 * xVals[i]))
    rss <-  rss + ((yVals[i] - yFit[i]) * (yVals[i] - yFit[i]))
    i <- i + 1
  }
  
  # create and print dataframe
  linRegDF<- data.frame("x" = c(xVals), "y(observed)" = c(yVals), "y(fitted)" = c(yFit))
  print(linRegDF)
  
  # print the residual sum of squares
  print(paste("The sum of the square of the residuals Sr =", rss), quote = FALSE)
}

# call function
linReg()




