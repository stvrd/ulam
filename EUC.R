# Euclids algorithm

library(tidyverse)

EUC_step <- function(s_i=c(0,0)){
  # input s_i = state and input values
  # output res = state, output and direction values
  if(all(s_i == c(0   ,0)))    return(c(0,0,"R"))
  if(all(s_i == c(0   ,1)))    return(c(1,1,"L"))
  if(all(s_i == c(1   ,0)))    return(c(10,1,"R"))
  if(all(s_i == c(1   ,1)))    return(c(1,1,"L"))
  if(all(s_i == c(10  ,0)))   return(c(1010,0,"R"))
  if(all(s_i == c(10  ,1)))   return(c(11,0,"R"))
  if(all(s_i == c(11  ,0)))   return(c(100,0,"R"))
  if(all(s_i == c(11  ,1)))   return(c(11,1,"R"))
  if(all(s_i == c(100 ,0)))  return(c(100,0,"R"))
  if(all(s_i == c(100 ,1)))  return(c(101,0,"R"))
  if(all(s_i == c(101 ,0)))  return(c(111,0,"L"))
  if(all(s_i == c(101 ,1)))  return(c(110,1,"L"))
  if(all(s_i == c(110 ,0)))  return(c(110,0,"L"))
  if(all(s_i == c(110 ,1)))  return(c(1,1,"L"))
  if(all(s_i == c(111 ,0)))  return(c(111,0,"L"))
  if(all(s_i == c(111 ,1)))  return(c(1000,1,"L"))
  if(all(s_i == c(1000,0))) return(c(1001,0,"L"))
  if(all(s_i == c(1000,1))) return(c(1000,1,"L"))
  if(all(s_i == c(1001,0))) return(c(10,0,"R"))
  if(all(s_i == c(1001,1))) return(c(1,1,"L"))
  if(all(s_i == c(1010,0))) return(c(0,0,"STOP"))
  if(all(s_i == c(1010,1))) return(c(1010,1,"R"))
  
}
EUC(c(0,0))

tape <- (c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,0))

EUC <- function(tape, sleep=F){
  i <- 1
  ret <- c(0,tape[i],"R")
  steps <- 0
  while(TRUE){
    steps <- steps+1
    ret <- EUC_step(c(ret[1],tape[i]))
    tape[i] <- as.numeric(ret[2])
    if(ret[3]=="R") i <- i+1
    if(ret[3]=="L") i <- i-1
    if(ret[3]=="STOP") break()
    # print(paste("state =",ret[1]))
    # print(paste("input =",ret[2]))
    # print(paste("i = ", i))
    cat(c("\r",tape))
    if(sleep) Sys.sleep(sleep)
  }
  cat("\n Steps = ", steps)
  
}

EUC(tape)
