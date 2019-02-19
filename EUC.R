

# Euclids algorithm for unary digits
library(tidyverse)

EUC <- function(s_i=c(0,0)){
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

==========================
#UNIVERSAL TURING MACHINE
==========================

UTM <- function(tape, sleep=F){
  i <- 1
  ret <- c(0,tape[i],"R")
  steps <- 0
  while(TRUE){
    steps <- steps+1
    ret <- EUC(c(ret[1],tape[i]))
    tape[i] <- as.numeric(ret[2])
    cat(c("\r", tape[1:(i-1)], paste0("[", tape[i], "]"), tape[(i+1):length(tape)],collapse = ""))
    if(ret[3]=="R") i <- i+1
    if(ret[3]=="L") i <- i-1
    if(ret[3]=="STOP") break()
    # print(paste("state =",ret[1]))
    # print(paste("input =",ret[2]))
    # print(paste("i = ", i))
    if(sleep) Sys.sleep(sleep)
  }
  cat("\n Steps = ", steps)
  
}

tape <- (c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,0))
tape <- (c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0,0))
UTM(tape,sleep = 0.5)


# Decimal 2 binary and binary 2 decimal
# Decimal 2 binary and binary 2 decimal

d2b <- function(x){
  # inverse function of b2d
  options(scipen=999)
  if(!is.numeric(x)) stop("x not numeric")
  if(x <2) {return(x)
  } else {
    
  pow <- 2^(floor(log(x,base = 2)):0)
  binary <- vector(length = length(pow))
  binary[1] <- 1
  for(i in 2:length(pow)) {
    # i <- i+1
    interm <- x-sum(pow*binary)
    if(interm>=pow[i]) binary[i] <- 1
  }
  as.numeric(paste(binary,collapse = ""))
  }
}

b2d <- function(x){
  # inverse function of d2b
  x <- as.numeric(strsplit(as.character(x), "")[[1]])
  if(any(x>1)) stop("x not binary")
  pow <- 2^((length(x)-1):0)
  sum(x*pow)
}

d2b(2e5)
b2d(1001)

# ==============================
# Expanded binary notation, p.55
# ==============================

contrapt <- function(s){
  # Inverse function of expand
  v <- grepl("0",s)
  if(!v[1]) v <- c(TRUE,v)
  end <- which(v)
  start <- lag(end)
  v2 <- mapply( seq, start[-1], end[-1])
  sapply(v2,function(x) length(x)-2)
  }

expand <- function(s2){
  # Inverse function of contrapt
  s3 <- sapply(s2,function(x) rep(TRUE,x))
  as.numeric(c(sapply(s3,function(x) c(FALSE,x)) %>% unlist,0))
}

d2expanded <- function(v){
  v2 <- c(strsplit(paste(sapply(v,d2b),collapse=","),"") %>% unlist(),",")
  if(any(v2>2)) stop("floating point problem ...")
  paste(as.numeric(sapply(v2,function(x) switch(x,"0"=0,"1"=10,","=110))),collapse = "")
}
                          
# Problem with floating point:
c(01000101101010110100011101010111100110)
s <- as.numeric(unlist(strsplit("01000101101010110100011101010111100110","")))

s2 <- contrapt(s)
expand(s2)
expand(c(1,3))

v <- c(5,13,0,1,1,4)
d2expanded(v)
d2expanded(13)

d2expanded(c(6,8))
d2expanded(c(1583169,8610))
                          

# TUM on expanded binary numbers:

XN2 <- tribble( ~state, ~z,      ~one,
         0,  c(0,0,"R"),  c(1,0,"R"),
         1,  c(0,1,"R"),  c(10,0,"R"),
         10, c(11,1,"R"), NA,
         11, c(0,1,"STOP"),NA)


