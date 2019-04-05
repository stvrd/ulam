
# Euclids algorithm for unary digits
library(tidyverse)


# ----------- Functions: --------------------

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
  if(length(x)==1) x <- as.numeric(strsplit(as.character(x), "")[[1]])
  if(any(x>1)) stop("x not binary")
  pow <- 2^((length(x)-1):0)
  sum(x*pow)
}


# ==============================
# Expanded binary notation, p.55
# ==============================

contrapt <- function(s){
  # Inverse function of expand
  v <- grepl("0",s)
  if(!v[1]) v <- c(TRUE,v)
  end <- which(v)
  start <- dplyr::lag(end)
  v2 <- mapply( seq, start[-1], end[-1])
  sapply(v2,function(x) length(x)-2)
}

expand <- function(s2){
  # Inverse function of contrapt
  s3 <- sapply(s2,function(x) rep(TRUE,x))
  as.numeric(c(sapply(s3,function(x) c(FALSE,x)) %>% unlist,0))
}

d2expanded <- function(v){
  # always add a comma at the end
  v2 <- c(strsplit(paste(sapply(v,d2b),collapse=","),"") %>% unlist(),",")
  if(any(v2>2)) stop("floating point problem ...")
  paste(as.numeric(sapply(v2,function(x) switch(x,"0"=0,"1"=10,","=110))),collapse = "")
}

# ----- Algorithms: -------------------

# Euclidean algorithm (finding biggest common divider) on unary numbers:
EUC <- data.frame(state=c(0,1,10,11,100,101,110,111,1000,1001,1010))
EUC$zero <- list(c(0,0,"R"),c(10,1,"R"),c(1010,0,"R"),c(100,0,"R"),c(100,0,"R"),c(111,0,"L"),c(110,0,"L"),c(111,0,"L"),c(1001,0,"L"),c(10,0,"R"),c(0,0,"STOP"))
EUC$one <- list(c(1,1,"L"),c(1,1,"L"),c(11,0,"R"),c(11,1,"R"),c(101,0,"R"),c(110,1,"L"),c(1,1,"L"),c(1000,1,"L"),c(1000,1,"L"),c(1,1,"L"),c(1010,1,"R"))
EUC[[3,2]]

# Multiply-by-2 algorithm on expanded binary numbers:
XN2 <- data.frame(state=c(0,1,10,11))
XN2$z <-  list(c(0,0,"R"),c(0,1,"R"),c(11,1,"R"),c(0,1,"STOP"))
XN2$one <- list(c(1,0,"R"),c(10,0,"R"),NA,NA)
XN2[[3,2]]


# --------- UNIVERSAL TURING MACHINE ----------------

UTM <- function(algorithm,tape, sleep=F, append=F){
  if(!is.numeric(tape)) tape <- as.numeric(strsplit(tape,"")[[1]])
  if(append) tape <- c(rep(0,append), tape, rep(0,append))
  i <- 1
  ret <- c(0,0,"R")
  steps <- 0
  while(TRUE){
    steps <- steps+1
    state <- b2d(ret[1]) + 1
    input <- tape[i] + 2
    # ret <- EUC(c(ret[1],tape[i]))
    ret <- algorithm[[state,input]]
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
  return(tape)
  
}

# ---- Testing functions: ---------
d2b(2047)
b2d(11111000100)

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

# ------ Testing algorithms --------------

tape <- (c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,0))
tape <- (c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0,0))
res <- UTM(EUC,tape,sleep = 0.01)
sum(res)
# EUC algorithm works!


tape <- d2expanded(8000)
res1 <- UTM(algorithm = XN2,tape = tape,sleep = 0.01,append=5)
res2 <- contrapt(res1)
b2d(res2[1:(which(res2 == 2)-1)])
# XN2 algorithm works!
