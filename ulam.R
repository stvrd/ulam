system.time(
)
library(tidyverse)
library(magrittr)
# Prime numbers

GivePrimes <- function(end,start=1){
  nn <- 1:end
  primes <- vector(length = end-start+1)
  for (i in nn[start:end]) {
    if (i==2) primes[i] <- {TRUE
    } else if (min(i %% nn[2:(i-1)]) != 0) { primes[i] <- TRUE }
  }
  primes
}

GivePrimes(end=11)
pr <- GivePrimes(1e4) 
length(which(pr))

create.ulam <- function(n=5,boolean=F) {
  x <- ceiling(n/2+0.25)
  y <- ceiling(n/2)
  ulam <- matrix(nrow = n, ncol=n)
  dir <- c("r","u","l","d")
  n.rep <- rep(1:n,each=2)
  dirs <- dir %>% rep(length.out=length(n.rep)) %>% rep(n.rep)
  if(boolean) {
    vec <- GivePrimes(end=n^2)
    ulam[x,y] <- vec[1] 
  } else {ulam[x,y] <- 1} 
  
  for(i in 1:n^2) {
    if(i%%10==0) print(i)
    if         (dirs[i]=="r") { y <- y+1
    }  else if (dirs[i]=="u") { x <- x-1
    }  else if (dirs[i]=="l") { y <- y-1
    }  else if (dirs[i]=="d") { x <- x+1
    }  else stop()
    if(boolean) {ulam[x,y] <- vec[i+1]
    } else {ulam[x,y] <- i+1}
  }
  ulam
}

u <- create.ulam(n = 200,boolean = T)
image(u)



