# Prime numbers ------
system.time()
library(magrittr)


# ------------- Naive Approach ------------------------------------------
GivePrimes <- function(end,start=1,verbose=F){
  nn <- 1:end
  primes <- vector(length = end-start+1)
  for (i in nn[start:end]) {
    if(verbose) {if(i%%1000==0) cat(i," ")}
    if (i==2) primes[i] <- {TRUE
    } else if (min(i %% nn[2:(i-1)]) != 0) { primes[i] <- TRUE }
  }
  primes
}

GivePrimes(50)
pr <- GivePrimes(1e4,verbose = T) 


#  ----- Sieve of Erastosthenes ----------------------

int main()
{
  int num;
  cin >> num;
  // num % 2 computes the remainder when num is divided by 2
  if ( num % 2 == 0 )
  {
    cout << num << " is even ";
  }
  
  return 0;
}

cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

create.matrix <- function(vec) {
  n <- sqrt(length(vec))
  x <- ceiling(n/2+0.25)
  y <- ceiling(n/2)
  ulam <- matrix(nrow = n, ncol=n)
  dir <- c("r","u","l","d")
  n.rep <- rep(1:n,each=2)
  dirs <- dir %>% rep(length.out=length(n.rep)) %>% rep(n.rep)
  ulam[x,y] <- -1 
  
  for(i in 1:n^2) {
    if (dirs[i]=="r") { y <- y+1
    }  else if (dirs[i]=="u") { x <- x-1
    }  else if (dirs[i]=="l") { y <- y-1
    }  else if (dirs[i]=="d") { x <- x+1
    }  else stop()
    ulam[x,y] <- vec[i+1]
    ulam
  }
  ulam
}

ulam <- function(n,plot=F) {
  par(mar=c(2,1,2,1), mgp=c(0.4,0,0))
  vec <- rep(TRUE,length = n)
  vec[1:2] <- c(F,T)
  for(i in 2:n){
    if(!plot & i %% 100 == 0) print(i)
    if(vec[i]) {
      vec2 <- 1:n^2 %% i != 0
      vec2[1:i] <- T
      vec <- vec & vec2 
      
      # Plot image
      if(plot){   
        tmp <- vec[i]
        vec[i] <- -1
        image(create.matrix(vec),axes=F,
              col=c("yellow","White","Blue"),
              xlab=paste("i =",i),
              main="Ulam Spiral")
        vec[i] <- tmp
        Sys.sleep(0.1)
      }
    }
  }
  if(!plot) vec
}

ulam(100)
ulam(20,plot=T)

which(ulam(1e3))

# -------- Frequency of Primes: --------------------
n <- 1e4
pr <- ulam(n)
length(which(pr))
freq <- vector(length = n)
x <- 1:n*n
for(i in x){
  if(i %% 1e5==0) print(i)
  freq[i/1000] <- length(which(pr[1:i]))/i
}


plot(x,1/log(x),lty=2,lwd=2,type="l",col="blue",xlab="",ylab="")
points(x,freq,type="p",pch=16,cex=0.5)
title("Frequency of Prime Numbers")


# ---------- Fermat primality test -------

np <- which(!pr)[2:50]
a <- 2:10

fermat <- outer(X = a,Y = np-1, FUN="^")
f2 <- apply(fermat,2, FUN = mod(np))
f2 == 1


a^(np-1) %% np
