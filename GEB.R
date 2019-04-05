isPrime <- function(x){
  div <- 2:ceiling(sqrt(x))
  !any(x %% div == 0)
}

isEven <- function(x){
  if(x %% 2 == 0) TRUE
  else FALSE
}

isPrime(6)

# Number Game of GEB, p.400
n <- 15
play <- function(n,vec=n){
  # print(n)
  vec <- c(vec,n)
  if (n == 1) vec
  else{
  if (isEven(n)) play(n/2,vec)
  else play(3*n+1,vec)
  }}

play <- function(n){
  # print(n)
  vec <- NULL
  while(n!=1){
    vec <- c(vec,n)
    if (isEven(n)) n <- n/2
    else n <- 3*n+1}
  vec}

# n = Starting Number
n <- 18
plot(play(22))
for(i in 1:15){print(play(i))}
for(i in 1:15)print(play(i))
