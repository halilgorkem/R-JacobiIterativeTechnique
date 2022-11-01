x <-  c(1,2,3)
vectnorm <- function(x)
{
  xnorm <- sqrt(sum(x^2))
  return(xnorm)
}
vectnorm(x)
norm(x, type="2")

A <- matrix(c(10,-1,2,0,
              -1,11,-1,3,
              2,-1,10,-1,
              0,3,-1,8), nrow = 4, byrow = T)
b <-c(6,25,-11,15)

Jacobi <- function(A, b, TOL = 1e-5, maxIter = 100)
{
  n <- nrow(A)
  iter <- 1
  x <- rep(0,n)
  x0 <- rep(0,n)
  X <- data.frame(c(x0))
  #Adım2
  while (iter <= maxIter) 
  {
    #Adım3
    for(i in 1:n)
    {
      x[i] <- (b[i] - sum(A[i,-i]*x0[-i]))/A[i,i]
    }
    X[,iter+1] <- x
    #Adım4
    if(norm((x-x0), type = "2") < TOL)
    {
      print("Başarılı")
      return(list(x = as.vector(x), X, NbIter = iter))
    }
    #adım5
    iter <- iter + 1
    #Adım6
    x0 <- x
  }
  #Adım7
  print(x)
  stop("Maximum iterasyon sayısına ulaşıldı.")
}
Jacobi(A,b)
