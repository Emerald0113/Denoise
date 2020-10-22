#This helper function (fwht2d) is provided by professor Keith Knight.
fwht2d <- function(x) {
  h <- 1
  len <- ncol(x)
  while (h < len) {
    for (i in seq(1,len,by=h*2)) {
      for (j in seq(i,i+h-1)) {
        a <- x[,j]
        b <- x[,j+h]
        x[,j] <- a + b
        x[,j+h] <- a - b
      }
    }
    h <- 2*h
  }
  h <- 1
  len <- nrow(x)
  while (h < len) {
    for (i in seq(1,len,by=h*2)) {
      for (j in seq(i,i+h-1)) {
        a <- x[j,]
        b <- x[j+h,]
        x[j,] <- a + b
        x[j+h,] <- a - b
      }
    }
    h <- 2*h
  }
  x
}



hard <- function(x, lambda){
  xhat <- fwht2d(x)
  xhat_thre <- ifelse(abs(xhat)>=lambda,xhat,0)
  x <- fwht2d(xhat_thre)/ncol(xhat_thre)^2
  x
}

soft <- function(x,lambda){
  xhat <- fwht2d(x)
  xhat_thre <- sign(xhat)*pmax(abs(xhat)-lambda, 0)
  x <- fwht2d(xhat_thre)/ncol(xhat_thre)^2
  x
}

design <- matrix(scan("~/desktop/design.txt"), ncol=256,byrow = T)
colours <- grey(seq(0,1,length=256))
image(design, axes=F, col = colours)

hard5 <- hard(design, 5)
image(hard5, axes=F, col = colours)
hard10 <- hard(design, 10)
image(hard10, axes=F, col = colours)
hard50 <- hard(design, 50)
image(hard50, axes=F, col = colours)
hard100 <- hard(design, 100)
image(hard100, axes=F, col = colours)

soft5 <- soft(design, 5)
image(soft5, axes=F, col = colours)
soft10 <- soft(design, 10)
image(soft10, axes=F, col = colours)
soft50 <- soft(design, 50)
image(soft50, axes=F, col = colours)
soft100 <- soft(design, 100)
image(soft100, axes=F, col = colours)

