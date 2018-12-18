# Oleh :  Muhammad Pandu Widodo (17523055)
          Dimas Fajar Imanto (17523193)

library (matlib)

trapezoid <- function(f, x, y){
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')}
  
  h <- y - x
  
  fzdz <- (h / 2) * (f(x) + f(y))
  
  return(fzdz)
}


f <- function(z) {
  return(z^2-z*sin(z))
}

trapezoid(f, 2, 6)

simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
  
  return(s)
}

f <- function(z) {
  return(z^2-z*sin(z))
}

simpsons.rule(f, 2, 6)