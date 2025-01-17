# Helper file for various functions that produce penalty shape figures

Lasso <- function(theta, l) {
  l * abs(theta)
}
dLasso <- function(theta, l=1) {
  rep(l, length(theta))
}
soft <- function(z, l) {
  (abs(z) > l) * (z - l * sign(z))
}


Ridge <- function(theta, l=1) {
  l * theta^2
}
dRidge <- function(theta, l=1) {
  l * abs(theta)
}

SCAD <- function(theta, l, a=3.7) {
  theta <- abs(theta)
  (theta <= l) * theta * l +
    (theta > l & theta <= a * l) * (a*l*theta - (theta^2 + l^2) / 2) / (a - 1) +
    (theta > a * l) * (l^2 * (a^2-1)) / (2 * (a-1))
}
dSCAD <- function(theta, l, a=3.7) {
  theta <- abs(theta)
  (theta <= l)*l + ((theta > l) & (theta < a*l)) * ((a*l-theta) / (a-1))
}
firmSCAD <- function(z, l, g=3.7) {
  (abs(z) > g*l)*z +
    (abs(z) > 2*l & abs(z) <= g*l) * (soft(z, g*l / (g-1)) / (1 - 1 / (g-1))) +
    (abs(z) <= 2*l)*soft(z, l)
}

MCP <- function(theta, l, a=3.7) {
  T <- length(theta)
  val <- numeric(T)
  for (i in 1:T) {
    x <- abs(theta[i])
    val[i] <- (x < a*l) * (l*x - x^2 / (2*a)) + (x >= a*l) * (1/2)*a*l^2
  }
  val
}
dMCP <- function(theta, l, a=3.7) {
  theta <- abs(theta)
  (theta <= a*l) * (l - theta/a)
}
firmMCP <- function(z, l, g=3.7) {
  (abs(z) > g*l)*z + (abs(z) <= g*l) * (soft(z, l) / (1-1/g))
}
