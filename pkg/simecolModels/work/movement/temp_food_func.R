## simple functions to emulate vertical gradients of temperature and food
## in a lake

z <-seq(0, 40, 0.1)

temp <- function(z, zmix = 10, k= -0.2) {
  ifelse(z < zmix, 20, 4 + 16 * exp(k * (z - zmix)))
}

plot(temp(z), z, ylim=c(40,0), type="l")

food <- function(z) {
  dnorm(z, mean=5, sd=2) + 0.2 * dnorm(z, mean=15,sd=1)
}

plot(food(z), z, ylim=c(40,0), type="l")
