{
  eta_sigma <- .1
  eta_xi <- .1
  x <- log(seq(0.01, 10, 0.1))
  y <- (eta_xi - eta_sigma) + x

  ell <- y - eta_xi + (exp(-eta_sigma + 1)) * (log(1 + exp(2 * y)))
  df <- data.frame(x = x, ell = ell)
  plot(df, type = "l", col = "red")

  for (eta_xi in seq(.1, .5, .1))
  {
    y <- (eta_xi - eta_sigma) + x
    ell <- y - eta_xi + (exp(-eta_sigma + 1)) * (log(1 + exp(2 * y)))
    df <- data.frame(x = x, ell = ell)
    lines(x, ell, col = "green")
  }


  eta_xi <- 1
  for (eta_sigma in seq(.1, .5, .1)) {
    y <- (eta_xi - eta_sigma) + x
    ell <- y - eta_xi + (exp(-eta_sigma + 1)) * (log(1 + exp(2 * y)))
    df <- data.frame(x = x, ell = ell)
    lines(x, ell, col = "blue")
  }
}

{
  xexp <- .35
  # x <- x - xe
  z <- (exp((1 - xexp) * x) + exp(-xexp * x))*exp(-eta_xi) - eta_sigma
  # z <- ((1-xexp)*exp(x) + xexp* exp(-x))
  # z <- (exp((1-xexp)*x) + exp(-xexp*x))/2 + .1 * log(2)
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "purple")
  }
}





# y + log (1+e-2x) --------------------------------------------------------

{
  x <- seq(-2, 2, 0.1)
  y <- x + log(cosh(x)) + log(2)
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
}

{
  z <- 2 * x + log(1 + exp(-2 * x))
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "green")
  }
}


{
  xexp <- .08
  xe <- x - .1
  z <- (exp((1 - xexp) * xe)) + exp(-xexp * xe) - 1.2
  # z <- ((1-xexp)*exp(x) + xexp* exp(-x))
  # z <- (exp((1-xexp)*x) + exp(-xexp*x))/2 + .1 * log(2)
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "purple")
  }
}
# (1+e2y)^{-1} --------------------------------------------------------------
{
  x <- seq(-2, 2, 0.1)
  y <- (1+exp(-2*x))^{-1}
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
}

# log(1+e2y) --------------------------------------------------------------
{
  x <- seq(-2, 2, 0.1)
  y <- log(1+exp(2*x))
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
}

for (eta_xi in seq(.1, .5, .1)){
  y <- (exp(-eta_xi)+1)*log(1+exp(2*x))
  df <- data.frame(x = x, y = y)
  lines(x, y, col = "green")
}

{
  a1 <- 1.1
  a2 <- -.3
  # xe <- x - 1
  z <- 1.1*(exp(a1 * x)) + 1.25*exp(-a2 * x) - 1
  # z <- ((1-xexp)*exp(x) + xexp* exp(-x))
  # z <- (exp((1-xexp)*x) + exp(-xexp*x))/2 + .1 * log(2)
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "blue")
  }
}

# log cosh ----------------------------------------------------------------


{ x <- seq(-2, 2, 0.1)
  y <- log(cosh(x))
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
}


{
  j <- abs(x) - log(2) + exp(-2 * abs(x)) / 2
  k <- log(1 + exp(abs(x))) - log(2)
}
{
  df_j <- data.frame(x = x, j = j, k = k)
  lines(x, j, col = "purple")
}
lines(x, k, col = "green")

y_ <- ifelse(x > .9, 2 * x, z)
df_ <- data.frame(x = x, y = y_)
lines(x, y_, col = "blue")


# sech --------------------------------------------------------------------


{
  x <- seq(-5, 5, 0.1)
  y <- 1 / cosh(x)
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
  z <- 1 / (1 + x^2 / 2)
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "green")
  }
}


# tanh ------------------------------------------------------------------

{
  x <- seq(-5, 5, 0.1)
  y <- 1 + tanh(x)
  df <- data.frame(x = x, y = y)
  plot(df, type = "l", col = "red")
}

{
  z <- 2 / (1 + exp(-2 * x))
  {
    df_z <- data.frame(x = x, z = z)
    lines(x, z, col = "green")
  }
}
