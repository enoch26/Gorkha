# A function to pad zeros around a matrix of its original dimensions 
padzeros2d <- function(mat) {
  padzeros <- function(mat, nzeros, side = "both") {
    if (is.vector(mat) == TRUE) mat <- t(as.matrix(mat))

    M.zeros <- matrix(0, nrow(mat), nzeros)

    newmat <- switch(side,
      both = cbind(M.zeros, mat, M.zeros),
      left = cbind(M.zeros, mat),
      right = cbind(mat, M.zeros)
    )
  }
  # how many rows/cols of zeroes are used to pad.
  width <- dim(mat)[1]
  pad <- ceiling(width / 2)
  # TODO do.call

  x_pad <- t(padzeros(mat = mat, nzeros = pad, side = "both"))
  x_pad <- t(padzeros(mat = x_pad, nzeros = pad, side = "both"))
  return(x_pad)
}
diag(3)
focalMat(r, 2, "Gauss")

kernel <- gsignal::conv2(padzeros2d(matrix(1, 2, 2)), matrix(1, 2, 2), shape = "valid")
kernel2 <- gsignal::conv2(padzeros2d(kernel), kernel, shape = "valid")

norm_kernel <- kernel / sum(abs(kernel))
norm_kernel2 <- kernel2 / sum(abs(kernel2))



gsignal::conv2(1, matrix(1, 2, 2), shape = "full")
gsignal::conv2(1, matrix(1, 2, 2), shape = "same")
gsignal::conv2(1, matrix(1, 2, 2), shape = "valid")

  
imagine::convolution2D(padzeros2d(matrix(1, 1, 1)), matrix(1, 2, 2))
imagine::convolution2D(matrix(1, 1, 1), matrix(1, 2, 2))
imagine::convolution2D(padzeros2d(padzeros2d(matrix(1, 1, 1))), matrix(1, 2, 2))

imagine::convolution2D(padzeros2d(matrix(1, 1, 1)), matrix(1, 2, 2), normalize = FALSE, na_only = FALSE)
imagine::convolution2D(padzeros2d(matrix(1, 1, 1)), matrix(1, 2, 2), normalize = FALSE, na_only = FALSE)


matrix(c(1, 2, 1, 2, 4, 2, 1, 2, 1), nrow = 3)
padzeros2d(padzeros2d(matrix(1, 1, 1)))

padzeros2d(matrix(1, 3, 3))

kernelsmooth <- function(x, kern, norm = TRUE) {
  # how many rows/cols of zeroes are used to pad.
  width <- dim(kern)[1]
  pad <- floor(width / 2)

  # record the width and height the input data matrix
  x_w <- ncol(x)
  x_h <- nrow(x)

  # Are we normalizing the kernel?
  if (norm == TRUE) {
    k <- kern / sum(abs(kern))
  } else {
    k <- kern
  }

  # pad all around the matrix an equal width of zeros
  x_pad <- t(padzeros(data = x, nzeros = pad, side = "both"))
  x_pad <- t(padzeros(data = x_pad, nzeros = pad, side = "both"))

  # Pre-allocate the final (smoothed) data matrix
  s <- matrix(0, nrow = x_h, ncol = x_w)

  # Pre-allocate a temporary matrix for the iterative calculations
  temp <- matrix(0, width, width)

  # Loop through the data to apply the kernel.
  for (col in 1:x_w) {
    for (row in 1:x_h) {
      temp <- x_pad[row:(row + width - 1), col:(col + width - 1)]
      s[row, col] <- sum(k * temp)
    }
  }

  # return the smoothed data
  return(s)
}

kernelsmooth(x = matrix(1, 3, 3), kern = matrix(1, 2, 2), norm = TRUE)


debugonce(kernelsmooth)
