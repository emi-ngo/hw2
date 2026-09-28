```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, fig.align = "center")
library(readr)
```

```{r}
## Read CSV file into data frame
#Note: AI was used to debug and turn code into functions for efficiency with 4 images
x1 <- read.csv("~/hw2/HW2imagefiles/image1.csv")

## Convert data frame into matrix
x <- data.matrix(x1)

image_compression <- function(x, k) {
  V <- t(x) %*% x
  eig <- eigen(V)
  U <- eig$vectors
  Z <- x %*% U
  U_k <- U[, 1:k, drop = FALSE]
  Z_k <- Z[, 1:k, drop = FALSE] 
  X_k <- Z_k %*% t(U_k)
  error_k <- norm(x - X_k, type = "F") #F for Frobenius norm
  list(approximation = X_k, 
       error = error_k, 
       eigenvalues = eig$values, 
       eigenvectors = U, 
       scores = Z)
}
```

```{r}
plot_image <- function(mat, main = "") {
  image(
    t(mat[nrow(mat):1, ]),
    col = gray.colors(256, start = 0, end = 1),
    axes = FALSE,
    asp = 1,
    main = main
  )
}

plot_grid <- function(x) {
  par(mfrow = c(3, 4), mar = c(1, 1, 2, 1))
  plot_image(x, main = "Original")
  for (k in 1:10) {
    plot_image(image_compression(x, k)$approximation,
               main = paste("k =", k))
  }
  par(mfrow = c(1, 1))
}

plot_errors <- function(x) {
  eig <- eigen(t(x) %*% x)
  U <- eig$vectors
  Z <- x %*% U
  max_k <- ncol(x)
  errors <- sapply(1:max_k, function(k) {
    X_k <- Z[, 1:k, drop = FALSE] %*% t(U[, 1:k, drop = FALSE])
    norm(x - X_k, type = "F")
  })
  par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))
  plot(1:max_k, errors, type = "b", pch = 19,
       xlab = "k", ylab = "Frobenius norm error",
       main = "Approximation Error vs. k")
  grid()
}

plot_first_pc <- function(x) {
  res <- image_compression(x, 1)
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  plot(res$eigenvectors[, 1], type = "l",
       xlab = "Column index", ylab = "Eigenvector value",
       main = "First Eigenvector")
  abline(h = 0, lty = 2)
  plot(res$scores[, 1], type = "l",
       xlab = "Row index", ylab = "PC score",
       main = "First PC Score")
  abline(h = 0, lty = 2)
  par(mfrow = c(1, 1))
}
```

## Image 1

```{r img1-load, message = FALSE}
x <- read_csv("~/hw2/HW2imagefiles/image1.csv")
x <- data.matrix(x)
```
### Original image and first 10 approximations

```{r img1-grid, fig.width=10, fig.height=8}
plot_grid(x)
```

### Approximation error vs. k

```{r img1-error, fig.width=7, fig.height=5}
plot_errors(x)
```

### First eigenvector and first PC score

```{r img1-pc, fig.width=10, fig.height=4}
plot_first_pc(x)
```

**Interpretation:**
  
  PLACEHOLDER - interpretation text goes here

**Choice of k:**
  
  PLACEHOLDER - choice of k text goes here

## Image 2

```{r img2-load, message = FALSE}
x <- read_csv("~/hw2/HW2imagefiles/image2.csv")
x <- data.matrix(x)
```

### Original image and first 10 approximations

```{r img2-grid, fig.width=10, fig.height=8}
plot_grid(x)
```

### Approximation error vs. k

```{r img2-error, fig.width=7, fig.height=5}
plot_errors(x)
```

### First eigenvector and first PC score

```{r img2-pc, fig.width=10, fig.height=4}
plot_first_pc(x)
```

**Interpretation:**
  
  PLACEHOLDER - interpretation text goes here

**Choice of k:**
  
  PLACEHOLDER - choice of k text goes here

## Image 3

```{r img3-load, message = FALSE}
x <- read_csv("image3.csv")
x <- data.matrix(x)
```

### Original image and first 10 approximations

```{r img3-grid, fig.width=10, fig.height=8}
plot_grid(x)
```

### Approximation error vs. k

```{r img3-error, fig.width=7, fig.height=5}
plot_errors(x)
```

### First eigenvector and first PC score

```{r img3-pc, fig.width=10, fig.height=4}
plot_first_pc(x)
```

**Interpretation:**
  
  PLACEHOLDER - interpretation text goes here

**Choice of k:**
  
  PLACEHOLDER - choice of k text goes here

## Image 4

```{r img4-load, message = FALSE}
x <- read_csv("image4.csv")
x <- data.matrix(x)
```

### Original image and first 10 approximations

```{r img4-grid, fig.width=10, fig.height=8}
plot_grid(x)
```

### Approximation error vs. k

```{r img4-error, fig.width=7, fig.height=5}
plot_errors(x)
```

### First eigenvector and first PC score

```{r img4-pc, fig.width=10, fig.height=4}
plot_first_pc(x)
```

**Interpretation:**
  
  PLACEHOLDER - interpretation text goes here

**Choice of k:**
  
  PLACEHOLDER - choice of k text goes here