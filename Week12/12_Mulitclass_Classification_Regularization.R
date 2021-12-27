'''
2021-2 - Data Mining Practicum
Prof. Hyebong Choi

Ch 9. Multi-class classification and regularization
'''

library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ucminf)
library(testthat)

path <- 'https://raw.githubusercontent.com/Paul-scpark/Data_Mining_Practicum/main/data/'
chiptest <- paste0(path, 'ex2data2.txt') %>% 
  read.csv(header = F) %>% 
  set_colnames(c('test_1', 'test_2', 'passed'))

p <- chiptest %>% 
  ggplot(aes(x = test_1, y = test_2)) + 
  geom_point(aes(col = factor(passed), shape = factor(passed))) + 
  scale_color_manual('accepted', values = c('red', 'blue'), labels = c('rejected', 'accepted')) + 
  scale_shape_manual('accepted', values = c(4, 1), labels = c('rejected', 'accepted')) + 
  theme_classic() + xlab('Microchip test1') + ylab('Microchip test2')

## Simple Logistic Regression
# Hypothesis Function
g <- function(z){1 / (1 + exp(-z))}
h <- function(theta, X){g(X %*% theta)}

# Cost function
J <- function(X, y, theta){
  (1 / length(y)) * sum(-y * log(h(theta, X)) - (1 - y) * log(1 - h(theta, X)))
}

# Gradient
gR <- function(X, y, theta){
  error <- h(theta, X) - y
  delta <- t(X) %*% error / length(y)
  return (delta)
}

## Training with non-linear optimizer (ucminf)
theta <- matrix(c(0, 0, 0), ncol = 1)
X <- chiptest[, 1:2] %>% as.matrix %>% cbind(1, .)
y <- chiptest[, 3] %>% as.matrix

ucminf_out <- ucminf(
  par = theta, 
  fn = function(t) J(X, y, t), 
  gr = function(t) gR(X, y, t)
)

## Trained Model
ucminf_out

theta <- ucminf_out$par
boundary <- function(x){
  (-1 / theta[3]) * (theta[2] * x + theta[1])
}

p + stat_function(fun = boundary, colour = 'black') + 
  coord_cartesian(xlim = c(-0.9, 1.2), ylim = c(-0.9, 1.2))

## Logistic Regression with Non-linear terms
map_feature <- function(X1, X2, degree){
  counter = 0
  for (i in 1:degree){
    for (j in 0:i){
      counter <- counter + 1
    }
  }
  
  out_matrix <- matrix(nrow = length(X1), ncol = counter)
  names_vec <- vector(length = counter)
  
  counter = 0
  for (i in 1:degree){
    for (j in 0:i){
      counter <- counter + 1
      out_matrix[, counter] <- ((X1^(i-j)) * (X2^j))
      
      names_vec[counter] <- paste("X1^", i-j, "*X2^", sep = "")
    }
  }
  
  out_matrix <- cbind(1, out_matrix)
  colnames(out_matrix) <- c(1, names_vec)
  return (out_matrix)
}

## Feature mapping
degree <- 6
poly <- map_feature(
  chiptest$test_1, 
  chiptest$test_2, 
  degree
)

poly %>% colnames()

## Training
theta <- rep(0, ncol(poly))
y <- chiptest$passed

ucminf_out <- ucminf(
  par = theta, 
  fn = function(t) J(poly, y, t), 
  gr = function(t) gR(poly, y, t)
)

ucminf_out$par
ucminf_out$value
ucminf_out$message

## Visualization
draw_boundary <- function(xy, theta, degree){
  u <- rep(xy, times = length(xy))
  v <- rep(xy, each = length(xy))
  
  cbind(u, v, z = NA) %>% 
    as.data.frame() %>% 
    tbl_df() %>% 
    mutate(z = h(theta, map_feature(u, v, degree)) %>% round)
}

boundary <- draw_boundary(
  seq(-1.5, 1.5, length = 500), 
  ucminf_out$par, 
  degree
)

p + geom_contour(
  data = boundary, 
  aes(x = u, y = v, z = z),
  bins = 1
) + coord_cartesian(xlim = c(-0.9, 1.2), ylim = c(-0.9, 1.2))

## Regularization
Jv_reg <- function(X, y, theta, lambda){
  m <- length(y)
  theta1 <- theta # Remove first value (theta_0)
  theta1[1] <- 0
  
  # Crossproduct is equivaelent to theta[-1]^2
  reg <- (lambda / (2*m)) * crossprod(theta1, theta1)
  
  # Create regularisation term
  -(1/m) * crossprod(
    c(y, 1-y), c(log(h(theta, X)), log(1 - h(theta, X)))
  ) + reg
}

gRv_reg <- function(X, y, theta, lambda){
  m <- length(y)
  reg <- (lambda / m) * theta
  error <- h(theta, X) - y
  delta <- crossprod(X, error) / m
  return (delta + reg)
}

Jv_reg(poly, y, theta, 0)

err <- function(y, pred){
  # Should really be implementing more unit tests throughout
  test_that(
    "Predicdtion and actual are the same length",
    expect_equal(length(y), length(pred))
  )
  error <- 1 - mean(y == (pred >= 0.5)) 
  error <- round(error, 2)
  return (error)
}

reg_lr <- function(X, y, theta, lambda){
  ucminf_out <- ucminf(
    par = theta, 
    fn = function(t) Jv_reg(X, y, t, lambda), 
    gr = function(t) gRv_reg(X, y, t, lambda)
  )
  error <- err(y, h(ucminf_out$par, X))
  
  return (
    list(theta = as.vector(ucminf_out$par), error = error)
  )
}

reg_lr_out <- reg_lr(X = poly, y = y, theta = theta, lambda = 1)

boundary <- draw_boundary(
  seq(-1.5, 1.5, length = 500), 
  reg_lr_out$theta, 
  degree
)

p + geom_contour(data = boundary, 
                 aes(x = u, y = v, z = z), 
                 bins = 1) + 
  coord_cartesian(xlim = c(-0.9, 1.2), ylim = c(-0.9, 1.2))

lambda <- c(0, 0.0001, 0.001, 0.01, 0.1, 1)
out_mat <- matrix(nrow = 500, ncol = length(lambda))
colnames(out_mat) <- paste(lambda, sep = "")
out_mat <- cbind(boundary[, 1:2], out_mat) %>% as.matrix()

# Add two 0s to the beginnig of the vector to make life easier in the for loop when referencing lambda
lambda <- c(0, 0, lambda)
for (i in 3:ncol(out_mat)){
  out <- draw_boundary(
    seq(-1.5, 1.5, length = 500),
    reg_lr(
      X = poly, 
      y = y, 
      theta = theta, 
      lambda = lambda[i]
    )$theta, 
    degree
  ) %$% z %>% as.vector()
  out_mat[, i] <- out
}

out_mat %>% 
  data.frame() %>% 
  gather(key, value, 3:ncol(out_mat)
         ) %>% 
  tbl_df %>% 
  ggplot(aes(x = u, y = v)) + 
  geom_contour(aes(z = value), bins = 1) + 
  facet_wrap(~ key, ncol = 2) + 
  geom_point(data = chiptest, 
             aes(x = test_1, y = test_2, colour = factor(passed), shape = factor(passed))) + 
  xlab('Microchip test 1') + ylab('Microchip test 2') + 
  coord_cartesian(xlim = c(-0.9, 1.2), ylim = c(-0.9, 1.2))
