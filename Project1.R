library("ggplot2")
die <- 1:6

probs = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8)

roll <- function() {
  dice<-sample(die,size = 2,replace = TRUE, prob = probs)
  sum(dice)
}


roll2 <- function(bones = 1:6) {
  dice <- sample(bones,2,TRUE)
  sum(dice)
}

x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y <- x^3

qplot(x,y)

x <- c(1, 2, 2, 2, 3, 3)
qplot(x,binwidth=1)

x2 <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4)
qplot(x2, binwidth = 1)

x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
qplot(x3, binwidth = 1)

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)

names(die) <- c("one", "two", "three", "four", "five", "six")
dim(die) <- c(3,1,2)

m <- matrix(die, nrow = 2, byrow = TRUE)
