hand <- c("ace", "king", "queen", "jack", "ten")

hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades", "spades", "spades", "spades")
matrix(hand1, ncol = 2)

card <- list("ace", "hearts", 1)

deal <- function(cards) {
  card <- cards[1,]
  cards <- cards[-1,]
  card
}



shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random,]
}

deck2 <- deck

deck2$value[c(13,26,39,52)] <- 14

deck3 <- shuffle(deck)

deck2$face == 'ace'
deck3$value[deck3$face == 'ace'] <- 14

deck4 <- deck
deck4$value <- 0
deck4$value[deck4$suit == 'hearts'] <- 1

queenOfSpades <- deck4$face == 'queen' & deck4$suit == 'spades'

deck4$value[queenOfSpades] <- 13


w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

w > 0
x > 10 & x < 20
y == 'February'
dayoftheweek <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
all(z %in% dayoftheweek)

deck5 <- deck
facecard <- deck5$face %in% c('king','queen','jack')
deck5$value[facecard] <- 10

deck5$value[deck5$face == 'ace'] <- NA

library(devtools)

as.environment("package:stats")

show_env <- function(){
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}

DECK <- deck2

deal <- function(){
  card <- deck[1,]
  assign("deck",deck[-1,],envir = globalenv())
  card
}

shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck",DECK[random,],envir=globalenv())
}

setup <- function(deck){
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1,]
    assign("deck",deck[-1,],envir= globalenv())
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck",DECK[random,],envir= globalenv())
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}


