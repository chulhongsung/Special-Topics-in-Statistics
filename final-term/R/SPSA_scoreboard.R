rm(list = ls())
gc(reset = T)

gk <- function(x, y, xpaddle, ypaddle)
{ 
  dk <- ifelse(rbinom(n = 2, size = 1, prob = 1/2) == 0, -1, 1)
  
  dif <- sqrt((x - (xpaddle + dk[1]))^2 + (y - (ypaddle + dk[2]))^2) + runif(1, -2, 2) - sqrt((x - (xpaddle - dk[1]))^2 + (y - (ypaddle-dk[2]))^2) + runif(1, -0.5, 0.5)
  
  return(c(dif/(2*dk[1]), dif/(2*dk[2]))) 
} 

set.seed(2)

score.board <- c()

i <- 1


while(i <= 1000){
 
  score <- 0
  high.score <- 0
  
  xmin <- 0.5
  xmax <- 29.4
  ymin <- 0.5
  ymax <- 29.4
  
  x <- 25
  y <- sample(5:25, 1)

  psize <- 4
  ypaddle <- 15
  xpaddle <- 0
  
  dx <- runif(1, .5, 1)
  dy <- runif(1, .5, 1)
  
  while (x > xpaddle - 1){

    x <- x + dx
    y <- y + dy

    if (x > xmax) {
      dx <- -dx * runif(1, .9, 1.1) 
      if (x > xmin) x <- xmax 
      
    }
    if (y < ymin | y > ymax) {
      if (y < ymin) y <- ymin
      if (y > ymax) y <- ymax
      dy <- -dy * runif(1, .9, 1.1)
    }
    
    if ( (x <= 20) & (sign(dx) <= 0) )
    { 
      tmp_gk <- gk(x, y, xpaddle, ypaddle)
      xpaddle <- xpaddle - tmp_gk[1] 
      ypaddle <- ypaddle - tmp_gk[2]
      
     
      if (ypaddle < (psize / 2)) ypaddle <- (psize / 2)
      if (ypaddle > 30 - (psize / 2)) ypaddle <- 30 - (psize / 2)
      
    } 
    
    if ( (x < xpaddle) & (y > ypaddle - (psize / 2)) & (y < ypaddle + (psize / 2))) {
      
      
      if (x < xpaddle + 0.5) x <- xpaddle
      dx <- -dx * runif(1, .9, 1.1)
      score <- score + 1
      xpaddle <- 0
      ypaddle <- 15
    }
  
  }
  
  score.board <- rbind(score.board, score)  
  
  i <- i + 1
}  

mean(score.board)
sd(score.board)
# [1] 2.07