# Sound library
rm(list = ls())
gc(reset = T)
require(beepr) 

# Define playing field
x11(width = 14)
par(mar = rep(1,4), bg = "black")
plot.new()
plot.window(xlim = c(0,50), ylim = c(0,30))
lines(c(1, 50), c(-0.5, -0.5), type = "l", lwd = 3, col = "white")
lines(c(1, 50), c(30.5, 30.5), type = "l", lwd = 3, col = "white")

# Playing field boundaries
xmin <- 0.5
xmax <- 49.5
ymin <- 0.5
ymax <- 29.5

# initial ball position
x <- 25
y <- sample(5:25, 1)
points(x, y, pch = 15, col = "white", cex = 2)

# Paddle control
psize <- 4
ypaddle1 <- 15
xpaddle1 <- 0

ypaddle2 <- 15
xpaddle2 <- 50

# Set direction
dx <- ifelse(rbinom(1, 1, 0.5) == 0, -1, 1)
dy <- runif(1, .5, 1)

# Noisy loss(= distance between ball and paddle) measurement
gk <- function(x, y, xpaddle, ypaddle)
{ 
  dk <- ifelse(rbinom(n = 2, size = 1, prob = 1/2) == 0, -1, 1)
  
  dif <- sqrt((x - (xpaddle + dk[1]))^2 + (y - (ypaddle + dk[2]))^2) + runif(1, -0.5, 0.5) - sqrt((x - (xpaddle - dk[1]))^2 + (y - (ypaddle-dk[2]))^2) + runif(1, -0.5, 0.5)
  
  return(c(dif/(2*dk[1]), dif/(2*dk[2]))) 
} 

gf <- function(x, y, xpaddle, ypaddle)
{ 
  dk <- ifelse(rbinom(n = 2, size = 1, prob = 1/2) == 0, -1, 1)
  
  dif1 <- sqrt((x - (xpaddle + dk[1]))^2 + (y - ypaddle)^2) + runif(1, -0.5, 0.5) - sqrt((x - (xpaddle - dk[1]))^2 + (y - ypaddle)^2) + runif(1, -0.5, 0.5)
  
  dif2 <- sqrt((x - xpaddle)^2 + (y - (ypaddle + dk[2]))^2) + runif(1, -0.5, 0.5) - sqrt((x - xpaddle)^2 + (y - (ypaddle - dk[2]))^2) + runif(1, -0.5, 0.5)
  
  return(c(dif1/(2*dk[1]), dif2/(2*dk[2]))) 
} 

# Game play
while ((x > xpaddle1 - 0.5) & (x < xpaddle2 + 0.5)){
  sound <- 0 # Silence 
  Sys.sleep(.15)# Pause screen
  points(x, y, pch = 15, col = "black", cex = 2) # Erase ball
  # Move ball
  x <- x + dx
  y <- y + dy
  # Collision detection
  if (x > xmax) {
    dx <- -dx * runif(1, .9, 1.1) # Bounce
    if (x > xmin) x <- xmax # Boundary
    sound <- 10 # Set sound
  }
  if (y < ymin | y > ymax) {
    if (y < ymin) y <- ymin
    if (y > ymax) y <- ymax
    dy <- -dy * runif(1, .9, 1.1)
    sound <- 10
  }
  
  # Draw ball
  points(x, y, pch = 15, col = "white", cex = 2)
  if (sound !=0) beep(sound)
  
  # Erase back line
  lines(c(0, 0), c(1, 29), type = "l", lwd = 8, col = "black")
  
  # Move paddle by SPSA method
  if ((x <= 20) & (sign(dx) <= 0) ) # Condition paddle observe  ball
  { # Erase back line
    lines(c(xpaddle1, xpaddle1), c(0, 30), type = "l", lwd = 8, col = "black")
    
    tmp_gk <- gk(x, y, xpaddle1, ypaddle1)
    xpaddle1 <- xpaddle1 - tmp_gk[1] 
    ypaddle1 <- ypaddle1 - tmp_gk[2]
    
    # Keep paddle in window
    if (ypaddle1 < (psize / 2)) ypaddle1 <- (psize / 2)
    if (ypaddle1 > 30 - (psize / 2)) ypaddle1 <- 30 - (psize / 2)
    
    # Draw paddle
    lines(c(xpaddle1, xpaddle1), c(ypaddle1 - (psize / 2), ypaddle1 + (psize / 2)), type = "l", lwd = 8, col = "green")
  } else ( lines(c(0, 0), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "green") )
  
  if ((x >= 30) & (sign(dx) >= 0) )
  {
    lines(c(xpaddle2, xpaddle2), c(0, 30), type = "l", lwd = 8, col = "black")
    
    tmp_gf <- gf(x, y, xpaddle2, ypaddle2)
    xpaddle2 <- xpaddle2 - tmp_gf[1] 
    ypaddle2 <- ypaddle2 - tmp_gf[2]
    
    # Keep paddle in window
    if (ypaddle2 < (psize / 2)) ypaddle2 <- (psize / 2)
    if (ypaddle2 > 30 - (psize / 2)) ypaddle2 <- 30 - (psize / 2)
    
    # Draw paddle
    lines(c(xpaddle2, xpaddle2), c(ypaddle2 - (psize / 2), ypaddle2 + (psize / 2)), type = "l", lwd = 8, col = "blue")
  } else ( lines(c(50, 50), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "blue") )
  
  # Caught by paddle?
  if ( (x < xpaddle1) & (y > ypaddle1 - (psize / 2)) & (y < ypaddle1 + (psize / 2))) {
    
    points(x, y, pch = 15, col = "black", cex = 2) 
    
    # Erase back line
    lines(c(xpaddle1, xpaddle1), c(0, 30), type = "l", lwd = 8, col = "black")
    
    # Set initail paddle 
    if (x < xpaddle1 + 0.4) x <- xpaddle1
    dx <- -dx * runif(1, .95, 1.1)
    sound <- 2
    xpaddle1 <- 0
    ypaddle1 <- 15
    lines(c(0, 0), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "green")
  }
  
  if ( (x > xpaddle2) & (y > ypaddle2 - (psize / 2)) & (y < ypaddle2 + (psize / 2))) {
    
    points(x, y, pch = 15, col = "black", cex = 2) 
    
    # Erase back line
    lines(c(xpaddle2, xpaddle2), c(0, 30), type = "l", lwd = 8, col = "black")
    
    # Set initail paddle 
    if (x > xpaddle2 + 0.4) x <- xpaddle2
    dx <- -dx * runif(1, .95, 1.1)
    sound <- 2
    xpaddle2 <- 50
    ypaddle2 <- 15
    lines(c(50, 50), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "blue")
  }
}

beep(8)
if(x < xpaddle1){
  tx <- "1P WIN!"
} else {
  tx <- "2P WIN!"
}
text(25, 15, tx, cex = 5, col = "white")
