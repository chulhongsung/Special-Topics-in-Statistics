# Sound library
rm(list = ls())
gc(reset = T)
require(beepr) 

# Game parameters
score <- 0
high.score <- 0

# Define playing field
x11()
par(mar = rep(1,4), bg = "black")
plot.new()
plot.window(xlim = c(0,30), ylim = c(0,30))
lines(c(1, 30), c(-0.5, -0.5), type = "l", lwd = 3, col = "white")
lines(c(1, 30), c(30.5, 30.5), type = "l", lwd = 3, col = "white")
lines(c(30, 30), c(-0.5, 30.5), type = "l", lwd = 3, col = "white")

# Playing field boundaries
xmin <- 0.5
xmax <- 29.4
ymin <- 0.5
ymax <- 29.4

# initial ball position
x <- 25
y <- sample(5:25, 1)
points(x, y, pch = 15, col = "white", cex = 2)

# Paddle control
psize <- 4
ypaddle <- 15
xpaddle <- 0

# Set direction
dx <- runif(1, .5, 1)
dy <- runif(1, .5, 1)
k <- 1
# Noisy loss(= distance between ball and paddle) measurement
twospsa <- function(x, y, xpaddle, ypaddle, k)
{ 
  dk1 <- ifelse(rbinom(n = 2, size = 1, prob = 1/2) == 0, -1, 1)
  dk2 <-ifelse(rbinom(n = 2, size = 1, prob = 1/2) == 0, -1, 1)
  
  dif <- sqrt((x - (xpaddle + dk2[1]))^2 + (y - (ypaddle + dk2[2]))^2) + runif(1, -2, 2) - sqrt((x - (xpaddle - dk2[1]))^2 + (y - (ypaddle-dk2[2]))^2) + runif(1, -0.5, 0.5)
  
  g <- c(dif/(2*dk2[1]), dif/(2*dk2[2]))
  
  dif1 <- sqrt((x - (xpaddle + dk2[1] + (1/(k+1))*dk1[1]))^2 + (y - (ypaddle + dk2[2] + (1/(k+1))*dk1[2]))^2) + runif(1, -0.01, 0.01) - sqrt((x - (xpaddle + dk2[1]))^2 + (y - (ypaddle + dk2[2]))^2) + runif(1, -0.01, 0.01) 
  dif2 <- sqrt((x - (xpaddle - dk2[1] + (1/(k+1))*dk1[1]))^2 + (y - (ypaddle - dk2[2] + (1/(k+1))*dk1[2]))^2) + runif(1, -0.01, 0.01) - sqrt((x - (xpaddle - dk2[1]))^2 + (y - (ypaddle - dk2[2]))^2) + runif(1, -0.01, 0.01) 
  
  g1 <- dif1/(1/(k+1)) * c(1/dk1[1], 1/dk1[2])
  g2 <- dif2/(1/(k+1)) * c(1/dk1[1], 1/dk1[2])
  
  dg <- ((g1 - g2)/2) %*% t(c(1/dk2[1], 1/dk2[2]))
  
  H <- (1/2)*(dg + t(dg))
   
  return(list(g = g, H = H)) 
}


k <- 1

H <- diag(c(1/2, 1/2), ncol = 2)

# Game play
while (x > xpaddle - 1){
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
  
  
  
  # Move paddle by 2SPSA method
  if ( (x <= 20) & (sign(dx) <= 0) ) # Condition paddle observe  ball
  { # Erase back line
    lines(c(xpaddle, xpaddle), c(0.2, 29.8), type = "l", lwd = 8, col = "black")
      
    tmp_2spsa <- twospsa(x, y, xpaddle, ypaddle, k)
    
    H <- k*H/(k+1) +  tmp_2spsa$H/(k+1)
    
    tmp <- solve(H) %*% tmp_2spsa$g
    
    xpaddle <- xpaddle - (1.2/k)*tmp[1]
    ypaddle <- ypaddle - (1.2/k)*tmp[2]
    
    k <- k + 1
    
    # Keep paddle in window
    if (ypaddle < (psize / 2)) ypaddle <- (psize / 2)
    if (ypaddle > 30 - (psize / 2)) ypaddle <- 30 - (psize / 2)
     
    # Draw paddle
    lines(c(xpaddle, xpaddle), c(ypaddle - (psize / 2), ypaddle + (psize / 2)), type = "l", lwd = 8, col = "white")
  } else ( lines(c(0, 0), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "white") )
  
  # Caught by paddle?
  if ( (x < xpaddle) & (y > ypaddle - (psize / 2)) & (y < ypaddle + (psize / 2))) {
    
    H <- diag(c(1/2, 1/2), ncol = 2)
    
    k <- 1
    
    points(x, y, pch = 15, col = "black", cex = 2) 
    
    # Erase back line
    lines(c(xpaddle, xpaddle), c(0.2, 29.8), type = "l", lwd = 8, col = "black")
    
    # Set initail paddle 
    if (x < xpaddle + 0.5) x <- xpaddle
    dx <- -dx * runif(1, .9, 1.1)
    sound <- 2
    score <- score + 1
    xpaddle <- 0
    ypaddle <- 15
    lines(c(0, 0), c(15 - (psize / 2), 15 + (psize / 2)), type = "l", lwd = 8, col = "white")
    
   }
  
}

beep(8)
text(15,15, "GAME OVER", cex=5, col = "white")
s <- ifelse(score == 1, "", "s")
text(15,5, paste0(score, " Point", s), cex=3, col = "white") 

