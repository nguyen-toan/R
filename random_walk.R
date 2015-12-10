# Input variables
starting.set <- c("N", "E", "S", "W")
steps <- 1000
steps <- steps + 1
x <- 1:steps
xx <- 2:steps
all.x <- rep(0, steps)
all.y <- rep(0, steps)
running.x <- rep(0, steps)
running.y <- rep(0, steps)

walk <- sample(starting.set, steps, replace=TRUE)
walk[1] <- "Null"

# Simulate random walk
for (i in x) {
  if (walk[i] == "N")  all.y[i] <- 1
  if (walk[i] == "S")  all.y[i] <- -1
  if (walk[i] == "E")  all.x[i] <- 1
  if (walk[i] == "W")  all.x[i] <- -1
}

for (i in xx) {
  running.x[i] <- running.x[i-1] + all.x[i]
  running.y[i] <- running.y[i-1] + all.y[i]
}

# Visualization of the walk
plot(running.x,
     running.y,
     type="l",
     xlab="Horizontal movement",
     ylab="Veritcal movement")
B <- matrix(c(0, running.x[steps], 0, running.y[steps]), nrow=2, ncol=2)
lines(B, col="orange", lwd=2)
points(B, col="blue", pch=19, cex=1.5)
points(0, 0, col="red", pch=19, cex=1.5)

# Calculate distance between starting and end point of the walk
x.cord <- sum((walk=="N") - (walk=="S"))
y.cord <- sum((walk=="E") - (walk=="W"))
dis <- sqrt(x.cord ^ 2 + y.cord ^ 2)
dis
