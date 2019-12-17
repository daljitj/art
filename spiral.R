library(ggplot2)


opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())



# for some fixed real a, b
a <- 2
b <- 3
theta <- seq(0,10*pi,0.01)
r <- a + b*theta
df <- data.frame(x=r*cos(theta), y=r*sin(theta)) # Cartesian coords
ggplot(df, aes(x,y)) + geom_point(col='orange') + opt






# for some fixed real a, b
a <- 20
b <- 30
theta <- seq(0,10*pi,0.01)
r <- (a+runif(1,-10,10)) + (b+runif(1,-10,10)*theta)
df <- data.frame(x=r*cos(theta), y=r*sin(theta)) # Cartesian coords

ggplot(df, aes(x,y)) + geom_path(col='orange') + opt

