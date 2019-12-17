library(Rcpp)
library(ggplot2)
library(dplyr)


setwd("C:/Users/User/Documents/R/R art")
getwd()

######## Clifford attractor ###########################################################


opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

a=-1.24458046630025
b=-1.25191834103316 
c=-1.81590817030519 
d=-1.90866735205054

df=createTrajectory(10000000, 0, 0, a, b, c, d)


png("Clifford1.png", units="px", width=1600, height=1600, res=300)
ggplot(df, aes(x, y)) + geom_point(color="red", shape=46, alpha=.01) + opt




####clthulu volours reversed
library(tidyverse)
seq(-3,3,by=.01) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x^3-sin(y^2)), y=(y^3-cos(x^2)))) +
  geom_point(alpha=.1, shape=20, size=0, color="black")+
  theme_void()+
  coord_fixed()+
  theme(panel.background = element_rect(fill="white"))+
  coord_polar()







##### cthulu invert
library(tidyverse)
seq(from=-10, to=10, by = 0.05) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(y=(x^2+pi*cos(y)^2), x=(y+pi*sin(x)))) +
  geom_point(alpha=.1, shape=20, size=1, color="black")+
  theme_void()+coord_fixed()



####   Rose Spiral 
library(tidyverse)
df <- data.frame(x=0, y=0)
for (i in 2:500){
  df[i,1] <- df[i-1,1]+((0.98)^i)*cos(i)
  df[i,2] <- df[i-1,2]+((0.98)^i)*sin(i)   
}
ggplot(df, aes(x,y)) + 
  geom_polygon()+
  theme_void()



########  Naive Sunflower

library(ggplot2)
a=pi*(3-sqrt(5))
n=500
ggplot(data.frame(r=sqrt(1:n),t=(1:n)*a),
       aes(x=r*cos(t),y=r*sin(t)))+
  geom_point(aes(x=0,y=0),
             size=190,
             colour="gold")+
  geom_point(aes(size=(n-r)),
             shape=21,fill="black",
             colour="gray90")+
  theme_void()+theme(legend.position="none")



### modify sunflower ###########

library(ggplot2)
a=pi*(3-sqrt(5))
n=500
ggplot(data.frame(r=sqrt(1:n),t=(1:n)*a),
       aes(x=r*cos(t),y=r*sin(t)))+
  geom_point(aes(x=0,y=0),
             size=100,
             colour="gold")+
  geom_point(aes(size=(n-r)),
             shape=21,fill="black",
             colour="gray90")+
  theme_void()+theme(legend.position="none")



##################################################

####################  


library(tidyverse)

# This function creates the segments of the original polygon
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

# This function creates segments from some mid-point of the edges
mid_points <- function(d, p, a, i, FUN = ratio_f) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

# This function connect the ending points of mid-segments
con_points <- function(d) {
  d %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

edges <-  6  # Number of edges of the original polygon
niter <- 200 # Number of iterations
pond <- 0.4  # Weight to calculate the point on the middle of each edge
step  <- 17  # No of times to draw mid-segments before connect ending points
alph  <- 0.7 # transparency of curves in geom_curve
angle <- 4.62 # angle of mid-segment with the edge
curv <- 1.0   # Curvature of curves
line_color <- "dark orange" # Color of curves in geom_curve
back_color <- "light blue" # Background of the ggplot
ratio_f <- 	function (x) { log(x + 1) } # To calculate the longitude of mid-segments

# Generation on the fly of the dataset
accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, pond, angle, y) else con_points(old)
}, 1:niter,
.init=polygon(edges)) %>% bind_rows() -> df

# Plot
hex <- ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = curv,
             color=line_color,
             alpha=alph)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill=back_color),
        plot.background  = element_rect(fill=back_color),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())
hex 

ggsave("polygon5.png", hex, width = 20, height = 20, units = "cm")


