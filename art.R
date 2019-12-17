setwd("C:/Users/User/Documents/R/R art")
getwd()

########## Kadinsky art #####################

devtools::install_github("gsimchoni/kandinsky")
library(kandinsky)
help(package = kandinsky)

kandinsky()
kandinsky(rv=runif(85368))

kandinsky(mtcars)
kandinsky(iris)
kandinsky(USArrests)


############################################

#devtools::install_github("cutterkom/generativeart")
library(generativeart)
help(package = generativeart)

# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
generativeart::setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

# call the main function to create five images with a polar coordinate system, polar coordinates
generativeart::generate_img(formula = my_formula, nr_of_img = 3, polar = FALSE)

# call the main function to create five images with a polar coordinate system, cartesian coordinates
generativeart::generate_img(formula = my_formula, nr_of_img = 3, polar = TRUE)


###################################################

############## clustering image art ###############

library(imager)
library(tidyverse)
library(purrr)

# Location of the photograph
file="img/frankenstein.jpg"

# Load, convert to grayscale, filter image (to convert it to bw) and sample
load.image(file) %>% 
  grayscale() %>% 
  threshold("45%") %>% 
  as.cimg() %>% 
  as.data.frame() -> franky

# This function cuts the dendrogram in x groups and removes clusters formed by just one point
clustResultx <- function(x) {
  clustCut <- tibble(cluster_id = cutree(clusters, x)) %>% bind_cols(data)
  clustCut %>% group_by(cluster_id) %>% 
    summarize(size = n()) %>% 
    filter(size > 1) %>% 
    select(cluster_id) %>% 
    inner_join(clustCut, by = "cluster_id") -> clustCut
  return(clustCut)
}

# This function adds the resulting segments of comparing two consecutive clustering results
add_segments <- function(x){
  
  df1 <- clustEvol[[x]]
  df0 <- clustEvol[[x-1]]
  
  new_points <- anti_join(df1, df0, by = "id")
  
  # If a new point is added to an existing cluster
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    group_by(id.1) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments1
  
  # If a new 2-points cluster is generated
  new_points %>% anti_join(bind_rows(select(new_segments1, id = p1), 
                                     select(new_segments1, id = p2)), by = "id") %>% 
    group_by(cluster_id) %>% 
    ungroup -> unpaired_points
  
  unpaired_points %>% inner_join(unpaired_points, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 < id.2) %>% 
    select(p1 = id.1, p2 = id.2) -> new_segments2
  
  # If two existing clusters are joined
  new_points <- anti_join(df1, df0, by = c("id", "cluster_id"))
  
  new_points %>% 
    inner_join(df1, by = "cluster_id", suffix = c(".1", ".2")) %>% 
    filter(id.1 != id.2) %>% 
    anti_join(new_points, by = c("id.2" = "id")) %>% 
    mutate(d = sqrt((x.1 - x.2)^2 + (y.1 - y.2)^2)) %>% 
    arrange(d) %>% 
    slice(1) %>% 
    select(p1 = id.1, p2 = id.2) %>% 
    ungroup -> new_segments3
  
  bind_rows(new_segments1, new_segments2, new_segments3)
}

# Sample size
n <- 5000

# Random sample of points from protograph
franky %>% 
  sample_n(n, weight=(1-value)) %>% 
  select(x,y) %>% mutate(id = row_number()) -> data

# Matrix of distance between points
dist_data <- dist(data %>% select(-id), method = "euclidean")

# Hierarchical clustering
clusters <- hclust(dist_data, method = 'single')

# List with all possible clusters from maximum to minimum number of clusters
nrow(data):1 %>% 
  map(function(x) clustResultx(x)) -> clustEvol

# Segments of clusters
2:length(clustEvol) %>% 
  map(function(x) add_segments(x)) %>% 
  bind_rows() -> segments_id

# Segments in (x, y) and (xend, yend) format
segments_id %>% 
  inner_join(data, by = c("p1" = "id"), suffix = c(".1", ".2")) %>% 
  inner_join(data, by = c("p2" = "id"), suffix = c(".1", ".2")) %>% 
  select(x = x.1, y = y.1, xend = x.2, yend = y.2) -> segments

# Plot
ggplot(segments) + 
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
             ncp = 10) +
  scale_x_continuous(expand=c(.1, .1)) +
  scale_y_continuous(expand=c(.1, .1), trans = scales::reverse_trans()) +
  coord_equal() +
  theme_void()

####################################################

####################### mathart #####################

install.packages(c("devtools", "mapproj", "tidyverse", "ggforce", "Rcpp"))
devtools::install_github("marcusvolz/mathart")
devtools::install_github("marcusvolz/ggart")


library(mathart)
library(ggart)
library(ggforce)
library(Rcpp)
library(tidyverse)

########### molusc ################################

df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
  geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03) +
  geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03) +
  facet_wrap(~id, nrow = 2, scales = "free") +
  theme_blankcanvas(margin_cm = 0.5)

ggsave("mollusc01.png", width = 80, height = 80, units = "cm")

############ harmonographs ########################

df1 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.004, d2 = 0.0065, d3 = 0.008, d4 = 0.019,
                    f1 = 3.001, f2 = 2, f3 = 3, f4 = 2,
                    p1 = 0, p2 = 0, p3 = pi/2, p4 = 3*pi/2) %>% mutate(id = 1)

df2 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.0085, d2 = 0, d3 = 0.065, d4 = 0,
                    f1 = 2.01, f2 = 3, f3 = 3, f4 = 2,
                    p1 = 0, p2 = 7*pi/16, p3 = 0, p4 = 0) %>% mutate(id = 2)

df3 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.039, d2 = 0.006, d3 = 0, d4 = 0.0045,
                    f1 = 10, f2 = 3, f3 = 1, f4 = 2,
                    p1 = 0, p2 = 0, p3 = pi/2, p4 = 0) %>% mutate(id = 3)

df4 <- harmonograph(A1 = 1, A2 = 1, A3 = 1, A4 = 1,
                    d1 = 0.02, d2 = 0.0315, d3 = 0.02, d4 = 0.02,
                    f1 = 2, f2 = 6, f3 = 1.002, f4 = 3,
                    p1 = pi/16, p2 = 3*pi/2, p3 = 13*pi/16, p4 = pi) %>% mutate(id = 4)

df <- rbind(df1, df2, df3, df4)

p <- ggplot() +
  geom_path(aes(x, y), df, alpha = 0.25, size = 0.5) +
  coord_equal() +
  facet_wrap(~id, nrow = 2) +
  theme_blankcanvas(margin_cm = 0)

ggsave("harmonograph01.png", p, width = 20, height = 20, units = "cm")



#########################Lissajous curves as k-nearest neighbour graphs ############################

df <- lissajous(a = runif(1, 0, 2), b = runif(1, 0, 2), A = runif(1, 0, 2), B = runif(1, 0, 2), d = 200) %>%
  sample_n(1001) %>%
  k_nearest_neighbour_graph(40)

p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend), df, size = 0.03) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0)

ggsave("knn_lissajous_002.png", p, width = 25, height = 25, units = "cm")



############################ Rose curves ######################


df <- data.frame(x = numeric(0), y = numeric(0), n = integer(0), d = integer(0))

for(n in 1:10) {
  for(d in 1:10) {
    result <- rose_curve(n, d) %>% mutate(n = n, d = d)
    df <- rbind(df, result)
  }
}

p <- ggplot() +
  geom_path(aes(x, y), df, size = 0.35, lineend = "round") +
  facet_grid(d ~ n) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 1)

ggsave("rose_curves.png", p, width = 20, height = 20, units = "cm")



######################### Lorenz attractor #############

df <- lorenz_attractor(a=20, b=8/3, c=28, n=1000000)

p <- ggplot() +
  geom_path(aes(x, z), df, alpha = 0.15, size = 0.03) +
  coord_equal() +
  xlim(-25, 25) + ylim(2.5, 52.5) +
  theme_blankcanvas(margin_cm = 0)

ggsave("lorenz_attractor.png", p, width = 20, height = 20, units = "cm")


####################### Rapidly exploring random tree ############################

# Generate rrt edges
set.seed(1)
df <- rapidly_exploring_random_tree() %>% mutate(id = 1:nrow(.))

# Create plot
p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend, size = -id, alpha = -id), df, lineend = "round") +
  coord_equal() +
  scale_size_continuous(range = c(0.1, 0.75)) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  theme_blankcanvas(margin_cm = 0)

# Save plot
ggsave("rapidly_exploring_random_tree.png", p, width = 20, height = 20, units = "cm")

################ Fractal ferns #######################

n <- 250000

params1 <- data.frame(
  a <- c(0, 0.85, 0.2, -0.15),
  b <- c(0, 0.04, -0.26, 0.28),
  c <- c(0, -0.04, 0.23, 0.26),
  d <- c(0.16, 0.85, 0.22, 0.24),
  e <- c(0, 0, 0, 0),
  f <- c(0, 1.6, 1.6, 0.44),
  p <- c(0.01, 0.85, 0.07, 0.07)
)

params2 <- data.frame(
  a <- c(0, 0.85, 0.09, -0.09),
  b <- c(0, 0.02, -0.28, 0.28),
  c <- c(0, -0.02, 0.3, 0.3),
  d <- c(0.25, 0.83, 0.11, 0.09),
  e <- c(0, 0, 0, 0),
  f <- c(-0.14, 1, 0.6, 0.7),
  p <- c(0.02, 0.84, 0.07, 0.07)
)

df1 <- fractal_fern(n = n, a = params1$a, b = params1$b, c_ = params1$c, d = params1$d, e = params1$e,
                    f = params1$f, p = params1$p) %>% mutate(id = 1)

df2 <- fractal_fern(n = n, a = params2$a, b = params2$b, c_ = params2$c, d = params2$d, e = params2$e,
                    f = params2$f, p = params2$p) %>% mutate(id = 2)

df <- rbind(df1, df2 %>% mutate(x = x*1.75, y = y*1.75))

p <- ggplot() +
  geom_point(aes(x, y), df, size = 0.03, alpha = 0.06) +
  coord_equal() +
  facet_wrap(~id, nrow = 1) +
  theme_blankcanvas(margin_cm = 1)

ggsave("fern01.png", width = 20, height = 20, units = "cm")


############# Weiszfeld convergence ###############

set.seed(10)

terminals <- data.frame(x = runif(10, 0, 10000), y = runif(10, 0, 10000))

df <- 1:10000 %>%
  map_df(~weiszfeld(terminals, c(points$x[.], points$y[.])), .id = "id")

p <- ggplot() +
  geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  geom_point(aes(x, y), terminals, size = 5, alpha = 1) +
  geom_line(aes(x, y, group = id), df, colour = "black", size = 0.5, alpha = 0.03) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

ggsave("weiszfeld.png", p, width = 20, height = 20, units = "in")


