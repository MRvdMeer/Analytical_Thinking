library(ggplot2)
library(dplyr)


simulate_point <- function(count = 10000){
    # simulates a number of x and y coordinates from a uniform distribution, and determines if each points is within the unit circle
    x <- runif(n = count, min = 0, max = 1)
    y <- runif(n = count, min = 0, max = 1)
    
    output <- data.frame(x, y) %>% mutate(in_circle = (x^2 + y^2 <= 1))
}

quarter_circle <- function(x){
    # used for plotting
    sqrt(1 - x^2)
}

set.seed(20160813)
random_points <- simulate_point()

ggplot(data = data.frame(x = c(0, 1), y = c(0, 1)), aes(x = x, y = y)) +
    stat_function(fun = quarter_circle, linetype = 1, size = 1) +
    geom_point(data = random_points, color = "blue", alpha = 0.1) +
    theme_bw()

pi_estimate <- 4 * sum(random_points$in_circle) / length(random_points$in_circle)
pi_estimate

random_points_larger <- simulate_point(count = 1000000)

pi_estimate_larger <- 4 * sum(random_points_larger$in_circle) / length(random_points_larger$in_circle)
pi_estimate_larger

random_points_largest <- simulate_point(count = 100000000)

pi_estimate_largest <- 4 * sum(random_points_largest$in_circle) / length(random_points_largest$in_circle)
pi_estimate_largest
