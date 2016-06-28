# Some analysis of the shooting drill 'plus one minus two'.
library(ggplot2)
library(dplyr)

run.drill <- function(upper = 10, lower = -10, plus = 1, minus = -2, prob = .5) {
    # prob represents the probability of making a shot
    
    score <- 0
    made <- 0
    count <- 0
    while(score > lower & score < upper){
        count <- count + 1
        shot <- sample(x = c(plus, minus), size = 1, prob = c(prob, 1 - prob))
        score <- score + shot
        if(shot > 0){
            made <- made + 1
            }
        }
    result <- ifelse(score <= lower, 0, 1) # 0 represents a loss, 1 represents a victory
    tbl_df(data.frame(result = result, count = count, pct = made / count))
}

simulate.drill <- function(num.drills = 1000, ...) {
    # initialize
    outcome <- run.drill(...)
    for(i in 1:(num.drills-1)){
        outcome <- rbind(outcome, run.drill(...))
    }
    outcome
}

# typical usage:
# simulate.drill(num.drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = .5)

x <- seq(from = 0, to = 1, by = .05)
y <- NULL

for(i in x){
    y <- c(y, rep(i, 1000))
}

results <- NULL
set.seed(12)
for(i in x){
    results <- rbind(results, simulate.drill(num.drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = i))
}
results <- tbl_df(cbind(prob = y, results))
rm(i, x, y)

str(results)
table(results$result)

ggplot(data = results, aes(x = count)) + geom_histogram(binwidth = 5, fill = "red") + facet_wrap(~prob, nrow = 5) + theme_bw()
ggplot(data = results, aes(x = count)) + geom_histogram(binwidth = 5, fill = "red") + facet_wrap(~result) + theme_bw()
ggplot(data = results, aes(x = factor(result), y = pct, col = factor(result))) + geom_boxplot() + theme_bw()
ggplot(data = results, aes(x = factor(result), y = count, col = factor(result))) + geom_boxplot() + theme_bw()

results.summary <- results %>% group_by(prob) %>% 
    summarize(avg.shots = mean(count),
              num.wins = sum(result),
              win.pct = num.wins / 1000,
              shot.pct = weighted.mean(pct, count))

print(results.summary, n = 21)

ggplot(data = results.summary, aes(x = prob, y = num.wins)) + geom_line(col = "blue") + theme_bw()
ggplot(data = results.summary, aes(x = prob, y = avg.shots)) + geom_line(col = "red") + theme_bw()
ggplot(data = results.summary, aes(x = prob, y = pct)) + geom_line(col = "orange") + theme_bw()

# Now let's zoom in a bit more between prob = .6 and prob = .7

x <- seq(from = .6, to = .7, by = .005)
y <- NULL

for(i in x){
    y <- c(y, rep(i, 1000))
}

results.zoom <- NULL
set.seed(12)
for(i in x){
    results.zoom <- rbind(results.zoom, simulate.drill(num.drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = i))
}
results.zoom <- tbl_df(cbind(prob = y, results.zoom))
rm(i, x, y)

ggplot(data = results.zoom, aes(x = factor(result), y = pct, col = factor(result))) + geom_boxplot() + theme_bw()
ggplot(data = results.zoom, aes(x = factor(result), y = count, col = factor(result))) + geom_boxplot() + theme_bw()


results.zoom.sum <- results.zoom %>% group_by(prob) %>% 
    summarize(avg.shots = mean(count),
              num.wins = sum(result),
              win.pct = num.wins/1000,
              shot.pct = weighted.mean(pct, count))

print(results.zoom.sum, n = 21)

ggplot(data = results.zoom.sum, aes(x = prob, y = num.wins)) + geom_line(col = "blue") + theme_bw()
ggplot(data = results.zoom.sum, aes(x = prob, y = avg.shots)) + geom_line(col = "red") + theme_bw()
ggplot(data = results.zoom.sum, aes(x = prob, y = pct)) + geom_line(col = "orange") + theme_bw()
