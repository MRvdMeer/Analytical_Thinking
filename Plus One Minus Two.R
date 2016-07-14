# Some analysis of the shooting drill 'plus one minus two'.
library(ggplot2)
library(dplyr)

run_drill <- function(upper = 10, lower = -10, plus = 1, minus = -2, prob = .5) {
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

simulate_drill <- function(num_drills = 1000, ...) {
    # initialize
    outcome <- run_drill(...)
    for(i in 1:(num_drills-1)){
        outcome <- rbind(outcome, run_drill(...))
    }
    outcome
}

# typical usage:
# simulate_drill(num_drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = .5)

x <- seq(from = 0, to = 1, by = .05)
y <- NULL

for(i in x){
    y <- c(y, rep(i, 1000))
}

results <- NULL
set.seed(12)
for(i in x){
    results <- rbind(results, simulate_drill(num_drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = i))
}
results <- tbl_df(cbind(prob = y, results))
rm(i, x, y)

str(results)
table(results$result)

ggplot(data = results, aes(x = count)) + geom_histogram(binwidth = 5, fill = "red") + facet_wrap(~prob, nrow = 5) + theme_bw()
ggplot(data = results, aes(x = count)) + geom_histogram(binwidth = 5, fill = "red") + facet_wrap(~result) + theme_bw()
ggplot(data = results, aes(x = factor(result), y = pct, col = factor(result))) + geom_boxplot() + theme_bw()
ggplot(data = results, aes(x = factor(result), y = count, col = factor(result))) + geom_boxplot() + theme_bw()

results_summary <- results %>% group_by(prob) %>% 
    summarize(avg_shots = mean(count),
              num_wins = sum(result),
              win_pct = num_wins / 1000,
              shot_pct = weighted.mean(pct, count))

print(results_summary, n = 21)

ggplot(data = results_summary, aes(x = prob, y = num_wins)) + geom_line(col = "blue") + theme_bw() + ggtitle("number of wins vs. probability of making a shot")
ggplot(data = results_summary, aes(x = prob, y = avg_shots)) + geom_line(col = "red") + theme_bw() + ggtitle("number of shots taken vs. probability of making a shot")
ggplot(data = results_summary, aes(x = prob, y = win_pct)) + geom_line(col = "orange") + theme_bw()
ggplot(data = results_summary, aes(x = prob, y = shot_pct)) + geom_line(col = "green") + theme_bw()

# Now let's zoom in a bit more between prob = .6 and prob = .7

x <- seq(from = .6, to = .7, by = .005)
y <- NULL

for(i in x){
    y <- c(y, rep(i, 1000))
}

results_zoom <- NULL
set.seed(12)
for(i in x){
    results_zoom <- rbind(results_zoom, simulate_drill(num_drills = 1000, upper = 10, lower = -10, plus = 1, minus = -2, prob = i))
}
results_zoom <- tbl_df(cbind(prob = y, results_zoom))
rm(i, x, y)

ggplot(data = results_zoom, aes(x = factor(result), y = pct, col = factor(result))) + geom_boxplot() + theme_bw()
ggplot(data = results_zoom, aes(x = factor(result), y = count, col = factor(result))) + geom_boxplot() + theme_bw()


results_zoom_summary <- results_zoom %>% group_by(prob) %>% 
    summarize(avg_shots = mean(count),
              num_wins = sum(result),
              win_pct = num_wins/1000,
              shot_pct = weighted.mean(pct, count))

print(results_zoom_summary, n = 21)

ggplot(data = results_zoom_summary, aes(x = prob, y = num_wins)) + geom_line(col = "blue") + theme_bw()
ggplot(data = results_zoom_summary, aes(x = prob, y = avg_shots)) + geom_line(col = "red") + theme_bw()
ggplot(data = results_zoom_summary, aes(x = prob, y = win_pct)) + geom_line(col = "orange") + theme_bw()
ggplot(data = results_zoom_summary, aes(x = prob, y = shot_pct)) + geom_line(col = "green") + theme_bw()
