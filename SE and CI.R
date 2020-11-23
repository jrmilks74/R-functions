#Standard Error calculation for the specified vector
se <- function(x){
  n <- length(x)
  sd(x) / sqrt(n)
}

#Returns the 95% confidence intervals for the specified vector
ci <- function(x, alpha){
        n <- length(x)
        se <- sd(x) / sqrt(n)
        c(mean(x) - qt(1 - alpha/2, df = n - 1) * se, mean(x) + qt(1 - alpha/2, df = n - 1) * se)
}

#Plots 95% confidence intervals on linear regression graphs
plotci <- function(x,y){
  fit <- lm(y~x)
  newx <- seq(min(x), max(x), by = 0.05)
  conf_interval <- predict(fit, newdata = data.frame(x = newx), interval = "confidence", level = 0.95)
  lines(newx, conf_interval[,2], col = "red", lty = 2)
  lines(newx, conf_interval[,3], col = "red", lty = 2)
}
