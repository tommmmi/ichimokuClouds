### libraries

library(TTR)
library(data.table)
library(ggplot2)
library(hrbrthemes)



### generate some data
# use normal distribution to get "nice" data
# do in a few parts to add "global variation"
# add some random numbers from Poisson distribution as a spice

ll <- 100
start <- as.Date("2020-09-01") # artificial start date
aa <- data.table(A = as.Date(1:ll, origin = start), B = rnorm(ll, 8, 1.3))
bb <- data.table(A = as.Date(100 + 1:ll, origin = start), B = rnorm(ll, 11, 0.8))
cc <- data.table(A = as.Date(200 + 1:ll, origin = start), B = rnorm(ll, 5, 0.5) - rpois(1, 0.56))
dd <- data.table(A = as.Date(300 + 1:ll, origin = start), B = rnorm(ll, 6, 0.6) + rpois(1, 1.4))
aa <- rbind(aa, bb, cc, dd)



### main function to calculate Ichimoku parameters
# utilises neat functions readily provided by data.table library

calculateIchimoku <- function(fast, medium, slow) {
  # calculate moving terms
  aa[, `:=`(turnUp = runMax(B, fast), turnDown = runMin(B, fast), baseUp = runMax(B, medium), baseDown = runMin(B, medium), slowUp = runMax(B, slow), slowDown = runMin(B, slow))]
  # calculate average turning points
  aa[, `:=`(turning = apply(.SD, 1, mean)), .SDcols = c("turnUp", "turnDown")]
  aa[, `:=`(baseline = apply(.SD, 1, mean)), .SDcols = c("baseUp", "baseDown")]
  # calculate lagging terms
  aa[, spanTime := apply(.SD, 1, function(x) stats::lag(mean(x), medium)), .SDcols = c("turning", "baseline")]
  aa[, slowTime := apply(.SD, 1, function(x) stats::lag(mean(x), medium)), .SDcols = c("slowUp", "slowDown")]
  
  # determine upper and lower limit order
  aa[!is.na(spanTime) & !is.na(slowTime) & spanTime >= slowTime, band := "A"]
  aa[!is.na(spanTime) & !is.na(slowTime) & spanTime <= slowTime, band := "B"]
  
  # identify each turn-over
  aa[, crisscross := rleid(band)]
}

# this adds the Ichimoku parameters to the generated dataset 'aa'
calculateIchimoku(9, 26, 52)



### "feather" function to expand range selection
# basically expands range c(6, 9) to c(5, 10)

expand.range <- function(data, a) {
  # figure out the original range
  orig.range <- range(a)
  # subtract one from the lower end and add one to the upper end
  exp.range <- range(a) + c(-1, 1)
  
  # flooring: if lower end goes below 1, truncate it to 1
  if (exp.range[1] < 1) {
    exp.range[1] <- orig.range[1]
  }
  # ceiling: if upper end goes above number of observations in the data, truncate to n.obs
  if (exp.range[2] > nrow(data)) {
    exp.range[2] <- orig.range[2]
  }
  
  return(exp.range)
}

# shortening function to seq()
print.seq <- function(a) {
  return(seq(a[1], a[2], 1))
}



### ggplot polygons
# without expanding the ranges geom_ribbon() by default cuts the ribbon fill to min and max values and causes "empty" "triangles" to the final figure
# fill the area between two curves: long-term trend and short-term trend to indicate difference or span between them
# colorize the filled area for interpretation: green for uptrending mood and red for downtrending mood

# define a list to gather outputs
ribbons2 <- list()

# create geom_ribbon() for each turn-over
for (i in 1:max(aa$crisscross)) {
  ribbons2[[i]] <- geom_ribbon(data = aa[print.seq(expand.range(aa, which(aa$crisscross == i)))],
                               aes(ymin = slowTime, ymax = spanTime),
                               fill = ifelse(aa[print.seq(expand.range(aa, which(aa$crisscross == i)))[2]-1]$band == "A",
                                             rgb(231/255, 155/255, 149/255, alpha=0.625),
                                             rgb(149/255, 231/255, 155/255, alpha=0.625)))
}



### final figure
ggplot(aa, aes(x = A)) +
  ribbons2 +
  geom_line(aes(y = spanTime), color = "darkgreen") + # curve for more sensitive (fast) "mood"
  geom_line(aes(y = slowTime), color = "orange") + # curve for long-term trend
  geom_line(aes(y = B), color = "grey60") + # actual data
  theme_ipsum_rc()

ggsave("example_ichi.png", dpi = 300, units = "px", width=1200, height=540, device = "png", scale = 1.6)
