library(TTR)
library(data.table)
library(ggplot2)
library(hrbrthemes)



ll <- 100
start <- as.Date("2020-09-01")
aa <- data.table(A = as.Date(1:ll, origin = start), B = rnorm(ll, 8, 1.3))
bb <- data.table(A = as.Date(100 + 1:ll, origin = start), B = rnorm(ll, 11, 0.8))
cc <- data.table(A = as.Date(200 + 1:ll, origin = start), B = rnorm(ll, 5, 0.5) - rpois(1, 0.56))
dd <- data.table(A = as.Date(300 + 1:ll, origin = start), B = rnorm(ll, 6, 0.6) + rpois(1, 1.4))
aa <- rbind(aa, bb, cc, dd)

nf <- 9
nm <- 26
ns <- 52

aa[, `:=`(turnUp = runMax(B, nf), turnDown = runMin(B, nf), baseUp = runMax(B, nm), baseDown = runMin(B, nm), slowUp = runMax(B, ns), slowDown = runMin(B, ns))]
aa[, `:=`(turning = apply(.SD, 1, mean)), .SDcols = c("turnUp", "turnDown")]
aa[, `:=`(baseline = apply(.SD, 1, mean)), .SDcols = c("baseUp", "baseDown")]
aa[, spanTime := apply(.SD, 1, function(x) stats::lag(mean(x), nm)), .SDcols = c("turning", "baseline")]
aa[, slowTime := apply(.SD, 1, function(x) stats::lag(mean(x), nm)), .SDcols = c("slowUp", "slowDown")]

aa[!is.na(spanTime) & !is.na(slowTime) & spanTime >= slowTime, band := "A"]
aa[!is.na(spanTime) & !is.na(slowTime) & spanTime <= slowTime, band := "B"]

aa[, crisscross := rleid(band)]



expand.range <- function(data, a) {
  orig.range <- range(a)
  exp.range <- range(a) + c(-1, 1)
  if (exp.range[1] < 1) {
    exp.range[1] <- orig.range[1]
  }
  if (exp.range[2] > nrow(data)) {
    exp.range[2] <- orig.range[2]
  }
  return(exp.range)
}

print.seq <- function(a) {
  return(seq(a[1], a[2], 1))
}




ribbons2 <- list()
for (i in 1:max(aa$crisscross)) {
  ribbons2[[i]] <- geom_ribbon(data = aa[print.seq(expand.range(aa, which(aa$crisscross == i)))],
                               aes(ymin = slowTime, ymax = spanTime),
                               fill = ifelse(aa[print.seq(expand.range(aa, which(aa$crisscross == i)))[2]-1]$band == "A",
                                             rgb(149/255, 231/255, 155/255, alpha=0.625),
                                             rgb(231/255, 155/255, 149/255, alpha=0.625)))
}

ggplot(aa, aes(x = A)) +
  ribbons2 +
  geom_line(aes(y = spanTime), color = "darkgreen") +
  geom_line(aes(y = slowTime), color = "orange") +
  geom_line(aes(y = B), color = "grey60") +
  theme_ipsum_rc()


