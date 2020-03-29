### points per shot attempt over time

# displays efficiency growth 

# efficiency growth and shot volume

# x value: time
# y value: points per attempt
# z value: shot volume
#   displayed as: another line, 2D area

# why points per attempt as efficiency metric?
# ignores free throws, as an improvement in FT shooting can disguise as an improvement in shot selection

install.packages("pracma")
install.packages("xts")
install.packages("quantmod")

library(pracma)
library(xts)
library(quantmod)

save(vassell, file = "vassell.Rdata")
load(file = "vassell.Rdata")


## per game ppa

vassell$ppa <- (vassell$X2P*2 + vassell$X3P*3) / vassell$FGA

## cumulative ppa

vassell$cump <- cumsum(vassell$X2P*2) + cumsum(vassell$X3P*3)
vassell$cuma <- cumsum(vassell$FGA)
vassell$cum.ppa <- vassell$cump / vassell$cuma

vassell$ppa.wma <- WMA(vassell$cum.ppa, 6)

plot(x = vassell$Rk, xlab = "game", 
     y = vassell$cum.ppa, ylab = "PPA",
     type = "h", main = "Devin Vassell (sophomore, Florida State) points per attempt
2018-2020, through 50 games")
lines(x = vassell$Rk, y = vassell$ppa.wma, type = "l", col = "firebrick1")

vassell$fga.wma <- WMA(vassell$FGA, 6)

plot(x = vassell$Rk[1:56], xlab = "game", 
     y = vassell$FGA[1:56], ylab = "FGA",
     type = "h", main = "Devin Vassell (sophomore, Florida State) shot attempts per game
2018-2020, through 50 games")
lines(x = vassell$Rk, y = vassell$fga.wma, type = "l", col = "firebrick1")



## ncaa averages

save(ncaa, file = "ncaa.Rdata")
load(file = "ncaa.Rdata")

ncaa$cump <- cumsum((ncaa$FG-ncaa$X3P)*2) + cumsum(ncaa$X3P*3)
ncaa$cuma <- cumsum(ncaa$FGA)
ncaa$cum.ppa <- ncaa$cump / ncaa$cuma
ncaa$ncaa.ppa <- ncaa$cum.ppa[353]

ncaa$ppa <- ((ncaa$FG-ncaa$X3P)*2 + ncaa$X3P*3) / ncaa$FGA

summary(ncaa$ppa)
ppa.95 <- quantile(ncaa$ppa, c(.05, .50, .95)) 



## ppa and fga plot

# bar/line graph
par(mar = c(4,4,4,4))
plot(x = vassell$Rk[1:56], xlab = "", 
     y = vassell$ppa.wma[1:56], ylab = "", ylim = c(.9,1.2),
     type = "h", lwd = 4, main = "Devin Vassell (sophomore, Florida State) 
moving average points per attempt and attempts per game
2018-2020 through 51 games", col = "cadetblue")
axis(2)
mtext("points per attempt", side = 2, line = 3, las = 3)
abline(h = ppa.95[1], lty = 2, lwd = 2, col = "black")
mtext("NCAA ppa - 5%", side = 2, col = "black", las = 2, line = -4.2, cex = .7, at = (ppa.95[1] + .005))
abline(h = ncaa$ncaa.ppa[1], lty = 2, lwd = 2, col = "black")
mtext("NCAA ppa", side = 2, col = "black", las = 2, line = -3, cex = .7, at = (ncaa$ncaa.ppa[1] + .005))
abline(h = ppa.95[3], lty = 2, lwd = 2, col = "black")
mtext("NCAA ppa - 95%", side = 2, col = "black", las = 2, line = -4.6, cex = .7, at = (ppa.95[3] + .005))
abline(h = 1.0503525, lty = 2, lwd = 2, col = "black")
mtext("FSU ppa", side = 2, col = "black", las = 2, line = -2.5, cex = .7, at = (ncaa$ppa[97] + .005))
par(new = TRUE)
plot(x = vassell$Rk[1:56], xlab = "game", 
     y = vassell$fga.wma[1:56], ylab = "", yaxt = "n",
     type = "l", lwd = 3, main = "", col = "brown4")
axis(4)
mtext("attempts per game", side = 4, line = 2.5, las = 3)
abline(v = 34, lty = 2, lwd = 2, col = "black")
mtext("2019 Season", side = 1, las = 1, line = .15, cex = .7, at = 34)
legend("bottomright", c("points per attempt", "attempts per game"),
        col = c("cadetblue","brown4"),
        lwd = c(4, 4), lty = c(1, 1), cex = .65)

# scatterplot
par(mar= c(4,4,4,4))
plot(y = vassell$ppa[1:56], xlab = "shot attempts",
     x = vassell$FGA[1:56], ylab = "points per attempt",
     type = "p", lwd = 4, lty = 4, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
points per attempt and attempts per game
2018-2020 through 51 games")
abline(h = ncaa$ncaa.ppa[1], lty = 2, lwd = 1, col = "black")
mtext("NCAA average ppa", side = 2, col = "black", las = 2, line = -5.5, cex = .7, at = (ncaa$ppa[1] + .06))
abline(h = 1.12, lty = 2, lwd = 1, col = "black")
mtext("Vassell ppa", side = 2, col = "black", las = 2, line = -3.5, cex = .7, at = (1.12 + .06))
mtext(".0259 r-squared", side = 4, las = 2, line = -7.5, cex = .8, at = 2.9, lwd = 3)
mtext(".2742 p-value", side = 4, las = 2, line = -7.5, cex = .8, at = 3, lwd = 3)



### points, shots per minute

par(mar= c(4,4,4,4))
plot(y = vassell$MP[1:56], ylab = "minutes played",
     x = vassell$PTS[1:56], xlab = "points per game",
     type = "p", lwd = 2, lty = 2, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
points per game and minutes played
2018-2020 through 51 games")

par(mar= c(4,4,4,4))
plot(y = vassell$MP[1:56], ylab = "minutes played",
     x = vassell$FGA[1:56], xlab = "shots per game",
     type = "p", lwd = 2, lty = 2, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
shots per game and minutes played
2018-2020 through 51 games")



### defensive analysis

steals.regression <- lm(vassell$STL[1:56] ~ vassell$MP[1:56])
summary(steals.regression)

blocks.regression <- lm(vassell$BLK[1:56] ~ vassell$MP[1:56])
summary(blocks.regression)

rebounds.regression <- lm(vassell$TRB[1:56] ~ vassell$MP[1:56])
summary(rebounds.regression)

efficiency.regression <- lm(vassell$FGA[1:56] ~ vassell$ppa[1:56])
summary(efficiency.regression)
efficiency.rsq <- 0.02593


par(mar= c(4,4,4,4))
plot(x = vassell$MP[1:56], xlab = "minutes played",
     y = vassell$STL[1:56], ylab = "steals per game",
     type = "p", lwd = 2, lty = 2, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
steals per game and minutes played
2018-2020 through 50 games")

par(mar= c(4,4,4,4))
plot(x = vassell$Rk[1:56], xlab = "game",
     y = vassell$STL[1:56], ylab = "steals", ylim = c(0,3),
     type = "h", lwd = 4, lty = 1, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
steals per game
2018-2020 through 51 games")
abline(v = 34, lty = 2, lwd = 2, col = "black")
mtext("2019 Season", side = 1, las = 1, line = .15, cex = .7, at = 34)

par(mar= c(4,4,4,4))
plot(x = vassell$Rk[1:56], xlab = "game",
     y = vassell$BLK[1:56], ylab = "blocks",
     type = "h", lwd = 4, lty = 1, col = "firebrick1", main = "Devin Vassell (sophomore, Florida State)
blocks per game
2018-2020 through 51 games")
abline(v = 34, lty = 2, lwd = 2, col = "black")
mtext("2019 Season", side = 1, las = 1, line = .15, cex = .7, at = 34)

par(mar= c(4,4,4,4))
plot(x = vassell$Rk[1:56], xlab = "game",
     y = vassell$TRB[1:56], ylab = "total rebounds",
     type = "h", lwd = 4, lty = 1, col = "forestgreen", main = "Devin Vassell (sophomore, Florida State)
rebounds per game
2018-2020 through 51 games")
abline(v = 34, lty = 2, lwd = 2, col = "black")
mtext("2019 Season", side = 1, las = 1, line = .15, cex = .7, at = 34)

par(mar= c(4,4,4,4))
plot(x = vassell$Rk[1:56], xlab = "game",
     y = vassell$PTS[1:56], ylab = "points",
     type = "h", lwd = 4, lty = 1, col = "cadetblue", main = "Devin Vassell (sophomore, Florida State)
points per game
2018-2020 through 51 games")
abline(v = 34, lty = 2, lwd = 2, col = "black")
mtext("2019 Season", side = 1, las = 1, line = .15, cex = .7, at = 34)