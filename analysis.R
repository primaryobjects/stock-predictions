## Including the required R packages.
packages <- c('ggplot2', 'reshape2')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(reshape2)

source('collect.R')

# Collect bids.
bids2016 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=180855&start=', 0, 700, 50, '2016-01-11')
bids2017 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=206949&start=', 0, 700, 50, '2017-01-11')

## BEGIN MANUAL FIXES
bids2016$clean[bids2016$clean$author == 'mickeyd','bid'] <- 2146.14 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'alisa4804','bid'] <- 2319.87 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'CUBuffs','bid'] <- 2108.00 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'saltycaper','bid'] <- 2131.82 # He made two bids, first mentioned 2131 and then said "So, I will guess 2131.82."
bids2016$clean[bids2016$clean$author == 'snarlyjack','bid'] <- 1950 # put year 2016 before bid ("S & P 12-31-2016")
bids2016$clean[bids2016$clean$author == 'grok87','bid'] <- 2126 # put year 2015 after bid
bids2016$clean[bids2016$clean$author == 'rarvesen','bid'] <- 1850 # put year 2016 before bid
## END MANUAL FIXES

# Merge bids into single dataset and save CSV.
bids <- list()
bids$original <- rbind(bids2016$original, bids2017$original)
bids$clean <- rbind(bids2016$clean, bids2017$clean)
write.csv(bids$original, file='data/predictions-raw.csv')
write.csv(bids$clean, file='data/predictions.csv')

### PLOTTING
## BEGIN SELECT DATASET
bids2 <- bids2017$clean
OPEN <- bids2017$open
## END SELECT DATASET

YEAR <- format(as.POSIXct(bids2[1,]$date), '%Y')
DATE <- paste(YEAR, '01-04', sep = '-')
bulls <- bids2[bids2$bull == TRUE,]
bears <- bids2[bids2$bull == FALSE,]

# Change in bid by post history.
fit <- lm(bids2$bid ~ I(bids2$history - mean(bids2$history)))
fit

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(x = date, y = bid))
g <- g + geom_point()
g <- g + ggtitle(paste0(YEAR, ' S&P 500 Predictions'))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + xlab('Date')
g <- g + ylab('S&P 500 Prediction')
g <- g + geom_smooth(method = "lm", se=FALSE, color="red")

# Save chart.
x <- as.POSIXlt(max(bids2$date))
x$mday <- x$mday - 1
x <- as.POSIXct(x)
saveChart(g, paste0('images/bids-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = x, y = 0))
print(g)

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(x = history, y = bid))
g <- g + geom_point()
g <- g + ggtitle('Prediction vs Post History')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + xlab('# of Posts')
g <- g + ylab('S&P 500 Prediction')
g <- g + geom_smooth(method = "lm", se=FALSE, color="red")

# Save chart.
saveChart(g, paste0('images/history-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = 30000, y = 0))
print(g)

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(x = date, y = bid, colour = bull))
g <- g + geom_point()
g <- g + ggtitle(paste0('Are Predictions Bullish or Bearish in ', YEAR, '?'))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + theme(legend.position='none')
g <- g + xlab('Date')
g <- g + ylab('S&P 500 Prediction')
g <- g + geom_hline(aes(yintercept = OPEN))
g <- g + scale_colour_manual(values = c('red', 'darkgreen'))
g <- g + geom_text(aes(x=max(bids2$date), y=OPEN-40, label=round(OPEN, 2)), color='black', size=3)
g <- g + geom_text(aes(x=as.POSIXct(DATE), y=3150, label= paste0('Bulls = ', nrow(bulls))), color='darkgreen', size=10)
g <- g + geom_text(aes(x=as.POSIXct(DATE), y=1350, label= paste0('Bears = ', nrow(bears))), color='red', size=10)

# Save chart.
saveChart(g, paste0('images/bullsvsbears-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = as.POSIXct(DATE), y = 0))
print(g)

stats <- data.frame(Lowest = min(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid), Average = mean(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid), Highest = max(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid))

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(bid))
g <- g + geom_histogram(binwidth=100, col='gray', alpha = .7)
g <- g + ggtitle('Histogram of Predictions')
g <- g + theme_bw()
g <- g + xlab('S&P 500 Prediction')
g <- g + ylab('# of Guesses in Range')
g <- g + geom_vline(aes(xintercept = stats$Average), colour = 'blue')
g <- g + annotate("text", x = c(stats$Average), y=c(170), label = paste0('Average = ', round(stats$Average, 2)), size = 6, colour = 'blue')

# Save chart.
saveChart(g, paste0('images/histogram-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = stats$Highest - 500, y = 0), 0, 6)
print(g)

g <- ggplot(melt(stats), aes(x = variable, y = value))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle(paste0('Overview of ', YEAR, ' Predictions'))
g <- g + theme_bw()
g <- g + xlab('')
g <- g + ylab('S&P 500 Prediction')
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(legend.title=element_blank())
g <- g + annotate("text", x = c(1,2,3), y=c(stats$Lowest / 2, stats$Average / 2, stats$Highest / 2), label = c(stats$Lowest, round(stats$Average, 2), stats$Highest), colour = 'white')

# Save chart.
saveChart(g, paste0('images/overview-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = 3, y = 0))
print(g)