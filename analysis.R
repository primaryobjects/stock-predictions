## Including the required R packages.
packages <- c('ggplot2', 'reshape2')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(ggplot2)
library(reshape2)

source('collect.R')

analyze <- function(originals, cleans, opens) {
  # Merge bids into single dataset and save CSV.
  bids <- list()
  bids$original <- do.call('rbind', originals)
  bids$clean <- do.call('rbind', cleans)
  
  # Create CSV dataset.
  write.csv(bids$original, file='data/predictions-raw.csv')
  write.csv(bids$clean, file='data/predictions.csv')
  
  bullsBears <- data.frame(year=numeric(), bulls=numeric(), bears=numeric())
  
  ### PLOTTING
  sapply(seq(1:length(cleans)), function(i) {
    data <- cleans[[i]]
    OPEN <- opens[[i]]

    YEAR <- as.numeric(format(as.POSIXct(data[1,]$date), '%Y'))
    DATE <- paste(YEAR, '01-04', sep = '-')
    bulls <- data[data$bull == TRUE,]
    bears <- data[data$bull == FALSE,]

    bullsBears <<- rbind(bullsBears, list(year=YEAR, bulls=nrow(bulls), bears=nrow(bears)))
    
    # Change in bid by post history.
    fit <- lm(data$bid ~ I(data$history - mean(data$history)))
    fit

    # Create folder.
    dir.create(paste('images', YEAR, sep='/'), showWarnings=FALSE)

    g <- ggplot(data[data$bid <= MAX_BID_TO_IGNORE,], aes(x = date, y = bid))
    g <- g + geom_point()
    g <- g + ggtitle(paste0(YEAR, ' S&P 500 Predictions'))
    g <- g + theme_bw()
    g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
    g <- g + xlab('Date')
    g <- g + ylab('S&P 500 Prediction')
    g <- g + geom_smooth(method = "lm", se=FALSE, color="red")

    # Save chart.
    x <- as.POSIXlt(max(data$date))
    x$mday <- x$mday - 1
    x <- as.POSIXct(x)
    saveChart(g, paste0('images/', YEAR, '/bids-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = x, y = 0))
    print(g)

    g <- ggplot(data[data$bid <= MAX_BID_TO_IGNORE,], aes(x = history, y = bid))
    g <- g + geom_point()
    g <- g + ggtitle('Prediction vs Post History')
    g <- g + theme_bw()
    g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
    g <- g + xlab('# of Posts')
    g <- g + ylab('S&P 500 Prediction')
    g <- g + geom_smooth(method = "lm", se=FALSE, color="red")

    # Save chart.
    saveChart(g, paste0('images/', YEAR, '/history-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = 30000, y = 0))
    print(g)

    g <- ggplot(data[data$bid <= MAX_BID_TO_IGNORE,], aes(x = date, y = bid, colour = bull))
    g <- g + geom_point()
    g <- g + ggtitle(paste0('Are Predictions Bullish or Bearish in ', YEAR, '?'))
    g <- g + theme_bw()
    g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
    g <- g + theme(legend.position='none')
    g <- g + xlab('Date')
    g <- g + ylab('S&P 500 Prediction')
    g <- g + geom_hline(aes(yintercept = OPEN))
    g <- g + scale_colour_manual(values = c('red', 'darkgreen'))
    g <- g + geom_text(aes(x=max(data$date), y=OPEN-40, label=round(OPEN, 2)), color='black', size=3)
    g <- g + geom_text(aes(x=as.POSIXct(DATE), y=3150, label= paste0('Bulls = ', nrow(bulls))), color='darkgreen', size=10)
    g <- g + geom_text(aes(x=as.POSIXct(DATE), y=1350, label= paste0('Bears = ', nrow(bears))), color='red', size=10)

    # Save chart.
    saveChart(g, paste0('images/', YEAR, '/bullsvsbears-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = x, y = 0))
    print(g)

    stats <- data.frame(Lowest = min(data[data$bid <= MAX_BID_TO_IGNORE,]$bid), Average = mean(data[data$bid <= MAX_BID_TO_IGNORE,]$bid), Highest = max(data[data$bid <= MAX_BID_TO_IGNORE,]$bid))

    g <- ggplot(data[data$bid <= MAX_BID_TO_IGNORE,], aes(bid))
    g <- g + geom_histogram(binwidth=100, col='gray', alpha = .7)
    g <- g + ggtitle('Histogram of Predictions')
    g <- g + theme_bw()
    g <- g + xlab('S&P 500 Prediction')
    g <- g + ylab('# of Guesses in Range')
    g <- g + geom_vline(aes(xintercept = stats$Average), colour = 'blue')
    g <- g + annotate("text", x = c(stats$Average), y=c(170), label = paste0('Average = ', round(stats$Average, 2)), size = 6, colour = 'blue')

    # Save chart.
    saveChart(g, paste0('images/', YEAR, '/histogram-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = stats$Highest - 500, y = 0), 0, 6)
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
    saveChart(g, paste0('images/', YEAR, '/overview-', YEAR, '.png'), aes(label = 'primaryobjects.com', x = 3, y = 0))
    print(g)
  })
  
  # Transpose the data.frame into a format for plotting with variable colors.                       
  bullBearCounts <- melt(bullsBears, id='year')

  # Add column of percentage of bearishness for each year.  
  bullBearCounts <- cbind(bullBearCounts, bearish=bullBears[bullBears$variable == 'bears',]$value / bullBears[bullBears$variable == 'bulls',]$value)
  
  # Draw bar chart of bulls vs bears across the years.
  g <- ggplot(bullBearCounts, aes(x = year, y = value, fill = variable))
  g <- g + geom_bar(alpha=I(.9), stat='identity')
  g <- g + ggtitle('Bulls vs Bears by Year')
  g <- g + theme_bw()
  g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g + xlab('Year')
  g <- g + ylab('Number of Predictions')
  g <- g + scale_fill_manual(values=c('#00bb00', '#bb0000'), labels=c('Bulls', 'Bears'))
  g <- g + theme(legend.title=element_blank())
  g <- g + geom_text(aes(label=value), position=position_stack(vjust=0.5), vjust=0, size=4, colour='#ffffff')

  # Save chart.
  x <- as.numeric(YEAR) - 1
  saveChart(g, 'images/bulls-vs-bears.png', aes(label = 'primaryobjects.com', x = x, y = 0), 0, 7)
  print(g)

  # Draw line chart of bearishness across the years.
  g <- ggplot(bullBears[bullBears$variable=='bears',], aes(x = year, y = bearish * 100))
  g <- g + geom_line(alpha=I(.9), colour='red', size=2)
  g <- g + ggtitle('Bearish Sentiment by Year')
  g <- g + theme_bw()
  g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g + xlab('Year')
  g <- g + ylab('Percent of Bearish Predictions')
  g <- g + theme(legend.title=element_blank())
  g <- g + geom_text(aes(label=paste0(round(bearish * 100), '%')), alpha=I(.7), vjust=-1.2, size=4, colour='black')
  
  # Save chart.
  x <- as.numeric(YEAR) - 1
  saveChart(g, 'images/bear-sentiment.png', aes(label = 'primaryobjects.com', x = x, y = 0), 0, 7)
  print(g)
  
  bullBearCounts
}

# Collect bids.
bids2010 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=11&t=47946&start=', 0, 700, 50, '2010-01-11')
bids2011 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=65806&start=', 0, 700, 50, '2011-01-11')
bids2012 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=87982&start=', 0, 700, 50, '2012-01-11')
bids2013 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=107946&start=', 0, 700, 50, '2013-01-11')
bids2014 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?t=129539&start=', 0, 700, 50, '2014-01-11')
bids2015 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?t=154407&start=', 0, 700, 50, '2015-01-11')
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

r <- analyze(list(bids2010$original, bids2011$original, bids2012$original, bids2013$original, bids2014$original, bids2015$original, bids2016$original, bids2017$original),
             list(bids2010$clean, bids2011$clean, bids2012$clean, bids2013$clean, bids2014$clean, bids2015$clean, bids2016$clean, bids2017$clean),
             list(bids2010$open, bids2011$open, bids2012$open, bids2013$open, bids2014$open, bids2015$open, bids2016$open, bids2017$open))