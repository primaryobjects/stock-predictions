## Including the required R packages.
packages <- c('XML', 'stringr', 'ggplot2', 'reshape2', 'RCurl')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
#

library(XML)
library(stringr)
library(ggplot2)
library(reshape2)
library(RCurl)

MAX_BID_TO_IGNORE <- 5000 # Bids beyond this value will be skipped in graphs.
YEAR <- format(Sys.Date(), '%Y')

getBids <- function(fileName) {
  # Read html page from file.
  data <- htmlTreeParse(fileName, useInternalNodes = T)
  
  # Find all ids.
  ids <- xpathSApply(data, "//div[contains(@class, 'post')]/@id")
    
  # Find all authors.
  authors <- xpathApply(data, "//div[@class='postbody']//p[@class='author']//a[@class='username' or @class='username-coloured']/text()")
  
  # Find all posts counts by authors.
  postCounts <- as.numeric(xpathApply(data, "//dd[@class='profile-posts']//a/text()", xmlValue))
  
  # Find all dates.
  dates <- xpathApply(data, "//div[@class='postbody']//p[@class='author']/text()")
  # Format dates.
  dates <- sapply(dates, xmlValue)
  dates <- as.POSIXlt(dates, format = '%a %b %d, %Y %I:%M %p')
  
  # Find bids.
  text <- xpathApply(data, "//div[@class='postbody']//div[@class='content']")
  bids <- sapply(text, function(body) {
    # Convert xml to text.
    body <- saveXML(body)
    
    # Remove blockquote.
    body <- sub('<blockquote>.*</blockquote>', '', body)
    
    # Convert text back into xml.
    doc = htmlParse(body, asText=TRUE)
    
    # Get text from html.
    body <- xpathSApply(doc, "//div", xmlValue)
    body <- paste(body, collapse = "\n")
    
    # Extract the bid. We could round the value to 2 decimal places, but we'll leave entire value intact.
    as.numeric(sub(',', '', str_extract(body, '\\d{1},?\\d{3}?(\\.\\d+)?')))
  })
  
  # Return result.
  data.frame(id = ids, author = sapply(authors, xmlValue), history = postCounts, date = dates, bid = bids, stringsAsFactors = FALSE)
}

crawl <- function(url, start, stop, size = 50) {
  bids <- data.frame()
  start <- 0

  path <- file.path('.', 'output')
  if (dir.exists(path)) {
    # Delete any old files.
    unlink(paste0(path, '/*.html'))
  }
  
  # Create folder.
  dir.create(path, showWarnings=FALSE)
  
  for (start in seq(from=start, to=stop, by=size)) {
    # Setup filename.
    fileName <- paste0('output/page', start, '.html')
    
    # Download the page, only if it doesn't already exist in the output folder.
    if (!file.exists(fileName)) {
      downloadUrl = paste0(url, start)
      download.file(downloadUrl, fileName)
    }
    
    # Get bids from page.
    collected <- getBids(fileName)
    if (!collected$id %in% bids$id) {
      bids <- rbind(bids, collected)
    }
    else {
      break
    }
  }
  
  bids
}

collectBids <- function(url, start, stop, size, endDate) {
  # Crawl the url for posts.
  bids <- crawl(url, start, stop, size)
  
  # Remove intro post by Taylor Larimore.
  bids2 <- bids[-1,]

  # Remove bids past endDate, which is the final time of accepted bids.
  bids2 <- bids2[bids2$date <= as.POSIXct(endDate),]
  
  # Remove NA bids.
  bids2 <- bids2[!is.na(bids2$bid),]
  
  # Find indices of duplicates by the same author.
  dups <- which(duplicated(bids2$author, fromLast = T))
  # Get duplicate rows (for info only).
  duplicates <- bids2[bids2$author %in% bids2[dups,'author'], ]
  # Remove duplicates.
  bids2 <- bids2[-dups,]
  
  # Get opening price on first trading day of year, reported by finance.yahoo.com historical prices.
  year <- format(as.POSIXct(endDate), '%Y')
  history <- read.csv(paste0('http://chart.finance.yahoo.com/table.csv?s=^GSPC&a=0&b=1&c=', year, '&d=0&e=5&f=', year, '&g=d&ignore=.csv'))
  open <- history[nrow(history),'Open']
  
    # Set boolean if bid was greater or equal to open price on 1/4/2015.
  bids2$bull <- bids2$bid >= open

  list(original = bids, clean = bids2, duplicates = duplicates, open = open)
}

# Collect bids.
bids2016 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=180855&start=', 0, 700, 50, '2016-01-11')

## BEGIN MANUAL FIXES
bids2016$clean[bids2016$clean$author == 'mickeyd','bid'] <- 2146.14 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'alisa4804','bid'] <- 2319.87 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'CUBuffs','bid'] <- 2108.00 # put year 2016 before bid
bids2016$clean[bids2016$clean$author == 'saltycaper','bid'] <- 2131.82 # He made two bids, first mentioned 2131 and then said "So, I will guess 2131.82."
bids2016$clean[bids2016$clean$author == 'snarlyjack','bid'] <- 1950 # put year 2016 before bid ("S & P 12-31-2016")
bids2016$clean[bids2016$clean$author == 'grok87','bid'] <- 2126 # put year 2015 after bid
bids2016$clean[bids2016$clean$author == 'rarvesen','bid'] <- 1850 # put year 2016 before bid
## END MANUAL FIXES

bids2017 <- collectBids('https://www.bogleheads.org/forum/viewtopic.php?f=10&t=206949&start=', 0, 700, 50, '2017-01-11')

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
g <- g + ggtitle(paste0(YEAR, ' Bogleheads Contest Bids'))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + xlab('Date')
g <- g + ylab('Guess')
g <- g + geom_smooth(method = "lm", se=FALSE, color="red")
print(g)

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(x = history, y = bid))
g <- g + geom_point()
g <- g + ggtitle('Does Post History Make a Boglehead Bullish or Bearish?')
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + xlab('# of Posts')
g <- g + ylab('Guess')
g <- g + geom_text(aes(x=16000, y=2800, label= 'Guesses decrease -$0.01 for every post :('), color='red', size=7)
g <- g + geom_smooth(method = "lm", se=FALSE, color="red")
print(g)

g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(x = date, y = bid, colour = bull))
g <- g + geom_point()
g <- g + ggtitle(paste0('Are Bogleheads Bullish or Bearish in ', YEAR, '?'))
g <- g + theme_bw()
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank())
g <- g + theme(legend.position='none')
g <- g + xlab('Date')
g <- g + ylab('Guess')
g <- g + geom_hline(aes(yintercept = OPEN))
g <- g + scale_colour_manual(values = c('red', 'darkgreen'))
g <- g + geom_text(aes(x=as.POSIXct(DATE), y=3150, label= paste0('Bulls = ', nrow(bulls))), color='darkgreen', size=10)
g <- g + geom_text(aes(x=as.POSIXct(DATE), y=1350, label= paste0('Bears = ', nrow(bears))), color='red', size=10)
print(g)

stats <- data.frame(Lowest = min(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid), Average = mean(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid), Highest = max(bids2[bids2$bid <= MAX_BID_TO_IGNORE,]$bid))

#inPopularRange <- nrow(bids2[bids2$bid >= 2182 & bids2$bid <= 2382,]) / nrow(bids2) * 100
g <- ggplot(bids2[bids2$bid <= MAX_BID_TO_IGNORE,], aes(bid))
g <- g + geom_histogram(binwidth=100, col='gray', alpha = .7)
g <- g + ggtitle('Histogram of Guesses')
g <- g + theme_bw()
g <- g + xlab('Guess')
g <- g + ylab('# of Guesses in Range')
g <- g + geom_vline(aes(xintercept = stats$Average), colour = 'blue')
g <- g + annotate("text", x = c(stats$Average), y=c(170), label = paste0('Average = ', round(stats$Average, 2)), size = 6, colour = 'blue')
#g <- g + annotate("text", x = c(2750), y=c(180), label = paste0(round(inPopularRange), '% of Guesses'), size = 8)
#g <- g + annotate("text", x = c(2665), y=c(150), label = 'Within 100pts', size = 8)
print(g)

g <- ggplot(melt(stats), aes(x = variable, y = value))
g <- g + geom_bar(alpha=I(.9), stat='identity')
g <- g + ggtitle(paste0('Overview of ', YEAR, ' Bogleheads Contest Bids'))
g <- g + theme_bw()
g <- g + xlab('')
g <- g + ylab('Guess')
g <- g + theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(legend.title=element_blank())
g <- g + annotate("text", x = c(1,2,3), y=c(stats$Lowest / 2, stats$Average / 2, stats$Highest / 2), label = c(stats$Lowest, round(stats$Average, 2), stats$Highest), colour = 'white')
print(g)

# Show guesses within .2 range of average. Did they do this on purpose? :)
#bids2[bids2$bid >= stats$Average - .2 & bids2$bid <= stats$Average + .2,]