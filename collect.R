## Including the required R packages.
packages <- c('XML', 'stringr', 'ggplot2', 'grid', 'gridExtra', 'reshape2', 'RCurl')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(XML)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
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

saveChart <- function(chart, fileName, aesthetic, hjust = 0, vjust = 8) {
  # Draw attribution.
  chart <- chart + geom_text(aesthetic, hjust = hjust, vjust = vjust, color="#a0a0a0", size=3.5)
  
  # Disable clip-area.
  gt <- ggplot_gtable(ggplot_build(chart))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
  
  # Save chart.
  #ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
  chart <- arrangeGrob(gt)
  ggsave(fileName, chart, dpi=100, width=9, height=7)
}