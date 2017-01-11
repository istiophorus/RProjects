library(png)
library(bitops)
library(RCurl)

initializeGraphItemsData <- function() {
    data <- read.csv(file = "d:/GitHub/graphInput.csv", header = TRUE, sep = ",")
    data
}

getStooqGraphUrl <- function(symbol, period) {
    resource <- paste("http://stooq.com/c/?s=", symbol, "&c=", period, "&t=l&a=ln")
    resource
}

myurl <- "http://stooq.com/c/?s=rcwhtaopen.pl&c=3y&t=l&a=ln"
my_image <- readPNG(getURLContent(myurl))

plot(0, type = 'n', xlim = 0:1, ylim = 0:1, main = "Not the best use, but this gives the idea")
rasterImage(my_image, 0, 0, 1, 1)

#http://stooq.com/c/?s=rcgldaopen.pl&c=3m&t=l&a=ln

#http://stooq.com/c/?s=rcwhtaopen.pl&c=5m&t=l&a=ln