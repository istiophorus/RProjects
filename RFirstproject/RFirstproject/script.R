library(jsonlite)

getIngCertData = function(symbol, period) {
    resource = paste("https://www.ingturbo.pl/services/product/", symbol, "/chart?period=", period, sep = "")
    print(resource)
    fromJSON(resource)
}

getIngBaseData = function(symbol, period) {
    resource = paste("https://www.ingturbo.pl/services/underlying/", symbol, "/chart?period=", period, sep = "")
    print(resource)
    fromJSON(resource)
}

#adidasWeek <- getIngBaseData("adidas", "week")
#adidasCertIntraday <- getIngCertData("PLINGNV14787", "intraday")
#pgeCertIntraday <- getIngCertData("PLINGNV16725", "intraday")
#pgnCertIntraday <- getIngCertData("PLINGNV04713", "intraday")
#bundCertIntraday <- getIngCertData("PLINGNV00497", "intraday")

drawCertGraph <- function(graphTitle, inputData) {
    certAsk <- inputData$AskQuotes
    certBid <- inputData$BidQuotes
    certRange <- range(certAsk[, 2], certBid[, 2])
    print(certRange)
    plot(certAsk[, 2], type = "l", col = "green", ylim = certRange)
    lines(certBid[, 2], type = "l", col = "red")
    title(main = graphTitle)
}

drawCertGraphOnScreen <- function(screenIndex, graphTitle, inputData) {
    screen(screenIndex)
    drawCertGraph(graphTitle, inputData)
}

initializeData <- function() {
    titles <- c("adidas", "pgn", "pge", "bund")
    certs <- c("PLINGNV14787", "PLINGNV04713", "PLINGNV16725", "PLINGNV00497")
    certItems <- data.frame(titles, certs)

    certItems
}

getAllData <- function(itemsList, period) {
    results <- list()

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)
        currentItemData <- getIngCertData(currentItem$certs, period)
        results <- append(results, currentItemData)
    }

    results
}

itemsList <- initializeData()
allData <- getAllData(d, "intraday")

drawAllGraphs <- function(itemsList, allData) {
    par(bg = "white") # erase.screen() will appear not to work
                  # if the background color is transparent 
                  # (as it is by default on most devices).

    # prepare screens

    split.screen(c(2, 1))
    split.screen(c(1, 3), screen = 1)
    split.screen(c(1, 3), screen = 2) # now split the bottom half into 3

    screenIndex = 3

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)
        currentItemData <- allData[i,]

        drawCertGraphOnScreen(screenIndex, currentItem$titles, currentItemData)
    }
}

drawAllGraphs(itemsList, allData)





#drawCertGraph("adidas", adidasCertIntraday)
#drawCertGraph("pgn", pgnCertIntraday)
#drawCertGraph("pge", pgeCertIntraday)
#drawCertGraph("bund", bundCertIntraday)
