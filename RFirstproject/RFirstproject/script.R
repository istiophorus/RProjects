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
    plot(certAsk[, 2], type = "l", col = "blue", ylim = certRange)
    lines(certBid[, 2], type = "l", col = "darkgray")
    title(main = graphTitle)
}

drawCertGraphOnScreen <- function(screenIndex, graphTitle, inputData) {
    print(screenIndex)
    screen(screenIndex)
    drawCertGraph(graphTitle, inputData)
}

initializeData <- function() {
    titles <- c("adidas", "pgn", "pge", "bund", "brent")
    certs <- c("PLINGNV14787", "PLINGNV04713", "PLINGNV16725", "PLINGNV00497", "PLINGNV12963")
    prices <- c(0, 0, 0, 0, 0)
    amount <- c(0, 0, 0, 0, 0)
    certItems <- data.frame(titles, certs, prices, amount)

    certItems
}

getAllData <- function(itemsList, period) {
    results <- list()

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)
        currentItemData <- getIngCertData(currentItem$certs, period)
        results[[i]] <- currentItemData
    }

    results
}

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
        currentItemData <- allData[[i]]

        drawCertGraphOnScreen(screenIndex, currentItem$titles, currentItemData)
        screenIndex <- screenIndex + 1
    }
}

calculateProfit <- function(itemsList, allData) {
    summaryValue <- 0

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)
        currentItemData <- allData[[i]]

        lastValue <- currentItemData$BidQuotes[nrow(currentItemData$BidQuotes), 2]
        outcome <- currentItem$prices * currentItem$amount * (1 - 0.0038)
        income <- lastValue * currentItem$amount * (1 - 0.0038)

        bilans <- income - outcome
        print(bilans)

        summaryValue <- summaryValue + bilans
    }

    summaryValue
}

loadCertDetails <- function() {
    certDetails <- read.csv(file = "d:/GitHub/export.csv", header = TRUE, sep = ";")
    certDetails
}

mergeData <- function(itemsList, certDetails) {
    #adidas
    itemsList[1,]$prices <- certDetails[3, 3]
    itemsList[1,]$amount <- certDetails[3, 2]
    #pgn
    itemsList[2,]$prices <- certDetails[7, 3]
    itemsList[2,]$amount <- certDetails[7, 2]
    #pge
    itemsList[3,]$prices <- certDetails[6, 3]
    itemsList[3,]$amount <- certDetails[6, 2]
    #bund
    itemsList[4,]$prices <- certDetails[5, 3]
    itemsList[4,]$amount <- certDetails[5, 2]
    #brent
    itemsList[5,]$prices <- certDetails[4, 3]
    itemsList[5,]$amount <- certDetails[4, 2]

    itemsList
}

refreshAllDataAndCalculateprofit <- function() {
    itemsList <- initializeData()
    allData <- getAllData(itemsList, "intraday")
    certDetails <- loadCertDetails()
    newItemsList <- mergeData(itemsList, certDetails)
    calculateProfit(newItemsList, allData)
}

itemsList <- initializeData()
allData <- getAllData(itemsList, "intraday")
drawAllGraphs(itemsList, allData)

allDataWeek <- getAllData(itemsList, "week")
drawAllGraphs(itemsList, allDataWeek)

certDetails <- loadCertDetails()
newItemsList <- mergeData(itemsList, certDetails)

calculateProfit(newItemsList, allData)







#drawCertGraph("adidas", adidasCertIntraday)
#drawCertGraph("pgn", pgnCertIntraday)
#drawCertGraph("pge", pgeCertIntraday)
#drawCertGraph("bund", bundCertIntraday)
