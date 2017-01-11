library(jsonlite)

getToday <- function() {
    as.Date(Sys.Date(), format = "%y-%m-%d")
}

getIngCertData <- function(symbol, period) {
    resource = paste("https://www.ingturbo.pl/services/product/", symbol, "/chart?period=", period, sep = "")
    print(resource)
    fromJSON(resource)
}

getIngBaseData <- function(symbol, period) {
    resource = paste("https://www.ingturbo.pl/services/underlying/", symbol, "/chart?period=", period, sep = "")
    print(resource)
    fromJSON(resource)
}

getWyborczaUrl <- function(symbol, dateValue) {
    resource = paste("http://xml.wyborcza.biz/exchangeFlashChartsData.servlet?p5=", symbol, "&p7=ONE_WEEK&p9=", dateValue, "&instrumentType=SHARE&disableRedirects=true", sep = "")
    print(resource)
    resource
}

getDataFromWyborcza <- function(symbol) {
    resource <- getWyborczaUrl(symbol, getToday())
    data <- read.csv(file = resource, header = FALSE, sep = ",")
    data
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
    certItems <- read.csv(file = "d:/GitHub/definitions.csv ", header = TRUE, sep = ",")
    certItems
}

getAllData <- function(itemsList, period) {
    print("[getAllData]")
    results <- list()

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)
        if (currentItem$source == 'ing') {
            currentItemData <- getIngCertData(currentItem$certs, period)
            results[[i]] <- currentItemData
        }
        else if (currentItem$source == 'wyb') {
            currentItemData <- getDataFromWyborcza(currentItem$certs)
            results[[i]] <- currentItemData
        }
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

        if (currentItem$drawGraph) {
            if (currentItem$source == 'ing') {
                currentItemData <- allData[[i]]

                drawCertGraphOnScreen(screenIndex, currentItem$titles, currentItemData)
                screenIndex <- screenIndex + 1
            }
            else if (currentItem$source == 'wyb') {

            }
        }
    }
}

calculateProfit <- function(itemsList, allData) {
    print("[calculateProfit]")
    summaryValue <- 0

    ingIndex <- 1

    for (i in 1:nrow(itemsList)) {
        print(i)
        currentItem = itemsList[i,]
        print(currentItem)

        currentItemData <- allData[[i]]

        if (currentItem$source == 'ing') {
            lastValue <- currentItemData$BidQuotes[nrow(currentItemData$BidQuotes), 2]
        }
        else {
            lastValue <- currentItemData[2,]$V5
        }

        print(lastValue)

        if (is.numeric(lastValue)) {
            outcome <- currentItem$prices * currentItem$amount * (1 - 0.0038)
            income <- lastValue * currentItem$amount * (1 - 0.0038)

            bilans <- income - outcome
            print(bilans)
            print(bilans / outcome)

            summaryValue <- summaryValue + bilans
        }
    }

    summaryValue
}

loadCertDetails <- function() {
    print("[loadCertDetails]")
    certDetails <- read.csv(file = "d:/GitHub/export.csv", header = TRUE, sep = ";")
    certDetails
}

mergeData <- function(itemsList, certDetails) {
    print("[mergeData]")
    buyPriceIndex = 3
    amountIndex = 2

    newItemsList <- list()

    for (ix in 1:nrow(itemsList)) {
        print(ix)
        currentItem = itemsList[ix,]

        currentItem$prices <- certDetails[currentItem$rowIndex, buyPriceIndex]
        currentItem$amount <- certDetails[currentItem$rowIndex, amountIndex]

        print(currentItem)

        itemsList[ix,] <- currentItem
    }

    itemsList
}

refreshAllDataAndCalculateprofit <- function() {
    print("[refreshAllDataAndCalculateprofit]")
    itemsList <- initializeData()
    allData <- getAllData(itemsList, "intraday")
    certDetails <- loadCertDetails()
    newItemsList <- mergeData(itemsList, certDetails)
    calculateProfit(newItemsList, allData)
}

drawAllIntradayGraphs <- function() {
    itemsList <- initializeData()
    allData <- getAllData(itemsList, "intraday")
    drawAllGraphs(itemsList, allData)
}

drawAllWeekGraphs <- function() {
    itemsList <- initializeData()
    allDataWeek <- getAllData(itemsList, "week")
    drawAllGraphs(itemsList, allDataWeek)
}

#certDetails <- loadCertDetails()
#newItemsList <- mergeData(itemsList, certDetails)
#calculateProfit(newItemsList, allData)

refreshAllDataAndCalculateprofit()

drawAllWeekGraphs()

