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

adidasWeek <- getIngBaseData("adidas", "week")
adidasCertIntraday <- getIngCertData("PLINGNV14787", "intraday")
pgeCertIntraday <- getIngCertData("PLINGNV16725", "intraday")
pgnCertIntraday <- getIngCertData("PLINGNV04713", "intraday")
bundCertIntraday <- getIngCertData("PLINGNV00497", "intraday")

drawCertGraph <- function(graphTitle, inputData) {
    certAsk <- inputData$AskQuotes
    certBid <- inputData$BidQuotes
    certRange <- range(certAsk[,2], certBid[,2])
    print(certRange)
    plot(certAsk[, 2], type = "l", col = "green", ylim = certRange)
    lines(certBid[, 2], type = "l", col = "red")
    title(main = graphTitle)
}