library(png)
library(bitops)
library(RCurl)

initializeGraphItemsData <- function() {
    data = read.csv(file = "d:/GitHub/graphInput.csv", header = TRUE, sep = ",")
    data
}

getStooqGraphUrl <- function(symbol, period) {
    resource = paste("http://stooq.com/c/?s=", symbol, "&c=", period, "&t=l&a=ln", sep="")
    resource
}

drawSingleImageOnScreen <- function(screenIndex, currentImage, title) {
    print("[drawSingleImageOnScreen]")
    print(title)
    print(screenIndex)
    screen(screenIndex)

    plot(0, type = 'n', xlim = 0:1, ylim = 0:1, main = title, axes = FALSE)
    rasterImage(currentImage, 0, 0, 1, 1)
}

drawDownloadedGraphs <- function(itemsList, imagesAndTitles) {
    print("[drawDownloadedGraphs]")
    close.screen(4, all.screens = TRUE)

    par(bg = "white", mfrow = c(1, 1), mar = c(1, 1, 1, 1))

    # prepare screens

    split.screen(c(3, 1))
    split.screen(c(1, 3), screen = 1)
    split.screen(c(1, 3), screen = 2) 
    split.screen(c(1, 3), screen = 3) 

    images = imagesAndTitles[[1]]
    titles = imagesAndTitles[[2]]

    screenIndex = 4

    for (i in 1:length(images)) {
        print(i)
        currentImage = images[[i]]
        currentTitle = titles[[i]]

        print(currentTitle)
        print(length(currentImage))

        drawSingleImageOnScreen(screenIndex, currentImage, currentTitle)
        screenIndex = screenIndex + 1
    }
}

getSingleImage <- function(imageUrl) {
    image = readPNG(getURLContent(imageUrl))
    image
}

getImages <- function(itemsData, periodSymbol) {
    print("[getImages]")

    si = nrow(itemsData)

    results = list()
    imageIndex = 1
    titles = list()

    for (i in 1:si) {
        currentItem = itemsData[i,]
        print(currentItem)
        if (currentItem$getGraph) {

            if (is.null(periodSymbol)) {
                currentPeriodSymbol = currentItem$graphPeriod
            }
            else {
                currentPeriodSymbol = periodSymbol
            }

            itemUrl = getStooqGraphUrl(currentItem$symbol, currentPeriodSymbol)
            print(itemUrl)
            nextImage = getSingleImage(itemUrl)
            results[[imageIndex]] <- nextImage
            titles[[imageIndex]] <- currentItem$symbol
            imageIndex = imageIndex + 1
        }
    }

    res = list()
    res[[1]] = results
    res[[2]] = titles

    res
}

itemsData <- initializeGraphItemsData()
imagesWithTitles <- getImages(itemsData, NULL)
drawDownloadedGraphs(itemsData, imagesWithTitles)
