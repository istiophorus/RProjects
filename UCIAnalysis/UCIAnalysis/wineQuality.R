library(Hmisc)

calculateLogs <- function(dataFrame) {
    print("[calculateLogs]")
    for (n in names(dataFrame)) {
        dataFrame[[paste(n, 'log', sep = ".")]] <- log(dataFrame[[n]])
    }

    dataFrame
}

whiteWineData <- read.csv("d:/GitHub/Data/Dane/wine-quality/winequality-white.csv", header = TRUE, sep = ";", dec = ".")

whiteWineDataCopy <- whiteWineData

whiteWineDataWithLogs <- calculateLogs(whiteWineData)

calculateRegression <- function(dataFrame) {
    print("[calculateRegression]")

    names1 <- c()
    names2 <- c()
    corrValues <- c()
    pValues <- c()

    for (n1 in names(dataFrame)) {
        for (n2 in names(dataFrame)) {
            if (n1 < n2) {
                if (!startsWith(n1, n2) && !startsWith(n2, n1)) {
                    print(n1)
                    print(n2)

                    dataFrame[[n1]]

                    #currentModel = lm(dataFrame[[n1]] ~ dataFrame[[n2]], data = dataFrame)

                    currentModel = cor.test(dataFrame[[n1]], dataFrame[[n2]], method = "pearson")

                    #print(currentModel)

                    names1 <- append(names1, n1)
                    names2 <- append(names2, n2)
                    corrValues <- append(corrValues, currentModel[[4]])
                    pValues <- append(pValues, currentModel$p.value)
                }
            }
        }
    }

    results = data.frame(names1, names2, corrValues, pValues)

    results
}

oldOp <- options()
options(max.print = 999)

whiteWineDataCopy <- whiteWineData

whiteWineDataWithLogsFiltered <- whiteWineDataWithLogs[complete.cases(whiteWineDataWithLogs),]

whiteWineDataWithLogsMatrix <- as.matrix(whiteWineDataWithLogsFiltered)

regRes <- calculateRegression(whiteWineDataWithLogs)

regResFiletered <- regRes[complete.cases(regRes),]

regResFiletered[abs(regResFiletered$corrValues) >= 0.3,]

#redWineData <- read.csv("d:/GitHub/Data/Dane/wine-quality/winequality-red.csv", header = TRUE, sep = ";", dec = ".")

#certRange <- range(certAsk[, 2], certBid[, 2])
#print(certRange)
#cor(mtcars, use="complete.obs", method="kendall")

#cor(whiteWineData, use = "complete.obs", method = "kendall")

#plot(whiteWineData$alcohol, whiteWineData$quality)


