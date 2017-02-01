library(Hmisc)

calculateLogs <- function(dataFrame) {
    print("[calculateLogs]")
    for (n in names(dataFrame)) {
        if (!endsWith(n, ".log") && !endsWith(n, ".exp")) {
            dataFrame[[paste(n, 'log', sep = ".")]] <- log(dataFrame[[n]])
        }
    }

    dataFrame
}

calculateExps <- function(dataFrame) {
    print("[calculateExps]")
    for (n in names(dataFrame)) {
        if (!endsWith(n, ".log") && !endsWith(n, ".exp")) {
            dataFrame[[paste(n, 'exp', sep = ".")]] <- exp(dataFrame[[n]])
        }
    }

    dataFrame
}

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

calculateRegressionWithQuality <- function(dataFrame) {
    print("[calculateRegressionWithQuality]")

    names1 <- c()
    names2 <- c()
    corrValues <- c()
    pValues <- c()
    corrValuesModule <- c()

    qualityNames <- c("quality", "quality.log", "quality.exp")

    for (n1 in names(dataFrame)) {
        for (n2 in qualityNames) {
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

    corrValuesModule <- abs(corrValues)

    results = data.frame(names1, names2, corrValues, pValues, corrValuesModule)

    results
}

oldOp <- options()

options(max.print = 999)

whiteWineData <- read.csv("d:/GitHub/Data/Dane/wine-quality/winequality-white.csv", header = TRUE, sep = ";", dec = ".")

whiteWineDataCopy <- whiteWineData

whiteWineDataWithLogs <- calculateLogs(whiteWineData)

whiteWineDataWithLogs <- calculateExps(whiteWineDataWithLogs)

whiteWineDataWithLogsFiltered <- whiteWineDataWithLogs[complete.cases(whiteWineDataWithLogs),]

regRes2 <- calculateRegressionWithQuality(whiteWineDataWithLogs)

regRes2Filetered <- regRes2[complete.cases(regRes2),]

regRes2Filetered2 <- regRes2Filetered[(regRes2Filetered$names2 == "quality"),]

regRes2FilteredOrdered <- regRes2Filetered2[with(regRes2Filetered2, order(corrValuesModule)),]

#regRes <- calculateRegression(whiteWineDataWithLogs)

#regResFiletered <- regRes[complete.cases(regRes),]

#regResFiletered[abs(regResFiletered$corrValues) >= 0.3,]

#regResFiletered[((regResFiletered$names1 == "quality") | (regResFiletered$names2 == "quality") | (regResFiletered$names1 == "quality.log") | (regResFiletered$names2 == "quality.log")),]

#redWineData <- read.csv("d:/GitHub/Data/Dane/wine-quality/winequality-red.csv", header = TRUE, sep = ";", dec = ".")

#certRange <- range(certAsk[, 2], certBid[, 2])
#print(certRange)
#cor(mtcars, use="complete.obs", method="kendall")

#cor(whiteWineData, use = "complete.obs", method = "kendall")

plot(whiteWineDataWithLogsFiltered$alcohol, whiteWineDataWithLogsFiltered$quality)

plot(whiteWineDataWithLogsFiltered$chlorides.log, whiteWineDataWithLogsFiltered$quality)

#alcohol +
#density.log -
#chlorides.log -
#volatile.acidity.log -
#total.sulfur.dioxide -
#fixed.acidity -

