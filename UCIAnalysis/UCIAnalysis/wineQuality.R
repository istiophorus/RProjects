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

splitData <- function(inputRecords, splitRatio, seedValue) {
    print("[splitData]")
    print(nrow(inputRecords))

    ## 75% of the sample size
    smp_size <- floor(splitRatio * nrow(inputRecords))

    ## set the seed to make your partition reproductible
    set.seed(seedValue)
    train_ind <- sample(seq_len(nrow(inputRecords)), size = smp_size)

    trainData <- inputRecords[train_ind,]
    testData <- inputRecords[ - train_ind,]

    result <- list()
    result[[1]] <- trainData
    result[[2]] <- testData
    result
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

splittedData <- splitData(whiteWineDataWithLogsFiltered, 0.7, 123456)

print(nrow(splittedData[[1]]))
print(nrow(splittedData[[2]]))
print(nrow(whiteWineDataWithLogsFiltered))

trainingData <- splittedData[[1]]
testData <- splittedData[[2]]

regModel <- lm(formula = quality ~ alcohol + density.log + chlorides.log + volatile.acidity.log + total.sulfur.dioxide + fixed.acidity, data = trainingData)

regRes2 <- calculateRegressionWithQuality(whiteWineDataWithLogs)

regRes2Filetered <- regRes2[complete.cases(regRes2),]

regRes2Filetered2 <- regRes2Filetered[(regRes2Filetered$names2 == "quality"),]

regRes2FilteredOrdered <- regRes2Filetered2[with(regRes2Filetered2, order(corrValuesModule)),]

#alcohol +
#density.log -
#chlorides.log -
#volatile.acidity.log -
#total.sulfur.dioxide -
#fixed.acidity -

predict(regModel, testData, interval = "confidence")
