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

eventsData <- read.csv(file = "d:/GitHub/Data/Dane/ForestFires/forestfiresPrepared.csv ", header = TRUE, sep = ",")
splitRatio = 0.70

trainData = splitData(eventsData, splitRatio, 123456)