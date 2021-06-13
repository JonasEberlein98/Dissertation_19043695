library(ECoL)
balance(bugbinary~., data, measures= "C1")
balance(bugbinary~., data, measures= "C2")
correlation(bugbinary~., data, measures= "C2")