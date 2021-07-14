library(ECoL)

tab <- matrix(data = NA, nrow = 1, ncol = 25)
tab <- data.frame(tab)

overlap <- overlapping(bugbinary~., data, measures= "all")
neighbor <- neighborhood(bugbinary~., data, measures= "all")
lin <- linearity(bugbinary~., data, measures= "all")
dim <- dimensionality(bugbinary~., data, measures= "all")
bal <- balance(bugbinary~., data, measures= "all")
net <- network(bugbinary~., data, measures= "all")
corr <- correlation(bugbinary~., data, measures= "all")
smooth <- smoothness(bugbinary~., data, measures= "all")

tab[nrow(tab)+1,] = c(overlap$F1[1],overlap$F1v[1],overlap$F2[1], overlap$F3[1],overlap$F4[1],neighbor$N1[1],neighbor$N2[1],neighbor$N3[1],neighbor$N4[1],neighbor$T1[1],neighbor$LSC[1],lin$L1[1],lin$L2[1],lin$L3[1],dim[1],dim[2],dim[3],bal$C1,bal$C2,net$Density,net$ClsCoef,net$Hubs[1],corr$C2[1],corr$C3[1],corr$C4[1])