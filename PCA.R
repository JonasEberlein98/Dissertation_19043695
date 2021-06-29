library(tidyverse)
library(tidymodels)
library(rpart)

data <- PROMISE_ivy14

#following https://cmdlinetips.com/2020/06/pca-with-tidymodels-in-r/


pca_estimates <- prep(pca_trans)
pca_estimates
sdev <- pca_estimates$steps[[3]]$res$sdev
percent_variation <- sdev^2 / sum(sdev^2)
var_df <- data.frame(PC=paste0("PC",1:length(sdev)),
                     +                      var_explained=percent_variation,
                     +                      stringsAsFactors = FALSE)
var_df %>%
  +     mutate(PC = fct_inorder(PC)) %>%
  +     ggplot(aes(x=PC,y=var_explained))+geom_col()
juice(pca_estimates)

#following https://www.analyticsvidhya.com/blog/2016/03/pca-practical-guide-principal-component-analysis-python/

spl = sample.split(data$bugbinary, SplitRatio = 0.7)
dataTrain = subset(data, spl==TRUE)
dataTest = subset(data, spl==FALSE)

prin_comp <- prcomp(dataTrain, scale. = T)
prin_comp$rotation
biplot(prin_comp, scale = 0)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)

prop_varex[1:20]
plot(prop_varex, xlab = "Principal Component",
     +      ylab = "Proportion of Variance Explained",
     +      type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
       +      ylab = "Cumulative Proportion of Variance Explained",
       +      type = "b")
train.data <- data.frame(bugbinary = dataTrain$bugbinary, prin_comp$x)
#choice of 20 principle components
train.data <- train.data[,1:21]
#model
rpart.model <- rpart(bugbinary ~ .,data = train.data, method = "anova")
test.data <- predict(prin_comp, newdata = dataTest)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:20]
rpart.prediction <- predict(rpart.model, test.data)