Library(tinymodels)
library(readxl)
data <- read_excel("~/Desktop/data.xlsx", 
                   +                    col_types = c("numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", "numeric", 
                                                      +                                  "numeric", "numeric", "numeric", "numeric"))
data <- 
  +               data %>% 
  +               mutate(bugbinary = factor(bugbinary))
data_split <- initial_split(data)
data_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  +               set_engine("ranger") %>%
  +               fit(bugbinary ~ ., data = training(data_split))
predict(data_ranger, testing(data_split))
data_ranger %>%
  +     predict(testing(data_split)) %>%
  +     bind_cols(testing(data_split)) %>%
  +     metrics(truth = bugbinary, estimate = .pred_class)
data_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  +     set_engine("randomForest") %>%
  +     fit(bugbinary ~ ., data = training(data_split))
predict(data_rf, testing(data_split))
data_rf %>%
  +     predict(testing(data_split)) %>%
  +     bind_cols(testing(data_split)) %>%
  +     metrics(truth = bugbinary, estimate = .pred_class)