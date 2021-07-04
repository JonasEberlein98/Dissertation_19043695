library(tidyverse)
library(tidymodels)

data <- 
  data %>% 
  mutate(bugbinary = factor(bugbinary))

#Build Model

#1 split the data
# strata to make sure both classes of bugbinary are represented in training and test
set.seed(123)
data_split <- initial_split(data, strata = bugbinary)
data_train <- training(data_split)
data_test <- testing(data_split)

#2 create bootstrap resamples
set.seed(123)
data_boot <- bootstraps(data_train)
data_boot

#3 model specifications
glm_spec <- logistic_reg() %>%
  set_engine("glm")
rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")
C50_spec <- decision_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0")
NB_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR")
NN_spec <- mlp() %>%
  set_mode("classification") %>%
  set_engine("nnet")
#models: decision_tree engine: C5.0, naive_Bayes engine: klaR, mlp engine nnet or keras)

#4 workflow
data_wf <- workflow() %>%
  add_formula(bugbinary ~ .)

#5 add models
glm_rs <- data_wf %>%
   add_model(glm_spec) %>%
   fit_resamples(resamples = data_boot, control = control_resamples(save_pred = TRUE))

rf_rs <- data_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(resamples = data_boot, control = control_resamples(save_pred = TRUE))

C50_rs <- data_wf %>%
  add_model(C50_spec) %>%
  fit_resamples(resamples = data_boot, control = control_resamples(save_pred = TRUE))

NB_rs <- data_wf %>%
  add_model(NB_spec) %>%
  fit_resamples(resamples = data_boot, control = control_resamples(save_pred = TRUE))

NN_rs <- data_wf %>%
  add_model(NN_spec) %>%
  fit_resamples(resamples = data_boot, control = control_resamples(save_pred = TRUE))

#6 Evaluation
collect_metrics(rf_rs)
collect_metrics(glm_rs)
collect_metrics(C50_rs)
collect_metrics(NB_rs)
collect_metrics(NN_rs)

#6 final models with last_fit()
#glm
glm_final <- data_wf %>%
  add_model(glm_spec) %>%
  last_fit(data_split)
collect_metrics(glm_final)
collect_predictions(glm_final) %>%
  conf_mat(bugbinary, .pred_class)

#rf
rf_final <- data_wf %>%
            add_model(rf_spec) %>%
            last_fit(data_split)
collect_metrics(rf_final)
collect_predictions(rf_final) %>%
  conf_mat(bugbinary, .pred_class)

#C50
C50_final <- data_wf %>%
  add_model(C50_spec) %>%
  last_fit(data_split)
collect_metrics(C50_final)
collect_predictions(C50_final) %>%
  conf_mat(bugbinary, .pred_class)

#NB
NB_final <- data_wf %>%
  add_model(NB_spec) %>%
  last_fit(data_split)
collect_metrics(NB_final)
collect_predictions(NB_final) %>%
  conf_mat(bugbinary, .pred_class)

#NN
NN_final <- data_wf %>%
  add_model(NN_spec) %>%
  last_fit(data_split)
collect_metrics(NN_final)
collect_predictions(NN_final) %>%
  conf_mat(bugbinary, .pred_class)