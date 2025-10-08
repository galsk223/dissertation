#Regression

lasso_model_r <- linear_reg(penalty = tune(), 
                          mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
rf_model_r <- rand_forest(mtry = tune(), 
                        min_n = tune(), 
                        trees = 250) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")
# xgboost_model_r <- boost_tree(trees = 500,
#                             tree_depth = tune(),
#                             learn_rate = tune()) %>%
#   set_engine("xgboost") %>%
#   set_mode("regression")
nn_model_r <- mlp(hidden_units = tune(), penalty = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")
models_r <- list(
  lasso = lasso_model_r,
  rf = rf_model_r,
  nn = nn_model_r)

# Classification

lasso_model_c <- logistic_reg(penalty = tune(), 
                            mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
rf_model_c <- rand_forest(mtry = tune(), 
                          min_n = tune(), 
                          trees = 250) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
# xgboost_model_c <- boost_tree(trees = 500, 
#                               tree_depth = tune(), 
#                               learn_rate = tune()) %>%
#   set_engine("xgboost") %>%
#   set_mode("classification")
nn_model_c <- mlp(hidden_units = tune(), penalty = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")
models_c <- list(
  lasso = lasso_model_c,
  rf = rf_model_c,
  nn = nn_model_c)

pred_wrapper <- function(object, newdata) {
  predict(object, new_data = newdata, type = "prob")$.pred_1  # Or replace with your positive class label
}

models <- c(models_r, models_c)

cat_log <- function(log_file, pname, df_subdata, d, m) {
  msg <- sprintf(
    "Projection: %s | Vars: %s\n%s with %s model\n",
    pname,
    paste(names(df_subdata[d]), collapse = ", "),
    stringr::str_to_title(m$mode),
    stringr::str_to_upper(m$engine)
  )
  cat(msg)                                # print to console
  write(msg, file = log_file, append = TRUE)  # also save to log
}


# nn_model <- mlp(hidden_units = tune(),
#                 penalty = tune()) %>%
#   set_engine("brulee") %>%
#   set_mode("regression")
# 
# params <- parameters(nn_model) %>%
#   update(
#     hidden_units = hidden_units(range = c(1, 20)),
#     penalty = penalty(range = c(-5, -1))  # log10 scale
#   )
# 
# m_workflow <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(nn_model)
# 
# nn_grid <- grid_random(params, size = 10)
# 
# tune_results <- tune_grid(m_workflow, resamples = folds, grid = nn_grid)
# 

# pred_wrapper <- function(model, newdata) {
#   as.numeric(predict(model, new_data = newdata)$.pred)
# }