i <- 1
d <- 1
# df_subdata <- df_subdata_cn
out_class <- function(df_subdata, models_c, pname, log_file){
  
  recover <- map(1:(length(df_subdata)), function(d){

    data <- df_subdata[[d]]
  
  # data <- df_subdata$NoNetwork
    
    # Train/test split
    set.seed(123)
    split <- initial_split(data, strata = Outcome) 
    train_data <- training(split)
    test_data <- testing(split)
    folds <- vfold_cv(train_data, v = 5, strata = Outcome)
    
    feature_names <- colnames(test_data %>% select(-Outcome))
    
    m <- models_c[[1]]
    out <- map(models_c, function(m){
      tryCatch({
      # cat(pname, "Projection | Vars:", names(df_subdata[d]), "\n",
      #     str_to_title(m$mode),"with",str_to_upper(m$engine), "model \n")
      cat_log(log_file, pname, df_subdata, d, m)
      
      rec <- recipe(Outcome ~ ., data = train_data) %>%
        step_normalize(all_numeric_predictors()) %>% 
        step_smote() 
      
      m_workflow <- workflow() %>%
        add_recipe(rec) %>%
        add_model(m)
      
      if(m$engine == "glmnet"){
        tune_results <- tune_grid(m_workflow, resamples = folds, 
                                  grid = grid_regular(
                                    penalty(),  # log10 scale → 1e-5 to 1
                                    levels = 30
                                  ))
      } else {
        tune_results <- tune_grid(m_workflow, resamples = folds, grid = 30)
      } 
      
      best_penalty <- select_best(tune_results, metric = "roc_auc")
      final_model <- finalize_workflow(m_workflow, best_penalty)
      final_fit <- fit(final_model, data = train_data)
      
      predictions_class <- predict(final_fit, new_data = test_data, type = "class")
      ss_m <- sum(test_data$Outcome == predictions_class$.pred_class)/nrow(test_data)
      
      vi <- vi(final_fit, 
               method = "shap", 
               feature_names = feature_names, 
               train = test_data %>% 
                 select(-Outcome), 
               target = test_data$Outcome,
               pred_wrapper = pred_wrapper) %>%
        mutate(Importance = abs(Importance),
               Variable = fct_reorder(Variable, Importance))
      
      mdout <- tibble(Projection = pname,
                      DataSubset = names(df_subdata[d]),
                      Type = str_to_title(m$mode),
                      Engine = str_to_upper(m$engine),
                      ModelPerformance = ss_m)
      
      return(list(sum = mdout,
                  vi = vi))
      
    }, error = function(e) {
      msg <- sprintf("Error in model %s | Projection: %s | Time: %s\n%s\n",
                     str_to_upper(m$engine), pname, Sys.time(), e$message)
      write(msg, file = log_file, append = TRUE)
      message(msg)
      return(NULL)  # skip failed model safely
    })
    
    })
    
  })
  
}
