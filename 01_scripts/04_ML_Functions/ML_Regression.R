# df_subdata <- df_subdata_rn
out_reg <- function(df_subdata, models_r, pname, log_file){
  
  d <- 1
  return <- map(1:(length(df_subdata)), function(d){

    data <- df_subdata[[d]]  %>%
        drop_na() 
  
    # data <- df_subdata$NoNetwork
    if(nrow(data)>750){
      data <- df_subdata[[d]] %>%
        drop_na() %>%
        sample_n(750)
    }
    
    # if (colnames(data)[[1]] != "Return") {
    #   message("Wrong Function")
    #   return(NULL)  # or return("some early value")
    # }
    
    # Train/test split
    set.seed(123)
    split <- initial_split(data, strata = Outcome) 
    train_data <- training(split)
    test_data <- testing(split)
    folds <- vfold_cv(train_data, v = 5, strata = Outcome)
    
    feature_names <- colnames(test_data %>% select(-Outcome))
    
    m <- models_r[[1]]
    out <- map(models_r, function(m){
        tryCatch({
      
      # cat("Projection:", pname, "| Vars:", names(df_subdata[d]), "\n",
      #     str_to_title(m$mode),"with",str_to_upper(m$engine), "model \n")
      cat_log(log_file, pname, df_subdata, d, m)
      
      rec <- recipe(Outcome ~ ., data = train_data) %>%
        step_normalize(all_numeric_predictors()) 
      # %>%
      #   step_poly(all_numeric_predictors(), degree = 2)
      
      m_workflow <- workflow() %>%
        add_recipe(rec) %>%
        add_model(m)
      
      if(m$engine == "glmnet"){
        tune_results <- tune_grid(m_workflow, resamples = folds,
                                  grid = grid_regular(
                                    penalty(),  # log10 scale → 1e-5 to 1
                                    # mixture(),   # ridge to lasso
                                    levels = 30
                                  ))
      } else {
        tune_results <- tune_grid(m_workflow, resamples = folds, grid = 5)
      }
      
      best_penalty <- select_best(tune_results, metric = "rmse")
      final_model <- finalize_workflow(m_workflow, best_penalty)
      final_fit <- fit(final_model, data = train_data)
      
      # predictions <- predict(final_fit, new_data = test_data)
      truth <- test_data$Outcome
      predictions <- tryCatch(predict(final_fit, new_data = test_data)$.pred,
                       error = function(e) rep(mean(truth), length(truth)))
      if (sd(truth) == 0 || sd(predictions) == 0) {
        corv <- NA_real_
      } else {
        corv <- cor(truth, predictions)
      }
      rmse <- yardstick::rmse_vec(truth, predictions)
      
      # corv <- cor(truth, predictions$.pred)
      # rmse <- yardstick::rmse_vec(truth, predictions$.pred)
      
      old_warn <- getOption("warn")
      options(warn = -1)
      sv <- tryCatch({
        kernelshap(final_fit,
                   X = test_data %>% 
                     select(-Outcome) %>% 
                     sample_n(min(100,nrow(test_data))),
                   bg_X = sample_n(data,50),
                   max_iter = 20)
      }, error = function(e){
        NULL
      })
      if (is.null(sv)) {
        mean_abs_shap <- 0
      } else {
        mean_abs_shap <- colMeans(abs(sv$S))
        
        shap_summary <- as.data.frame(sv$S) %>%
          pivot_longer(everything(), names_to = "variable", values_to = "shap") %>%
          group_by(variable) %>%
          summarise(
            mean = mean(shap, na.rm = TRUE),         # average (directional)
            se   = sd(shap, na.rm = TRUE) / sqrt(n()), # standard error
            ci_low  = mean - qt(0.975, df = n()-1) * se,
            ci_high = mean + qt(0.975, df = n()-1) * se,
            mean_abs = mean(abs(shap), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(mean))
        
      }
      options(warn = old_warn)
      
      mdout <- tibble(Projection = pname,
                      DataSubset = names(df_subdata[d]),
                      Type = str_to_title(m$mode),
                      Engine = str_to_upper(m$engine),
                      ModelPerformance = rmse)
      
      return(list(sum = mdout,
                  vi = mean_abs_shap,
                  shap_summary = shap_summary))
      
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




