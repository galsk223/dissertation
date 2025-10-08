d <- 1
out_portion <- function(df_subdata, models_r, pname){
  
  return <- map(1:length(df_subdata), function(d){
    
    data <- df_subdata[[d]] %>% 
      mutate(PortionVessels = PortionVessels*100) %>% 
      # select(-Modularity) %>% 
      filter(!is.na(PortionVessels))
    # if (colnames(data)[[1]] != "Return") {
    #   message("Wrong Function")
    #   return(NULL)  # or return("some early value")
    # }
    
    # Train/test split
    set.seed(123)
    split <- initial_split(data, strata = PortionVessels) 
    train_data <- training(split)
    test_data <- testing(split)
    folds <- vfold_cv(train_data, v = 5, strata = PortionVessels)
    
    feature_names <- colnames(test_data %>% select(-PortionVessels))
    
    m <- models_r[[3]]
    out <- map(models_r, function(m){
      
      cat(pname, "Projection | Vars:", names(df_subdata[d]), "\n",
          str_to_title(m$mode),"with",str_to_upper(m$engine), "model \n")
      
      rec <- recipe(PortionVessels ~ ., data = train_data) %>%
        # step_zv(all_numeric_predictors()) %>% 
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
        # } else if(m$engine %in% c("ranger", "xgboost")) {
        #   
      } else {
        tune_results <- tune_grid(m_workflow, resamples = folds, grid = 30)
      }
      
      best_penalty <- select_best(tune_results, metric = "rmse")
      final_model <- finalize_workflow(m_workflow, best_penalty)
      final_fit <- fit(final_model, data = train_data)
      
      predictions <- predict(final_fit, new_data = test_data)
      truth <- test_data$PortionVessels
      corv <- cor(truth, predictions$.pred)
      rmse <- yardstick::rmse_vec(truth, predictions$.pred)
      
      # plot(truth, predictions$.pred)
      
      # fitted_model <- extract_fit_parsnip(final_fit)$fit
      # Get SHAP values
      sv <- kernelshap(final_fit, 
                       X = test_data %>% 
                         sample_n(150) %>% 
                         select(-PortionVessels), 
                       bg_X = sample_n(data,150))
      
      mean_abs_shap <- colMeans(abs(sv$S))
      std.error <- function(x) sd(x) / sqrt(length(x))
      shapdf <- tibble(
        var = names(mean_abs_shap),
        mean_abs = colMeans(abs(sv$S)),
        mean = colMeans(sv$S),
        std_err = apply(sv$S, 2, std.error),
        CI_lower = mean - 1.96 * std_err,
        CI_upper = mean + 1.96 * std_err
      ) 
      
      mdout <- tibble(Projection = pname,
                      DataSubset = names(df_subdata[d]),
                      Type = str_to_title(m$mode),
                      Engine = str_to_upper(m$engine),
                      ModelPerformance = rmse)
      
      return(list(sum = mdout,
                  shapdf = shapdf,
                  vi = mean_abs_shap))
      
    })
    
  })
  
}
