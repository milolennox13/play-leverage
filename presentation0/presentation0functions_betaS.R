library(nflfastR)
library(nflreadr)
library(dplyr)
library(forcats)
library(Matrix)
library(xgboost)

# Percentile functions 0.0, trained on negative log-likelihood with respect to S

beta_nll_obj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  mu     <- getinfo(dtrain, "base_margin")  # WP prior to play
  
  eps <- 1e-6
  labels <- pmin(pmax(labels, eps), 1 - eps)
  S <- pmax(exp(preds), eps)
  a <- pmax(mu * S, eps)
  b <- pmax((1 - mu) * S, eps)
  
  g1 <- digamma(S) - mu * digamma(a) - (1 - mu) * digamma(b) +
    mu * log(labels) + (1 - mu) * log(1 - labels)
  h1 <- trigamma(S) - mu^2 * trigamma(a) - (1 - mu)^2 * trigamma(b)
  
  grad <- g1 * S
  hess <- (h1 * S^2) + (g1 * S)
  
  return(list(grad = -grad, hess = -hess))
}

beta_nll <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  mu     <- getinfo(dtrain, "base_margin")  # WP prior to play
  
  eps <- 1e-6
  labels <- pmin(pmax(labels, eps), 1 - eps)
  S <- pmax(exp(preds), eps)
  a <- pmax(mu * S, eps)
  b <- pmax((1 - mu) * S, eps)
  
  value <- mean((a - 1) * log(labels) + (b - 1) * log(1 - labels) - lbeta(a, b), na.rm = TRUE)
  
  return(list(metric = "beta_nll", value = -value))
}

full_xwpa_model <- function(data, factors, numerics) {
  
  # ---------------------------
  # 1. Identify valid rows
  # ---------------------------
  valid_rows <- which(!is.na(data$wp) & !is.na(data$wpa))
  
  valid_data = data[valid_rows, ]
  
  # ---------------------------
  # 2. Preprocessing
  # ---------------------------
  data_model <- valid_data %>%
    mutate(,
      wp_post = wp + wpa
    ) %>%
    mutate(across(all_of(factors), as.factor))
  
  # Create _isNA indicators and replace NAs in numerics
  data_model_safe <- data_model %>%
    mutate(
      across(all_of(numerics), ~ as.integer(is.na(.)), .names = "{.col}_isNA"),
      across(all_of(numerics), ~ ifelse(is.na(.), 0, .)),
      across(all_of(factors), ~ fct_na_value_to_level(.))
    )
  
  numerics_with_flags <- c(numerics, paste0(numerics, "_isNA"))
  
  # ---------------------------
  # 3. Define predictors
  # ---------------------------
  predictors <- c(factors, numerics_with_flags)
  predictors <- predictors[sapply(predictors, function(v) {
    x <- data_model_safe[[v]]
    
    if (is.numeric(x)) {
      # keep if variance > 0 (ignoring NAs)
      return(var(x, na.rm = TRUE) > 0)
    } else {
      # for factors: keep if more than 1 unique non-NA level
      return(length(unique(x[!is.na(x)])) > 1)
    }
  })]
  
  # ---------------------------
  # 4. Model: betasize (S)
  # ---------------------------
  data_train <- data_model_safe %>%
    filter(wp_post > 0 & wp_post < 1 & abs(wpa) > 1e-4)
  
  # Build model matrices
  f <- reformulate(predictors, intercept = FALSE)
  X_full <- sparse.model.matrix(f, data = data_model_safe)
  
  train_idx <- which(data_model_safe$wp_post > 0 & data_model_safe$wp_post < 1 & abs(data_model_safe$wpa) > 1e-4)
  
  X_train <- sparse.model.matrix(f, data = data_model_safe[train_idx, ])
  y_post <- data_model_safe$wp_post[train_idx]
  wp_prior <- data_model_safe$wp[train_idx]
  
  dtrain <- xgb.DMatrix(
    data = X_train,
    label = y_post,        # WP_post
    base_margin = wp_prior # WP_prior
  )
  
  xgb <- xgboost(
    data = dtrain,
    objective = beta_nll_obj,
    nrounds = 300,
    eta = 0.15,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    verbose = 1,
    feval = beta_nll
  )
  
  # Predict
  data_model$fitted_betasize <- exp(predict(xgb, X_full))
  
  return(data_model)
}
