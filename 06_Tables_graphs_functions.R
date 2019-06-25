
# Main functions:
# lm_trans_const                 - logistic-transforms data (for a given 'const'), runs linear regression, and back-transforms predictions
# lm_trans                       - runs 'lm_trans_const' with a 'const' that has been optimised
# pred_logistic                  - calls 'lm_trans' but possibly with an 'a' value > 0 in case of zero-values
# pred_logistic_from_stationname - calls 'pred_logistic' (given data, station,variable)  

# In addition:
# polygon.lines                  - Add shaded polygon made from x and two y vars (top and bottom)
# round_pvalue                   - Rounds p-values
# lm_trans_sumsquare             - Used by 'lm_trans' 


# Add shaded polygon made from x and two y vars (top and bottom)
polygon.lines <- function(x, y.lo, y.hi, col="grey", border=NULL, ...) { 
  polygon( c(x,rev(x)), c(y.lo, rev(y.hi)), col=col, border=border, ...)
}


#
# Make function, fixed const 
#
lm_trans_const <- function(x, y, x_pred = NULL, const = 1.05){
  if (is.null(x_pred))
    x_pred <- seq(min(x), max(x), length = 100)
  # Transform for linearizing
  C <- max(y)*const
  y_trans <- log((y/C)/(1-(y/C)))
  # Linear model on transformed data
  df_data <- tibble(x = x, y_trans = y_trans) 
  mod <- lm(y_trans ~ x, data = df_data)
  df_pred <- tibble(x = x_pred)
  fit <- predict(mod, new = df_pred, se.fit = TRUE)
  df_pred$y_trans <- fit$fit
  df_pred$y_trans_lo <- fit$fit - 2*fit$se.fit
  df_pred$y_trans_hi <- fit$fit + 2*fit$se.fit
  df_pred$y <- with(df_pred, exp(y_trans)/(exp(y_trans)+1)*C)
  df_pred$y_lo <- with(df_pred, exp(y_trans_lo)/(exp(y_trans_lo)+1)*C)
  df_pred$y_hi <- with(df_pred, exp(y_trans_hi)/(exp(y_trans_hi)+1)*C)
  list(fit = df_pred, model = mod, data = df_data)
}

#
# Test function, 1
#
# lm_trans_const(df$x, df$y2)

#
# Optimizing const as well
#

#
# Define lm_trans_sumsquare, function to use for optimisation
#
lm_trans_sumsquare <- function(const, x, y){
  # Transform for linearizing
  C <- max(y)*(1 + exp(const))    # (1 + exp(const))  is always > 1 
  y_trans <- log((y/C)/(1-(y/C)))
  # Linear model on transformed data
  df_data <- tibble(x = x, y_trans = y_trans) 
  mod <- lm(y_trans ~ x, data = df_data)
  y_trans_pred <- predict(mod)
  y_pred <- exp(y_trans_pred)/(exp(y_trans_pred)+1)*C
  sum((y - y_pred)^2)
}

# Test
# lm_trans_sumsquare(-3, df$x, df$y2) 
# lm_trans_sumsquare(-4, df$x, df$y2)
# optim(-4, lm_trans_sumsquare, x = df$x, y = df$y2, method = "BFGS")

#
# Define lm_trans, which runs 'lm_trans_const' with a 'const' that has been optimised
#
lm_trans <- function(x, y, x_pred = NULL){
  optimised <- optim(-4, lm_trans_sumsquare, x = x, y = y, method = "L-BFGS-B", lower = -20, upper = 20)
  const <- 1 + exp(optimised[[1]])
  lm_trans_const(x = x, y = y, x_pred = x_pred, const = const)
}


#
# pred_logistic - calls lm_trans but possobly with an 'a' value > 0 in case of zero-values
#
pred_logistic <- function(x, y, x_range = NULL, a = 0){
  if (is.null(x_range))
    x_range <- range(x, na.rm = TRUE)
  df_pred <- data.frame(x = seq(x_range[1], x_range[2], length = 100))
  pred <- lm_trans(x, y + a, x_pred = df_pred$x)
  df_pred$Pred <- pred$fit$y - a
  df_pred$Pred_lo <- pred$fit$y_lo - a
  df_pred$Pred_hi <- pred$fit$y_hi - a
  list(fit = df_pred, model = pred$model)
}

# Function for rounding p-values
round_pvalue <- function(pvalue){
  case_when(pvalue < 0.0001 ~ 0,
            pvalue < 0.001 ~ round(pvalue, 4),
            pvalue < 0.05 ~ round(pvalue, 3),
            pvalue >= 0.05 ~ round(pvalue, 2)
  )
}

#
# Function for getting predicted values for a given station 'st' 
#   and a given variable 'variable'
# Calls 'pred_logistic' 
#
pred_logistic_from_stationname <- function(st, variable, data = dat_intersex_litt_summ){
  df <- data %>% as.data.frame()
  df <- df[df$Station %in% st & !is.na(df[[variable]]),]
  # pred = list of dataframe ('fit') and model ('model')
  pred <- pred_logistic(df$Year, df[[variable]], x_range = c(2005, 2018), a = 0.05)
  # Add Station to the data frame
  pred$fit <- pred$fit %>% mutate(Station = st)
  # Also make a dataframe with P-value text
  pvalue <- summary(pred$model)$coef["x","Pr(>|t|)"] %>% round_pvalue()
  pvalue <- data.frame(Station = st, 
                       Text = ifelse(pvalue == 0, "P < 0.0001", paste("P = ", pvalue)), 
                       stringsAsFactors = FALSE)
  list(fit = pred$fit, pvalue = pvalue)
}

pred_logistic_from_data <- function(data, variable){
  df <- data %>% as.data.frame()
  # pred = list of dataframe ('fit') and model ('model')
  pred <- pred_logistic(df$Year, df[[variable]], x_range = c(2005, 2018), a = 0.05)
  # Add Station to the data frame
  pred$fit <- pred$fit %>% mutate(Station = st)
  # Also make a dataframe with P-value text
  pvalue <- summary(pred$model)$coef["x","Pr(>|t|)"] %>% round_pvalue()
  pvalue <- data.frame(Text = ifelse(pvalue == 0, "P < 0.0001", paste("P = ", pvalue)), 
                       stringsAsFactors = FALSE)
  list(fit = pred$fit, pvalue = pvalue)
}


#
# As 'pred_logistic_from_stationname' above, but makes linear regression (not logistic)
#
pred_linear_from_stationname <- function(st, variable, data = dat_intersex_litt_summ){
  df <- data %>% as.data.frame()
  df <- df[df$Station %in% st & !is.na(df[[variable]]),]
  mod <- lm(df[[variable]] ~ df$Year)
  # Create data frame with fits
  df_pred <- data.frame(x = df$Year)
  fit <- predict.lm(mod, newdata = df_pred, se = TRUE)
  df_pred$Pred <- fit$fit
  df_pred$Pred_lo <- fit$fit - 2*fit$se.fit
  df_pred$Pred_hi <- fit$fit + 2*fit$se.fit
  df_pred$Station <- st
  # Also make a dataframe with P-value text
  pvalue <- summary(mod)$coef["df$Year","Pr(>|t|)"] %>% round_pvalue()
  pvalue <- data.frame(Station = st, 
                       Text = ifelse(pvalue == 0, "P < 0.0001", paste("P = ", pvalue)), 
                       stringsAsFactors = FALSE)
  
  list(fit = df_pred, pvalue = pvalue)
}
# Test
# debugonce(pred_linear_from_stationname)
# pred_linear_from_stationname("6", "ISI_mean")

#
# As 'pred_logistic_from_stationname' above, but makes linear regression (not logistic)
#
pred_linear_from_data <- function(data, variable){
  df <- data %>% as.data.frame()
  mod <- lm((df[[variable]] + a) ~ df$Year)
  # Create data frame with fits
  df_pred <- data.frame(x = df$Year)
  fit <- predict.lm(mod, newdata = df_pred, se = TRUE)
  df_pred$Pred <- fit$fit
  df_pred$Pred_lo <- fit$fit - 2*fit$se.fit
  df_pred$Pred_hi <- fit$fit + 2*fit$se.fit
  df_pred$Station <- st
  # Also make a dataframe with P-value text
  pvalue <- summary(mod)$coef["df$Year","Pr(>|t|)"] %>% round_pvalue()
  pvalue <- data.frame(Station = st, 
                       Text = ifelse(pvalue == 0, "P < 0.0001", paste("P = ", pvalue)), 
                       stringsAsFactors = FALSE)
  
  list(fit = df_pred, pvalue = pvalue)
}
