# =============================================================================
# SECULARIZATION AND DEVELOPMENT: A GLOBAL ANALYSIS (1945–2010)
# World Religion Project (WRP 2020) + World Bank WDI
#
# =============================================================================
# 
# REQUIRED PACKAGES:
#   install.packages(c("tidyverse", "readr", "corrplot", "ggplot2", "scales",
#                      "randomForest", "pdp", "factoextra", "cluster",
#                      "caret", "knitr", "glmnet"))
#
# DATA FILES NEEDED (from WRP_2020 zip):
#   - WRP_2020/WRP_national.csv
#   - WRP_2020/COW-country-codes.csv
#   - WRP_2020/wdi-csv-zip-57-mb/WDIData.csv
#
# =============================================================================


# ── 0. LOAD PACKAGES ─────────────────────────────────────────────────────────

library(tidyverse)   # dplyr, tidyr, ggplot2, readr, purrr, stringr
library(corrplot)    # correlation heatmap
library(scales)      # axis formatting
library(randomForest)# Random Forest regression
library(pdp)         # Partial Dependence Plots
library(factoextra)  # cluster visualisation (fviz_*)
library(cluster)     # silhouette
library(caret)       # train/test split helpers (optional)
library(glmnet)      # Ridge / Lasso regularization


# ── 1. DATA LOADING ──────────────────────────────────────────────────────────

wrp_path <- "/Users/srishtikansra/Documents/uni of milan/ML_Stats/WRP_2020/WRP_national.csv"
cow_path <- "/Users/srishtikansra/Documents/uni of milan/ML_Stats/WRP_2020/COW-country-codes.csv"
wdi_path <- "/Users/srishtikansra/Documents/uni of milan/ML_Stats/WRP_2020/wdi-csv-zip-57-mb/WDIData.csv"

cat("Loading WRP national dataset...\n")
wrp_raw <- read_csv(wrp_path, show_col_types = FALSE)

cat("Loading COW country codes...\n")
cow_raw <- read_csv(cow_path, show_col_types = FALSE)

cat("Loading World Bank WDI data (large file, this may take a moment)...\n")
wdi_raw <- read_csv(wdi_path, show_col_types = FALSE)


# ── 2. CLEAN AND RESHAPE WRP DATA ────────────────────────────────────────────

# Keep percentage columns and a few key identifiers
genpct_cols <- names(wrp_raw)[str_ends(names(wrp_raw), "genpct")]

keep_cols <- c("year", "name", "pop", "nonreligpct", genpct_cols)
wrp_clean <- wrp_raw %>% select(all_of(keep_cols))

# Rename columns to readable English labels
wrp_clean <- wrp_clean %>%
  rename(
    country_code      = name,
    population        = pop,
    christian_pct     = chrstgenpct,
    muslim_pct        = islmgenpct,
    hindu_pct         = hindgenpct,
    buddhist_pct      = budgenpct,
    jewish_pct        = judgenpct,
    zoroastrian_pct   = zorogenpct,
    sikh_pct          = sikhgenpct,
    shinto_pct        = shntgenpct,
    bahai_pct         = bahgenpct,
    taoist_pct        = taogenpct,
    jain_pct          = jaingenpct,
    confucian_pct     = confgenpct,
    syncretic_pct     = syncgenpct,
    animist_pct       = anmgenpct,
    other_religions_pct = othrgenpct,
    unaffiliated_pct  = nonreligpct
  )

# WRP stores percentages as 0–1 proportions; convert to 0–100
pct_cols <- names(wrp_clean)[str_ends(names(wrp_clean), "_pct")]
wrp_clean <- wrp_clean %>%
  mutate(across(all_of(pct_cols), ~ . * 100))


# ── 3. MERGE WITH COUNTRY NAMES (COW) ────────────────────────────────────────

cow_clean <- cow_raw %>%
  select(StateAbb, StateNme) %>%
  rename(country_code = StateAbb,
         country_name = StateNme) %>%
  mutate(across(everything(), str_trim))

wrp_final <- wrp_clean %>%
  left_join(cow_clean, by = "country_code")

# Report any unmatched codes
missing_codes <- wrp_final %>%
  filter(is.na(country_name)) %>%
  pull(country_code) %>%
  unique()
if (length(missing_codes) > 0) {
  cat("Country codes with no name match:", paste(missing_codes, collapse = ", "), "\n")
}

# Reorder columns neatly
wrp_final <- wrp_final %>%
  select(country_code, country_name, year, population, all_of(pct_cols)) %>%
  arrange(country_code, year)

cat("WRP final shape:", nrow(wrp_final), "rows x", ncol(wrp_final), "cols\n")


# ── 4. PROCESS WORLD BANK INDICATORS ─────────────────────────────────────────

# Indicators of interest (World Bank codes -> readable names)
indicator_map <- c(
  "SP.POP.TOTL"    = "wb_population_total",
  "SP.DYN.TFRT.IN" = "fertility_rate",
  "SP.URB.TOTL.IN.ZS" = "urbanization_rate",
  "SP.DYN.LE00.IN" = "life_expectancy",
  "NY.GDP.PCAP.CD" = "gdp_per_capita"
)

# Identify year columns (numeric column names ≥ 1960)
year_cols_wdi <- names(wdi_raw)[
  suppressWarnings(!is.na(as.integer(names(wdi_raw)))) &
    suppressWarnings(as.integer(names(wdi_raw))) >= 1960
]

# Filter, pivot, and reshape
wb_long <- wdi_raw %>%
  filter(`Indicator Code` %in% names(indicator_map)) %>%
  select(`Country Name`, `Country Code`, `Indicator Code`, all_of(year_cols_wdi)) %>%
  pivot_longer(
    cols      = all_of(year_cols_wdi),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))

wb_wide <- wb_long %>%
  pivot_wider(
    id_cols     = c(`Country Name`, `Country Code`, year),
    names_from  = `Indicator Code`,
    values_from = value
  ) %>%
  rename_with(~ indicator_map[.], .cols = names(indicator_map)) %>%
  rename(country_name = `Country Name`,
         country_code = `Country Code`)

cat("World Bank wide shape:", nrow(wb_wide), "rows x", ncol(wb_wide), "cols\n")


# ── 5. MERGE WRP + WORLD BANK ────────────────────────────────────────────────

df <- wrp_final %>%
  inner_join(wb_wide %>% select(-country_name),
             by = c("country_code", "year"))

cat("Merged dataset shape:", nrow(df), "rows x", ncol(df), "cols\n")
glimpse(df)


# ── 6. CREATE SECULARIZATION VARIABLE ────────────────────────────────────────

# Secularization is proxied by % of population unaffiliated with any religion
df <- df %>%
  mutate(secularization = unaffiliated_pct)

cat("\nPreview of key variables:\n")
df %>%
  select(country_name, year, secularization) %>%
  head(10) %>%
  print()

pdf(
  "Secularization_Project_Figures.pdf",
  width = 12,
  height = 8
)

# ── 7. EXPLORATORY DATA ANALYSIS ─────────────────────────────────────────────

## 7a. Correlation matrix ─────────────────────────────────────────────────────

vars_for_corr <- c("secularization", "fertility_rate",
                   "urbanization_rate", "life_expectancy", "gdp_per_capita")

corr_df  <- df %>% select(all_of(vars_for_corr)) %>% drop_na()
corr_mat <- cor(corr_df, use = "complete.obs")

cat("\nCorrelation matrix (rounded):\n")
print(round(corr_mat, 2))

# Visualise as a heatmap
corrplot(
  corr_mat,
  method  = "color",
  type    = "upper",
  addCoef.col = "black",
  number.cex  = 0.8,
  tl.cex      = 0.9,
  col     = colorRampPalette(c("#d73027", "white", "#1a9850"))(200),
  title   = "Correlation Matrix – Secularization & Development Indicators",
  mar     = c(0, 0, 2, 0)
)


## 7b. Global secularization trend ────────────────────────────────────────────

global_trend <- df %>%
  group_by(year) %>%
  summarise(avg_secularization = mean(secularization, na.rm = TRUE), .groups = "drop")

ggplot(global_trend, aes(x = year, y = avg_secularization)) +
  geom_line(colour = "#2166ac", linewidth = 1) +
  geom_point(colour = "#2166ac", size = 2) +
  labs(
    title    = "Global Secularization Trend (1945–2010)",
    subtitle = "Average % population unaffiliated with any religion",
    x        = "Year",
    y        = "Average % Unaffiliated"
  ) +
  theme_minimal(base_size = 13)


## 7c. Scatter plots: secularization vs predictors ────────────────────────────

plot_scatter <- function(data, xvar, xlabel, log_x = FALSE) {
  p <- ggplot(data, aes(x = .data[[xvar]], y = secularization)) +
    geom_point(alpha = 0.4, colour = "#4393c3") +
    geom_smooth(method = "lm", se = TRUE, colour = "#d6604d") +
    labs(
      title = paste(xlabel, "vs Secularization"),
      x     = xlabel,
      y     = "% Unaffiliated (Secularization)"
    ) +
    theme_minimal(base_size = 12)

  if (log_x) p <- p + scale_x_log10(labels = label_comma())
  print(p)
}

scatter_data <- df %>% select(all_of(vars_for_corr)) %>% drop_na()

plot_scatter(scatter_data, "fertility_rate",    "Fertility Rate")
plot_scatter(scatter_data, "gdp_per_capita",   "GDP per Capita (log scale)", log_x = TRUE)
plot_scatter(scatter_data, "urbanization_rate", "Urban Population (%)")
plot_scatter(scatter_data, "life_expectancy",   "Life Expectancy (years)")


# ── 8. SIMPLE REGRESSION: FERTILITY → SECULARIZATION ─────────────────────────

reg_df_simple <- df %>%
  select(secularization, fertility_rate) %>%
  drop_na()

model_simple <- lm(secularization ~ fertility_rate, data = reg_df_simple)
cat("\n--- Simple OLS: Secularization ~ Fertility Rate ---\n")
print(summary(model_simple))

# Predicted values + 95% CI for the regression line
x_grid <- data.frame(
  fertility_rate = seq(
    min(reg_df_simple$fertility_rate),
    max(reg_df_simple$fertility_rate),
    length.out = 100
  )
)
pred_simple <- predict(model_simple, newdata = x_grid, interval = "confidence")
pred_df_simple <- cbind(x_grid, as.data.frame(pred_simple))

ggplot() +
  geom_point(data = reg_df_simple,
             aes(x = fertility_rate, y = secularization),
             alpha = 0.35, colour = "#4393c3") +
  geom_ribbon(data = pred_df_simple,
              aes(x = fertility_rate, ymin = lwr, ymax = upr),
              fill = "#d6604d", alpha = 0.25) +
  geom_line(data = pred_df_simple,
            aes(x = fertility_rate, y = fit),
            colour = "#d6604d", linewidth = 1) +
  labs(
    title    = "Fertility Rate vs Secularization with 95% CI",
    subtitle = "Simple OLS regression",
    x        = "Fertility Rate",
    y        = "% Unaffiliated"
  ) +
  theme_minimal(base_size = 12)


# ── 9. MULTIVARIATE OLS REGRESSION ───────────────────────────────────────────

reg_df_multi <- df %>%
  select(secularization, fertility_rate, urbanization_rate,
         life_expectancy, gdp_per_capita) %>%
  drop_na()

model_multi <- lm(
  secularization ~ fertility_rate + urbanization_rate +
                   life_expectancy + gdp_per_capita,
  data = reg_df_multi
)

cat("\n--- Multivariate OLS: Secularization ~ All Predictors ---\n")
print(summary(model_multi))

# Multicollinearity diagnostics (condition number) referenced in report Section 6.2
X_design <- model.matrix(model_multi)
cat("\nCondition number of design matrix:", kappa(X_design), "\n")

# Coefficient plot (analogous to the Plotly scatter in the notebook)
coef_df <- as.data.frame(confint(model_multi))
names(coef_df) <- c("ci_low", "ci_high")
coef_df$variable <- rownames(coef_df)
coef_df$coef     <- coef(model_multi)[coef_df$variable]
coef_df <- coef_df %>% filter(variable != "(Intercept)")

ggplot(coef_df, aes(x = coef, y = variable)) +
  geom_point(size = 3, colour = "#2166ac") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, colour = "#2166ac") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  labs(
    title = "Multivariate OLS – Coefficient Estimates (95% CI)",
    x     = "Estimated Effect on Secularization",
    y     = "Predictor"
  ) +
  theme_minimal(base_size = 12)


# ── 9b. OLS RESIDUAL DIAGNOSTICS ─────────────────────────────────────────────

diag_df <- data.frame(
  fitted    = fitted(model_multi),
  residuals = resid(model_multi)
)

# Residuals vs Fitted (checks linearity/homoscedasticity)
ggplot(diag_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.4, colour = "#4393c3") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "#d6604d") +
  labs(
    title = "Residuals vs Fitted Values – Multivariate OLS",
    x     = "Fitted Secularization",
    y     = "Residual"
  ) +
  theme_minimal(base_size = 12)

# Normal Q-Q plot (checks normality of residuals)
ggplot(diag_df, aes(sample = residuals)) +
  stat_qq(colour = "#4393c3", alpha = 0.5) +
  stat_qq_line(colour = "#d6604d") +
  labs(
    title = "Normal Q-Q Plot of OLS Residuals",
    x     = "Theoretical Quantiles",
    y     = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12)

cat("\nResidual diagnostics: residuals show right-skew/heteroscedasticity if",
    "spread widens with fitted values and Q-Q tail deviates from the line.\n")


# ── 9c. OLS WITH INTERACTION TERM (fertility x urbanization) ────────────────

model_interact <- lm(
  secularization ~ fertility_rate * urbanization_rate +
                   life_expectancy + gdp_per_capita,
  data = reg_df_multi
)

cat("\n--- OLS with Fertility x Urbanization Interaction ---\n")
print(summary(model_interact))
cat(sprintf("\nAdjusted R^2 without interaction: %.4f | with interaction: %.4f\n",
            summary(model_multi)$adj.r.squared,
            summary(model_interact)$adj.r.squared))


# ── 9d. RIDGE AND LASSO REGRESSION ───────────────────────────────────────────

X_mat <- model.matrix(secularization ~ fertility_rate + urbanization_rate +
                         life_expectancy + gdp_per_capita, data = reg_df_multi)[, -1]
y_vec <- reg_df_multi$secularization

set.seed(42)
cv_ridge <- cv.glmnet(X_mat, y_vec, alpha = 0)   # alpha = 0 -> Ridge
set.seed(42)
cv_lasso <- cv.glmnet(X_mat, y_vec, alpha = 1)   # alpha = 1 -> Lasso

cat("\nRidge: best lambda =", cv_ridge$lambda.min, "\n")
cat("Lasso: best lambda =", cv_lasso$lambda.min, "\n")

ridge_coef <- as.matrix(coef(cv_ridge, s = "lambda.min"))
lasso_coef <- as.matrix(coef(cv_lasso, s = "lambda.min"))

reg_compare <- data.frame(
  variable = rownames(ridge_coef),
  OLS      = c(coef(model_multi)["(Intercept)"], coef(model_multi)[-1])[rownames(ridge_coef)],
  Ridge    = ridge_coef[, 1],
  Lasso    = lasso_coef[, 1]
)
cat("\nCoefficient comparison (OLS vs Ridge vs Lasso):\n")
print(reg_compare)

# Lasso path plot (shows coefficients shrinking to zero as lambda increases)
lasso_full <- glmnet(X_mat, y_vec, alpha = 1)
lasso_path_df <- as.data.frame(t(as.matrix(lasso_full$beta)))
lasso_path_df$log_lambda <- log(lasso_full$lambda)
lasso_path_long <- lasso_path_df %>%
  pivot_longer(-log_lambda, names_to = "variable", values_to = "coef")

ggplot(lasso_path_long, aes(x = log_lambda, y = coef, colour = variable)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Lasso Coefficient Paths",
    x     = "log(Lambda)",
    y     = "Coefficient",
    colour = "Predictor"
  ) +
  theme_minimal(base_size = 12)

# Compare predictive performance: Ridge/Lasso CV-MSE vs OLS
ols_mse <- mean(resid(model_multi)^2)
cat(sprintf("\nIn-sample MSE  |  OLS: %.3f  |  Ridge (CV-min): %.3f  |  Lasso (CV-min): %.3f\n",
            ols_mse,
            min(cv_ridge$cvm),
            min(cv_lasso$cvm)))


# ── 10. RANDOM FOREST REGRESSION ─────────────────────────────────────────────

set.seed(42)

features <- c("fertility_rate", "urbanization_rate",
              "life_expectancy", "gdp_per_capita")

rf_df <- df %>%
  select(secularization, all_of(features)) %>%
  drop_na()

# Train / test split (70 / 30)
train_idx <- sample(seq_len(nrow(rf_df)), size = floor(0.7 * nrow(rf_df)))
train_rf  <- rf_df[ train_idx, ]
test_rf   <- rf_df[-train_idx, ]

cat("\nTraining Random Forest (ntree = 300)...\n")
rf_model <- randomForest(
  secularization ~ .,
  data      = train_rf,
  ntree     = 300,
  importance = TRUE
)

# ── Evaluate ─────────────────────────────────────────────────────────────────
y_pred_rf <- predict(rf_model, newdata = test_rf)
r2_rf   <- 1 - sum((test_rf$secularization - y_pred_rf)^2) /
               sum((test_rf$secularization - mean(test_rf$secularization))^2)
rmse_rf <- sqrt(mean((test_rf$secularization - y_pred_rf)^2))

cat(sprintf("Random Forest  |  R² = %.4f  |  RMSE = %.4f\n", r2_rf, rmse_rf))


# ── 10b. K-FOLD CROSS-VALIDATION (RESAMPLING) FOR RANDOM FOREST ─────────────
# A single 70/30 split can be optimistic or pessimistic depending on which
# rows land in the test set. 5-fold CV gives a more robust performance
# estimate by averaging over 5 different train/test partitions.

set.seed(42)
k_folds   <- 5
fold_ids  <- sample(rep(1:k_folds, length.out = nrow(rf_df)))
cv_r2     <- numeric(k_folds)
cv_rmse   <- numeric(k_folds)

for (k in 1:k_folds) {
  train_k <- rf_df[fold_ids != k, ]
  test_k  <- rf_df[fold_ids == k, ]

  rf_k <- randomForest(secularization ~ ., data = train_k, ntree = 300)
  pred_k <- predict(rf_k, newdata = test_k)

  cv_r2[k]   <- 1 - sum((test_k$secularization - pred_k)^2) /
                    sum((test_k$secularization - mean(test_k$secularization))^2)
  cv_rmse[k] <- sqrt(mean((test_k$secularization - pred_k)^2))
}

cat("\n5-fold CV results for Random Forest:\n")
cat("R² per fold:   ", round(cv_r2, 3), "\n")
cat("RMSE per fold: ", round(cv_rmse, 3), "\n")
cat(sprintf("Mean R² = %.4f (SD = %.4f)  |  Mean RMSE = %.4f (SD = %.4f)\n",
            mean(cv_r2), sd(cv_r2), mean(cv_rmse), sd(cv_rmse)))

cv_results_df <- data.frame(fold = 1:k_folds, R2 = cv_r2, RMSE = cv_rmse)

ggplot(cv_results_df, aes(x = factor(fold), y = R2)) +
  geom_col(fill = "#4393c3") +
  geom_hline(yintercept = mean(cv_r2), linetype = "dashed", colour = "#d6604d") +
  labs(
    title    = "Random Forest R\u00B2 Across 5 Cross-Validation Folds",
    subtitle = sprintf("Mean R\u00B2 = %.3f (dashed line)", mean(cv_r2)),
    x        = "Fold",
    y        = "R\u00B2"
  ) +
  theme_minimal(base_size = 12)

# ── Feature importance ────────────────────────────────────────────────────────
importance_df <- importance(rf_model, type = 1) %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  rename(Importance = `%IncMSE`) %>%
  arrange(Importance)

ggplot(importance_df, aes(x = Importance, y = reorder(Feature, Importance))) +
  geom_bar(stat = "identity", fill = "#4393c3") +
  labs(
    title = "Random Forest – Feature Importance for Secularization",
    x     = "% Increase in MSE (permutation importance)",
    y     = "Feature"
  ) +
  theme_minimal(base_size = 12)

# ── Cross-method importance comparison: Lasso vs Random Forest ──────────────
# If two very different methods (linear, regularized vs non-linear, ensemble)
# agree on which predictor matters most, that is stronger evidence than either
# method alone.

lasso_imp <- data.frame(
  Feature = rownames(lasso_coef)[-1],
  Lasso_AbsCoef = abs(lasso_coef[-1, 1])
)
rf_imp <- importance_df %>% rename(Feature = Feature, RF_Importance = Importance)

imp_compare <- lasso_imp %>%
  inner_join(rf_imp, by = "Feature") %>%
  mutate(
    Lasso_norm = Lasso_AbsCoef / max(Lasso_AbsCoef),
    RF_norm    = RF_Importance / max(RF_Importance)
  ) %>%
  pivot_longer(cols = c(Lasso_norm, RF_norm), names_to = "Method", values_to = "Importance_norm")

ggplot(imp_compare, aes(x = Importance_norm, y = Feature, fill = Method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Lasso_norm" = "#d6604d", "RF_norm" = "#4393c3"),
                     labels = c("Lasso |coef| (normalised)", "Random Forest %IncMSE (normalised)")) +
  labs(
    title = "Predictor Importance: Lasso vs Random Forest",
    x     = "Normalised Importance (0-1)",
    y     = "Predictor",
    fill  = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ── Actual vs Predicted ───────────────────────────────────────────────────────
pred_actual_df <- data.frame(
  Actual    = test_rf$secularization,
  Predicted = y_pred_rf
)

ggplot(pred_actual_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.45, colour = "#4393c3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  labs(
    title    = "Random Forest: Actual vs Predicted Secularization",
    subtitle = sprintf("R² = %.3f | RMSE = %.2f", r2_rf, rmse_rf),
    x        = "Actual % Unaffiliated",
    y        = "Predicted % Unaffiliated"
  ) +
  theme_minimal(base_size = 12)

# ── Partial Dependence Plot: fertility_rate ───────────────────────────────────
pdp_fertility <- partial(rf_model, pred.var = "fertility_rate", grid.resolution = 50)

ggplot(pdp_fertility, aes(x = fertility_rate, y = yhat)) +
  geom_line(colour = "#d6604d", linewidth = 1) +
  labs(
    title    = "Partial Dependence Plot – Fertility Rate",
    subtitle = "Effect of fertility rate on secularization (averaging over other predictors)",
    x        = "Fertility Rate",
    y        = "Partial Effect on % Unaffiliated"
  ) +
  theme_minimal(base_size = 12)


# ── 11. K-MEANS CLUSTERING ───────────────────────────────────────────────────

cluster_vars <- c("secularization", "fertility_rate",
                  "urbanization_rate", "gdp_per_capita")

cluster_df_raw <- df %>%
  select(country_code, country_name, year, all_of(cluster_vars)) %>%
  drop_na()

# Standardise (z-scores) — essential for distance-based methods
X_scaled <- scale(cluster_df_raw %>% select(all_of(cluster_vars)))


## 11a. Elbow method to choose k ───────────────────────────────────────────────
set.seed(42)
inertia <- map_dbl(2:7, function(k) {
  km <- kmeans(X_scaled, centers = k, nstart = 25, iter.max = 100)
  km$tot.withinss
})

elbow_df <- data.frame(k = 2:7, inertia = inertia)

ggplot(elbow_df, aes(x = k, y = inertia)) +
  geom_line(colour = "#2166ac", linewidth = 1) +
  geom_point(size = 3, colour = "#2166ac") +
  labs(
    title = "Elbow Method for Choosing Number of Clusters",
    x     = "Number of Clusters (k)",
    y     = "Total Within-Cluster SS (Inertia)"
  ) +
  theme_minimal(base_size = 12)


## 11b. Run k-means with k = 4 ─────────────────────────────────────────────────
set.seed(42)
km_fit <- kmeans(X_scaled, centers = 4, nstart = 25, iter.max = 100)

cluster_df_raw$cluster <- km_fit$cluster

# IMPORTANT: kmeans() cluster IDs (1,2,3,4) are arbitrary and can vary between
# runs/seeds/package versions. Labels must be assigned FROM the centroid values,
# never hardcoded to a fixed ID, or labels can silently attach to the wrong cluster.
centroid_summary <- cluster_df_raw %>%
  group_by(cluster) %>%
  summarise(across(all_of(cluster_vars), mean), .groups = "drop")

label_lookup <- centroid_summary %>%
  mutate(cluster_label = case_when(
    gdp_per_capita == max(gdp_per_capita) ~ "Highly Developed / Secular",
    secularization == max(secularization) ~ "Secularizing Middle-Income",
    fertility_rate  == max(fertility_rate)  ~ "Developing Religious",
    TRUE ~ "Traditional / High Fertility"
  )) %>%
  select(cluster, cluster_label)

cluster_df_raw <- cluster_df_raw %>%
  left_join(label_lookup, by = "cluster")
cat("\nCluster summary (means):\n")
cluster_df_raw %>%
  group_by(cluster_label) %>%
  summarise(across(all_of(cluster_vars), ~ round(mean(.), 2)), .groups = "drop") %>%
  print()

cat("\nCluster sizes:\n")
print(table(cluster_df_raw$cluster_label))


## 11c. Visualise clusters ─────────────────────────────────────────────────────

cluster_colours <- c(
  "Traditional / High Fertility" = "#6699CC",
  "Secularizing Middle-Income"   = "#E58A6E",
  "Developing Religious"         = "#5FAE8C",
  "Highly Developed / Secular"   = "#B79CD4"
)
# Centroids computed as ONE row per cluster (mirrors the Python version's
# single black X per cluster) — do NOT use stat_summary here, since combining
# it with point-level data can cause the centroid glyph to be drawn multiple
# times per cluster instead of once.
centroids_fert <- cluster_df_raw %>%
  group_by(cluster_label) %>%
  summarise(
    fertility_rate = mean(fertility_rate),
    secularization  = mean(secularization),
    .groups = "drop"
  )

ggplot(cluster_df_raw,
       aes(x = fertility_rate, y = secularization, colour = cluster_label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_point(data = centroids_fert,
             aes(x = fertility_rate, y = secularization),
             inherit.aes = FALSE,
             shape = 4, size = 5, stroke = 1.5, colour = "black") +
  geom_text(data = centroids_fert,
            aes(x = fertility_rate, y = secularization, label = cluster_label),
            inherit.aes = FALSE,
            vjust = -1, size = 3.2, fontface = "bold", colour = "black") +
  scale_colour_manual(values = cluster_colours) +
  labs(
    title   = "K-means Clusters: Fertility vs Secularization (k = 4)",
    x       = "Fertility Rate",
    y       = "% Unaffiliated (Secularization)",
    colour  = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

# GDP (log) vs Secularization coloured by cluster
ggplot(cluster_df_raw,
       aes(x = gdp_per_capita, y = secularization, colour = cluster_label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  scale_x_log10(labels = label_comma()) +
  scale_colour_manual(values = cluster_colours) +
  labs(
    title  = "K-means Clusters: GDP per Capita vs Secularization (k = 4)",
    x      = "GDP per Capita (log scale, USD)",
    y      = "% Unaffiliated (Secularization)",
    colour = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))


# ── 11d. HIERARCHICAL CLUSTERING (VALIDATES K-MEANS SOLUTION) ───────────────
# If the same 4-group structure emerges from a totally different clustering
# algorithm, that is strong evidence the groups reflect real data structure
# rather than an artifact of k-means' specific assumptions.

dist_mat <- dist(X_scaled, method = "euclidean")
hc_fit   <- hclust(dist_mat, method = "ward.D2")

# Dendrogram (subsample for readability since n is in the hundreds)
set.seed(42)
sample_idx_hc <- sample(seq_len(nrow(X_scaled)), min(80, nrow(X_scaled)))
hc_sub <- hclust(dist(X_scaled[sample_idx_hc, ]), method = "ward.D2")

plot(hc_sub,
     labels = FALSE,
     main   = "Hierarchical Clustering Dendrogram (Ward's method, n = 80 sample)",
     xlab   = "Countries (sample)",
     ylab   = "Height")
rect.hclust(hc_sub, k = 4, border = "#d6604d")

# Cut full tree at k = 4 and compare against k-means labels
hc_clusters_4 <- cutree(hc_fit, k = 4)
agreement_tab <- table(KMeans = cluster_df_raw$cluster_label, Hierarchical = hc_clusters_4)
cat("\nAgreement between K-means and Hierarchical clustering (k = 4):\n")
print(agreement_tab)


# ── 12. PCA VISUALISATION (optional complement to clustering) ─────────────────

pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)  # already scaled

# Scree plot
pca_var  <- pca_res$sdev^2
pca_prop <- pca_var / sum(pca_var)

scree_df <- data.frame(
  PC  = paste0("PC", seq_along(pca_prop)),
  var = pca_prop,
  cum = cumsum(pca_prop)
)

ggplot(scree_df, aes(x = PC, y = var, group = 1)) +
  geom_bar(stat = "identity", fill = "#4393c3", alpha = 0.7) +
  geom_line(aes(y = cum), colour = "#d6604d", linewidth = 1) +
  geom_point(aes(y = cum), colour = "#d6604d", size = 2) +
  scale_y_continuous(
    labels   = percent_format(),
    sec.axis = sec_axis(~ ., name = "Cumulative Variance Explained",
                        labels = percent_format())
  ) +
  labs(
    title = "PCA Scree Plot",
    x     = "Principal Component",
    y     = "Proportion of Variance Explained"
  ) +
  theme_minimal(base_size = 12)

# Biplot (first two PCs coloured by cluster)
pca_scores <- as.data.frame(pca_res$x[, 1:2])
pca_scores$cluster_label <- cluster_df_raw$cluster_label

ggplot(pca_scores, aes(x = PC1, y = PC2, colour = cluster_label)) +
  geom_point(alpha = 0.5, size = 1.8) +
  labs(
    title  = "PCA Biplot – Countries Coloured by K-means Cluster",
    x      = paste0("PC1 (", percent(pca_prop[1], accuracy = 0.1), ")"),
    y      = paste0("PC2 (", percent(pca_prop[2], accuracy = 0.1), ")"),
    colour = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

dev.off()
# ── END ───────────────────────────────────────────────────────────────────────
cat("\nAll analyses complete.\n")
