############################################################
# Data Science Project – PISA - Lucas Salamanca
############################################################

## 0. Packages ----

library(haven)
library(dplyr)
library(tibble)
library(readr)

############################################################
# 1. Read and merge student & school datasets
############################################################

# Paths to SPSS files (adjust if needed)
school_path  <- "/Users/lucassalamanca/Downloads/BSE/DataScience/Lucas_Salamanca_FinalProject/CY08MSP_SCH_QQQ.SAV"
student_path <- "/Users/lucassalamanca/Downloads/BSE/DataScience/Lucas_Salamanca_FinalProject/CY08MSP_STU_QQQ.SAV"

# Read SPSS files
school_data  <- read_sav(school_path)
student_data <- read_sav(student_path)

cat("School data dimensions (rows, cols):", dim(school_data),  "\n")
cat("Student data dimensions (rows, cols):", dim(student_data), "\n")

# Inspect key variables (for merge)
# Typically, PISA merges by CNTSCHID (international school ID)
# and often also CNTRYID, but CNTSCHID alone usually suffices.
if (!("CNTSCHID" %in% names(school_data) && "CNTSCHID" %in% names(student_data))) {
  stop("CNTSCHID not found in both datasets. Check key variable names.")
}

# Merge: student-level rows with attached school-level variables
# We keep all students and attach their school's info
merged_data <- student_data %>%
  left_join(school_data, by = "CNTSCHID", suffix = c("_stu", "_sch"))

cat("\nMerged data dimensions (rows, cols):", dim(merged_data), "\n")

############################################################
# 2. Build joint dictionary (variable name + label)
############################################################

# Extract SPSS labels from merged dataset
var_labels_merged <- sapply(
  merged_data,
  function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else as.character(lbl)
  }
)

dictionary_merged <- tibble(
  variable = names(merged_data),
  label    = ifelse(is.na(var_labels_merged), "", var_labels_merged)
)

# Quick look
cat("\nFirst rows of merged dictionary:\n")
print(head(dictionary_merged, 20))

# Save dictionary to CSV
dict_out_path <- "CY08_merged_variable_dictionary.csv"
write_csv(dictionary_merged, dict_out_path)

cat("\nMerged variable dictionary saved to:", dict_out_path, "\n")

############################################################
# 3. Keep only conceptual variables (indices, not item-level codes)
############################################################

# We define "conceptual" as:
# - variable name consists only of capital letters (no digits, no underscores)
# - length >= 5 (so we capture names like ANXMAT, MATHMOT, IMMIG, FAMCON, etc.)
# This automatically excludes item-level vars like ST341Q02JA, IC005Q1T, etc.

all_vars <- dictionary_merged$variable

conceptual_vars <- all_vars[
  grepl("^[A-Z]+$", all_vars) &      # only letters
    nchar(all_vars) >= 5            # reasonably long names
]

# variables that should be kept but do not match regex
manual_includes <- c(
  "MATHEFF",
  "CNT_stu"
)

manual_includes <- intersect(manual_includes, all_vars)

# Combine automatically-detected conceptual vars + manual additions
conceptual_vars <- unique(c(conceptual_vars, manual_includes))

# Keep only conceptual vars that exist in the merged data
conceptual_vars_available <- intersect(conceptual_vars, names(merged_data))

# Build conceptual-only dataframe
data_conceptual <- merged_data %>%
  select(all_of(conceptual_vars_available))

cat("\nNumber of conceptual variables kept:", length(conceptual_vars_available), "\n")
cat("Dimensions of conceptual-only merged df (rows, cols):", dim(data_conceptual), "\n\n")

cat("Conceptual variables included (first 40):\n")
print(head(conceptual_vars_available, 40))

# save this conceptual-only dataset
saveRDS(data_conceptual, "merged_data_conceptual.rds")
write_csv(data_conceptual, "merged_data_conceptual.csv")

cat("\nConceptual-only merged data saved as:\n",
    "- merged_data_conceptual.rds\n",
    "- merged_data_conceptual.csv\n")

############################################################
# 4. Drop observations with missing MATHEFF
############################################################

n_before <- nrow(data_conceptual)
data_conceptual <- data_conceptual %>% filter(!is.na(MATHEFF))
n_after <- nrow(data_conceptual)

cat("\nDropped", n_before - n_after, "observations due to missing MATHEFF.\n")
cat("Remaining observations:", n_after, "\n")

############################################################
# 5. Plot histogram of MATHEFF
############################################################

hist(
  data_conceptual$MATHEFF,
  breaks = 30,
  col = "skyblue",
  border = "white",
  main = "Distribution of MATHEFF",
  xlab = "MATHEFF"
)
box()

# save to file:
png("hist_MATHEFF.png", width = 800, height = 600)
hist(
  data_conceptual$MATHEFF,
  breaks = 30,
  col = "skyblue",
  border = "white",
  main = "Distribution of MATHEFF",
  xlab = "MATHEFF"
)
box()
dev.off()


############################################################
# 6. Drop variables with > 40% missing values
############################################################

missing_prop <- sapply(data_conceptual, function(x) mean(is.na(x)))

# Variables that exceed the threshold
vars_too_missing <- names(missing_prop[missing_prop > 0.40])

cat("\nVariables dropped due to >40% missingness:", length(vars_too_missing), "\n")
print(vars_too_missing)

# Keep the rest
data_conceptual <- data_conceptual %>%
  select(all_of(setdiff(names(data_conceptual), vars_too_missing)))

cat("\nDimensions after dropping high-missing variables (rows, cols): ",
    dim(data_conceptual), "\n")

############################################################
# 7. Check missingness for all remaining variables
############################################################

# Number and proportion of missing values for each variable
missing_summary <- tibble(
  variable = names(data_conceptual),
  n_missing = sapply(data_conceptual, function(x) sum(is.na(x))),
  prop_missing = sapply(data_conceptual, function(x) mean(is.na(x)))
)

# Add labels from dictionary_merged
missing_summary <- missing_summary %>%
  left_join(dictionary_merged, by = "variable") %>%
  rename(label = label)

# Sort by proportion of missing values (descending)
missing_summary <- missing_summary %>%
  arrange(desc(prop_missing))

cat("\nMissingness summary (first 20 variables with highest missingness):\n")
print(head(missing_summary, 20))

# Save for documentation
write_csv(missing_summary, "missingness_summary.csv")
cat("\nFull missingness summary saved to missingness_summary.csv\n")


############################################################
# 8. Correlation of all variables with MATHEFF
############################################################

# Only numeric variables can be correlated as such
numeric_vars <- names(data_conceptual)[sapply(data_conceptual, is.numeric)]
numeric_predictors <- setdiff(numeric_vars, "MATHEFF")

cors <- sapply(
  numeric_predictors,
  function(v) cor(
    data_conceptual[[v]],
    data_conceptual$MATHEFF,
    use = "pairwise.complete.obs"
  )
)

cor_summary <- tibble(
  variable = numeric_predictors,
  correlation_with_MATHEFF = as.numeric(cors)
) %>%
  left_join(dictionary_merged, by = "variable") %>%
  arrange(desc(abs(correlation_with_MATHEFF)))

cat("\nTop 20 variables most correlated with MATHEFF:\n")
print(head(cor_summary, 20))

write_csv(cor_summary, "correlations_with_MATHEFF.csv")
cat("\nFull correlation summary saved to correlations_with_MATHEFF.csv\n")


############################################################
# 8b. Heatmap of MATHEFF and Top Predictors
############################################################

library(ggplot2)
library(tidyr)

# How many top variables to show
top_n <- 20

# Select top predictors by absolute correlation
top_vars <- cor_summary %>%
  mutate(abs_cor = abs(correlation_with_MATHEFF)) %>%
  arrange(desc(abs_cor)) %>%
  slice(1:top_n) %>%
  pull(variable)

# Variables to include in the heatmap
heat_vars <- c("MATHEFF", top_vars)

# Compute correlation matrix
cor_mat <- cor(
  data_conceptual[, heat_vars],
  use = "pairwise.complete.obs"
)

# Convert to long format for ggplot2
cor_df <- as.data.frame(cor_mat)
cor_df$Var1 <- rownames(cor_df)

cor_long <- cor_df %>%
  pivot_longer(
    cols = -Var1,
    names_to = "Var2",
    values_to = "correlation"
  )

# Order variables for nicer visual
cor_long$Var1 <- factor(cor_long$Var1, levels = heat_vars)
cor_long$Var2 <- factor(cor_long$Var2, levels = rev(heat_vars))

# Create heatmap
p_heat <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "lightblue",
    mid = "white",
    high = "red",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Correlation Heatmap: MATHEFF and Top Predictors",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_equal()

# Display in RStudio
print(p_heat)

# Save high-resolution PNG for report
ggsave(
  filename = "heatmap_MATHEFF_top_predictors.png",
  plot = p_heat,
  width = 8,
  height = 8,
  dpi = 300
)

cat("\nHeatmap saved as heatmap_MATHEFF_top_predictors.png\n")

############################################################
# 8c. Single-column heatmap (Top 20) WITH correlation labels
############################################################

library(ggplot2)

# Select top 20 predictors by absolute correlation
top20 <- cor_summary %>%
  mutate(abs_cor = abs(correlation_with_MATHEFF)) %>%
  arrange(desc(abs_cor)) %>%
  slice(1:20)

# Reorder factors by absolute correlation
top20$variable <- factor(
  top20$variable,
  levels = top20$variable[order(top20$abs_cor, decreasing = FALSE)]
)

# Round the correlation for labels
top20$label_corr <- sprintf("%.2f", top20$correlation_with_MATHEFF)

# Create heatmap with labels
p_single20 <- ggplot(top20,
                     aes(x = "MATHEFF",
                         y = variable,
                         fill = correlation_with_MATHEFF)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label_corr), size = 3.5) +   # <<–– ADD LABELS
  scale_fill_gradient2(
    low = "lightblue",
    mid = "white",
    high = "red",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Correlation with MATHEFF – Top 20 Predictors",
    x = "",
    y = "Predictor"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_fixed(ratio = 0.08)

# Show plot
print(p_single20)

# Save high-resolution PNG
ggsave(
  filename = "heatmap_single_column_top20_MATHEFF_with_labels.png",
  plot = p_single20,
  width = 4,
  height = 8,
  dpi = 300
)

# Create prettier heatmap
p_pretty <- ggplot(top20,
                   aes(x = "Correlation",
                       y = variable,
                       fill = correlation_with_MATHEFF)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = label_corr), 
            color = "black", 
            size = 4.2, 
            fontface = "bold") +
  
  # Better color scale: blue-white-red
  scale_fill_gradient2(
    low = "#6BAED6",      # nicer muted blue
    mid = "white",
    high = "red",     # nicer muted red
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  
  labs(
    title = "Top 20 Predictors Most Correlated with MATHEFF",
    x = "",
    y = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(r = 5)),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)
  ) +
  
  coord_fixed(ratio = 0.12)

# Show plot
print(p_pretty)

# Save high-resolution PNG
ggsave(
  filename = "pretty_top20_correlation_heatmap.png",
  plot = p_pretty,
  width = 7,
  height = 9,
  dpi = 300
)
cat("\nSingle-column heatmap (top 20) WITH labels saved as heatmap_single_column_top20_MATHEFF_with_labels.png\n")


############################################################
# 9. Select variables for modelling (|r| >= 0.05 + key controls)
############################################################

threshold <- 0.05

# Variables with at least some linear relationship with MATHEFF
selected_vars <- cor_summary %>%
  filter(abs(correlation_with_MATHEFF) >= threshold) %>%
  pull(variable)

cat("\nNumber of predictors with |r| >=", threshold, ":", length(selected_vars), "\n")

# Key controls to keep regardless of correlation
key_controls <- c("IMMIG", "HISCED", "BISCED", "ESCS", "LANGN", "GRADE", "REPEAT", "CNT_stu")
key_controls <- intersect(key_controls, names(data_conceptual))

# IDs (kept but not to be used as predictors later)
id_vars <- c("CNTSTUID", "CNTSCHID", "CNTRYID_stu", "BOOKID",
             "STRATUM", "SUBNATIO_stu")
id_vars <- intersect(id_vars, names(data_conceptual))

# Final set of variables to use in modelling / imputation
vars_for_model <- unique(c("MATHEFF", selected_vars, key_controls, id_vars))

data_model <- data_conceptual %>%
  dplyr::select(all_of(vars_for_model))

cat("\nDimensions of data_model (rows, cols):", dim(data_model), "\n")
cat("Variables in data_model:", length(vars_for_model), "\n")

mean(complete.cases(data_model))

############################################################
# 9b. Reduce sample to students with fewer missing values
############################################################

target_n <- 50000  # desired sample size (change if you want)

# Number of missing values per row
row_na_count <- rowSums(is.na(data_model))

# Order row indices by increasing number of missings
ordered_idx <- order(row_na_count)

# How many rows can we actually take?
n_available <- nrow(data_model)
n_keep <- min(target_n, n_available)

# Indices of rows to keep: those with least missingness
keep_idx <- ordered_idx[seq_len(n_keep)]

data_model_small <- data_model[keep_idx, ]

cat("\nOriginal N:", n_available,
    "\nKept N (least missing rows):", n_keep, "\n")

cat("\nMean number of missing values per row (original):",
    mean(row_na_count), "\n")
cat("Mean number of missing values per row (kept subset):",
    mean(row_na_count[keep_idx]), "\n")

# Optional: check complete cases on the reduced sample
cat("\nProportion of complete cases in reduced sample:",
    mean(complete.cases(data_model_small)), "\n")


############################################################
# Export dictionary with variable name, label, and values 
# to check which vars are continuous and categorical
############################################################

library(dplyr)
library(tibble)
library(readr)

# Maximum number of unique values to show
# If more than 20, we will collapse them (continuous variable)
max_vals <- 20

# Function to get a summary of values
summarize_values <- function(x) {
  vals <- unique(x)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return("no values")
  
  # Collapse long numeric variables
  if (length(vals) > max_vals) {
    return(paste0("unique values > ", max_vals, " (likely continuous)"))
  }
  
  # Otherwise list them
  paste(vals, collapse = ", ")
}

# Build table
#dictionary_values <- tibble(
#  variable = names(data_model),
#  label    = dictionary_merged$label[match(names(data_model), dictionary_merged$variable)],
#  values   = sapply(data_model, summarize_values)
#)

# Build table smaller sample
dictionary_values <- tibble(
  variable = names(data_model_small),
  label    = dictionary_merged$label[match(names(data_model_small), dictionary_merged$variable)],
  values   = sapply(data_model_small, summarize_values)
)

# Save for inspection
write_csv(dictionary_values, "dictionary_values.csv")

cat("\nDictionary with variable names, labels, and value sets saved as dictionary_values.csv\n")

print(dictionary_values, n = Inf)

############################################################
# 10. Simple imputation based on dictionary classification
############################################################

library(haven)

# Start from the modelling dataset
#data_imputed <- data_model
data_imputed <- data_model_small

# 0. Convert haven_labelled to proper R types (numeric / factor)
for (v in names(data_imputed)) {
  x <- data_imputed[[v]]
  
  if (inherits(x, "haven_labelled") && is.numeric(x)) {
    data_imputed[[v]] <- as.numeric(x)
  } else if (inherits(x, "haven_labelled")) {
    data_imputed[[v]] <- as_factor(x, levels = "labels")
  }
}

# 1. IDs (we do not impute these)
id_vars <- c("CNTSTUID", "CNTSCHID", "BOOKID")
id_vars <- intersect(id_vars, names(data_imputed))

# 2. Continuous variables (median imputation)
continuous_vars <- c(
  "MATHEFF","FAMCON","MATHPERS","ANXMAT","SDLEFF","CREATOP","CREATEFF",
  "PERSEVAGR","HOMEPOS","CREATSCH","CREATFAM","CURIOAGR","COGACRCO",
  "HISEI","ICTQUAL","COGACMCO","ASSERAGR","EMOCOAGR","IMAGINE",
  "RELATST","FEELSAFE","BELONG","FAMSUP","SCHSUST","DISCLIM",
  "EMPATAGR","EXPOFA","PROBSCRI","PROBSELF","TEACHSUP","OPENART",
  "STUBEHA","EDUSHORT","TOTMATH","ALLACTIV","BULLIED","ACTCRESC",
  "SCHRISK","SCHAUTO","TOTAT","INFOSEEK","GROSAGR","TOTSTAFF",
  "MACTIV","EXERPRAC","SRESPCUR","WORKPAY","PAREDINT","SRESPRES",
  "STUDYHMW", "TCHPART"
)

continuous_vars <- intersect(continuous_vars, names(data_imputed))

# 3. Categorical variables (mode imputation)
categorical_vars <- c(
  "MATHPREF","MATHEASE","SKIPPING","TARDYSD","REPEAT","MISSSC",
  "EXPECEDU","FISCED","HISCED","MISCED",
  "SCHLTYPE","IMMIG","CREACTIV","GRADE","LANGN", "CNT_stu"
)

categorical_vars <- intersect(categorical_vars, names(data_imputed))

# Convert categoricals to factor
if (length(categorical_vars) > 0) {
  data_imputed[, categorical_vars] <- lapply(
    data_imputed[, categorical_vars, drop = FALSE],
    factor
  )
}

# 4. Impute continuous variables with median
for (v in continuous_vars) {
  med <- median(data_imputed[[v]], na.rm = TRUE)
  data_imputed[[v]][is.na(data_imputed[[v]])] <- med
}

# 5. Impute categorical variables with mode (most frequent category)
for (v in categorical_vars) {
  tab <- table(data_imputed[[v]], useNA = "no")
  if (length(tab) == 0) next
  mode_level <- names(which.max(tab))
  data_imputed[[v]][is.na(data_imputed[[v]])] <- mode_level
}

# 6. Check remaining missing values
col_na <- colSums(is.na(data_imputed))
cat("\nNumber of variables with any remaining missing values:",
    sum(col_na > 0), "\n")

if (sum(col_na > 0) > 0) {
  cat("Variables still with missing values:\n")
  print(col_na[col_na > 0])
} else {
  cat("No missing values remain in data_imputed.\n")
}

# 7. Save imputed data
saveRDS(data_imputed, "data_imputed_simple.rds")
write_csv(data_imputed, "data_imputed_simple.csv")

cat("\nImputed dataset saved as:\n",
    "- data_imputed_simple.rds\n",
    "- data_imputed_simple.csv\n")

############################################################
# 11. Prepare modelling dataset
############################################################

# IDs to exclude
id_vars <- c("CNTSTUID", "CNTSCHID", "BOOKID")
id_vars <- intersect(id_vars, names(data_imputed))

# Target variable
target <- "MATHEFF"

# All predictors = everything except target + IDs
predictors <- setdiff(names(data_imputed), c(target, id_vars))

# Subset modelling data
model_data <- data_imputed[, c(target, predictors)]

############################################################
# 12. Identify which variables to standardize (reuse imputation classification)
############################################################

# Remove MATHEFF and any IDs and categoricals — we do NOT standardize these
vars_to_standardize <- setdiff(continuous_vars, c("MATHEFF"))


# Only standardize predictors that actually exist in model_data
vars_to_standardize <- intersect(vars_to_standardize, predictors)

cat("\nNumber of continuous variables to standardize:", 
    length(vars_to_standardize), "\n")

############################################################
# 13. Standardize continuous predictors
############################################################

model_data_scaled <- model_data
model_data_scaled[, vars_to_standardize] <- scale(model_data_scaled[, vars_to_standardize])


############################################################
# 13b. Train / validation / test split (60 / 20 / 20)
############################################################

set.seed(123)

n <- nrow(model_data_scaled)

# Random permutation of row indices
idx <- sample(seq_len(n))

n_train <- floor(0.6 * n)
n_val   <- floor(0.2 * n)
n_test  <- n - n_train - n_val

train_idx <- idx[1:n_train]
val_idx   <- idx[(n_train + 1):(n_train + n_val)]
test_idx  <- idx[(n_train + n_val + 1):n]

train_data <- model_data_scaled[train_idx, ]
val_data   <- model_data_scaled[val_idx, ]
test_data  <- model_data_scaled[test_idx, ]

cat("\nTrain size:", nrow(train_data),
    "\nValidation size:", nrow(val_data),
    "\nTest size:", nrow(test_data), "\n")

############################################################
# 14. Build model matrices for regularised models
############################################################

# Drop high-cardinality variable from regularised models
high_cardinality <- c("LANGN")
predictors_reg <- setdiff(predictors, high_cardinality)

# Use these predictors + MATHEFF from the scaled data
reg_data <- model_data_scaled[, c("MATHEFF", predictors_reg)]

library(Matrix)

# Sparse matrix for full sample
X_full <- sparse.model.matrix(MATHEFF ~ ., data = reg_data)[, -1]
y_full <- reg_data$MATHEFF

# Split into train / val / test
X_train <- X_full[train_idx, ]
X_val   <- X_full[val_idx, ]
X_test  <- X_full[test_idx, ]

y_train <- y_full[train_idx]
y_val   <- y_full[val_idx]
y_test  <- y_full[test_idx]

cat("\nDimensions of X_train:", dim(X_train),
    "\nDimensions of X_val:",   dim(X_val),
    "\nDimensions of X_test:",  dim(X_test), "\n")

############################################################
# RMSE and R2
############################################################

library(Metrics)

compute_r2 <- function(y, y_hat) {
  ss_res <- sum((y - y_hat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

############################################################
# 15. OLS (train on top 20 predictors, validate & test)
############################################################

top20_vars <- cor_summary %>%
  mutate(abs_cor = abs(correlation_with_MATHEFF)) %>%
  arrange(desc(abs_cor)) %>%
  dplyr::slice(1:20) %>%
  pull(variable)

ols_train <- train_data[, c("MATHEFF", top20_vars)]
ols_val   <- val_data[,   c("MATHEFF", top20_vars)]
ols_test  <- test_data[,  c("MATHEFF", top20_vars)]

ols_fit <- lm(MATHEFF ~ ., data = ols_train)
summary(ols_fit)

# Predictions
y_hat_ols_val  <- predict(ols_fit, newdata = ols_val)
y_hat_ols_test <- predict(ols_fit, newdata = ols_test)

############################################################
# 20. Hyperparameter grid for penalised regression
############################################################

library(glmnet)

# Log-spaced lambda grid
lambda_grid <- 10^seq(-4, 2, length.out = 100)

############################################################
# 20a. LASSO tuned on validation set
############################################################

lasso_models <- lapply(lambda_grid, function(lam) {
  glmnet(X_train, y_train, alpha = 1, lambda = lam)
})

lasso_val_rmse <- sapply(lasso_models, function(m) {
  pred <- as.numeric(predict(m, X_val))
  rmse(y_val, pred)
})

lasso_best_lambda <- lambda_grid[which.min(lasso_val_rmse)]
cat("\nBest LASSO lambda (validation):", lasso_best_lambda, "\n")

lasso_fit <- glmnet(X_train, y_train, alpha = 1, lambda = lasso_best_lambda)
y_hat_lasso_val  <- as.numeric(predict(lasso_fit, X_val))
y_hat_lasso_test <- as.numeric(predict(lasso_fit, X_test))

############################################################
# 20b. Ridge tuned on validation set
############################################################

ridge_models <- lapply(lambda_grid, function(lam) {
  glmnet(X_train, y_train, alpha = 0, lambda = lam)
})

ridge_val_rmse <- sapply(ridge_models, function(m) {
  pred <- as.numeric(predict(m, X_val))
  rmse(y_val, pred)
})

ridge_best_lambda <- lambda_grid[which.min(ridge_val_rmse)]
cat("\nBest Ridge lambda (validation):", ridge_best_lambda, "\n")

ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = ridge_best_lambda)
y_hat_ridge_val  <- as.numeric(predict(ridge_fit, X_val))
y_hat_ridge_test <- as.numeric(predict(ridge_fit, X_test))

############################################################
# 20c. Elastic Net (alpha = 0.5) tuned on validation set
############################################################

enet_models <- lapply(lambda_grid, function(lam) {
  glmnet(X_train, y_train, alpha = 0.5, lambda = lam)
})

enet_val_rmse <- sapply(enet_models, function(m) {
  pred <- as.numeric(predict(m, X_val))
  rmse(y_val, pred)
})

enet_best_lambda <- lambda_grid[which.min(enet_val_rmse)]
cat("\nBest Elastic Net lambda (validation):", enet_best_lambda, "\n")

enet_fit <- glmnet(X_train, y_train, alpha = 0.5, lambda = enet_best_lambda)
y_hat_enet_val  <- as.numeric(predict(enet_fit, X_val))
y_hat_enet_test <- as.numeric(predict(enet_fit, X_test))

############################################################
# 21. Extract coefficients (focus on LASSO)
############################################################

coef_lasso <- as.matrix(coef(lasso_fit))
coef_ridge <- as.matrix(coef(ridge_fit))
coef_enet  <- as.matrix(coef(enet_fit))

lasso_nonzero <- coef_lasso[coef_lasso != 0, , drop = FALSE]

cat("\nNumber of non-zero LASSO coefficients (including intercept):",
    nrow(lasso_nonzero), "\n")

lasso_table <- tibble(
  variable    = rownames(lasso_nonzero),
  coefficient = as.numeric(lasso_nonzero)
) %>%
  arrange(desc(abs(coefficient)))

print(lasso_table)
write_csv(lasso_table, "lasso_nonzero_coefficients.csv")
cat("\nLASSO non-zero coefficients saved to lasso_nonzero_coefficients.csv\n")

############################################################
# 22. RMSE & R2 comparison: validation and test (linear models)
############################################################

# Validation
rmse_ols_val   <- rmse(y_val, y_hat_ols_val)
rmse_ridge_val <- rmse(y_val, y_hat_ridge_val)
rmse_lasso_val <- rmse(y_val, y_hat_lasso_val)
rmse_enet_val  <- rmse(y_val, y_hat_enet_val)

r2_ols_val   <- compute_r2(y_val, y_hat_ols_val)
r2_ridge_val <- compute_r2(y_val, y_hat_ridge_val)
r2_lasso_val <- compute_r2(y_val, y_hat_lasso_val)
r2_enet_val  <- compute_r2(y_val, y_hat_enet_val)

rmse_val_table <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net"),
  RMSE  = c(rmse_ols_val, rmse_ridge_val, rmse_lasso_val, rmse_enet_val)
)

r2_val_table <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net"),
  R2    = c(r2_ols_val, r2_ridge_val, r2_lasso_val, r2_enet_val)
)

cat("\nValidation RMSE (linear models):\n")
print(rmse_val_table)
cat("\nValidation R2 (linear models):\n")
print(r2_val_table)

write_csv(rmse_val_table, "model_rmse_linear_val.csv")
write_csv(r2_val_table,  "model_r2_linear_val.csv")

# Test
rmse_ols_test   <- rmse(y_test, y_hat_ols_test)
rmse_ridge_test <- rmse(y_test, y_hat_ridge_test)
rmse_lasso_test <- rmse(y_test, y_hat_lasso_test)
rmse_enet_test  <- rmse(y_test, y_hat_enet_test)

r2_ols_test   <- compute_r2(y_test, y_hat_ols_test)
r2_ridge_test <- compute_r2(y_test, y_hat_ridge_test)
r2_lasso_test <- compute_r2(y_test, y_hat_lasso_test)
r2_enet_test  <- compute_r2(y_test, y_hat_enet_test)

rmse_test_table <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net"),
  RMSE  = c(rmse_ols_test, rmse_ridge_test, rmse_lasso_test, rmse_enet_test)
)

r2_test_table <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net"),
  R2    = c(r2_ols_test, r2_ridge_test, r2_lasso_test, r2_enet_test)
)

cat("\nTest RMSE (linear models):\n")
print(rmse_test_table)
cat("\nTest R2 (linear models):\n")
print(r2_test_table)

write_csv(rmse_test_table, "model_rmse_linear_test.csv")
write_csv(r2_test_table,  "model_r2_linear_test.csv")

############################################################
# 23. Random Forest tuned on validation set
############################################################

library(randomForest)

rf_train <- train_data[, c("MATHEFF", predictors_reg)]
rf_val   <- val_data[,   c("MATHEFF", predictors_reg)]
rf_test  <- test_data[,  c("MATHEFF", predictors_reg)]

# Subsample RF training data to reduce memory load
set.seed(123)
n_rf_train <- min(5000, nrow(rf_train))   # adjust if needed
rf_idx <- sample(seq_len(nrow(rf_train)), size = n_rf_train)
rf_train_small <- rf_train[rf_idx, ]

cat("\nRF training subset size:", nrow(rf_train_small), "\n")

p_rf <- length(predictors_reg)
mtry_grid  <- unique(pmax(1, c(floor(sqrt(p_rf)),
                               floor(p_rf / 4),
                               floor(p_rf / 2))))
ntree_grid <- c(100, 200)

best_rf_rmse <- Inf
best_mtry    <- NA
best_ntree   <- NA
best_rf_fit  <- NULL

for (mtry_val in mtry_grid) {
  for (ntree_val in ntree_grid) {
    cat("Fitting RF with mtry =", mtry_val, "ntree =", ntree_val, "\n")
    rf_tmp <- randomForest(
      MATHEFF ~ .,
      data = rf_train_small,
      ntree = ntree_val,
      mtry  = mtry_val,
      importance = TRUE
    )
    pred_val <- predict(rf_tmp, newdata = rf_val)
    rmse_val_rf <- rmse(rf_val$MATHEFF, pred_val)
    
    if (rmse_val_rf < best_rf_rmse) {
      best_rf_rmse <- rmse_val_rf
      best_mtry    <- mtry_val
      best_ntree   <- ntree_val
      best_rf_fit  <- rf_tmp
    }
  }
}

cat("\nBest RF hyperparameters (validation): mtry =", best_mtry,
    ", ntree =", best_ntree,
    ", Val RMSE =", best_rf_rmse, "\n")

rf_fit <- best_rf_fit

# Predictions
y_hat_rf_val  <- predict(rf_fit, newdata = rf_val)
y_hat_rf_test <- predict(rf_fit, newdata = rf_test)

rmse_rf_val  <- rmse(rf_val$MATHEFF,  y_hat_rf_val)
rmse_rf_test <- rmse(rf_test$MATHEFF, y_hat_rf_test)

r2_rf_val  <- compute_r2(rf_val$MATHEFF,  y_hat_rf_val)
r2_rf_test <- compute_r2(rf_test$MATHEFF, y_hat_rf_test)

cat("\nRandom Forest - Validation RMSE:", rmse_rf_val,
    " R2:", r2_rf_val, "\n")
cat("Random Forest - Test RMSE:", rmse_rf_test,
    " R2:", r2_rf_test, "\n")

rf_imp <- importance(rf_fit)
rf_imp_df <- as.data.frame(rf_imp)
rf_imp_df$variable <- rownames(rf_imp_df)

if ("%IncMSE" %in% colnames(rf_imp_df)) {
  rf_imp_df <- rf_imp_df %>%
    dplyr::arrange(desc(`%IncMSE`))
} else {
  main_col <- colnames(rf_imp_df)[1]
  rf_imp_df <- rf_imp_df %>%
    dplyr::arrange(dplyr::desc(.data[[main_col]]))
}

head(rf_imp_df, 20)
readr::write_csv(rf_imp_df, "random_forest_variable_importance.csv")
cat("\nRandom Forest variable importance saved to random_forest_variable_importance.csv\n")

############################################################
# 24. XGBoost tuned on validation set
############################################################

library(xgboost)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval   <- xgb.DMatrix(data = X_val,   label = y_val)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Small grid for max_depth and eta
param_grid <- expand.grid(
  max_depth = c(4, 6),
  eta       = c(0.05, 0.10)
)

best_xgb_rmse <- Inf
best_xgb_fit  <- NULL
best_params   <- NULL

set.seed(123)

for (i in seq_len(nrow(param_grid))) {
  md  <- param_grid$max_depth[i]
  lr  <- param_grid$eta[i]
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = md,
    eta = lr,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  cat("Training XGBoost with max_depth =", md, "eta =", lr, "\n")
  
  xgb_tmp <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  val_rmse <- xgb_tmp$best_score   # rmse on validation set
  
  if (val_rmse < best_xgb_rmse) {
    best_xgb_rmse <- val_rmse
    best_xgb_fit  <- xgb_tmp
    best_params   <- params
  }
}

cat("\nBest XGBoost params (validation):\n")
print(best_params)
cat("Best validation RMSE:", best_xgb_rmse, "\n")

xgb_fit <- best_xgb_fit

y_hat_xgb_val  <- predict(xgb_fit, dval)
y_hat_xgb_test <- predict(xgb_fit, dtest)

rmse_xgb_val  <- rmse(y_val,  y_hat_xgb_val)
rmse_xgb_test <- rmse(y_test, y_hat_xgb_test)

r2_xgb_val  <- compute_r2(y_val,  y_hat_xgb_val)
r2_xgb_test <- compute_r2(y_test, y_hat_xgb_test)

cat("\nXGBoost - Validation RMSE:", rmse_xgb_val,
    " R2:", r2_xgb_val, "\n")
cat("XGBoost - Test RMSE:", rmse_xgb_test,
    " R2:", r2_xgb_test, "\n")

xgb_imp <- xgb.importance(model = xgb_fit)
print(head(xgb_imp, 20))
xgb.importance(model = xgb_fit) %>%
  write_csv("xgboost_variable_importance.csv")
cat("\nXGBoost variable importance saved to xgboost_variable_importance.csv\n")

############################################################
# 25. Extended model comparison (validation & test)
############################################################

rmse_val_extended <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net",
            "Random Forest", "XGBoost"),
  RMSE  = c(rmse_ols_val, rmse_ridge_val, rmse_lasso_val, rmse_enet_val,
            rmse_rf_val, rmse_xgb_val)
)

rmse_test_extended <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net",
            "Random Forest", "XGBoost"),
  RMSE  = c(rmse_ols_test, rmse_ridge_test, rmse_lasso_test, rmse_enet_test,
            rmse_rf_test, rmse_xgb_test)
)

r2_val_extended <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net",
            "Random Forest", "XGBoost"),
  R2    = c(r2_ols_val, r2_ridge_val, r2_lasso_val, r2_enet_val,
            r2_rf_val, r2_xgb_val)
)

r2_test_extended <- tibble(
  Model = c("OLS (top 20)", "Ridge", "LASSO", "Elastic Net",
            "Random Forest", "XGBoost"),
  R2    = c(r2_ols_test, r2_ridge_test, r2_lasso_test, r2_enet_test,
            r2_rf_test, r2_xgb_test)
)

cat("\nValidation RMSE (extended):\n")
print(rmse_val_extended)
cat("\nTest RMSE (extended):\n")
print(rmse_test_extended)

cat("\nValidation R2 (extended):\n")
print(r2_val_extended)
cat("\nTest R2 (extended):\n")
print(r2_test_extended)

write_csv(rmse_val_extended,  "model_rmse_comparison_val_extended.csv")
write_csv(rmse_test_extended, "model_rmse_comparison_test_extended.csv")
write_csv(r2_val_extended,    "model_r2_comparison_val_extended.csv")
write_csv(r2_test_extended,   "model_r2_comparison_test_extended.csv")

cat("\nExtended RMSE and R2 tables saved.\n")





######################################
# GRAPHS FOR DOCUMENT
######################################

library(ggplot2)

vars_cont <- c("ANXMAT", "MATHPERS", "HOMEPOS")  # ajusta a tus top vars

install.packages("hexbin")
library(hexbin)

set.seed(123)
sample_df <- train_data %>% sample_frac(0.06)  # 3% sample

vars_cont <- c("ANXMAT", "MATHPERS", "CREATSCH", "FAMCON", "CURIOAGR", "COGACRCO", "HISEI", "PERSEVAGR")  # ajusta a tus top vars

for (v in vars_cont) {
  p <- ggplot(sample_df, aes_string(x = v, y = "MATHEFF")) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    theme_minimal() +
    labs(
      title = paste("Relationship between", v, "and MATHEFF"),
      x = v,
      y = "MATHEFF"
    )
  print(p)
}

ggplot(sample_df, aes(ANXMAT, MATHEFF)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal()

## MAP ##


# Packages (install first if needed)
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "viridis"))

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# 1. Median MATHEFF by country -------------------------------------------

country_med <- data_model %>%          
  group_by(CNT_stu) %>%                      # PISA country identifier
  summarise(median_MATHEFF = median(MATHEFF, na.rm = TRUE)) %>%
  ungroup()

# Make sure it's character, not factor
country_med$CNT_stu <- as.character(country_med$CNT_stu)

head(country_med)

# 2. World map shapefile (sf object) -------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# rnaturalearth uses ISO3 codes in column 'iso_a3'
# Join PISA country medians to world map by ISO3 code
world_pisa <- world %>%
  left_join(country_med, by = c("iso_a3" = "CNT_stu"))

# 3. Choropleth map ------------------------------------------------------

ggplot(world_pisa) +
  geom_sf(aes(fill = median_MATHEFF), color = "grey60", size = 0.1) +
  scale_fill_viridis(
    name = "Median MATHEFF",
    option = "plasma",
    na.value = "grey90"
  ) +
  labs(
    title = "Median Mathematics Self-Efficacy (MATHEFF) by Country",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


library(ggplot2)
library(dplyr)

# Combine predictions into one data frame
pred_df <- tibble(
  actual    = rep(y_test, 3),
  predicted = c(y_hat_xgb_test, y_hat_lasso_test, y_hat_ridge_test),
  model     = factor(rep(c("XGBoost", "LASSO", "Ridge"),
                         each = length(y_test)))
)

# Faceted Predicted vs Actual plot
p_pred_vs_actual <- ggplot(pred_df, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  facet_wrap(~ model, ncol = 3) +
  labs(
    title = "Predicted vs Actual MATHEFF (Test Set)",
    x = "Actual MATHEFF",
    y = "Predicted MATHEFF"
  ) +
  theme_minimal()

print(p_pred_vs_actual)

# rf_imp_df already has one row per variable; choose correct column
# If %IncMSE exists, use that; otherwise use first column
rf_imp_plot_df <- rf_imp_df

if ("%IncMSE" %in% colnames(rf_imp_plot_df)) {
  rf_imp_plot_df <- rf_imp_plot_df %>%
    arrange(desc(`%IncMSE`)) %>%
    dplyr::slice(1:20) %>%
    mutate(variable = reorder(variable, `%IncMSE`))
  
  p_rf_imp <- ggplot(rf_imp_plot_df,
                     aes(x = variable, y = `%IncMSE`)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Random Forest Variable Importance (Top 20)",
      x = "",
      y = "% Increase in MSE"
    ) +
    theme_minimal()
  
} else {
  main_col <- colnames(rf_imp_plot_df)[1]
  rf_imp_plot_df <- rf_imp_plot_df %>%
    arrange(desc(.data[[main_col]])) %>%
    dplyr::slice(1:20) %>%
    mutate(variable = reorder(variable, .data[[main_col]]))
  
  p_rf_imp <- ggplot(rf_imp_plot_df,
                     aes(x = variable, y = .data[[main_col]])) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Random Forest Variable Importance (Top 20)",
      x = "",
      y = main_col
    ) +
    theme_minimal()
}

print(p_rf_imp)


# xgb_imp is the output of xgb.importance(model = xgb_fit)
# Use Gain as importance measure
xgb_imp_top <- xgb_imp %>%
  arrange(desc(Gain)) %>%
  dplyr::slice(1:20) %>%
  mutate(Feature = reorder(Feature, Gain))

p_xgb_imp <- ggplot(xgb_imp_top,
                    aes(x = Feature, y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "XGBoost Variable Importance (Top 20)",
    x = "",
    y = "Gain"
  ) +
  theme_minimal()

print(p_xgb_imp)


# lasso_table has columns: variable, coefficient
lasso_plot_df <- lasso_table %>%
  filter(variable != "(Intercept)") %>%      # drop intercept
  arrange(desc(abs(coefficient))) %>%
  dplyr::slice(1:20) %>%
  mutate(variable = reorder(variable, coefficient))

p_lasso_coef <- ggplot(lasso_plot_df,
                       aes(x = variable, y = coefficient,
                           fill = coefficient > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    guide = "none") +
  labs(
    title = "LASSO Coefficients (Top 20 by |value|)",
    x = "",
    y = "Coefficient"
  ) +
  theme_minimal()

print(p_lasso_coef)

resid_df <- tibble(
  actual    = rep(y_test, 3),
  predicted = c(y_hat_xgb_test, y_hat_lasso_test, y_hat_ridge_test),
  model     = factor(rep(c("XGBoost", "LASSO", "Ridge"),
                         each = length(y_test)))
) %>%
  mutate(residual = actual - predicted)

# Faceted residual plot: residual vs predicted
p_resid <- ggplot(resid_df, aes(x = predicted, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~ model, ncol = 3) +
  labs(
    title = "Residuals vs Predicted MATHEFF (Test Set)",
    x = "Predicted MATHEFF",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()

print(p_resid)

