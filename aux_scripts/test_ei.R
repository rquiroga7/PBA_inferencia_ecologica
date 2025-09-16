library(data.table)
library(dplyr)
library(eiPack)

# Install tidyr if needed
if (!require(tidyr, quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}

# Test script for ecological inference with subset of data
cat("Testing ecological inference script with sample data...\n")

# Read data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

# Test with small subset
parties_2023 <- sort(unique(data_2023$nombreAgrupacion))
parties_2025 <- sort(unique(data_2025$nombreAgrupacion))

# Use actual political parties (exclude VOTANTES, NO_VOTANTES)
political_parties_2023 <- setdiff(parties_2023, c("VOTANTES", "NO_VOTANTES"))
political_parties_2025 <- setdiff(parties_2025, c("VOTANTES", "NO_VOTANTES"))

cat("Political parties 2023:", paste(political_parties_2023, collapse = ", "), "\n")
cat("Political parties 2025:", paste(political_parties_2025, collapse = ", "), "\n")

# Filter data
data_2023_main <- data_2023[nombreAgrupacion %in% political_parties_2023]
data_2025_main <- data_2025[nombreAgrupacion %in% political_parties_2025]

# Create circuit identifiers
data_2023_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]
data_2025_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]

# Get common circuits and take only first 50 for testing
common_circuits <- intersect(data_2023_main$circuit_key, data_2025_main$circuit_key)
test_circuits <- head(common_circuits, 50)

cat("Testing with", length(test_circuits), "circuits\n")

# Filter to test circuits
data_2023_test <- data_2023_main[circuit_key %in% test_circuits]
data_2025_test <- data_2025_main[circuit_key %in% test_circuits]

# Reshape to wide format
votes_2023_wide <- data_2023_test %>%
  filter(nombreAgrupacion %in% political_parties_2023) %>%
  select(circuit_key, nombreAgrupacion, votos, cantidadElectores) %>%
  pivot_wider(
    names_from = nombreAgrupacion, 
    values_from = votos, 
    values_fill = 0,
    names_prefix = "v2023_"
  )

votes_2025_wide <- data_2025_test %>%
  filter(nombreAgrupacion %in% political_parties_2025) %>%
  select(circuit_key, nombreAgrupacion, votos) %>%
  pivot_wider(
    names_from = nombreAgrupacion, 
    values_from = votos, 
    values_fill = 0,
    names_prefix = "v2025_"
  )

# Get total electors
electores_info <- data_2023_test %>% 
  filter(nombreAgrupacion == political_parties_2023[1]) %>% 
  select(circuit_key, cantidadElectores)

# Merge data
test_data <- votes_2023_wide %>%
  inner_join(votes_2025_wide, by = "circuit_key") %>%
  inner_join(electores_info, by = "circuit_key") %>%
  as.data.table()

cat("Test dataset created with", nrow(test_data), "circuits\n")

# Calculate proportions dynamically
vote_cols_2023 <- paste0("v2023_", political_parties_2023)
vote_cols_2025 <- paste0("v2025_", political_parties_2025)

test_data[, total_votes_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2023]
test_data[, total_votes_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2025]

# Create proportion columns dynamically
for(party in political_parties_2023) {
  vote_col <- paste0("v2023_", party)
  prop_col <- paste0("p2023_", gsub(" ", "_", party))
  test_data[, (prop_col) := get(vote_col) / total_votes_2023]
}

for(party in political_parties_2025) {
  vote_col <- paste0("v2025_", party)
  prop_col <- paste0("p2025_", gsub(" ", "_", party))
  test_data[, (prop_col) := get(vote_col) / total_votes_2025]
}

# Remove invalid cases
test_data <- test_data[total_votes_2023 > 0 & total_votes_2025 > 0]
test_data <- test_data[complete.cases(test_data)]

cat("Clean test dataset:", nrow(test_data), "circuits\n")

if(nrow(test_data) < 5) {
  cat("Not enough data for testing. Need at least 5 complete circuits.\n")
  quit()
}

# Prepare matrices dynamically
prop_cols_2023 <- paste0("p2023_", gsub(" ", "_", political_parties_2023))
prop_cols_2025 <- paste0("p2025_", gsub(" ", "_", political_parties_2025))

X <- as.matrix(test_data[, ..prop_cols_2023])
Y <- as.matrix(test_data[, ..prop_cols_2025])
N <- test_data$total_votes_2023

# Clean column names for formula (no spaces, special chars)
clean_names_2023 <- gsub(" ", "_", political_parties_2023)
clean_names_2025 <- gsub(" ", "_", political_parties_2025)

colnames(X) <- paste0(clean_names_2023, "_2023")
colnames(Y) <- paste0(clean_names_2025, "_2025")

cat("Running test ecological inference model...\\n")
cat("X matrix (2023):", colnames(X), "\\n")
cat("Y matrix (2025):", colnames(Y), "\\n")

# Create formula dynamically with clean names
y_vars <- paste(colnames(Y), collapse = ", ")
x_vars <- paste(colnames(X), collapse = ", ")
formula_str <- paste0("cbind(", y_vars, ") ~ cbind(", x_vars, ")")
cat("Formula:", formula_str, "\\n")

cat("Running test ecological inference model...\n")
cat("This is a quick test with minimal iterations.\n")

# Quick test with few iterations
set.seed(12345)
ei_test <- ei.MD.bayes(
  formula = as.formula(formula_str),
  data = data.frame(X, Y),
  total = N,
  burnin = 100,       # Minimal for testing
  sample = 500,       # Minimal for testing  
  thin = 1,
  verbose = 1
)

cat("\nTest completed successfully!\n")

# Check the structure of results
cat("EI results structure:\n")
str(ei_test)

# Try to extract transfer matrix
if("betabs" %in% names(ei_test)) {
  transfer_matrix <- ei_test$betabs
  cat("Transfer matrix preview:\n")
  print(round(transfer_matrix, 3))
} else {
  cat("Available result components:", names(ei_test), "\n")
  # Try other common names
  if("beta" %in% names(ei_test)) {
    cat("Using 'beta' component:\n")
    print(head(ei_test$beta))
  }
}

cat("\nTest successful. The full script should work correctly.\n")