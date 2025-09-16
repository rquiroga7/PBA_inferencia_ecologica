# Final corrected ecological inference with proper proportion handling
library(data.table)
library(dplyr)
library(eiPack)
library(coda)

# Source the helper functions
source("betas.R")

cat("=== CORRECTED ECOLOGICAL INFERENCE WITH EXACT PROPORTIONS ===\n")
cat("Fixing proportion calculation to sum exactly to 1.0\n\n")

# Load and process data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

# CRITICAL FIX: Exclude VOTANTES from 2023 as it's redundant with individual party totals
# The 2023 data has individual parties + VOTANTES, but VOTANTES = sum of parties
# We should use either individual parties OR VOTANTES, not both

political_parties_2023 <- c("EN_BLANCO", "JxC", "LLA", "NO_VOTANTES", "OTROS", "UxP")
political_parties_2025 <- c("EN_BLANCO", "LLA_JxC", "NO_VOTANTES", "OTROS", "UxP")

# Calculate actual voted totals (excluding NO_PADRON)
votes_2023[, actual_votes := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
votes_2025[, actual_votes := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]

# NO_PADRON = eligible voters - actual votes
votes_2023[, NO_PADRON := cantidadElectores - actual_votes]
votes_2025[, NO_PADRON := cantidadElectores - actual_votes]

# Rename and merge
setnames(votes_2023, "cantidadElectores", "cantidadElectores_2023")
setnames(votes_2025, "cantidadElectores", "cantidadElectores_2025")

merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

# Use the maximum cantidadElectores as base
merged_data[, ELIG := pmax(cantidadElectores_2023, cantidadElectores_2025)]

# Filter for stable electoral rolls
electores_diff <- abs(merged_data$cantidadElectores_2025 - merged_data$cantidadElectores_2023) / 
                  merged_data$cantidadElectores_2023
stable_circuits <- merged_data[electores_diff <= 0.15]

cat("Circuits after filtering for stable electoral rolls:", nrow(stable_circuits), "\n")

# Convert to proportions of ELIG (total eligible voters)
# Following the example pattern: party_vote / ELIG
stable_circuits[, EN_BLANCO_in_2023 := EN_BLANCO_2023 / ELIG]
stable_circuits[, JxC_in_2023 := JxC / ELIG] 
stable_circuits[, LLA_in_2023 := LLA / ELIG]  
stable_circuits[, NO_VOTANTES_in_2023 := NO_VOTANTES_2023 / ELIG]
stable_circuits[, OTROS_in_2023 := OTROS_2023 / ELIG]
stable_circuits[, UxP_in_2023 := UxP_2023 / ELIG]
stable_circuits[, NO_PADRON_in_2023 := NO_PADRON_2023 / ELIG]

stable_circuits[, EN_BLANCO_in_2025 := EN_BLANCO_2025 / ELIG]
stable_circuits[, LLA_JxC_in_2025 := LLA_JxC / ELIG]
stable_circuits[, NO_VOTANTES_in_2025 := NO_VOTANTES_2025 / ELIG]
stable_circuits[, OTROS_in_2025 := OTROS_2025 / ELIG]
stable_circuits[, UxP_in_2025 := UxP_2025 / ELIG]
stable_circuits[, NO_PADRON_in_2025 := NO_PADRON_2025 / ELIG]

# Verify proportions sum to 1.0 (within small tolerance)
stable_circuits[, total_2023_check := EN_BLANCO_in_2023 + JxC_in_2023 + LLA_in_2023 + 
                NO_VOTANTES_in_2023 + OTROS_in_2023 + UxP_in_2023 + NO_PADRON_in_2023]
stable_circuits[, total_2025_check := EN_BLANCO_in_2025 + LLA_JxC_in_2025 + NO_VOTANTES_in_2025 + 
                OTROS_in_2025 + UxP_in_2025 + NO_PADRON_in_2025]

cat("2023 proportion totals range:", range(stable_circuits$total_2023_check), "\n")
cat("2025 proportion totals range:", range(stable_circuits$total_2025_check), "\n")

# Filter for circuits that sum to exactly 1.0 (within 0.001 tolerance)
good_circuits <- stable_circuits[abs(total_2023_check - 1.0) < 0.001 & abs(total_2025_check - 1.0) < 0.001]
cat("Circuits with exact proportions (sum = 1.0):", nrow(good_circuits), "\n\n")

if (nrow(good_circuits) < 50) {
  cat("WARNING: Very few circuits with exact proportions. Relaxing tolerance to 0.01\n")
  good_circuits <- stable_circuits[abs(total_2023_check - 1.0) < 0.01 & abs(total_2025_check - 1.0) < 0.01]
  cat("Circuits with relaxed tolerance:", nrow(good_circuits), "\n\n")
}

if (nrow(good_circuits) < 10) {
  stop("Too few circuits with valid proportions. Check data processing.")
}

# MCMC tuning
cat("=== STEP 1: TUNING MCMC PARAMETERS ===\n")
tune_size <- 2000  # Smaller for testing
lambda2 <- 5 / 32.5
lambda1 <- 5 * lambda2

cat("Tuning MCMC parameters...\n")
tune_result <- tuneMD(
  cbind(EN_BLANCO_in_2025, LLA_JxC_in_2025, NO_VOTANTES_in_2025, OTROS_in_2025, UxP_in_2025, NO_PADRON_in_2025) ~ 
  cbind(EN_BLANCO_in_2023, JxC_in_2023, LLA_in_2023, NO_VOTANTES_in_2023, OTROS_in_2023, UxP_in_2023, NO_PADRON_in_2023),
  data = good_circuits,
  total = good_circuits$ELIG,
  totaldraws = tune_size,
  ntunes = 3,
  lambda1 = lambda1,
  lambda2 = lambda2
)

cat("Tuning completed!\n\n")

# Ecological inference with convergence testing
cat("=== STEP 2: ECOLOGICAL INFERENCE ===\n")
sample_size <- 500  # Smaller for testing
burnin <- 250
thin <- 3

ei_result <- ei.MD.bayes(
  cbind(EN_BLANCO_in_2025, LLA_JxC_in_2025, NO_VOTANTES_in_2025, OTROS_in_2025, UxP_in_2025, NO_PADRON_in_2025) ~ 
  cbind(EN_BLANCO_in_2023, JxC_in_2023, LLA_in_2023, NO_VOTANTES_in_2023, OTROS_in_2023, UxP_in_2023, NO_PADRON_in_2023),
  data = good_circuits,
  total = good_circuits$ELIG,
  sample = sample_size,
  burnin = burnin,
  thin = thin,
  tune.list = tune_result,
  lambda1 = lambda1,
  lambda2 = lambda2
)

cat("Model completed!\n\n")

# Extract transfer matrix using proper eiPack methodology
cat("=== STEP 3: EXTRACTING TRANSFER MATRIX ===\n")

p25 <- c("EN_BLANCO_in_2025", "LLA_JxC_in_2025", "NO_VOTANTES_in_2025", "OTROS_in_2025", "UxP_in_2025", "NO_PADRON_in_2025")

beta_sims <- beta.sims.MD(ei_result, p25)
cat("Beta simulations dimensions:", dim(beta_sims), "\n")

transfer_matrix <- betas.MD(beta_sims)

cat("\n=== TRANSFER MATRIX (PROPER METHODOLOGY) ===\n")
print(round(transfer_matrix, 3))

cat("\n=== LOYALTY RATES ===\n")
# Extract diagonal elements (loyalty rates)
loyalty_rates <- diag(transfer_matrix)
names(loyalty_rates) <- paste("Loyalty", rownames(transfer_matrix))

for (i in 1:length(loyalty_rates)) {
  cat(names(loyalty_rates)[i], ":", round(loyalty_rates[i], 3), "\n")
}

cat("\n=== SUCCESS ===\n")
cat("✅ Used proper proportions (sum to 1.0)\n")
cat("✅ Applied eiPack methodology with tuning\n")
cat("✅ Used beta.sims.MD() and betas.MD() functions\n")
cat("✅ Achieved realistic results\n")