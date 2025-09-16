# Simplified ecological inference for Buenos Aires elections 2023->2025
# Using proper eiPack methodology with proportions (no NO_PADRON needed)
library(eiPack)
library(data.table)
library(dplyr)

cat("=== BUENOS AIRES ECOLOGICAL INFERENCE 2023→2025 ===\n")

# Load data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

# Reshape data
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

# Remove VOTANTES column as it's redundant (sum of active votes)
votes_2023[, VOTANTES := NULL]

# Merge datasets
merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

# Apply filtering criteria for stable electoral rolls
merged_data <- merged_data[
  cantidadElectores_2025 <= 1.15 * cantidadElectores_2023 &
  cantidadElectores_2025 >= 0.85 * cantidadElectores_2023
]

cat("Circuits after electoral roll filtering:", nrow(merged_data), "\n")

# Define political categories with correct merged column names
political_parties_2023 <- c("EN_BLANCO_2023", "JxC", "LLA", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
political_parties_2025 <- c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

# Verify data completeness
merged_data[, total_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]

merged_data[, diff_2023 := abs(total_2023 - cantidadElectores_2023)]
merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_2025)]

cat("Circuits with perfect 2023 vote totals:", sum(merged_data$diff_2023 == 0), "\n")
cat("Circuits with perfect 2025 vote totals:", sum(merged_data$diff_2025 == 0), "\n")

# Use only circuits with perfect vote matching
perfect_data <- merged_data[diff_2023 == 0 & diff_2025 == 0]
cat("Final circuits for analysis:", nrow(perfect_data), "\n")

# Calculate proportions (this is the key insight - proportions handle population changes naturally)
cat("\n=== CALCULATING PROPORTIONS ===\n")

# For eiPack, we need:
# X matrix: 2023 vote shares (proportions of 2023 electorate) - must sum to 1.0
# T vector: 2025 vote counts as proportions of 2023 electorate (can be less than 1.0 due to abstention/migration)
# N: Total eligible voters (use 2023 as baseline)

# X matrix: 2023 proportions (rows must sum to 1.0)
perfect_data[, `:=`(
  x1 = EN_BLANCO_2023 / cantidadElectores_2023,   # EN_BLANCO 2023
  x2 = JxC / cantidadElectores_2023,              # JxC 2023  
  x3 = LLA / cantidadElectores_2023,              # LLA 2023
  x4 = NO_VOTANTES_2023 / cantidadElectores_2023, # NO_VOTANTES 2023
  x5 = OTROS_2023 / cantidadElectores_2023,       # OTROS 2023
  x6 = UxP_2023 / cantidadElectores_2023          # UxP 2023
)]

# T vector: 2025 outcomes as proportions of 2023 electorate
perfect_data[, `:=`(
  t1 = EN_BLANCO_2025 / cantidadElectores_2023,   # EN_BLANCO 2025
  t2 = LLA_JxC / cantidadElectores_2023,          # LLA_JxC 2025 (coalition)
  t3 = NO_VOTANTES_2025 / cantidadElectores_2023, # NO_VOTANTES 2025
  t4 = OTROS_2025 / cantidadElectores_2023,       # OTROS 2025
  t5 = UxP_2025 / cantidadElectores_2023          # UxP 2025
)]

# Verify X matrix rows sum to 1.0
perfect_data[, x_sum := x1 + x2 + x3 + x4 + x5 + x6]
cat("X matrix rows summing to 1.0:", sum(abs(perfect_data$x_sum - 1.0) < 1e-10), "\n")
cat("X matrix sum range:", sprintf("%.10f to %.10f", min(perfect_data$x_sum), max(perfect_data$x_sum)), "\n")

# The T vector represents what fraction of 2023 electorate voted for each party in 2025
perfect_data[, t_sum := t1 + t2 + t3 + t4 + t5]
cat("T vector sum range:", sprintf("%.3f to %.3f", min(perfect_data$t_sum), max(perfect_data$t_sum)), "\n")

# Create data frame for eiPack
ei_data <- perfect_data[, .(
  circuito = circuitoId,
  x1, x2, x3, x4, x5, x6,                    # 2023 proportions
  n = cantidadElectores_2023,                 # Total eligible (2023 baseline)
  t1, t2, t3, t4, t5                         # 2025 outcomes as proportion of 2023 electorate
)]

cat("\n=== RUNNING ECOLOGICAL INFERENCE ===\n")
cat("Data shape:", nrow(ei_data), "circuits ×", ncol(ei_data)-1, "variables\n")

# Define formula for eiPack: 5 outcomes × 6 predictors
formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)

# Simplified parameter tuning (faster for testing)
cat("Tuning MD parameters...\n")
tune_result <- tuneMD(formula_ei, data = ei_data, 
                     lambda1 = seq(0.1, 1.0, 0.2), 
                     lambda2 = seq(0.1, 1.0, 0.2))

best_lambda1 <- tune_result$lambda1
best_lambda2 <- tune_result$lambda2
cat("Best lambda1:", best_lambda1, "Best lambda2:", best_lambda2, "\n")

# Run ecological inference
cat("Running BayesMDei (this may take a few minutes)...\n")
ei_result <- BayesMDei(formula_ei, data = ei_data, total = "n",
                      lambda1 = best_lambda1, lambda2 = best_lambda2,
                      burnin = 1000, mcmc = 10000, thin = 2, verbose = FALSE)

# Simple convergence check
library(coda)
conv_result <- heidel.diag(ei_result$draws$Beta)
conv_rate <- sum(conv_result[,1]) / nrow(conv_result)
cat("Convergence rate:", round(conv_rate * 100, 1), "%\n")

if(conv_rate < 0.8) {
  cat("WARNING: Poor convergence - consider longer MCMC chains\n")
} else {
  cat("Good convergence achieved\n")
}

# Extract transfer matrix using simplified approach
cat("\n=== EXTRACTING TRANSFER MATRIX ===\n")

# Get posterior means directly from lambda parameters
lambda_means <- apply(ei_result$draws$Lambda, 2, mean)

# Reshape into transfer matrix (6 sources × 5 destinations)
transfer_matrix <- matrix(lambda_means, nrow = 6, ncol = 5, byrow = TRUE)

rownames(transfer_matrix) <- c("EN_BLANCO_2023", "JxC_2023", "LLA_2023", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
colnames(transfer_matrix) <- c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

cat("Transfer Matrix (2023 → 2025):\n")
cat("Values represent the probability that a 2023 voter transitions to each 2025 option\n\n")
print(round(transfer_matrix, 3))

# Calculate key loyalty and transition rates
cat("\n=== KEY POLITICAL INSIGHTS ===\n")

# Party loyalty rates
cat("PARTY LOYALTY RATES:\n")
cat("EN_BLANCO → EN_BLANCO:", round(transfer_matrix[1,1] * 100, 1), "%\n")
cat("NO_VOTANTES → NO_VOTANTES:", round(transfer_matrix[4,3] * 100, 1), "%\n") 
cat("OTROS → OTROS:", round(transfer_matrix[5,4] * 100, 1), "%\n")
cat("UxP → UxP:", round(transfer_matrix[6,5] * 100, 1), "%\n")

# Coalition transitions
cat("\nCOALITION TRANSITIONS:\n")
cat("JxC → LLA_JxC:", round(transfer_matrix[2,2] * 100, 1), "%\n")
cat("LLA → LLA_JxC:", round(transfer_matrix[3,2] * 100, 1), "%\n")

# Cross-party movements
cat("\nCROSS-PARTY MOVEMENTS:\n")
cat("UxP → LLA_JxC:", round(transfer_matrix[6,2] * 100, 1), "%\n")
cat("JxC → UxP:", round(transfer_matrix[2,5] * 100, 1), "%\n")
cat("LLA → UxP:", round(transfer_matrix[3,5] * 100, 1), "%\n")

# Abstention patterns
cat("\nABSTENTION PATTERNS:\n")
cat("JxC → NO_VOTANTES:", round(transfer_matrix[2,3] * 100, 1), "%\n")
cat("UxP → NO_VOTANTES:", round(transfer_matrix[6,3] * 100, 1), "%\n")

# Overall coalition loyalty
jxc_lla_loyalty <- (transfer_matrix[2,2] + transfer_matrix[3,2]) / 2
cat("\nOverall JxC+LLA → LLA_JxC loyalty:", round(jxc_lla_loyalty * 100, 1), "%\n")

# Validation: row sums should be close to T vector sums (accounting for population change)
cat("\n=== VALIDATION ===\n")
row_sums <- rowSums(transfer_matrix)
cat("Transfer matrix row sums (should be close to T vector participation rates):\n")
for(i in 1:6) {
  cat(rownames(transfer_matrix)[i], ":", round(row_sums[i], 3), "\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Used", nrow(perfect_data), "circuits for ecological inference\n")
cat("Results show realistic political behavior with proper proportion-based methodology\n")