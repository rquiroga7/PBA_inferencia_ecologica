# Correct ecological inference using proper eiPack methodology
# Key insight: NO_PADRON is not needed - the data already sums correctly
library(eiPack)
library(data.table)
library(dplyr)

# Source the helper functions  
source("betas.R")
source("prepare_results.R")

# Load data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("=== PROCESSING DATA WITHOUT NO_PADRON ===\n")

# Reshape data
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

# Remove VOTANTES column as it's redundant
votes_2023[, VOTANTES := NULL]

# Merge datasets
merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

# Apply filtering criteria
merged_data <- merged_data[
  cantidadElectores_2025 <= 1.15 * cantidadElectores_2023 &
  cantidadElectores_2025 >= 0.85 * cantidadElectores_2023
]

cat("Circuits after filtering:", nrow(merged_data), "\n")

# Define political categories (same for both years)
political_parties_2023 <- c("EN_BLANCO", "JxC", "LLA", "NO_VOTANTES", "OTROS", "UxP")
political_parties_2025 <- c("EN_BLANCO", "LLA_JxC", "NO_VOTANTES", "OTROS", "UxP")

# Verify data completeness - votes should sum to cantidadElectores
merged_data[, total_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]

# Check if totals match electorate
merged_data[, diff_2023 := abs(total_2023 - cantidadElectores_2023)]
merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_2025)]

cat("Circuits with perfect 2023 vote totals:", sum(merged_data$diff_2023 == 0), "\n")
cat("Circuits with perfect 2025 vote totals:", sum(merged_data$diff_2025 == 0), "\n")
cat("Max difference 2023:", max(merged_data$diff_2023), "\n")
cat("Max difference 2025:", max(merged_data$diff_2025), "\n")

# Use only circuits with perfect vote matching
perfect_data <- merged_data[diff_2023 == 0 & diff_2025 == 0]
cat("Final circuits for analysis:", nrow(perfect_data), "\n")

# Calculate proportions using cantidadElectores as base
# This should give us exact proportions that sum to 1.0

cat("\n=== CALCULATING PROPORTIONS ===\n")

# 2023 proportions
perfect_data[, `:=`(
  prop_EN_BLANCO_2023 = EN_BLANCO / cantidadElectores_2023,
  prop_JxC_2023 = JxC / cantidadElectores_2023,
  prop_LLA_2023 = LLA / cantidadElectores_2023,
  prop_NO_VOTANTES_2023 = NO_VOTANTES / cantidadElectores_2023,
  prop_OTROS_2023 = OTROS / cantidadElectores_2023,
  prop_UxP_2023 = UxP / cantidadElectores_2023
)]

# 2025 proportions (using 2025 electorate)
perfect_data[, `:=`(
  prop_EN_BLANCO_2025 = EN_BLANCO.1 / cantidadElectores_2025,
  prop_LLA_JxC_2025 = LLA_JxC / cantidadElectores_2025,
  prop_NO_VOTANTES_2025 = NO_VOTANTES.1 / cantidadElectores_2025,
  prop_OTROS_2025 = OTROS.1 / cantidadElectores_2025,
  prop_UxP_2025 = UxP.1 / cantidadElectores_2025
)]

# Verify proportions sum to 1.0
perfect_data[, prop_sum_2023 := prop_EN_BLANCO_2023 + prop_JxC_2023 + prop_LLA_2023 + 
                                prop_NO_VOTANTES_2023 + prop_OTROS_2023 + prop_UxP_2023]

perfect_data[, prop_sum_2025 := prop_EN_BLANCO_2025 + prop_LLA_JxC_2025 + prop_NO_VOTANTES_2025 + 
                                prop_OTROS_2025 + prop_UxP_2025]

cat("Circuits with 2023 proportions summing to 1.0:", sum(abs(perfect_data$prop_sum_2023 - 1.0) < 1e-10), "\n")
cat("Circuits with 2025 proportions summing to 1.0:", sum(abs(perfect_data$prop_sum_2025 - 1.0) < 1e-10), "\n")

# Check example
cat("\nExample proportions for circuit", perfect_data$circuitoId[1], ":\n")
example <- perfect_data[1]
cat("2023 proportions sum:", example$prop_sum_2023, "\n")
cat("2025 proportions sum:", example$prop_sum_2025, "\n")

# Create data frame for eiPack
ei_data <- data.frame(
  circuito = perfect_data$circuitoId,
  
  # X matrix: 2023 proportions (rows must sum to 1)
  x1 = perfect_data$prop_EN_BLANCO_2023,
  x2 = perfect_data$prop_JxC_2023, 
  x3 = perfect_data$prop_LLA_2023,
  x4 = perfect_data$prop_NO_VOTANTES_2023,
  x5 = perfect_data$prop_OTROS_2023,
  x6 = perfect_data$prop_UxP_2023,
  
  # N: Total eligible voters (using 2023 as baseline)
  n = perfect_data$cantidadElectores_2023,
  
  # T: 2025 outcomes as proportions of 2023 electorate
  t1 = perfect_data$EN_BLANCO.1 / perfect_data$cantidadElectores_2023,
  t2 = perfect_data$LLA_JxC / perfect_data$cantidadElectores_2023,
  t3 = perfect_data$NO_VOTANTES.1 / perfect_data$cantidadElectores_2023,
  t4 = perfect_data$OTROS.1 / perfect_data$cantidadElectores_2023,
  t5 = perfect_data$UxP.1 / perfect_data$cantidadElectores_2023
)

cat("\n=== FINAL DATA VERIFICATION ===\n")
cat("Rows in ei_data:", nrow(ei_data), "\n")

# Check X matrix row sums
ei_data$x_sum <- ei_data$x1 + ei_data$x2 + ei_data$x3 + ei_data$x4 + ei_data$x5 + ei_data$x6
cat("X matrix rows summing to 1.0:", sum(abs(ei_data$x_sum - 1.0) < 1e-10), "\n")
cat("X matrix sum range:", range(ei_data$x_sum), "\n")

# The T vector doesn't need to sum to 1 (people can abstain, migrate, etc.)
ei_data$t_sum <- ei_data$t1 + ei_data$t2 + ei_data$t3 + ei_data$t4 + ei_data$t5
cat("T vector sum range:", range(ei_data$t_sum), "\n")

cat("\n=== RUNNING ECOLOGICAL INFERENCE ===\n")

# Define the formula for eiPack
formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)

# Tune MD parameters
cat("Tuning MD parameters...\n")
tune_result <- tuneMD(formula_ei, data = ei_data, 
                     lambda1 = seq(0.1, 2, 0.1), 
                     lambda2 = seq(0.1, 2, 0.1))

best_lambda1 <- tune_result$lambda1
best_lambda2 <- tune_result$lambda2
cat("Best lambda1:", best_lambda1, "Best lambda2:", best_lambda2, "\n")

# Run ecological inference with tuned parameters
cat("Running BayesMDei...\n")
ei_result <- BayesMDei(formula_ei, data = ei_data, total = "n",
                      lambda1 = best_lambda1, lambda2 = best_lambda2,
                      burnin = 5000, mcmc = 50000, thin = 5, verbose = TRUE)

cat("Checking convergence...\n")
# Check convergence
conv_result <- heidel.diag(ei_result$draws$Beta)
cat("Convergence test passed for", sum(conv_result[,1]), "out of", nrow(conv_result), "parameters\n")

if(sum(conv_result[,1]) < 0.8 * nrow(conv_result)) {
  cat("WARNING: Poor convergence detected\n")
} else {
  cat("Good convergence achieved\n")
}

# Extract transfer matrix using beta.sims.MD
cat("\n=== EXTRACTING TRANSFER MATRIX ===\n")
beta_sims <- beta.sims.MD(ei_result)

# Calculate posterior means of transfer probabilities  
transfer_matrix <- betas.MD(ei_result)

cat("Transfer Matrix (2023 → 2025):\n")
cat("Rows: 2023 parties (EN_BLANCO, JxC, LLA, NO_VOTANTES, OTROS, UxP)\n")
cat("Cols: 2025 parties (EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP)\n\n")

rownames(transfer_matrix) <- c("EN_BLANCO_2023", "JxC_2023", "LLA_2023", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
colnames(transfer_matrix) <- c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

print(round(transfer_matrix, 3))

# Calculate and display loyalty rates (diagonal elements)
cat("\n=== PARTY LOYALTY RATES ===\n")
cat("EN_BLANCO loyalty:", round(transfer_matrix[1,1] * 100, 1), "%\n")
cat("JxC → LLA_JxC loyalty:", round(transfer_matrix[2,2] * 100, 1), "%\n") 
cat("LLA → LLA_JxC loyalty:", round(transfer_matrix[3,2] * 100, 1), "%\n")
cat("NO_VOTANTES loyalty:", round(transfer_matrix[4,3] * 100, 1), "%\n")
cat("OTROS loyalty:", round(transfer_matrix[5,4] * 100, 1), "%\n")
cat("UxP loyalty:", round(transfer_matrix[6,5] * 100, 1), "%\n")

# Combined JxC+LLA → LLA_JxC rate
jxc_lla_to_lla_jxc <- (transfer_matrix[2,2] + transfer_matrix[3,2]) / 2
cat("Combined JxC+LLA → LLA_JxC rate:", round(jxc_lla_to_lla_jxc * 100, 1), "%\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Used", nrow(perfect_data), "circuits for ecological inference\n")
cat("Results should show realistic loyalty rates above 80% for major parties\n")