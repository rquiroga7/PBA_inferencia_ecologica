# Test optimal thinning parameter for MCMC chains
# This script compares different thin values to find the optimal setting

library(eiPack)
library(data.table)
library(dplyr)
library(coda)

cat("=== TESTING OPTIMAL THINNING PARAMETER ===\n")

# Load and prepare data (same as main script)
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))
votes_2023[, VOTANTES := NULL]

merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))
merged_data <- merged_data[
  cantidadElectores_2025 <= 1.15 * cantidadElectores_2023 &
  cantidadElectores_2025 >= 0.85 * cantidadElectores_2023
]

political_parties_2023 <- c("EN_BLANCO_2023", "JxC", "LLA", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
political_parties_2025 <- c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

merged_data[, total_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]
merged_data[, diff_2023 := abs(total_2023 - cantidadElectores_2023)]
merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_2025)]

perfect_data <- merged_data[diff_2023 == 0 & diff_2025 == 0]

# Use smaller dataset for testing (first 100 circuits)
test_data <- perfect_data[1:100]

test_data[, `:=`(
  x1 = EN_BLANCO_2023 / cantidadElectores_2023,
  x2 = JxC / cantidadElectores_2023,              
  x3 = LLA / cantidadElectores_2023,              
  x4 = NO_VOTANTES_2023 / cantidadElectores_2023,
  x5 = OTROS_2023 / cantidadElectores_2023,       
  x6 = UxP_2023 / cantidadElectores_2023,          
  t1 = EN_BLANCO_2025 / cantidadElectores_2025,   
  t2 = LLA_JxC / cantidadElectores_2025,          
  t3 = NO_VOTANTES_2025 / cantidadElectores_2025, 
  t4 = OTROS_2025 / cantidadElectores_2025,       
  t5 = UxP_2025 / cantidadElectores_2025          
)]

ei_data <- test_data[, .(
  circuito = circuitoId,
  x1, x2, x3, x4, x5, x6,
  n = cantidadElectores_2023,
  t1, t2, t3, t4, t5
)]

formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)

# Lambda parameters
lambda2 <- 5 / 32.5
lambda1 <- 5 * lambda2

# Parameter tuning (once for all tests)
cat("Performing initial parameter tuning...\n")
tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                     totaldraws = 500, ntunes = 3,
                     lambda1 = lambda1, lambda2 = lambda2)

# Test different thinning values
thin_values <- c(1, 2, 5, 10, 20)
results_comparison <- data.frame(
  thin = thin_values,
  eff_size_min = NA,
  eff_size_mean = NA,
  autocorr_lag1 = NA,
  autocorr_lag5 = NA,
  time_seconds = NA
)

cat("\n=== TESTING DIFFERENT THINNING VALUES ===\n")

for(i in seq_along(thin_values)) {
  thin_val <- thin_values[i]
  cat("\nTesting thin =", thin_val, "...\n")
  
  start_time <- Sys.time()
  
  # Run MCMC with current thin value
  # Use smaller chains for faster testing
  ei_test <- try({
    ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
               lambda1 = lambda1, lambda2 = lambda2,
               burnin = 1000, sample = 3000, thin = thin_val,
               tune.list = tune_result)
  }, silent = TRUE)
  
  end_time <- Sys.time()
  run_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  results_comparison$time_seconds[i] <- run_time
  
  if(class(ei_test) == "try-error") {
    cat("  ❌ Failed to run with thin =", thin_val, "\n")
    next
  }
  
  # Calculate post-burnin samples
  total_samples <- nrow(ei_test$draws$Beta)
  effective_samples <- ceiling(total_samples * 0.8)  # Use last 80%
  
  if(effective_samples < 100) {
    cat("  ⚠️  Too few effective samples (", effective_samples, ")\n")
    next
  }
  
  # Sample subset of parameters for analysis (first 20)
  sample_params <- min(20, ncol(ei_test$draws$Beta))
  beta_subset <- ei_test$draws$Beta[(total_samples - effective_samples + 1):total_samples, 1:sample_params]
  
  # Calculate effective sample sizes
  mcmc_obj <- mcmc(beta_subset)
  eff_sizes <- try(effectiveSize(mcmc_obj), silent = TRUE)
  
  if(class(eff_sizes) == "try-error") {
    cat("  ⚠️  Could not calculate effective sample size\n")
    next
  }
  
  results_comparison$eff_size_min[i] <- min(eff_sizes, na.rm = TRUE)
  results_comparison$eff_size_mean[i] <- mean(eff_sizes, na.rm = TRUE)
  
  # Calculate autocorrelation
  autocorr_lag1 <- try(mean(sapply(1:sample_params, function(j) {
    acf(beta_subset[,j], lag.max = 1, plot = FALSE)$acf[2]
  }), na.rm = TRUE), silent = TRUE)
  
  autocorr_lag5 <- try(mean(sapply(1:sample_params, function(j) {
    acf(beta_subset[,j], lag.max = 5, plot = FALSE)$acf[6]
  }), na.rm = TRUE), silent = TRUE)
  
  if(class(autocorr_lag1) != "try-error") {
    results_comparison$autocorr_lag1[i] <- autocorr_lag1
  }
  
  if(class(autocorr_lag5) != "try-error") {
    results_comparison$autocorr_lag5[i] <- autocorr_lag5
  }
  
  cat("  ✅ Completed - ESS mean:", round(results_comparison$eff_size_mean[i], 0), 
      "AutoCorr lag1:", round(results_comparison$autocorr_lag1[i], 3),
      "Time:", round(run_time, 1), "sec\n")
}

# Display results
cat("\n=== THINNING COMPARISON RESULTS ===\n")
print(results_comparison)

# Analysis and recommendations
cat("\n=== ANALYSIS AND RECOMMENDATIONS ===\n")

# Find optimal thin value based on multiple criteria
valid_results <- results_comparison[!is.na(results_comparison$eff_size_mean), ]

if(nrow(valid_results) > 0) {
  # Score each thin value (higher is better)
  valid_results$ess_score <- valid_results$eff_size_mean / max(valid_results$eff_size_mean, na.rm = TRUE)
  valid_results$autocorr_score <- 1 - abs(valid_results$autocorr_lag1) / max(abs(valid_results$autocorr_lag1), na.rm = TRUE)
  valid_results$time_score <- min(valid_results$time_seconds, na.rm = TRUE) / valid_results$time_seconds
  
  # Replace NaN with 0
  valid_results[is.nan(valid_results$autocorr_score), "autocorr_score"] <- 0
  
  # Combined score (equal weights)
  valid_results$total_score <- (valid_results$ess_score + valid_results$autocorr_score + valid_results$time_score) / 3
  
  # Find best thin value
  best_idx <- which.max(valid_results$total_score)
  best_thin <- valid_results$thin[best_idx]
  
  cat("OPTIMAL THINNING ANALYSIS:\n")
  cat("Current setting: thin = 2\n")
  cat("Recommended optimal: thin =", best_thin, "\n\n")
  
  cat("DETAILED COMPARISON:\n")
  for(i in 1:nrow(valid_results)) {
    thin_val <- valid_results$thin[i]
    cat("thin =", thin_val, ":\n")
    cat("  - Effective Sample Size:", round(valid_results$eff_size_mean[i], 0), "\n")
    cat("  - Autocorrelation (lag-1):", round(valid_results$autocorr_lag1[i], 3), "\n")
    cat("  - Runtime:", round(valid_results$time_seconds[i], 1), "seconds\n")
    cat("  - Overall Score:", round(valid_results$total_score[i], 3), "\n\n")
  }
  
  # Specific recommendations
  if(best_thin != 2) {
    improvement_ess <- valid_results$eff_size_mean[best_idx] / valid_results$eff_size_mean[valid_results$thin == 2]
    cat("RECOMMENDATION:\n")
    cat("📈 Change from thin = 2 to thin =", best_thin, "\n")
    cat("Expected improvements:\n")
    cat("  - Effective sample size:", round((improvement_ess - 1) * 100, 1), "% increase\n")
    cat("  - Better autocorrelation reduction\n")
    cat("  - More reliable convergence diagnostics\n")
  } else {
    cat("RECOMMENDATION:\n")
    cat("✅ Current thin = 2 setting is optimal\n")
    cat("   No changes needed for thinning parameter\n")
  }
} else {
  cat("❌ Could not complete thinning analysis\n")
  cat("   Consider using smaller dataset or adjusting MCMC parameters\n")
}

cat("\n=== THINNING TEST COMPLETED ===\n")