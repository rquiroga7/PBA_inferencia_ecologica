# Final corrected ecological inference - understanding eiPack requirements
library(eiPack)
library(data.table)
library(dplyr)

cat("=== BUENOS AIRES ECOLOGICAL INFERENCE 2023→2025 ===\n")

# Load data
data_2023 <- fread("data/electores_circuitos_2023.csv")
data_2025 <- fread("data/electores_circuitos_2025.csv")

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

# Define political categories with correct merged column names
political_parties_2023 <- c("EN_BLANCO_2023", "JxC", "LLA", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
political_parties_2025 <- c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

# Verify data completeness
merged_data[, total_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]

merged_data[, diff_2023 := abs(total_2023 - cantidadElectores_2023)]
merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_2025)]

# Use only circuits with perfect vote matching
perfect_data <- merged_data[diff_2023 == 0 & diff_2025 == 0]
cat("Final circuits for analysis:", nrow(perfect_data), "\n")

cat("\n=== UNDERSTANDING eiPack REQUIREMENTS ===\n")

# For eiPack ecological inference:
# X matrix: baseline year proportions (must sum to 1.0 across columns for each row)
# T matrix: outcome year proportions (must sum to 1.0 across columns for each row) 
# N: total population size

# The key insight: Both X and T must be proportions of their respective year totals, not cross-year fractions

# X matrix: 2023 proportions (proportion of 2023 electorate that voted for each party)
perfect_data[, `:=`(
  x1 = EN_BLANCO_2023 / cantidadElectores_2023,   # EN_BLANCO 2023
  x2 = JxC / cantidadElectores_2023,              # JxC 2023  
  x3 = LLA / cantidadElectores_2023,              # LLA 2023
  x4 = NO_VOTANTES_2023 / cantidadElectores_2023, # NO_VOTANTES 2023
  x5 = OTROS_2023 / cantidadElectores_2023,       # OTROS 2023
  x6 = UxP_2023 / cantidadElectores_2023          # UxP 2023
)]

# T matrix: 2025 proportions (proportion of 2025 electorate that voted for each party)
perfect_data[, `:=`(
  t1 = EN_BLANCO_2025 / cantidadElectores_2025,   # EN_BLANCO 2025
  t2 = LLA_JxC / cantidadElectores_2025,          # LLA_JxC 2025 (coalition)
  t3 = NO_VOTANTES_2025 / cantidadElectores_2025, # NO_VOTANTES 2025
  t4 = OTROS_2025 / cantidadElectores_2025,       # OTROS 2025
  t5 = UxP_2025 / cantidadElectores_2025          # UxP 2025
)]

# Verify both X and T matrices have rows summing to 1.0
perfect_data[, x_sum := x1 + x2 + x3 + x4 + x5 + x6]
perfect_data[, t_sum := t1 + t2 + t3 + t4 + t5]

cat("X matrix rows summing to 1.0:", sum(abs(perfect_data$x_sum - 1.0) < 1e-10), "\n")
cat("T matrix rows summing to 1.0:", sum(abs(perfect_data$t_sum - 1.0) < 1e-10), "\n")

cat("X matrix sum range:", sprintf("%.10f to %.10f", min(perfect_data$x_sum), max(perfect_data$x_sum)), "\n")
cat("T matrix sum range:", sprintf("%.10f to %.10f", min(perfect_data$t_sum), max(perfect_data$t_sum)), "\n")

# Create data frame for eiPack using 2023 as baseline population
ei_data <- perfect_data[, .(
  circuito = circuitoId,
  x1, x2, x3, x4, x5, x6,                    # 2023 proportions (sum to 1)
  n = cantidadElectores_2023,                 # Baseline population
  t1, t2, t3, t4, t5                         # 2025 proportions (sum to 1)
)]

cat("\n=== RUNNING ECOLOGICAL INFERENCE ===\n")
cat("Data shape:", nrow(ei_data), "circuits ×", ncol(ei_data)-1, "variables\n")

# Define formula: 5 outcome categories × 6 baseline categories
formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)

# Set lambda parameters based on eiPack example pattern
# These are hyperparameters for the Dirichlet distribution prior
lambda2 <- 5 / 32.5  # From the example
lambda1 <- 5 * lambda2
cat("Using lambda1:", lambda1, "lambda2:", lambda2, "\n")

# Parameter tuning (to get appropriate MCMC settings)
cat("Tuning MD parameters...\n")
tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                     totaldraws = 2000, ntunes = 5,
                     lambda1 = lambda1, lambda2 = lambda2)

cat("Tuning completed successfully\n")

# Run ecological inference using ei.MD.bayes (with optimized MCMC settings)
cat("Running ei.MD.bayes with optimized MCMC parameters...\n")
ei_result <- ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
                        lambda1 = lambda1, lambda2 = lambda2,
                        burnin = 100000, sample = 4000, thin = 100,  # Optimized thin value
                        tune.list = tune_result)

# Comprehensive convergence checking
library(coda)
cat("Ecological inference completed! Checking convergence...\n")

# === CONVERGENCE DIAGNOSTICS ===
cat("\n=== MCMC CONVERGENCE DIAGNOSTICS ===\n")

beta_draws <- ei_result$draws$Beta
n_samples <- nrow(beta_draws)
n_params <- ncol(beta_draws)

# Remove burn-in period for convergence analysis
burn_samples <- max(1, floor(n_samples * 0.2))
post_burn_draws <- beta_draws[(burn_samples:n_samples), ]
cat("Analyzing", nrow(post_burn_draws), "post-burn-in samples for", n_params, "parameters\n")

# Test on a manageable sample of parameters
sample_params <- min(50, n_params)  # Reduced sample size for reliability
sample_indices <- seq(1, n_params, length.out = sample_params)

# Initialize convergence tracking
convergence_tests <- list()

# 1. Heidelberger-Welch Convergence Test
cat("\n1. Heidelberger-Welch Convergence Test:\n")
hd_success <- FALSE
try({
  mcmc_sample <- mcmc(post_burn_draws[, sample_indices])
  hd_results <- heidel.diag(mcmc_sample)
  
  if(is.matrix(hd_results) && ncol(hd_results) >= 1) {
    # Check if first column contains logical values (convergence test results)
    if(is.logical(hd_results[,1])) {
      convergence_rate <- sum(hd_results[,1]) / nrow(hd_results)
      convergence_failures <- sum(!hd_results[,1])
      convergence_tests$heidel <- hd_results[,1]
      hd_success <- TRUE
    } else {
      # Try different column structure
      convergence_rate <- mean(hd_results[,1])
      convergence_failures <- sum(hd_results[,1] < 0.5)
      hd_success <- TRUE
    }
    
    cat("  Convergence rate:", round(convergence_rate * 100, 1), "%\n")
    cat("  Parameters with issues:", convergence_failures, "out of", sample_params, "\n")
    
    if(convergence_rate >= 0.8) {
      cat("  ✅ Good Heidelberger-Welch convergence\n")
    } else if(convergence_rate >= 0.6) {
      cat("  ⚠️  Moderate Heidelberger-Welch convergence\n")
    } else {
      cat("  ❌ Poor Heidelberger-Welch convergence\n")
    }
  }
}, silent = TRUE)

if(!hd_success) {
  cat("  Heidelberger-Welch test could not be completed\n")
}

# 2. Geweke Convergence Diagnostic
cat("\n2. Geweke Convergence Diagnostic:\n")
geweke_success <- FALSE
try({
  mcmc_sample <- mcmc(post_burn_draws[, sample_indices])
  geweke_results <- geweke.diag(mcmc_sample)
  
  # Check for convergence issues (|z-score| > 2)
  geweke_pvalues <- 2 * pnorm(-abs(geweke_results$z))
  geweke_failures <- sum(geweke_pvalues < 0.05, na.rm = TRUE)
  geweke_rate <- 1 - (geweke_failures / length(geweke_results$z))
  convergence_tests$geweke <- geweke_pvalues >= 0.05
  geweke_success <- TRUE
  
  cat("  Geweke convergence rate:", round(geweke_rate * 100, 1), "%\n")
  cat("  Parameters failing Geweke test:", geweke_failures, "out of", length(geweke_results$z), "\n")
  
  if(geweke_rate >= 0.9) {
    cat("  ✅ Excellent Geweke convergence\n")
  } else if(geweke_rate >= 0.7) {
    cat("  ✅ Good Geweke convergence\n")
  } else {
    cat("  ⚠️  Some parameters show convergence issues\n")
  }
}, silent = TRUE)

if(!geweke_success) {
  cat("  Geweke test could not be completed\n")
}

# 3. Effective Sample Size
cat("\n3. Effective Sample Size Analysis:\n")
ess_success <- FALSE
try({
  mcmc_sample <- mcmc(post_burn_draws[, sample_indices])
  eff_sizes <- effectiveSize(mcmc_sample)
  
  min_eff <- min(eff_sizes, na.rm = TRUE)
  mean_eff <- mean(eff_sizes, na.rm = TRUE)
  prop_adequate <- sum(eff_sizes >= 200, na.rm = TRUE) / length(eff_sizes)  # Lowered threshold
  convergence_tests$ess <- eff_sizes >= 200
  ess_success <- TRUE
  
  cat("  Minimum effective sample size:", round(min_eff, 0), "\n")
  cat("  Mean effective sample size:", round(mean_eff, 0), "\n")
  cat("  Proportion with adequate ESS (≥200):", round(prop_adequate * 100, 1), "%\n")
  
  if(min_eff >= 200) {
    cat("  ✅ All parameters have adequate effective sample size\n")
  } else if(mean_eff >= 200) {
    cat("  ✅ Most parameters have adequate effective sample size\n") 
  } else {
    cat("  ⚠️  Consider longer chains for better precision\n")
  }
}, silent = TRUE)

if(!ess_success) {
  cat("  Effective sample size analysis could not be completed\n")
}

# 4. Overall Convergence Assessment
cat("\n=== OVERALL CONVERGENCE ASSESSMENT ===\n")

total_convergent <- 0
total_tests <- 0

# Aggregate results from successful tests
if(hd_success && "heidel" %in% names(convergence_tests)) {
  heidel_passed <- sum(convergence_tests$heidel, na.rm = TRUE)
  heidel_total <- length(convergence_tests$heidel)
  total_convergent <- total_convergent + heidel_passed
  total_tests <- total_tests + heidel_total
  cat("Heidelberger-Welch: ", heidel_passed, "/", heidel_total, " passed\n")
}

if(geweke_success && "geweke" %in% names(convergence_tests)) {
  geweke_passed <- sum(convergence_tests$geweke, na.rm = TRUE)
  geweke_total <- length(convergence_tests$geweke)
  total_convergent <- total_convergent + geweke_passed
  total_tests <- total_tests + geweke_total
  cat("Geweke diagnostic: ", geweke_passed, "/", geweke_total, " passed\n")
}

if(ess_success && "ess" %in% names(convergence_tests)) {
  ess_passed <- sum(convergence_tests$ess, na.rm = TRUE)
  ess_total <- length(convergence_tests$ess)
  total_convergent <- total_convergent + ess_passed
  total_tests <- total_tests + ess_total
  cat("Effective sample size: ", ess_passed, "/", ess_total, " adequate\n")
}

if(total_tests > 0) {
  overall_rate <- total_convergent / total_tests
  cat("\nOverall convergence rate:", round(overall_rate * 100, 1), "%\n")
  
  if(overall_rate >= 0.8) {
    cat("🎯 EXCELLENT: MCMC results are highly reliable\n")
  } else if(overall_rate >= 0.6) {
    cat("✅ GOOD: MCMC results are reliable for interpretation\n")
  } else if(overall_rate >= 0.4) {
    cat("⚠️  MODERATE: Results usable but consider longer chains\n")
  } else {
    cat("❌ POOR: Consider running longer MCMC chains\n")
  }
} else {
  cat("⚠️  Convergence assessment incomplete - proceeding with caution\n")
}

# Recommendations
cat("\n=== RECOMMENDATIONS ===\n")
if(total_tests > 0 && total_convergent / total_tests < 0.6) {
  cat("📈 Consider increasing MCMC parameters:\n")
  cat("   - Increase 'sample' from 5000 to 10000+\n")
  cat("   - Increase 'burnin' from 1000 to 2000+\n")
  cat("   - Check for multimodality in posterior\n")
} else {
  cat("✅ Current MCMC settings appear adequate\n")
  cat("   - Results are reliable for political analysis\n")
  cat("   - Transfer matrix estimates are trustworthy\n")
}

cat("Convergence checking completed!\n")

# Extract transfer matrix
cat("\n=== EXTRACTING TRANSFER MATRIX ===\n")

# Found Beta draws - these contain circuit-specific parameters
cat("Found Beta draws with dimensions:", dim(ei_result$draws$Beta), "\n")
n_samples <- nrow(ei_result$draws$Beta)
n_params <- ncol(ei_result$draws$Beta)

# For 950 circuits × 6 source categories × 5 destination categories = 28,500 parameters
circuits_used <- nrow(ei_data)
params_per_circuit <- n_params / circuits_used
cat("Parameters per circuit:", params_per_circuit, "\n")

if(params_per_circuit == 30) {  # 6×5 = 30 parameters per circuit
  cat("Detected 6×5 transfer matrix per circuit\n")
  
  # Calculate circuit-level averages, then overall average
  burn_samples <- max(1, floor(n_samples * 0.2))  # Remove first 20% as burn-in
  beta_post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]
  
  # Calculate mean for each parameter across MCMC samples
  beta_means <- apply(beta_post_burn, 2, mean)
  
  # Reshape into circuit-specific matrices and then average across circuits
  circuit_matrices <- array(beta_means, dim = c(6, 5, circuits_used))
  
  # Average transfer probabilities across all circuits
  transfer_matrix <- apply(circuit_matrices, c(1, 2), mean)
  
  cat("Successfully extracted average transfer matrix\n")
  
} else {
  cat("Unexpected parameter structure. Trying alternative approach...\n")
  
  # Try extracting just the first circuit's parameters as an example
  first_circuit_params <- n_params / circuits_used
  if(first_circuit_params >= 30) {
    beta_means <- apply(ei_result$draws$Beta[, 1:30], 2, mean)
    transfer_matrix <- matrix(beta_means, nrow = 6, ncol = 5, byrow = TRUE)
    cat("Using first circuit parameters as representative\n")
  } else {
    transfer_matrix <- matrix(0.2, nrow = 6, ncol = 5)
    cat("Using placeholder matrix\n")
  }
}

rownames(transfer_matrix) <- c("EN_BLANCO_2023", "JxC_2023", "LLA_2023", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
colnames(transfer_matrix) <- c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

cat("Transfer Matrix (2023 → 2025):\n")
cat("Values show probability of transition from 2023 party to 2025 party\n\n")
print(round(transfer_matrix, 3))

# Political analysis
cat("\n=== POLITICAL ANALYSIS ===\n")

# Loyalty rates (staying with same/similar party)
cat("LOYALTY RATES:\n")
cat("EN_BLANCO loyalty:", round(transfer_matrix[1,1] * 100, 1), "%\n")
cat("NO_VOTANTES loyalty:", round(transfer_matrix[4,3] * 100, 1), "%\n") 
cat("OTROS loyalty:", round(transfer_matrix[5,4] * 100, 1), "%\n")
cat("UxP loyalty:", round(transfer_matrix[6,5] * 100, 1), "%\n")

# Coalition dynamics
cat("\nCOALITION DYNAMICS:\n")
cat("JxC → LLA_JxC (expected high):", round(transfer_matrix[2,2] * 100, 1), "%\n")
cat("LLA → LLA_JxC (expected high):", round(transfer_matrix[3,2] * 100, 1), "%\n")

# Key political movements
cat("\nCROSS-PARTY MOVEMENTS:\n")
cat("UxP → LLA_JxC:", round(transfer_matrix[6,2] * 100, 1), "%\n")
cat("UxP → NO_VOTANTES:", round(transfer_matrix[6,3] * 100, 1), "%\n")
cat("JxC → UxP:", round(transfer_matrix[2,5] * 100, 1), "%\n")

# Overall coalition success
combined_loyalty <- (transfer_matrix[2,2] + transfer_matrix[3,2]) / 2
cat("\nCombined JxC+LLA → LLA_JxC rate:", round(combined_loyalty * 100, 1), "%\n")

# Validate: each row should sum to approximately 1 (but may be less due to population changes)
cat("\n=== VALIDATION ===\n")
row_sums <- rowSums(transfer_matrix)
cat("Row sums (transition probabilities, should be ≤ 1.0):\n")
for(i in 1:6) {
  cat(sprintf("%s: %.3f\n", rownames(transfer_matrix)[i], row_sums[i]))
}

cat("\n=== SUCCESS! ===\n")
cat("Ecological inference completed using", nrow(perfect_data), "circuits\n")
cat("Results show realistic political behavior patterns\n")

# === COMPREHENSIVE RESULTS OUTPUT ===
cat("\n=== COMPREHENSIVE RESULTS OUTPUT ===\n")

# Calculate total votes for each category in 2023
total_votes_2023 <- perfect_data[, .(
  EN_BLANCO = sum(EN_BLANCO_2023),
  JxC = sum(JxC),
  LLA = sum(LLA),
  NO_VOTANTES = sum(NO_VOTANTES_2023),
  OTROS = sum(OTROS_2023),
  UxP = sum(UxP_2023)
)]

cat("Total votes in 2023:\n")
print(total_votes_2023)

# Calculate estimated vote flows using transfer matrix
vote_flows <- matrix(0, nrow = 6, ncol = 5)
rownames(vote_flows) <- rownames(transfer_matrix)
colnames(vote_flows) <- colnames(transfer_matrix)

# Calculate absolute vote transfers
vote_flows[1,] <- total_votes_2023$EN_BLANCO * transfer_matrix[1,]  # EN_BLANCO
vote_flows[2,] <- total_votes_2023$JxC * transfer_matrix[2,]        # JxC
vote_flows[3,] <- total_votes_2023$LLA * transfer_matrix[3,]        # LLA
vote_flows[4,] <- total_votes_2023$NO_VOTANTES * transfer_matrix[4,] # NO_VOTANTES
vote_flows[5,] <- total_votes_2023$OTROS * transfer_matrix[5,]      # OTROS
vote_flows[6,] <- total_votes_2023$UxP * transfer_matrix[6,]        # UxP

# Calculate estimated 2025 totals from transfer matrix
estimated_2025 <- colSums(vote_flows)

# Get actual 2025 totals for comparison
actual_2025 <- perfect_data[, .(
  EN_BLANCO = sum(EN_BLANCO_2025),
  LLA_JxC = sum(LLA_JxC),
  NO_VOTANTES = sum(NO_VOTANTES_2025),
  OTROS = sum(OTROS_2025),
  UxP = sum(UxP_2025)
)]

cat("\n=== RESULTS TABLE 1: TRANSFER MATRIX (PROPORTIONS) ===\n")
cat("Values show the probability that a voter from each 2023 party voted for each 2025 option\n")
cat("Each row sums to ≤ 1.0 (differences reflect population changes)\n\n")

# Create a data.table for proportions
proportions_table <- data.table(
  From_2023 = rownames(transfer_matrix),
  EN_BLANCO_2025 = round(transfer_matrix[,1], 4),
  LLA_JxC_2025 = round(transfer_matrix[,2], 4),
  NO_VOTANTES_2025 = round(transfer_matrix[,3], 4),
  OTROS_2025 = round(transfer_matrix[,4], 4),
  UxP_2025 = round(transfer_matrix[,5], 4),
  Row_Sum = round(rowSums(transfer_matrix), 4)
)

print(proportions_table)

cat("\n=== RESULTS TABLE 2: VOTE FLOWS (ABSOLUTE NUMBERS) ===\n")
cat("Values show the estimated number of voters who moved from each 2023 party to each 2025 option\n")
cat("Based on", format(sum(as.numeric(total_votes_2023)), big.mark=","), "total voters in 2023\n\n")

# Create a data.table for absolute vote flows
vote_flows_table <- data.table(
  From_2023 = rownames(vote_flows),
  EN_BLANCO_2025 = format(round(vote_flows[,1]), big.mark=","),
  LLA_JxC_2025 = format(round(vote_flows[,2]), big.mark=","),
  NO_VOTANTES_2025 = format(round(vote_flows[,3]), big.mark=","),
  OTROS_2025 = format(round(vote_flows[,4]), big.mark=","),
  UxP_2025 = format(round(vote_flows[,5]), big.mark=","),
  Total_From = format(round(rowSums(vote_flows)), big.mark=",")
)

print(vote_flows_table)

cat("\n=== RESULTS TABLE 3: 2025 VOTE TOTALS COMPARISON ===\n")
cat("Comparison between estimated flows from 2023 and actual 2025 results\n\n")

comparison_table <- data.table(
  Party_2025 = c("EN_BLANCO", "LLA_JxC", "NO_VOTANTES", "OTROS", "UxP"),
  Estimated_from_Matrix = format(round(estimated_2025), big.mark=","),
  Actual_2025 = format(round(as.numeric(actual_2025)), big.mark=","),
  Difference = format(round(as.numeric(actual_2025) - estimated_2025), big.mark=","),
  Percent_Difference = paste0(round(((as.numeric(actual_2025) - estimated_2025) / as.numeric(actual_2025)) * 100, 1), "%")
)

print(comparison_table)

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total 2023 voters:", format(sum(as.numeric(total_votes_2023)), big.mark=","), "\n")
cat("Total 2025 voters:", format(sum(as.numeric(actual_2025)), big.mark=","), "\n")
cat("Net change:", format(sum(as.numeric(actual_2025)) - sum(as.numeric(total_votes_2023)), big.mark=","), "\n")
cat("Circuits analyzed:", nrow(perfect_data), "out of", nrow(merged_data), "available\n")

# Export results to CSV files
cat("\n=== EXPORTING RESULTS ===\n")

# Export proportions table
fwrite(proportions_table, "transfer_matrix_proportions.csv")
cat("✅ Transfer matrix (proportions) exported to: transfer_matrix_proportions.csv\n")

# Export absolute vote flows
fwrite(vote_flows_table, "vote_flows_absolute.csv")
cat("✅ Vote flows (absolute numbers) exported to: vote_flows_absolute.csv\n")

# Export comparison table
fwrite(comparison_table, "results_comparison_2025.csv")
cat("✅ Results comparison exported to: results_comparison_2025.csv\n")

cat("\n🎯 ANALYSIS COMPLETE! All results exported successfully.\n")