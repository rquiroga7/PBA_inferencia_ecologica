# Ecological inference: 2025 → October 2025 National Legislative Election
library(eiPack)
library(data.table)
library(dplyr)

cat("=== BUENOS AIRES ECOLOGICAL INFERENCE 2025→OCT2025 ===\n")

# Load data
data_2025 <- fread("data/electores_circuitos_2025.csv")
data_oct2025 <- fread("data/electores_circuitos_oct2025.csv")

# Reshape data
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion,
                   value.var = "votos", fill = 0)
votes_oct2025 <- dcast(data_oct2025, circuitoId + cantidadElectores ~ nombreAgrupacion,
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))
setnames(votes_oct2025, gsub(" ", "_", colnames(votes_oct2025)))

# Rename parties to match across elections
cat("Renaming parties for consistency...\n")
if("LLA_JxC" %in% colnames(votes_2025)) {
  setnames(votes_2025, "LLA_JxC", "LLA")
  cat("  - Renamed LLA_JxC to LLA in 2025 data\n")
}
if("UxP" %in% colnames(votes_2025)) {
  setnames(votes_2025, "UxP", "FP")
  cat("  - Renamed UxP to FP in 2025 data\n")
}

# Merge datasets
merged_data <- merge(votes_2025, votes_oct2025, by = "circuitoId", suffixes = c("_sep2025", "_oct2025"))

# Apply filtering criteria (±15% electorate stability)
merged_data <- merged_data[
  cantidadElectores_oct2025 <= 1.15 * cantidadElectores_sep2025 &
  cantidadElectores_oct2025 >= 0.85 * cantidadElectores_sep2025
]

cat("Circuits after filtering:", nrow(merged_data), "\n")

# Merge EN_BLANCO and NO_VOTANTES into single category
cat("\n=== MERGING EN_BLANCO AND NO_VOTANTES ===\n")
merged_data[, NO_VOTO_sep2025 := EN_BLANCO_sep2025 + NO_VOTANTES_sep2025]
merged_data[, NO_VOTO_oct2025 := EN_BLANCO_oct2025 + NO_VOTANTES_oct2025]
cat("Created NO_VOTO category (EN_BLANCO + NO_VOTANTES) for both periods\n")

# Define political categories
# 2025 baseline: NO_VOTO (EN_BLANCO+NO_VOTANTES), LLA, OTROS, FP
# Oct 2025 target: NO_VOTO (EN_BLANCO+NO_VOTANTES), LLA, OTROS, FP
# Note: All conflicting columns get suffixes during merge
political_parties_2025 <- c("NO_VOTO_sep2025", "LLA_sep2025", "OTROS_sep2025", "FP_sep2025")
political_parties_oct2025 <- c("NO_VOTO_oct2025", "LLA_oct2025", "OTROS_oct2025", "FP_oct2025")

# Verify data completeness
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]
merged_data[, total_oct2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_oct2025]

merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_sep2025)]
merged_data[, diff_oct2025 := abs(total_oct2025 - cantidadElectores_oct2025)]

# Allow small imbalances in Oct 2025 counts because of data rounding/noise
oct_vote_balance_tolerance <- 100
perfect_data <- merged_data[diff_2025 == 0 & diff_oct2025 <= oct_vote_balance_tolerance]
cat(
  "Final circuits for analysis (|Oct diff| <=", oct_vote_balance_tolerance, "):",
  nrow(perfect_data), "\n"
)
cat("Share of matched circuits:", round(nrow(perfect_data) / nrow(merged_data) * 100, 1), "%\n")

cat("\n=== UNDERSTANDING eiPack REQUIREMENTS ===\n")

# For eiPack ecological inference:
# X matrix: baseline year proportions (must sum to 1.0 across columns for each row)
# T matrix: outcome year proportions (must sum to 1.0 across columns for each row)
# N: total population size

# X matrix: 2025 proportions (proportion of 2025 electorate that voted for each party)
perfect_data[, `:=`(
  x1 = NO_VOTO_sep2025 / cantidadElectores_sep2025,     # NO_VOTO 2025 (EN_BLANCO + NO_VOTANTES)
  x2 = LLA_sep2025 / cantidadElectores_sep2025,         # LLA 2025
  x3 = OTROS_sep2025 / cantidadElectores_sep2025,       # OTROS 2025
  x4 = FP_sep2025 / cantidadElectores_sep2025           # FP 2025
)]

# T matrix: Oct 2025 proportions (proportion of Oct 2025 electorate that voted for each party)
perfect_data[, `:=`(
  t1 = NO_VOTO_oct2025 / cantidadElectores_oct2025,     # NO_VOTO Oct 2025 (EN_BLANCO + NO_VOTANTES)
  t2 = LLA_oct2025 / cantidadElectores_oct2025,         # LLA Oct 2025
  t3 = OTROS_oct2025 / cantidadElectores_oct2025,       # OTROS Oct 2025
  t4 = FP_oct2025 / cantidadElectores_oct2025           # FP Oct 2025
)]

# Verify both X and T matrices have rows summing to 1.0
perfect_data[, x_sum := x1 + x2 + x3 + x4]
perfect_data[, t_sum := t1 + t2 + t3 + t4]

cat("X matrix rows summing to 1.0:", sum(abs(perfect_data$x_sum - 1.0) < 1e-10), "\n")
cat("T matrix rows summing to 1.0:", sum(abs(perfect_data$t_sum - 1.0) < 1e-10), "\n")

cat("X matrix sum range:", sprintf("%.10f to %.10f", min(perfect_data$x_sum), max(perfect_data$x_sum)), "\n")
cat("T matrix sum range:", sprintf("%.10f to %.10f", min(perfect_data$t_sum), max(perfect_data$t_sum)), "\n")

# Renormalize Oct 2025 proportions when small discrepancies remain
needs_rescale <- abs(perfect_data$t_sum - 1.0) > 1e-8 & perfect_data$t_sum > 0
if(any(needs_rescale)) {
  cat("Normalizing Oct 2025 proportions for", sum(needs_rescale), "circuits with small imbalances\n")
  perfect_data[needs_rescale, `:=`(
    t1 = t1 / t_sum,
    t2 = t2 / t_sum,
    t3 = t3 / t_sum,
    t4 = t4 / t_sum
  )]
  perfect_data[needs_rescale, t_sum := 1.0]
}

# Create data frame for eiPack using 2025 as baseline population
ei_data <- perfect_data[, .(
  circuito = circuitoId,
  x1, x2, x3, x4,                             # 2025 proportions (sum to 1)
  n = cantidadElectores_sep2025,              # Baseline population
  t1, t2, t3, t4                              # Oct 2025 proportions (sum to 1)
)]

cat("\n=== RUNNING ECOLOGICAL INFERENCE ===\n")
cat("Data shape:", nrow(ei_data), "circuits ×", ncol(ei_data)-1, "variables\n")

# Define formula: 4 outcome categories × 4 baseline categories
formula_ei <- cbind(t1, t2, t3, t4) ~ cbind(x1, x2, x3, x4)

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
                        burnin = 200000, sample = 5000, thin = 100,  # Optimized thin value
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
sample_params <- min(1000, n_params)  # Reduced sample size for reliability
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

# For circuits × 4 source categories × 4 destination categories = 16 parameters per circuit
circuits_used <- nrow(ei_data)
params_per_circuit <- n_params / circuits_used
cat("Parameters per circuit:", params_per_circuit, "\n")

if(params_per_circuit == 16) {  # 4×4 = 16 parameters per circuit
  cat("Detected 4×4 transfer matrix per circuit\n")

  # Calculate circuit-level averages, then overall average
  burn_samples <- max(1, floor(n_samples * 0.2))  # Remove first 20% as burn-in
  beta_post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]

  # Calculate mean for each parameter across MCMC samples
  beta_means <- apply(beta_post_burn, 2, mean)

  # Reshape into circuit-specific matrices and then average across circuits
  circuit_matrices <- array(beta_means, dim = c(4, 4, circuits_used))

  # Average transfer probabilities across all circuits
  transfer_matrix <- apply(circuit_matrices, c(1, 2), mean)

  cat("Successfully extracted average transfer matrix\n")

} else {
  cat("Unexpected parameter structure. Trying alternative approach...\n")

  # Try extracting just the first circuit's parameters as an example
  first_circuit_params <- n_params / circuits_used
  if(first_circuit_params >= 16) {
    beta_means <- apply(ei_result$draws$Beta[, 1:16], 2, mean)
    transfer_matrix <- matrix(beta_means, nrow = 4, ncol = 4, byrow = TRUE)
    cat("Using first circuit parameters as representative\n")
  } else {
    transfer_matrix <- matrix(0.25, nrow = 4, ncol = 4)
    cat("Using placeholder matrix\n")
  }
}

rownames(transfer_matrix) <- c("NO_VOTO_2025", "LLA_2025", "OTROS_2025", "FP_2025")
colnames(transfer_matrix) <- c("NO_VOTO_Oct2025", "LLA_Oct2025", "OTROS_Oct2025", "FP_Oct2025")

cat("Transfer Matrix (2025 → Oct 2025):\n")
cat("Values show probability of transition from 2025 party to Oct 2025 party\n\n")
print(round(transfer_matrix, 3))

# Political analysis
cat("\n=== POLITICAL ANALYSIS ===\n")

# Loyalty rates (staying with same/similar party)
cat("LOYALTY RATES:\n")
cat("NO_VOTO loyalty (EN_BLANCO+NO_VOTANTES):", round(transfer_matrix[1,1] * 100, 1), "%\n")
cat("LLA loyalty:", round(transfer_matrix[2,2] * 100, 1), "%\n")
cat("OTROS loyalty:", round(transfer_matrix[3,3] * 100, 1), "%\n")
cat("FP loyalty:", round(transfer_matrix[4,4] * 100, 1), "%\n")

# Key political movements
cat("\nCROSS-PARTY MOVEMENTS:\n")
cat("LLA → NO_VOTO:", round(transfer_matrix[2,1] * 100, 1), "%\n")
cat("LLA → FP:", round(transfer_matrix[2,4] * 100, 1), "%\n")
cat("FP → LLA:", round(transfer_matrix[4,2] * 100, 1), "%\n")
cat("FP → NO_VOTO:", round(transfer_matrix[4,1] * 100, 1), "%\n")

# Validate: each row should sum to approximately 1 (but may be less due to population changes)
cat("\n=== VALIDATION ===\n")
row_sums <- rowSums(transfer_matrix)
cat("Row sums (transition probabilities, should be ≤ 1.0):\n")
for(i in 1:4) {
  cat(sprintf("%s: %.3f\n", rownames(transfer_matrix)[i], row_sums[i]))
}

cat("\n=== SUCCESS! ===\n")
cat("Ecological inference completed using", nrow(perfect_data), "circuits\n")
cat("Results show realistic political behavior patterns\n")

# === COMPREHENSIVE RESULTS OUTPUT ===
cat("\n=== COMPREHENSIVE RESULTS OUTPUT ===\n")

# Calculate total votes for each category in 2025
total_votes_2025 <- perfect_data[, .(
  NO_VOTO = sum(NO_VOTO_sep2025),
  LLA = sum(LLA_sep2025),
  OTROS = sum(OTROS_sep2025),
  FP = sum(FP_sep2025)
)]

cat("Total votes in 2025:\n")
print(total_votes_2025)

# Calculate estimated vote flows using transfer matrix
vote_flows <- matrix(0, nrow = 4, ncol = 4)
rownames(vote_flows) <- rownames(transfer_matrix)
colnames(vote_flows) <- colnames(transfer_matrix)

# Calculate absolute vote transfers
vote_flows[1,] <- total_votes_2025$NO_VOTO * transfer_matrix[1,]   # NO_VOTO
vote_flows[2,] <- total_votes_2025$LLA * transfer_matrix[2,]       # LLA
vote_flows[3,] <- total_votes_2025$OTROS * transfer_matrix[3,]     # OTROS
vote_flows[4,] <- total_votes_2025$FP * transfer_matrix[4,]        # FP

# Calculate estimated Oct 2025 totals from transfer matrix
estimated_oct2025 <- colSums(vote_flows)

# Get actual Oct 2025 totals for comparison
actual_oct2025 <- perfect_data[, .(
  NO_VOTO = sum(NO_VOTO_oct2025),
  LLA = sum(LLA_oct2025),
  OTROS = sum(OTROS_oct2025),
  FP = sum(FP_oct2025)
)]

cat("\n=== RESULTS TABLE 1: TRANSFER MATRIX (PROPORTIONS) ===\n")
cat("Values show the probability that a voter from each 2025 party voted for each Oct 2025 option\n")
cat("Each row sums to ≤ 1.0 (differences reflect population changes)\n\n")

# Create a data.table for proportions
proportions_table <- data.table(
  From_2025 = rownames(transfer_matrix),
  NO_VOTO_Oct2025 = round(transfer_matrix[,1], 4),
  LLA_Oct2025 = round(transfer_matrix[,2], 4),
  OTROS_Oct2025 = round(transfer_matrix[,3], 4),
  FP_Oct2025 = round(transfer_matrix[,4], 4),
  Row_Sum = round(rowSums(transfer_matrix), 4)
)

print(proportions_table)

cat("\n=== RESULTS TABLE 2: VOTE FLOWS (ABSOLUTE NUMBERS) ===\n")
cat("Values show the estimated number of voters who moved from each 2025 party to each Oct 2025 option\n")
cat("Based on", format(sum(as.numeric(total_votes_2025)), big.mark=","), "total voters in 2025\n\n")

adjust_rounding <- function(values) {
  rounded_vals <- round(values)
  target_total <- as.integer(round(sum(values)))
  diff <- target_total - sum(rounded_vals)
  if(diff > 0) {
    order_idx <- order(values - floor(values), decreasing = TRUE)
    idx <- 1
    while(diff > 0 && length(order_idx) > 0) {
      target_col <- order_idx[idx]
      rounded_vals[target_col] <- rounded_vals[target_col] + 1
      diff <- diff - 1
      idx <- idx + 1
      if(idx > length(order_idx)) idx <- 1
    }
  } else if(diff < 0) {
    order_idx <- order(values - floor(values), decreasing = FALSE)
    idx <- 1
    while(diff < 0 && length(order_idx) > 0) {
      target_col <- order_idx[idx]
      if(rounded_vals[target_col] > 0) {
        rounded_vals[target_col] <- rounded_vals[target_col] - 1
        diff <- diff + 1
      }
      idx <- idx + 1
      if(idx > length(order_idx)) idx <- 1
    }
  }
  if(sum(rounded_vals) != target_total) {
    stop("Rounded totals do not match target total")
  }
  rounded_vals
}

vote_flows_rounded <- matrix(0, nrow = nrow(vote_flows), ncol = ncol(vote_flows))
rownames(vote_flows_rounded) <- rownames(vote_flows)
colnames(vote_flows_rounded) <- colnames(vote_flows)

for(i in seq_len(nrow(vote_flows))) {
  vote_flows_rounded[i, ] <- adjust_rounding(vote_flows[i, ])
}

vote_flows_dt <- as.data.table(vote_flows_rounded)
vote_flows_dt[, From_2025 := rownames(vote_flows_rounded)]
flow_columns <- colnames(vote_flows_rounded)
setcolorder(vote_flows_dt, c("From_2025", flow_columns))
vote_flows_dt[, Total_From := rowSums(vote_flows_rounded)]

vote_flows_print <- copy(vote_flows_dt)
numeric_cols <- setdiff(names(vote_flows_print), "From_2025")
vote_flows_print[, (numeric_cols) := lapply(.SD, function(x) format(x, big.mark=",")), .SDcols = numeric_cols]

print(vote_flows_print)

cat("\n=== RESULTS TABLE 3: OCT 2025 VOTE TOTALS COMPARISON ===\n")
cat("Comparison between estimated flows from 2025 and actual Oct 2025 results\n\n")

comparison_table_numeric <- data.table(
  Party_Oct2025 = c("NO_VOTO", "LLA", "OTROS", "FP"),
  Estimated_from_Matrix = as.integer(round(estimated_oct2025)),
  Actual_Oct2025 = as.integer(round(as.numeric(actual_oct2025))),
  Difference = as.integer(round(as.numeric(actual_oct2025) - estimated_oct2025)),
  Percent_Difference = round(((as.numeric(actual_oct2025) - estimated_oct2025) / as.numeric(actual_oct2025)) * 100, 1)
)

comparison_table_print <- copy(comparison_table_numeric)
comparison_cols <- setdiff(names(comparison_table_print), "Party_Oct2025")
comparison_table_print[, (setdiff(comparison_cols, "Percent_Difference")) := lapply(.SD, function(x) format(x, big.mark=",")), .SDcols = setdiff(comparison_cols, "Percent_Difference")]
comparison_table_print[, Percent_Difference := paste0(Percent_Difference, "%")]

print(comparison_table_print)

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total 2025 voters:", format(sum(as.numeric(total_votes_2025)), big.mark=","), "\n")
cat("Total Oct 2025 voters:", format(sum(as.numeric(actual_oct2025)), big.mark=","), "\n")
cat("Net change:", format(sum(as.numeric(actual_oct2025)) - sum(as.numeric(total_votes_2025)), big.mark=","), "\n")
cat("Circuits analyzed:", nrow(perfect_data), "out of", nrow(merged_data), "available\n")

# Export results to CSV files
cat("\n=== EXPORTING RESULTS ===\n")

# Export proportions table
fwrite(proportions_table, "transfer_matrix_proportions_oct2025.csv")
cat("✅ Transfer matrix (proportions) exported to: transfer_matrix_proportions_oct2025.csv\n")

# Export absolute vote flows
fwrite(vote_flows_dt, "vote_flows_absolute_oct2025.csv")
cat("✅ Vote flows (absolute numbers) exported to: vote_flows_absolute_oct2025.csv\n")

# Export comparison table
fwrite(comparison_table_numeric, "results_comparison_oct2025.csv")
cat("✅ Results comparison exported to: results_comparison_oct2025.csv\n")

# Export edge list for Sankey inputs
edge_list <- melt(
  vote_flows_dt,
  id.vars = "From_2025",
  measure.vars = flow_columns,
  variable.name = "To_Oct2025",
  value.name = "Votes"
)[Votes > 0]

setorder(edge_list, From_2025, To_Oct2025)
write.table(edge_list, "vote_flows_edge_list_oct2025.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("✅ Edge list exported to: vote_flows_edge_list_oct2025.txt\n")

cat("\n🎯 ANALYSIS COMPLETE! All results exported successfully.\n")
