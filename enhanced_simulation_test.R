# Enhanced Ecological Inference Simulation with Noise Analysis
# This script measures real-world noise patterns and tests multiple scenarios

library(eiPack)
library(data.table)
library(dplyr)
library(parallel)  # For parallel processing

cat("=== ENHANCED ECOLOGICAL INFERENCE SIMULATION ===\n")

# Set reproducible seed
set.seed(42)
cat("Using random seed: 42 for reproducibility\n")

# Load real data to measure noise patterns
cat("\n=== ANALYZING REAL DATA NOISE PATTERNS ===\n")

data_2023 <- fread("data/electores_circuitos_2023.csv")
data_2025 <- fread("data/electores_circuitos_2025.csv")

# Reshape data
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names and remove VOTANTES
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))
votes_2023[, VOTANTES := NULL]

# Debug: show actual column names
cat("2023 columns:", paste(colnames(votes_2023), collapse = ", "), "\n")
cat("2025 columns:", paste(colnames(votes_2025), collapse = ", "), "\n")

# Merge real data to analyze electorate changes (noise patterns)
real_merged <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

# Calculate per-circuit electorate change (this represents real-world noise)
real_merged[, electorate_change := cantidadElectores_2025 / cantidadElectores_2023]
real_merged[, abs_change := abs(cantidadElectores_2025 - cantidadElectores_2023)]

# Measure electorate noise statistics
electorate_noise_stats <- list(
  mean_ratio = mean(real_merged$electorate_change, na.rm = TRUE),
  sd_ratio = sd(real_merged$electorate_change, na.rm = TRUE),
  mean_abs_change = mean(real_merged$abs_change, na.rm = TRUE),
  sd_abs_change = sd(real_merged$abs_change, na.rm = TRUE),
  min_ratio = min(real_merged$electorate_change, na.rm = TRUE),
  max_ratio = max(real_merged$electorate_change, na.rm = TRUE)
)

cat("Real electorate change patterns:\n")
cat("- Mean ratio (2025/2023):", round(electorate_noise_stats$mean_ratio, 4), "\n")
cat("- SD of ratio:", round(electorate_noise_stats$sd_ratio, 4), "\n")
cat("- Mean absolute change:", round(electorate_noise_stats$mean_abs_change, 1), "voters\n")
cat("- SD of absolute change:", round(electorate_noise_stats$sd_abs_change, 1), "voters\n")
cat("- Ratio range:", round(electorate_noise_stats$min_ratio, 3), "to", round(electorate_noise_stats$max_ratio, 3), "\n")

# Analyze noise patterns for each party separately
cat("\n=== ANALYZING PARTY-SPECIFIC NOISE PATTERNS ===\n")

# Calculate vote proportion changes for each party
party_noise_stats <- list()

# EN_BLANCO remains the same
if("EN_BLANCO_2023" %in% colnames(real_merged) && "EN_BLANCO_2025" %in% colnames(real_merged)) {
  real_merged[, EN_BLANCO_2023_prop := EN_BLANCO_2023 / cantidadElectores_2023]
  real_merged[, EN_BLANCO_2025_prop := EN_BLANCO_2025 / cantidadElectores_2025]
  real_merged[, EN_BLANCO_change := EN_BLANCO_2025_prop - EN_BLANCO_2023_prop]
  
  party_noise_stats$EN_BLANCO <- list(
    mean_change = mean(real_merged$EN_BLANCO_change, na.rm = TRUE),
    sd_change = sd(real_merged$EN_BLANCO_change, na.rm = TRUE),
    mean_2023_prop = mean(real_merged$EN_BLANCO_2023_prop, na.rm = TRUE),
    mean_2025_prop = mean(real_merged$EN_BLANCO_2025_prop, na.rm = TRUE)
  )
}

# LLA + JxC combined (LLA_JxC_2025)
if("LLA_2023" %in% colnames(real_merged) && "JxC_2023" %in% colnames(real_merged) && "LLA_JxC_2025" %in% colnames(real_merged)) {
  real_merged[, LLA_JxC_2023_prop := (LLA_2023 + JxC_2023) / cantidadElectores_2023]
  real_merged[, LLA_JxC_2025_prop := LLA_JxC_2025 / cantidadElectores_2025]
  real_merged[, LLA_JxC_change := LLA_JxC_2025_prop - LLA_JxC_2023_prop]
  
  party_noise_stats$LLA_JxC <- list(
    mean_change = mean(real_merged$LLA_JxC_change, na.rm = TRUE),
    sd_change = sd(real_merged$LLA_JxC_change, na.rm = TRUE),
    mean_2023_prop = mean(real_merged$LLA_JxC_2023_prop, na.rm = TRUE),
    mean_2025_prop = mean(real_merged$LLA_JxC_2025_prop, na.rm = TRUE)
  )
}

# NO_VOTANTES remains the same
if("NO_VOTANTES_2023" %in% colnames(real_merged) && "NO_VOTANTES_2025" %in% colnames(real_merged)) {
  real_merged[, NO_VOTANTES_2023_prop := NO_VOTANTES_2023 / cantidadElectores_2023]
  real_merged[, NO_VOTANTES_2025_prop := NO_VOTANTES_2025 / cantidadElectores_2025]
  real_merged[, NO_VOTANTES_change := NO_VOTANTES_2025_prop - NO_VOTANTES_2023_prop]
  
  party_noise_stats$NO_VOTANTES <- list(
    mean_change = mean(real_merged$NO_VOTANTES_change, na.rm = TRUE),
    sd_change = sd(real_merged$NO_VOTANTES_change, na.rm = TRUE),
    mean_2023_prop = mean(real_merged$NO_VOTANTES_2023_prop, na.rm = TRUE),
    mean_2025_prop = mean(real_merged$NO_VOTANTES_2025_prop, na.rm = TRUE)
  )
}

# OTROS remains the same
if("OTROS_2023" %in% colnames(real_merged) && "OTROS_2025" %in% colnames(real_merged)) {
  real_merged[, OTROS_2023_prop := OTROS_2023 / cantidadElectores_2023]
  real_merged[, OTROS_2025_prop := OTROS_2025 / cantidadElectores_2025]
  real_merged[, OTROS_change := OTROS_2025_prop - OTROS_2023_prop]
  
  party_noise_stats$OTROS <- list(
    mean_change = mean(real_merged$OTROS_change, na.rm = TRUE),
    sd_change = sd(real_merged$OTROS_change, na.rm = TRUE),
    mean_2023_prop = mean(real_merged$OTROS_2023_prop, na.rm = TRUE),
    mean_2025_prop = mean(real_merged$OTROS_2025_prop, na.rm = TRUE)
  )
}

# UxP remains the same
if("UxP_2023" %in% colnames(real_merged) && "UxP_2025" %in% colnames(real_merged)) {
  real_merged[, UxP_2023_prop := UxP_2023 / cantidadElectores_2023]
  real_merged[, UxP_2025_prop := UxP_2025 / cantidadElectores_2025]
  real_merged[, UxP_change := UxP_2025_prop - UxP_2023_prop]
  
  party_noise_stats$UxP <- list(
    mean_change = mean(real_merged$UxP_change, na.rm = TRUE),
    sd_change = sd(real_merged$UxP_change, na.rm = TRUE),
    mean_2023_prop = mean(real_merged$UxP_2023_prop, na.rm = TRUE),
    mean_2025_prop = mean(real_merged$UxP_2025_prop, na.rm = TRUE)
  )
}

# Display party-specific noise statistics
for(party in names(party_noise_stats)) {
  stats <- party_noise_stats[[party]]
  cat(paste0(party, " noise patterns:\n"))
  cat("  - Mean proportion change:", round(stats$mean_change, 4), "\n")
  cat("  - SD of proportion change:", round(stats$sd_change, 4), "\n")
  cat("  - 2023 avg proportion:", round(stats$mean_2023_prop, 4), "\n")
  cat("  - 2025 avg proportion:", round(stats$mean_2025_prop, 4), "\n")
  cat("  - Relative volatility:", round(stats$sd_change / stats$mean_2023_prop, 2), "\n\n")
}

# Define four different ground truth scenarios
cat("\n=== DEFINING FOUR GROUND TRUTH SCENARIOS ===\n")

# Scenario 1: Strong party loyalty with coalition success
true_transfer_matrix1 <- matrix(c(
  # From EN_BLANCO_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.70, 0.05, 0.20, 0.04, 0.01,
  # From JxC_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP  
  0.03, 0.75, 0.10, 0.07, 0.05,
  # From LLA_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.02, 0.80, 0.12, 0.04, 0.02,
  # From NO_VOTANTES_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.03, 0.85, 0.05, 0.02,
  # From OTROS_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.10, 0.25, 0.15, 0.40, 0.10,
  # From UxP_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.02, 0.00, 0.16, 0.02, 0.80
), nrow = 6, ncol = 5, byrow = TRUE)

# Scenario 2: Strong party loyalty except for LLA
true_transfer_matrix2 <- matrix(c(
  # From EN_BLANCO_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.70, 0.05, 0.20, 0.04, 0.01,
  # From JxC_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP  
  0.03, 0.75, 0.10, 0.07, 0.05,
  # From LLA_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.07, 0.15, 0.65, 0.10, 0.03,
  # From NO_VOTANTES_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.03, 0.85, 0.05, 0.02,
  # From OTROS_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.05, 0.10, 0.70, 0.10,
  # From UxP_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.01, 0.00, 0.11, 0.01, 0.87
), nrow = 6, ncol = 5, byrow = TRUE)

# Scenario 3: Strong party loyalty except for JxC
true_transfer_matrix3 <- matrix(c(
  # From EN_BLANCO_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.70, 0.05, 0.20, 0.04, 0.01,
  # From JxC_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.07, 0.15, 0.65, 0.10, 0.03,  
  # From LLA_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.03, 0.75, 0.10, 0.07, 0.05,
  # From NO_VOTANTES_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.03, 0.85, 0.05, 0.02,
  # From OTROS_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.05, 0.10, 0.70, 0.10,
  # From UxP_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.01, 0.00, 0.08, 0.01, 0.90
), nrow = 6, ncol = 5, byrow = TRUE)

# Scenario 4: Low loyalty scenario
true_transfer_matrix4 <- matrix(c(
  # From EN_BLANCO_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.70, 0.00, 0.20, 0.10, 0.00,
  # From JxC_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.20, 0.30, 0.30, 0.20, 0.00,  
  # From LLA_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.10, 0.40, 0.40, 0.00, 0.10,
  # From NO_VOTANTES_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.00, 0.00, 0.90, 0.05, 0.05,
  # From OTROS_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.05, 0.20, 0.60, 0.10,
  # From UxP_2023 to: EN_BLANCO, LLA_JxC, NO_VOTANTES, OTROS, UxP
  0.05, 0.00, 0.20, 0.05, 0.70
), nrow = 6, ncol = 5, byrow = TRUE)

# Add row/column names to all matrices
matrix_names <- list(
  rows = c("EN_BLANCO_2023", "JxC_2023", "LLA_2023", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023"),
  cols = c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")
)

rownames(true_transfer_matrix1) <- matrix_names$rows
colnames(true_transfer_matrix1) <- matrix_names$cols
rownames(true_transfer_matrix2) <- matrix_names$rows  
colnames(true_transfer_matrix2) <- matrix_names$cols
rownames(true_transfer_matrix3) <- matrix_names$rows
colnames(true_transfer_matrix3) <- matrix_names$cols
rownames(true_transfer_matrix4) <- matrix_names$rows
colnames(true_transfer_matrix4) <- matrix_names$cols

# Display scenarios
scenarios <- list(
  "Strong party loyalty with coalition success" = true_transfer_matrix1,
  "Strong party loyalty except for LLA" = true_transfer_matrix2, 
  "Strong party loyalty except for JxC" = true_transfer_matrix3,
  "Low loyalty scenario" = true_transfer_matrix4
)

for(scenario_name in names(scenarios)) {
  cat("\n", scenario_name, "Scenario:\n")
  print(round(scenarios[[scenario_name]], 3))
  
  # Verify normalization
  row_sums <- rowSums(scenarios[[scenario_name]])
  if(all(abs(row_sums - 1.0) < 1e-10)) {
    cat("✅ Properly normalized\n")
  } else {
    cat("❌ Not normalized!\n")
    stop(paste("Fix", scenario_name, "matrix"))
  }
}

# Enhanced simulation function with party-specific noise patterns
simulate_with_noise <- function(votes_2023_vec, transfer_matrix, circuit_id, electorate_noise_stats, party_noise_stats, scenario_seed) {
  # Set seed based on circuit_id and scenario for reproducible noise
  # Convert circuit_id to numeric if it's character
  numeric_circuit_id <- as.numeric(gsub("[^0-9]", "", circuit_id))
  if(is.na(numeric_circuit_id)) numeric_circuit_id <- 1
  
  set.seed(scenario_seed + numeric_circuit_id)
  
  # Apply electorate size noise based on real patterns
  base_electorate <- sum(votes_2023_vec)
  
  # Sample noise ratio from normal distribution based on real data
  noise_ratio <- rnorm(1, mean = electorate_noise_stats$mean_ratio, sd = electorate_noise_stats$sd_ratio)
  noise_ratio <- pmax(0.7, pmin(1.3, noise_ratio))  # Constrain to reasonable bounds
  
  new_electorate <- round(base_electorate * noise_ratio)
  
  party_names_2025 <- c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")
  votes_2025 <- rep(0, 5)
  names(votes_2025) <- party_names_2025
  
  # Calculate base proportions for 2023
  base_props_2023 <- votes_2023_vec / base_electorate
  
  # Simulate transitions with transfer matrix
  for(i in 1:6) {
    n_voters <- votes_2023_vec[i]
    if(n_voters > 0) {
      transitions <- sample(1:5, size = n_voters, replace = TRUE, 
                           prob = transfer_matrix[i, ])
      for(j in 1:5) {
        votes_2025[j] <- votes_2025[j] + sum(transitions == j)
      }
    }
  }
  
  # Apply party-specific noise based on real volatility patterns
  # Get party names for 2025 results
  party_mapping <- list(
    "EN_BLANCO_2025" = "EN_BLANCO",
    "LLA_JxC" = "LLA_JxC", 
    "NO_VOTANTES_2025" = "NO_VOTANTES",
    "OTROS_2025" = "OTROS",
    "UxP_2025" = "UxP"
  )
  
  # Apply realistic noise to each party based on observed patterns
  for(j in 1:5) {
    party_key <- party_mapping[[party_names_2025[j]]]
    if(!is.null(party_key) && party_key %in% names(party_noise_stats)) {
      current_prop <- votes_2025[j] / sum(votes_2025)
      
      # Add party-specific proportional noise
      noise_sd <- party_noise_stats[[party_key]]$sd_change
      prop_noise <- rnorm(1, mean = 0, sd = noise_sd)
      
      # Apply noise while keeping proportions reasonable
      new_prop <- pmax(0, pmin(1, current_prop + prop_noise))
      target_votes <- round(new_prop * new_electorate)
      
      # Smooth transition to avoid dramatic changes
      votes_2025[j] <- round(0.7 * votes_2025[j] + 0.3 * target_votes)
    }
  }
  
  # Final adjustment: ensure total matches target electorate
  current_total <- sum(votes_2025)
  if(current_total > 0) {
    votes_2025 <- round(votes_2025 * new_electorate / current_total)
  }
  
  return(list(votes = votes_2025, electorate = new_electorate))
}

# Function to find optimal MCMC parameters using the first scenario
find_optimal_params <- function(ei_data, formula_ei, lambda1, lambda2) {
  cat("Finding optimal MCMC parameters using first scenario...\n")
  
  # Define parameter grid as requested
  burnin_values <- c(100000)
  sample_values <- c(2000)
  thin_values <- c(100)
  
  # Create all parameter combinations
  param_grid <- expand.grid(
    burnin = burnin_values,
    sample = sample_values, 
    thin = thin_values,
    stringsAsFactors = FALSE
  )
  
  # Filter invalid combinations (keeping only reasonable ones)
  #param_grid <- param_grid[param_grid$sample >= param_grid$burnin/10, ]
  
  # Skip parallel optimization if only one parameter combination
  if(nrow(param_grid) == 1) {
    cat("Only one parameter combination - skipping parallel optimization\n")
    
    best_params <- list(
      burnin = param_grid$burnin[1],
      sample = param_grid$sample[1],
      thin = param_grid$thin[1]
    )
    
    results_summary <- data.table(
      burnin = best_params$burnin,
      sample = best_params$sample,
      thin = best_params$thin,
      avg_eff_size = 999,  # Dummy value
      convergence_score = 999,
      runtime_seconds = 0,
      success = TRUE
    )
    
    cat(sprintf("Using single parameter set: burnin=%d, sample=%d, thin=%d\n", 
               best_params$burnin, best_params$sample, best_params$thin))
    cat("tuneMD optimization will be done separately for each scenario in parallel\n")
    
    return(list(
      optimal_params = best_params,
      best_result = NULL,  # No need to run inference here
      param_summary = results_summary,
      tune_result = NULL   # Each scenario will do its own tuning
    ))
  }
  
  # Multiple parameter combinations - run parallel optimization
  cat(sprintf("Testing %d valid parameter combinations in parallel...\n", nrow(param_grid)))
  
  # Run tuning once with comprehensive settings for all parameter testing
  cat("Performing comprehensive MCMC tuning (this may take a moment)...\n")
  tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                       totaldraws = 1000, ntunes = 5,  # Comprehensive tuning
                       lambda1 = lambda1, lambda2 = lambda2)
  cat("Tuning completed. Using same tuning for all parameter combinations.\n")
  
  # Set up parallel cluster (use available cores - 1)
  n_cores <- max(1, detectCores() - 1)
  cat(sprintf("Using %d cores for parallel processing\n", n_cores))
  cl <- makeCluster(n_cores)
  
  # Export ALL necessary variables to cluster workers
  clusterExport(cl, c("formula_ei", "lambda1", "lambda2", "tune_result", "ei_data"), envir = environment())
  clusterEvalQ(cl, {
    library(eiPack)
    library(coda)
    library(data.table)
  })
  
  # Define function to test single parameter combination 
  test_single_params <- function(params_row) {
    burnin <- params_row$burnin
    sample <- params_row$sample
    thin <- params_row$thin
    
    start_time <- Sys.time()
    
    tryCatch({
      # Run inference with pre-computed tuning
      ei_result <- ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
                              lambda1 = lambda1, lambda2 = lambda2,
                              burnin = burnin, sample = sample, thin = thin,
                              tune.list = tune_result)
      
      # Quick convergence assessment
      n_samples <- nrow(ei_result$draws$Beta)
      burn_samples <- max(1, floor(n_samples * 0.2))
      post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]
      
      # Test subset of parameters for speed
      if(ncol(post_burn) >= 30) {
        sample_indices <- seq(1, min(ncol(post_burn), 100), length.out = 30)
        sample_params <- post_burn[, sample_indices]
        eff_sizes <- effectiveSize(mcmc(sample_params))
        avg_eff_size <- mean(eff_sizes, na.rm = TRUE)
        
        # Calculate convergence score (focus on accuracy, not speed)
        runtime_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        convergence_score <- avg_eff_size  # Higher ESS = better convergence
        
        return(list(
          burnin = burnin,
          sample = sample,
          thin = thin,
          avg_eff_size = avg_eff_size,
          convergence_score = convergence_score,
          runtime_seconds = runtime_seconds,
          success = TRUE
        ))
      } else {
        return(list(
          burnin = burnin,
          sample = sample,
          thin = thin,
          avg_eff_size = NA,
          convergence_score = 0,
          runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
          success = FALSE
        ))
      }
    }, error = function(e) {
      return(list(
        burnin = burnin,
        sample = sample,
        thin = thin,
        avg_eff_size = NA,
        convergence_score = 0,
        runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
        success = FALSE,
        error = as.character(e)
      ))
    })
  }
  
  # Run parameter testing in parallel
  cat("Running parallel parameter testing...\n")
  start_parallel <- Sys.time()
  
  # Convert param_grid to list for parLapply
  param_list <- split(param_grid, seq(nrow(param_grid)))
  results_list <- parLapply(cl, param_list, test_single_params)
  
  end_parallel <- Sys.time()
  
  # Clean up cluster
  stopCluster(cl)
  
  cat(sprintf("Parallel testing completed in %.1f seconds\n", 
             as.numeric(difftime(end_parallel, start_parallel, units = "secs"))))
  
  # Convert results to data.table
  results_summary <- rbindlist(results_list, fill = TRUE)
  
  # Find best parameters
  successful_results <- results_summary[success == TRUE & !is.na(avg_eff_size) & avg_eff_size > 50]
  
  if(nrow(successful_results) > 0) {
    best_row <- successful_results[which.max(convergence_score)]
    best_params <- list(
      burnin = best_row$burnin,
      sample = best_row$sample,
      thin = best_row$thin
    )
    best_score <- best_row$convergence_score
    
    # For best_result, we need to re-run the optimal parameters (since we can't return the full ei_result from parallel)
    cat("Re-running optimal parameters to get full result...\n")
    best_result <- ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
                              lambda1 = lambda1, lambda2 = lambda2,
                              burnin = best_params$burnin, 
                              sample = best_params$sample, 
                              thin = best_params$thin,
                              tune.list = tune_result)
  } else {
    best_params <- NULL
    best_result <- NULL
    best_score <- 0
  }
  
  # Print summary of parameter testing
  cat("\n=== OPTIMAL PARAMETER SELECTION SUMMARY ===\n")
  setorder(results_summary, -convergence_score)
  print(head(results_summary, 10))
  
  if(is.null(best_params)) {
    cat("  ❌ Could not find optimal parameters\n")
    return(NULL)
  }
  
  cat(sprintf("\nSelected optimal parameters: burnin=%d, sample=%d, thin=%d\n", 
             best_params$burnin, best_params$sample, best_params$thin))
  
  return(list(
    optimal_params = best_params,
    best_result = best_result,
    param_summary = results_summary,
    tune_result = tune_result  # Include tuning result for reuse
  ))
}

# Function to run ecological inference with specific MCMC parameters
run_with_fixed_params <- function(ei_data, formula_ei, lambda1, lambda2, scenario_name, burnin, sample, thin, tune_result = NULL) {
  cat("Running", scenario_name, "with fixed parameters: burnin =", burnin, ", sample =", sample, ", thin =", thin, "\n")
  
  start_time <- Sys.time()
  
  try({
    # Always compute optimal tuning for each scenario to get best lambda parameters
    cat("Computing optimal tuning parameters for this scenario...\n")
    tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                         totaldraws = 1000, ntunes = 5,  # Comprehensive tuning for accuracy
                         lambda1 = lambda1, lambda2 = lambda2)
    
    # Run inference with fixed parameters and optimized tuning
    ei_result <- ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
                            lambda1 = lambda1, lambda2 = lambda2,
                            burnin = burnin, sample = sample, thin = thin,
                            tune.list = tune_result)
    
    # Quick convergence assessment
    n_samples <- nrow(ei_result$draws$Beta)
    burn_samples <- max(1, floor(n_samples * 0.2))
    post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]
    
    # Test subset of parameters for convergence
    if(ncol(post_burn) >= 30) {
      library(coda)
      sample_indices <- seq(1, min(ncol(post_burn), 100), length.out = 30)
      sample_params <- post_burn[, sample_indices]
      eff_sizes <- effectiveSize(mcmc(sample_params))
      avg_eff_size <- mean(eff_sizes, na.rm = TRUE)
      
      runtime_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      
      cat("  Results: ESS =", round(avg_eff_size, 1), ", Runtime =", round(runtime_seconds, 1), "seconds\n")
      
      return(list(ei_result = ei_result, avg_eff_size = avg_eff_size, runtime = runtime_seconds))
    }
    
  }, silent = FALSE)
  
  cat("  ❌ Failed to complete inference\n")
  return(NULL)
}

# Function to test a single scenario with CSV export
test_scenario <- function(scenario_name, true_matrix, votes_2023, electorate_noise_stats, party_noise_stats, scenario_number, quick_test = FALSE) {
  cat("\n=== TESTING SCENARIO:", scenario_name, "===\n")
  
  # Set scenario-specific seed for reproducible noise
  scenario_seed <- 42 + scenario_number * 1000
  
  # Generate simulated data with noise
  cat("Generating simulated 2025 data with realistic noise (seed:", scenario_seed, ")...\n")
  
  simulated_data <- votes_2023[, {
    votes_vec <- c(EN_BLANCO, JxC, LLA, NO_VOTANTES, OTROS, UxP)
    sim_result <- simulate_with_noise(votes_vec, true_matrix, circuitoId, electorate_noise_stats, party_noise_stats, scenario_seed)
    
    list(
      circuitoId = circuitoId,
      cantidadElectores_2025 = sim_result$electorate,
      EN_BLANCO_2025 = sim_result$votes["EN_BLANCO_2025"],
      LLA_JxC = sim_result$votes["LLA_JxC"],
      NO_VOTANTES_2025 = sim_result$votes["NO_VOTANTES_2025"],
      OTROS_2025 = sim_result$votes["OTROS_2025"],
      UxP_2025 = sim_result$votes["UxP_2025"]
    )
  }, by = circuitoId]
  
  # Convert to long format for CSV export (matching original format)
  csv_export_data <- melt(simulated_data, 
                         id.vars = c("circuitoId", "cantidadElectores_2025"),
                         measure.vars = c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025"),
                         variable.name = "nombreAgrupacion", 
                         value.name = "votos")
  
  # Add additional columns to match original format
  csv_export_data[, `:=`(
    secprov = 2,  # Buenos Aires province code
    seccionId = as.numeric(substr(circuitoId, 1, 3)),  # Extract section from circuit
    name = "Simulated",
    mesasTotalizadas = 1,
    cantidadElectores = cantidadElectores_2025,
    anioEleccion = 2025
  )]
  
  # Reorder columns to match original format
  setcolorder(csv_export_data, c("secprov", "seccionId", "name", "circuitoId", 
                                "nombreAgrupacion", "mesasTotalizadas", 
                                "cantidadElectores", "votos", "anioEleccion"))
  
  # Export to CSV
  csv_filename <- sprintf("data/electores_circuitos_2025_sim%d.csv", scenario_number)
  fwrite(csv_export_data, csv_filename)
  cat("Exported simulated data to:", csv_filename, "\n")
  
  # Continue with inference analysis
  simulated_clean <- simulated_data[, .(circuitoId, cantidadElectores_2025,
                                       EN_BLANCO_2025, LLA_JxC, NO_VOTANTES_2025, OTROS_2025, UxP_2025)]
  
  merged_sim <- merge(votes_2023, simulated_clean, by = "circuitoId")
  setnames(merged_sim, "cantidadElectores", "cantidadElectores_2023")
  
  # Create proportions with proper normalization
  merged_sim[, `:=`(
    x1 = EN_BLANCO / cantidadElectores_2023,
    x2 = JxC / cantidadElectores_2023,
    x3 = LLA / cantidadElectores_2023,
    x4 = NO_VOTANTES / cantidadElectores_2023,
    x5 = OTROS / cantidadElectores_2023,
    x6 = UxP / cantidadElectores_2023
  )]
  
  # Calculate T matrix proportions and normalize to exactly 1.0
  merged_sim[, `:=`(
    t1_raw = EN_BLANCO_2025 / cantidadElectores_2025,
    t2_raw = LLA_JxC / cantidadElectores_2025,
    t3_raw = NO_VOTANTES_2025 / cantidadElectores_2025,
    t4_raw = OTROS_2025 / cantidadElectores_2025,
    t5_raw = UxP_2025 / cantidadElectores_2025
  )]
  
  # Normalize T matrix to sum to 1.0 (handles noise-induced deviations)
  merged_sim[, t_sum_raw := t1_raw + t2_raw + t3_raw + t4_raw + t5_raw]
  merged_sim[, `:=`(
    t1 = t1_raw / t_sum_raw,
    t2 = t2_raw / t_sum_raw,
    t3 = t3_raw / t_sum_raw,
    t4 = t4_raw / t_sum_raw,
    t5 = t5_raw / t_sum_raw
  )]
  
  # Verify proportions
  merged_sim[, x_sum := x1 + x2 + x3 + x4 + x5 + x6]
  merged_sim[, t_sum := t1 + t2 + t3 + t4 + t5]
  
  cat("Proportion verification:\n")
  cat("- X matrix rows summing to 1.0:", sum(abs(merged_sim$x_sum - 1.0) < 1e-6), "/", nrow(merged_sim), "\n")
  cat("- T matrix rows summing to 1.0:", sum(abs(merged_sim$t_sum - 1.0) < 1e-6), "/", nrow(merged_sim), "\n")
  
  ei_data <- merged_sim[, .(circuito = circuitoId, x1, x2, x3, x4, x5, x6, 
                           n = cantidadElectores_2023, t1, t2, t3, t4, t5)]
  
  cat("Running inference on", nrow(ei_data), "circuits\n")
  
  # Set up inference
  formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)
  lambda2 <- 5 / 32.5
  lambda1 <- 5 * lambda2
  
  if(quick_test) {
    # Quick test with basic parameters
    cat("Quick test mode - using basic MCMC parameters\n")
    tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                         totaldraws = 200, ntunes = 2,
                         lambda1 = lambda1, lambda2 = lambda2)
    
    ei_result <- ei.MD.bayes(formula_ei, data = ei_data, total = ei_data$n,
                            lambda1 = lambda1, lambda2 = lambda2,
                            burnin = 1000, sample = 5000, thin = 5,
                            tune.list = tune_result)
    param_summary <- NULL
  } else {
    # Full comprehensive parameter testing
    ei_results <- run_comprehensive_ei(ei_data, formula_ei, lambda1, lambda2, scenario_name)
    if(is.null(ei_results)) return(NULL)
    ei_result <- ei_results$ei_result
    param_summary <- ei_results$param_summary
  }
  
  # Extract transfer matrix
  n_samples <- nrow(ei_result$draws$Beta)
  n_params <- ncol(ei_result$draws$Beta)
  circuits_used <- nrow(ei_data)
  
  burn_samples <- max(1, floor(n_samples * 0.2))
  beta_post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]
  beta_means <- apply(beta_post_burn, 2, mean)
  
  circuit_matrices <- array(beta_means, dim = c(6, 5, circuits_used))
  estimated_matrix <- apply(circuit_matrices, c(1, 2), mean)
  
  rownames(estimated_matrix) <- rownames(true_matrix)
  colnames(estimated_matrix) <- colnames(true_matrix)
  
  # Calculate accuracy metrics
  difference <- estimated_matrix - true_matrix
  mae <- mean(abs(difference))
  rmse <- sqrt(mean(difference^2))
  correlation <- cor(as.vector(true_matrix), as.vector(estimated_matrix))
  
  # Return results
  return(list(
    scenario = scenario_name,
    scenario_number = scenario_number,
    csv_file = csv_filename,
    true_matrix = true_matrix,
    estimated_matrix = estimated_matrix,
    difference = difference,
    mae = mae,
    rmse = rmse,
    correlation = correlation,
    circuits_used = circuits_used,
    param_summary = param_summary
  ))
}

# ===== MAIN EXECUTION =====

cat("\n=== STEP 1: FIND OPTIMAL MCMC PARAMETERS ===\n")

# Generate data for first scenario to find optimal parameters
scenario_seed_1 <- 42 + 1 * 1000
cat("Generating first scenario data for parameter optimization (seed:", scenario_seed_1, ")...\n")

sim1_data <- votes_2023[, {
  votes_vec <- c(EN_BLANCO, JxC, LLA, NO_VOTANTES, OTROS, UxP)
  sim_result <- simulate_with_noise(votes_vec, true_transfer_matrix1, circuitoId, electorate_noise_stats, party_noise_stats, scenario_seed_1)
  
  list(
    circuitoId = circuitoId,
    cantidadElectores_2025 = sim_result$electorate,
    EN_BLANCO_2025 = sim_result$votes["EN_BLANCO_2025"],
    LLA_JxC = sim_result$votes["LLA_JxC"],
    NO_VOTANTES_2025 = sim_result$votes["NO_VOTANTES_2025"],
    OTROS_2025 = sim_result$votes["OTROS_2025"],
    UxP_2025 = sim_result$votes["UxP_2025"]
  )
}, by = circuitoId]

# Prepare data for parameter optimization
sim1_clean <- sim1_data[, .(circuitoId, cantidadElectores_2025,
                           EN_BLANCO_2025, LLA_JxC, NO_VOTANTES_2025, OTROS_2025, UxP_2025)]

merged1 <- merge(votes_2023, sim1_clean, by = "circuitoId")
setnames(merged1, "cantidadElectores", "cantidadElectores_2023")

# Create proportions
merged1[, `:=`(
  x1 = EN_BLANCO / cantidadElectores_2023,
  x2 = JxC / cantidadElectores_2023,
  x3 = LLA / cantidadElectores_2023,
  x4 = NO_VOTANTES / cantidadElectores_2023,
  x5 = OTROS / cantidadElectores_2023,
  x6 = UxP / cantidadElectores_2023
)]

merged1[, `:=`(
  t1_raw = EN_BLANCO_2025 / cantidadElectores_2025,
  t2_raw = LLA_JxC / cantidadElectores_2025,
  t3_raw = NO_VOTANTES_2025 / cantidadElectores_2025,
  t4_raw = OTROS_2025 / cantidadElectores_2025,
  t5_raw = UxP_2025 / cantidadElectores_2025
)]

merged1[, t_sum_raw := t1_raw + t2_raw + t3_raw + t4_raw + t5_raw]
merged1[, `:=`(
  t1 = t1_raw / t_sum_raw,
  t2 = t2_raw / t_sum_raw,
  t3 = t3_raw / t_sum_raw,
  t4 = t4_raw / t_sum_raw,
  t5 = t5_raw / t_sum_raw
)]

ei1_data <- merged1[, .(circuito = circuitoId, x1, x2, x3, x4, x5, x6, 
                       n = cantidadElectores_2023, t1, t2, t3, t4, t5)]

# Find optimal parameters
formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)
lambda2 <- 5 / 32.5
lambda1 <- 5 * lambda2

optimal_results <- find_optimal_params(ei1_data, formula_ei, lambda1, lambda2)

if(is.null(optimal_results)) {
  stop("Could not find optimal parameters")
}

# Extract optimal parameters
optimal_params <- optimal_results$optimal_params
cat(sprintf("\n🎯 OPTIMAL PARAMETERS SELECTED: burnin=%d, sample=%d, thin=%d\n", 
           optimal_params$burnin, optimal_params$sample, optimal_params$thin))

# Function to test scenario with optimal parameters
test_scenario_optimized <- function(scenario_name, true_matrix, scenario_number, optimal_results) {
  optimal_params <- optimal_results$optimal_params
  cat("\n=== TESTING SCENARIO", scenario_number, ":", scenario_name, "===\n")
  
  # Set scenario-specific seed for reproducible noise
  scenario_seed <- 42 + scenario_number * 1000
  
  # Generate simulated data
  cat("Generating simulated 2025 data with optimal parameters (seed:", scenario_seed, ")...\n")
  
  simulated_data <- votes_2023[, {
    votes_vec <- c(EN_BLANCO, JxC, LLA, NO_VOTANTES, OTROS, UxP)
    sim_result <- simulate_with_noise(votes_vec, true_matrix, circuitoId, electorate_noise_stats, party_noise_stats, scenario_seed)
    
    list(
      circuitoId = circuitoId,
      cantidadElectores_2025 = sim_result$electorate,
      EN_BLANCO_2025 = sim_result$votes["EN_BLANCO_2025"],
      LLA_JxC = sim_result$votes["LLA_JxC"],
      NO_VOTANTES_2025 = sim_result$votes["NO_VOTANTES_2025"],
      OTROS_2025 = sim_result$votes["OTROS_2025"],
      UxP_2025 = sim_result$votes["UxP_2025"]
    )
  }, by = circuitoId]
  
  # Export to CSV with correct format
  csv_export_data <- melt(simulated_data, 
                         id.vars = c("circuitoId", "cantidadElectores_2025"),
                         measure.vars = c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025"),
                         variable.name = "nombreAgrupacion", 
                         value.name = "votos")
  
  csv_export_data[, `:=`(
    secprov = 2,
    seccionId = as.numeric(substr(circuitoId, 1, 3)),
    name = "Simulated",
    mesasTotalizadas = 1,
    cantidadElectores = cantidadElectores_2025,
    anioEleccion = 2025
  )]
  
  setcolorder(csv_export_data, c("secprov", "seccionId", "name", "circuitoId", 
                                "nombreAgrupacion", "mesasTotalizadas", 
                                "cantidadElectores", "votos", "anioEleccion"))
  
  csv_filename <- sprintf("data/electores_circuitos_2025_sim%d.csv", scenario_number)
  fwrite(csv_export_data, csv_filename)
  cat("Exported corrected data to:", csv_filename, "\n")
  
  # Prepare inference data
  simulated_clean <- simulated_data[, .(circuitoId, cantidadElectores_2025,
                                       EN_BLANCO_2025, LLA_JxC, NO_VOTANTES_2025, OTROS_2025, UxP_2025)]
  
  merged_sim <- merge(votes_2023, simulated_clean, by = "circuitoId")
  setnames(merged_sim, "cantidadElectores", "cantidadElectores_2023")
  
  # Create proportions
  merged_sim[, `:=`(
    x1 = EN_BLANCO / cantidadElectores_2023,
    x2 = JxC / cantidadElectores_2023,
    x3 = LLA / cantidadElectores_2023,
    x4 = NO_VOTANTES / cantidadElectores_2023,
    x5 = OTROS / cantidadElectores_2023,
    x6 = UxP / cantidadElectores_2023
  )]
  
  merged_sim[, `:=`(
    t1_raw = EN_BLANCO_2025 / cantidadElectores_2025,
    t2_raw = LLA_JxC / cantidadElectores_2025,
    t3_raw = NO_VOTANTES_2025 / cantidadElectores_2025,
    t4_raw = OTROS_2025 / cantidadElectores_2025,
    t5_raw = UxP_2025 / cantidadElectores_2025
  )]
  
  merged_sim[, t_sum_raw := t1_raw + t2_raw + t3_raw + t4_raw + t5_raw]
  merged_sim[, `:=`(
    t1 = t1_raw / t_sum_raw,
    t2 = t2_raw / t_sum_raw,
    t3 = t3_raw / t_sum_raw,
    t4 = t4_raw / t_sum_raw,
    t5 = t5_raw / t_sum_raw
  )]
  
  ei_data <- merged_sim[, .(circuito = circuitoId, x1, x2, x3, x4, x5, x6, 
                           n = cantidadElectores_2023, t1, t2, t3, t4, t5)]
  
  cat("Running inference on", nrow(ei_data), "circuits with optimal parameters\n")
  
  # Run inference with optimal parameters (each scenario does its own tuning)
  ei_results <- run_with_fixed_params(ei_data, formula_ei, lambda1, lambda2, scenario_name, 
                                     optimal_params$burnin, optimal_params$sample, optimal_params$thin,
                                     NULL)  # Pass NULL to force new tuning for this scenario
  
  if(is.null(ei_results)) return(NULL)
  
  # Extract transfer matrix
  ei_result <- ei_results$ei_result
  n_samples <- nrow(ei_result$draws$Beta)
  burn_samples <- max(1, floor(n_samples * 0.2))
  beta_post_burn <- ei_result$draws$Beta[(burn_samples:n_samples), ]
  beta_means <- apply(beta_post_burn, 2, mean)
  
  circuits_used <- nrow(ei_data)
  circuit_matrices <- array(beta_means, dim = c(6, 5, circuits_used))
  estimated_matrix <- apply(circuit_matrices, c(1, 2), mean)
  
  rownames(estimated_matrix) <- rownames(true_matrix)
  colnames(estimated_matrix) <- colnames(true_matrix)
  
  # Calculate accuracy metrics
  difference <- estimated_matrix - true_matrix
  mae <- mean(abs(difference))
  rmse <- sqrt(mean(difference^2))
  correlation <- cor(as.vector(true_matrix), as.vector(estimated_matrix))
  
  return(list(
    scenario = scenario_name,
    scenario_number = scenario_number,
    csv_file = csv_filename,
    true_matrix = true_matrix,
    estimated_matrix = estimated_matrix,
    difference = difference,
    mae = mae,
    rmse = rmse,
    correlation = correlation,
    circuits_used = circuits_used,
    eff_size = ei_results$avg_eff_size,
    runtime = ei_results$runtime
  ))
}

cat("\n=== STEP 2: TEST ALL FOUR SCENARIOS IN PARALLEL ===\n")

# Define all scenarios to test in parallel
scenarios_to_test <- list(
  list(name = "Strong Loyalty", matrix = true_transfer_matrix1, number = 1),
  list(name = "Political Volatility", matrix = true_transfer_matrix2, number = 2),
  list(name = "Extreme Polarization", matrix = true_transfer_matrix3, number = 3),
  list(name = "Low Loyalty", matrix = true_transfer_matrix4, number = 4)
)

# Set up parallel cluster for scenario testing
n_cores_scenarios <- min(4, detectCores() - 1)  # Use up to 4 cores (one per scenario)
cat(sprintf("Testing all %d scenarios in parallel using %d cores...\n", length(scenarios_to_test), n_cores_scenarios))

cl_scenarios <- makeCluster(n_cores_scenarios)

# Export necessary variables and functions to cluster workers
clusterExport(cl_scenarios, c("test_scenario_optimized", "optimal_results", "votes_2023", 
                              "electorate_noise_stats", "party_noise_stats", "simulate_with_noise",
                              "run_with_fixed_params", "formula_ei", "lambda1", "lambda2"), 
              envir = environment())

clusterEvalQ(cl_scenarios, {
  library(eiPack)
  library(coda)
  library(data.table)
  library(dplyr)
})

# Function to test a single scenario (wrapper for parallel execution)
test_scenario_parallel <- function(scenario_info) {
  return(test_scenario_optimized(scenario_info$name, scenario_info$matrix, 
                                scenario_info$number, optimal_results))
}

# Run all scenarios in parallel
cat("Running parallel scenario testing...\n")
start_scenarios <- Sys.time()
parallel_results <- parLapply(cl_scenarios, scenarios_to_test, test_scenario_parallel)
end_scenarios <- Sys.time()

# Clean up cluster
stopCluster(cl_scenarios)

cat(sprintf("Parallel scenario testing completed in %.1f seconds\n", 
           as.numeric(difftime(end_scenarios, start_scenarios, units = "secs"))))

# Process results
all_results <- list()
for(i in 1:length(parallel_results)) {
  result <- parallel_results[[i]]
  if(!is.null(result)) {
    scenario_key <- paste0("scenario", result$scenario_number)
    all_results[[scenario_key]] <- result
    cat(sprintf("✅ Scenario %d (%s) completed successfully\n", 
                result$scenario_number, result$scenario))
  } else {
    cat(sprintf("❌ Scenario %d failed\n", i))
  }
}

# ===== COMPREHENSIVE RESULTS SUMMARY =====

cat("\n\n=== COMPREHENSIVE RESULTS SUMMARY ===\n")
cat(sprintf("Optimal MCMC Parameters Used: burnin=%d, sample=%d, thin=%d\n", 
           optimal_params$burnin, optimal_params$sample, optimal_params$thin))

results_table <- data.table()

for(scenario_key in names(all_results)) {
  result <- all_results[[scenario_key]]
  
  cat("\n=== SCENARIO", result$scenario_number, ":", result$scenario, "===\n")
  cat("CSV Export:", result$csv_file, "\n")
  cat("ESS:", round(result$eff_size, 1), ", Runtime:", round(result$runtime, 1), "seconds\n")
  
  cat("\nTRUE TRANSFER MATRIX:\n")
  print(round(result$true_matrix, 3))
  
  cat("\nESTIMATED TRANSFER MATRIX:\n") 
  print(round(result$estimated_matrix, 3))
  
  cat("\nACCURACY METRICS:\n")
  cat("- Mean Absolute Error (MAE):", round(result$mae, 4), "\n")
  cat("- Root Mean Square Error (RMSE):", round(result$rmse, 4), "\n")
  cat("- Correlation:", round(result$correlation, 4), "\n")
  cat("- Circuits used:", result$circuits_used, "\n")
  
  # Assessment
  if(result$mae <= 0.05) {
    assessment <- "🎯 EXCELLENT"
    cat("🎯 EXCELLENT: Very accurate recovery of true matrix\n")
  } else if(result$mae <= 0.10) {
    assessment <- "✅ GOOD"
    cat("✅ GOOD: Reasonable accuracy\n")
  } else if(result$mae <= 0.15) {
    assessment <- "⚠️ MODERATE"
    cat("⚠️  MODERATE: Some estimation errors\n")
  } else {
    assessment <- "❌ POOR"
    cat("❌ POOR: Significant estimation errors\n")
  }
  
  # Add to summary table
  results_table <- rbind(results_table, data.table(
    scenario = result$scenario,
    scenario_number = result$scenario_number,
    csv_file = result$csv_file,
    mae = result$mae,
    rmse = result$rmse,
    correlation = result$correlation,
    circuits = result$circuits_used,
    eff_size = result$eff_size,
    runtime = result$runtime,
    assessment = assessment
  ))
}

cat("\n=== FINAL SUMMARY TABLE ===\n")
print(results_table)

cat("\n=== PARAMETER OPTIMIZATION RESULTS ===\n")
if(!is.null(optimal_results$param_summary)) {
  cat("Top 5 parameter combinations tested:\n")
  print(head(optimal_results$param_summary[order(-convergence_score)], 5))
}

cat("\n=== FILES GENERATED ===\n")
csv_files <- c("data/electores_circuitos_2025_sim1.csv", 
               "data/electores_circuitos_2025_sim2.csv", 
               "data/electores_circuitos_2025_sim3.csv",
               "data/electores_circuitos_2025_sim4.csv")

for(csv_file in csv_files) {
  if(file.exists(csv_file)) {
    file_info <- file.info(csv_file)
    cat("✅", csv_file, "(", round(file_info$size/1024, 1), "KB )\n")
  } else {
    cat("❌", csv_file, "(not generated)\n")
  }
}

cat("\n=== COMPREHENSIVE TESTING COMPLETED ===\n")
cat("Optimal parameters found using first scenario, applied to all four scenarios\n")
cat("CSV datasets corrected and saved for reproducible analysis\n")