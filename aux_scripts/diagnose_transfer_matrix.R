library(data.table)
library(dplyr)

# Quick diagnostic to understand the transfer matrix issues
cat("=== DIAGNOSTIC: Transfer Matrix Issues ===\n")

# Load the results if they exist
if(file.exists("ecological_inference_results_2023_2025.RData")) {
  load("ecological_inference_results_2023_2025.RData")
  
  cat("\nLoaded results from previous run\n")
  cat("X matrix dimensions:", dim(X), "\n")
  cat("Y matrix dimensions:", dim(Y), "\n")
  cat("X column names:", colnames(X), "\n")
  cat("Y column names:", colnames(Y), "\n")
  
  # Check totals
  cat("\nX row sums (2023 totals per circuit):\n")
  cat("Min:", min(rowSums(X)), "Max:", max(rowSums(X)), "Mean:", round(mean(rowSums(X))), "\n")
  
  cat("Y row sums (2025 totals per circuit):\n")
  cat("Min:", min(rowSums(Y)), "Max:", max(rowSums(Y)), "Mean:", round(mean(rowSums(Y))), "\n")
  
  # Check if totals match
  total_diffs <- rowSums(X) - rowSums(Y)
  cat("Differences (X-Y) range:", range(total_diffs), "\n")
  cat("Number of circuits with equal totals:", sum(total_diffs == 0), "/", length(total_diffs), "\n")
  
  # Check aggregate totals by party
  cat("\n=== AGGREGATE PARTY TOTALS ===\n")
  cat("2023 (X matrix):\n")
  x_totals <- colSums(X)
  for(i in 1:length(x_totals)) {
    pct <- x_totals[i] / sum(x_totals) * 100
    cat(sprintf("  %-15s: %8.0f votes (%.1f%%)\n", colnames(X)[i], x_totals[i], pct))
  }
  
  cat("\n2025 (Y matrix):\n")
  y_totals <- colSums(Y)
  for(i in 1:length(y_totals)) {
    pct <- y_totals[i] / sum(y_totals) * 100
    cat(sprintf("  %-15s: %8.0f votes (%.1f%%)\n", colnames(Y)[i], y_totals[i], pct))
  }
  
  # Check the transfer matrix calculation
  if(exists("transfer_matrix")) {
    cat("\n=== TRANSFER MATRIX ANALYSIS ===\n")
    cat("Transfer matrix dimensions:", dim(transfer_matrix), "\n")
    cat("Row names:", rownames(transfer_matrix), "\n")
    cat("Column names:", colnames(transfer_matrix), "\n")
    
    # Check row sums
    row_sums_check <- rowSums(transfer_matrix)
    cat("Row sums range:", range(row_sums_check), "\n")
    
    # Show problematic transfers to NO_PADRON_2025
    no_padron_col <- which(colnames(transfer_matrix) == "NO_PADRON_2025")
    if(length(no_padron_col) > 0) {
      cat("\nTransfers TO NO_PADRON_2025 (should be very small):\n")
      for(i in 1:nrow(transfer_matrix)) {
        transfer_rate <- transfer_matrix[i, no_padron_col]
        if(transfer_rate > 0.1) {  # More than 10%
          cat(sprintf("  %s -> NO_PADRON_2025: %.1f%% (PROBLEMATIC!)\n", 
                      rownames(transfer_matrix)[i], transfer_rate * 100))
        }
      }
    }
    
    # Check diagonal elements (loyalty rates)
    cat("\nDiagonal elements (loyalty rates):\n")
    party_names_2023 <- gsub("_2023", "", rownames(transfer_matrix))
    party_names_2025 <- gsub("_2025", "", colnames(transfer_matrix))
    
    for(i in 1:length(party_names_2023)) {
      party_2023 <- party_names_2023[i]
      # Find matching party in 2025
      matching_col <- which(party_names_2025 == party_2023)
      if(length(matching_col) > 0) {
        loyalty_rate <- transfer_matrix[i, matching_col]
        cat(sprintf("  %s loyalty: %.1f%%\n", party_2023, loyalty_rate * 100))
      }
    }
  }
  
  # Check ei.MD.bayes parameters
  if(exists("ei_results") && "draws" %in% names(ei_results)) {
    cat("\n=== MODEL DIAGNOSTICS ===\n")
    alpha_draws <- ei_results$draws$Alpha
    cat("Alpha draws dimensions:", dim(alpha_draws), "\n")
    
    # Check parameter means
    param_means <- apply(alpha_draws, 2, mean)
    cat("Parameter means range:", range(param_means), "\n")
    cat("Parameters > 1 (problematic):", sum(param_means > 1), "\n")
    
    if(sum(param_means > 1) > 0) {
      cat("Problematic parameters (>1):\n")
      problematic <- which(param_means > 1)
      for(p in problematic[1:min(10, length(problematic))]) {
        cat(sprintf("  Parameter %d: %.3f\n", p, param_means[p]))
      }
    }
  }
  
} else {
  cat("No results file found. Run ecological_inference.R first.\n")
}

cat("\n=== SUGGESTED FIXES ===\n")
cat("1. Check if NO_PADRON categories are being artificially inflated\n")
cat("2. Verify ei.MD.bayes input requirements (row totals must match)\n") 
cat("3. Consider removing NO_PADRON balancing logic\n")
cat("4. Check if transfer matrix is being constructed correctly\n")