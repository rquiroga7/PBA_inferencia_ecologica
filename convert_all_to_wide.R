# Enhanced CSV Long to Wide Format Converter
# Converts simulation files (*_sim*.csv) and main election files (electores_circuitos_*.csv)
# Transforms vote amounts by nombreAgrupacion into separate columns

library(data.table)
library(dplyr)

cat("=== ENHANCED CSV LONG TO WIDE FORMAT CONVERTER ===\n")

# Define file patterns to process
file_patterns <- list(
  simulation = ".*_sim[0-9]+\\.csv$",  # Only original sim files, not _wide versions
  main_2023 = "data/^electores_circuitos_2023\\.csv$", 
  main_2025 = "data/^electores_circuitos_2025\\.csv$"
)

all_files <- c()

# Find files for each pattern
for(pattern_name in names(file_patterns)) {
  pattern <- file_patterns[[pattern_name]]
  files <- list.files(pattern = pattern, full.names = FALSE)
  
  if(length(files) > 0) {
    cat("Found", length(files), pattern_name, "file(s):\n")
    for(i in seq_along(files)) {
      cat(sprintf("  %s: %s\n", pattern_name, files[i]))
    }
    all_files <- c(all_files, files)
  } else {
    cat("No", pattern_name, "files found\n")
  }
}

if(length(all_files) == 0) {
  cat("❌ No files found to process\n")
  stop("No files to process")
}

cat("\nTotal files to process:", length(all_files), "\n")

# Process each file
for(file in all_files) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("=== Processing:", file, "===\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Load the data
  data <- fread(file)
  cat("Loaded data with", nrow(data), "rows and", ncol(data), "columns\n")
  
  # Check basic required columns (flexible for different file types)
  basic_cols <- c("secprov", "seccionId", "circuitoId", "nombreAgrupacion", "votos")
  missing_basic <- setdiff(basic_cols, colnames(data))
  
  if(length(missing_basic) > 0) {
    cat("❌ Missing basic columns:", paste(missing_basic, collapse = ", "), "\n")
    next
  }
  
  cat("✅ Basic columns present\n")
  
  # Show all available columns
  cat("Available columns:", paste(colnames(data), collapse = ", "), "\n")
  
  # Check unique parties
  unique_parties <- unique(data$nombreAgrupacion)
  cat("Parties found:", length(unique_parties), "\n")
  for(party in unique_parties) {
    party_count <- sum(data$nombreAgrupacion == party)
    cat(sprintf("  - %s: %d records\n", party, party_count))
  }
  
  # Verify data consistency
  cat("Checking data consistency...\n")
  
  # Get all non-vote columns for grouping (excluding votos and nombreAgrupacion)
  grouping_cols <- setdiff(colnames(data), c("votos", "nombreAgrupacion"))
  
  # Check consistency for each circuit - simplified approach
  inconsistent_circuits <- 0
  for(col in grouping_cols) {
    if(col %in% colnames(data)) {
      circuit_consistency <- data[, .(unique_count = length(unique(get(col)))), by = circuitoId]
      col_inconsistent <- sum(circuit_consistency$unique_count > 1)
      if(col_inconsistent > 0) {
        inconsistent_circuits <- inconsistent_circuits + col_inconsistent
        cat("  ⚠️ ", col_inconsistent, "circuits have inconsistent", col, "\n")
      }
    }
  }
  
  if(inconsistent_circuits == 0) {
    cat("✅ All circuit metadata is consistent\n")
  } else {
    cat("⚠️  Found inconsistencies, proceeding with first occurrence values...\n")
  }
  
  # Create the dcast formula dynamically
  # Left side: all columns except votos and nombreAgrupacion
  # Right side: nombreAgrupacion
  id_vars <- setdiff(colnames(data), c("votos", "nombreAgrupacion"))
  formula_left <- paste(id_vars, collapse = " + ")
  formula_str <- paste(formula_left, "~ nombreAgrupacion")
  
  cat("Using formula:", formula_str, "\n")
  cat("Converting to wide format...\n")
  
  # Convert to wide format
  wide_data <- dcast(data, 
                    formula = as.formula(formula_str),
                    value.var = "votos",
                    fill = 0,  # Fill missing values with 0
                    fun.aggregate = sum)  # In case of duplicates, sum them
  
  cat("Wide format data created with", nrow(wide_data), "rows and", ncol(wide_data), "columns\n")
  
  # Clean up party column names (replace spaces with underscores)
  party_columns <- unique_parties
  cleaned_party_names <- gsub(" ", "_", party_columns)
  
  # Update column names in wide_data
  for(i in seq_along(party_columns)) {
    old_name <- party_columns[i]
    new_name <- cleaned_party_names[i]
    if(old_name != new_name && old_name %in% colnames(wide_data)) {
      setnames(wide_data, old_name, new_name)
      cat("  Renamed column:", old_name, "→", new_name, "\n")
    }
  }
  
  # Verify the conversion
  cat("Verification:\n")
  cat("  Original total votes:", format(sum(data$votos), big.mark = ","), "\n")
  
  # Calculate total votes in wide format (sum across all party columns)
  current_party_cols <- intersect(cleaned_party_names, colnames(wide_data))
  total_votes_wide <- rowSums(wide_data[, ..current_party_cols], na.rm = TRUE)
  cat("  Wide format total votes:", format(sum(total_votes_wide), big.mark = ","), "\n")
  
  # Check for vote conservation
  vote_difference <- sum(data$votos) - sum(total_votes_wide)
  if(abs(vote_difference) < 1) {
    cat("  ✅ Vote totals match (difference:", vote_difference, ")\n")
  } else {
    cat("  ⚠️  Vote totals differ by:", vote_difference, "\n")
  }
  
  # Show sample of wide data
  cat("\nSample of wide format data:\n")
  print(head(wide_data, 3))
  
  # Create output filename
  output_file <- gsub("\\.csv$", "_wide.csv", file)
  
  # Write the wide format data
  fwrite(wide_data, output_file)
  cat("✅ Wide format data saved as:", output_file, "\n")
  
  # Summary statistics
  cat("\nSummary statistics for", output_file, ":\n")
  cat("  Circuits:", nrow(wide_data), "\n")
  cat("  Parties:", length(current_party_cols), "\n")
  cat("  Total votes:", format(sum(total_votes_wide), big.mark = ","), "\n")
  
  # Check if cantidadElectores column exists
  if("cantidadElectores" %in% colnames(wide_data)) {
    cat("  Average voters per circuit:", round(mean(wide_data$cantidadElectores), 0), "\n")
    cat("  Average votes per circuit:", round(mean(total_votes_wide), 0), "\n")
    
    # Calculate turnout
    avg_turnout <- mean(total_votes_wide / wide_data$cantidadElectores) * 100
    cat("  Average turnout:", round(avg_turnout, 1), "%\n")
  }
  
  # Party-wise vote totals
  cat("\n  Party totals:\n")
  for(party in current_party_cols) {
    if(party %in% colnames(wide_data)) {
      party_total <- sum(wide_data[[party]], na.rm = TRUE)
      party_pct <- (party_total / sum(total_votes_wide)) * 100
      cat(sprintf("    %s: %s votes (%.1f%%)\n", 
                  party, format(party_total, big.mark = ","), party_pct))
    }
  }
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎯 CONVERSION COMPLETE!\n")
cat("All files have been converted to wide format\n")
cat("Wide format files have '_wide.csv' suffix\n")

# List all created files
wide_files <- list.files(pattern = ".*_wide\\.csv$", full.names = FALSE)
if(length(wide_files) > 0) {
  cat("\nCreated wide format files:\n")
  for(i in seq_along(wide_files)) {
    file_size <- file.info(wide_files[i])$size
    cat(sprintf("  %d. %s (%.1f KB)\n", i, wide_files[i], file_size/1024))
  }
} else {
  cat("\n❌ No wide format files were created\n")
}

cat("\n📊 All wide format files are ready for analysis!\n")