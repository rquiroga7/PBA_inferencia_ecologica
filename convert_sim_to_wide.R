# Convert simulation CSV files from long to wide format
# This script processes *_sim*.csv files and outputs *_sim*_wide.csv
# Transforms vote amounts by nombreAgrupacion into separate columns

library(data.table)
library(dplyr)

cat("=== CSV LONG TO WIDE FORMAT CONVERTER ===\n")

# Find all simulation CSV files
sim_files <- list.files(pattern = "data/.*_sim.*\\.csv$", full.names = FALSE)

if(length(sim_files) == 0) {
  cat("❌ No simulation CSV files found matching pattern '.*_sim.*\\.csv$'\n")
  stop("No simulation files to process")
}

cat("Found", length(sim_files), "simulation files to process:\n")
for(i in seq_along(sim_files)) {
  cat(sprintf("  %d. %s\n", i, sim_files[i]))
}

# Process each simulation file
for(file in sim_files) {
  cat("\n=== Processing:", file, "===\n")
  
  # Load the data
  data <- fread(file)
  cat("Loaded data with", nrow(data), "rows and", ncol(data), "columns\n")
  
  # Check required columns
  required_cols <- c("secprov", "seccionId", "name", "circuitoId", "nombreAgrupacion", 
                    "mesasTotalizadas", "cantidadElectores", "votos", "anioEleccion", 
                    "cantidadElectores_2025")
  
  missing_cols <- setdiff(required_cols, colnames(data))
  if(length(missing_cols) > 0) {
    cat("❌ Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    next
  }
  
  cat("✅ All required columns present\n")
  
  # Check unique parties
  unique_parties <- unique(data$nombreAgrupacion)
  cat("Parties found:", length(unique_parties), "\n")
  for(party in unique_parties) {
    party_count <- sum(data$nombreAgrupacion == party)
    cat(sprintf("  - %s: %d records\n", party, party_count))
  }
  
  # Verify data consistency - each circuit should have the same metadata across parties
  cat("Checking data consistency...\n")
  
  # Check if all circuits have the same cantidadElectores and cantidadElectores_2025 values
  circuit_meta <- data[, .(
    electores_check = length(unique(cantidadElectores)),
    electores_2025_check = length(unique(cantidadElectores_2025)),
    mesas_check = length(unique(mesasTotalizadas)),
    secprov_check = length(unique(secprov)),
    seccionId_check = length(unique(seccionId)),
    name_check = length(unique(name)),
    anio_check = length(unique(anioEleccion))
  ), by = circuitoId]
  
  # Check for inconsistencies
  inconsistent_circuits <- circuit_meta[
    electores_check > 1 | electores_2025_check > 1 | mesas_check > 1 | 
    secprov_check > 1 | seccionId_check > 1 | name_check > 1 | anio_check > 1
  ]
  
  if(nrow(inconsistent_circuits) > 0) {
    cat("⚠️  Found", nrow(inconsistent_circuits), "circuits with inconsistent metadata\n")
    cat("Proceeding with first occurrence values...\n")
  } else {
    cat("✅ All circuit metadata is consistent\n")
  }
  
  # Convert to wide format
  cat("Converting to wide format...\n")
  
  # Use dcast to pivot the data
  wide_data <- dcast(data, 
                    secprov + seccionId + name + circuitoId + mesasTotalizadas + 
                    cantidadElectores + anioEleccion + cantidadElectores_2025 ~ nombreAgrupacion,
                    value.var = "votos",
                    fill = 0,  # Fill missing values with 0
                    fun.aggregate = sum)  # In case of duplicates, sum them
  
  cat("Wide format data created with", nrow(wide_data), "rows and", ncol(wide_data), "columns\n")
  
  # Verify the conversion
  cat("Verification:\n")
  cat("  Original total votes:", format(sum(data$votos), big.mark = ","), "\n")
  
  # Calculate total votes in wide format (sum across all party columns)
  party_columns <- unique_parties
  total_votes_wide <- rowSums(wide_data[, ..party_columns], na.rm = TRUE)
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
  cat("  Parties:", length(party_columns), "\n")
  cat("  Total votes:", format(sum(total_votes_wide), big.mark = ","), "\n")
  cat("  Average voters per circuit:", round(mean(wide_data$cantidadElectores), 0), "\n")
  cat("  Average votes per circuit:", round(mean(total_votes_wide), 0), "\n")
  
  # Party-wise vote totals
  cat("\n  Party totals:\n")
  for(party in party_columns) {
    party_total <- sum(wide_data[[party]], na.rm = TRUE)
    cat(sprintf("    %s: %s votes\n", party, format(party_total, big.mark = ",")))
  }
}

cat("\n🎯 CONVERSION COMPLETE!\n")
cat("All simulation files have been converted to wide format\n")
cat("Wide format files have '_wide.csv' suffix\n")

# List all created files
wide_files <- list.files(pattern = "data/.*_sim.*_wide\\.csv$", full.names = FALSE)
if(length(wide_files) > 0) {
  cat("\nCreated wide format files:\n")
  for(i in seq_along(wide_files)) {
    cat(sprintf("  %d. %s\n", i, wide_files[i]))
  }
} else {
  cat("\n❌ No wide format files were created\n")
}