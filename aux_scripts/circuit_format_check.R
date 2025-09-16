library(data.table)

# File paths
file_2023 <- "electores_circuitos_2023.csv"
file_2025 <- "electores_circuitos_2025.csv"

cat("=== CIRCUIT ID FORMAT INVESTIGATION ===\n")

# Read data
data_2023 <- fread(file_2023)
data_2025 <- fread(file_2025)

cat("Data loaded:\n")
cat("2023:", nrow(data_2023), "rows\n")
cat("2025:", nrow(data_2025), "rows\n")

# Check the structure of the data
cat("\n=== DATA STRUCTURE COMPARISON ===\n")
cat("2023 columns:", paste(names(data_2023), collapse = ", "), "\n")
cat("2025 columns:", paste(names(data_2025), collapse = ", "), "\n")

# Look at sample circuit IDs
cat("\n=== SAMPLE CIRCUIT IDS ===\n")
cat("2023 sample circuitoId values:\n")
print(head(unique(data_2023$circuitoId), 10))
cat("2025 sample circuitoId values:\n")
print(head(unique(data_2025$circuitoId), 10))

# Check circuit key formats
data_2023[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]
data_2025[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]

cat("\n=== SAMPLE CIRCUIT KEYS ===\n")
cat("2023 sample circuit_key values:\n")
print(head(unique(data_2023$circuit_key), 10))
cat("2025 sample circuit_key values:\n")
print(head(unique(data_2025$circuit_key), 10))

# Check total unique circuits
circuits_2023 <- unique(data_2023$circuit_key)
circuits_2025 <- unique(data_2025$circuit_key)

cat("\n=== CIRCUIT OVERLAP DETAILED ANALYSIS ===\n")
cat("Total unique circuits 2023:", length(circuits_2023), "\n")
cat("Total unique circuits 2025:", length(circuits_2025), "\n")

# Check if it's really a mismatch or data structure issue
common_circuits <- intersect(circuits_2023, circuits_2025)
only_2023 <- setdiff(circuits_2023, circuits_2025)
only_2025 <- setdiff(circuits_2025, circuits_2023)

cat("Common circuits:", length(common_circuits), "\n")
cat("Only in 2023:", length(only_2023), "\n")
cat("Only in 2025:", length(only_2025), "\n")

# Show some examples of circuits that don't match
cat("\n=== EXAMPLES OF NON-MATCHING CIRCUITS ===\n")
cat("Circuits only in 2023 (first 10):\n")
print(head(only_2023, 10))
cat("Circuits only in 2025 (first 10):\n")
print(head(only_2025, 10))

# Check if there's a pattern in the differences
cat("\n=== CHECKING FOR PATTERNS ===\n")
# Extract just the circuit IDs (without secprov and seccionId)
circuitoId_2023 <- unique(data_2023$circuitoId)
circuitoId_2025 <- unique(data_2025$circuitoId)

cat("Unique circuitoId in 2023:", length(circuitoId_2023), "\n")
cat("Unique circuitoId in 2025:", length(circuitoId_2025), "\n")
cat("Common circuitoId:", length(intersect(circuitoId_2023, circuitoId_2025)), "\n")

# Check if the issue is with secprov or seccionId
secprov_2023 <- unique(data_2023$secprov)
secprov_2025 <- unique(data_2025$secprov)
seccionId_2023 <- unique(data_2023$seccionId)
seccionId_2025 <- unique(data_2025$seccionId)

cat("\n=== CHECKING GEOGRAPHIC IDENTIFIERS ===\n")
cat("Unique secprov in 2023:", length(secprov_2023), "\n")
cat("Unique secprov in 2025:", length(secprov_2025), "\n")
cat("Common secprov:", length(intersect(secprov_2023, secprov_2025)), "\n")

cat("Unique seccionId in 2023:", length(seccionId_2023), "\n")
cat("Unique seccionId in 2025:", length(seccionId_2025), "\n")
cat("Common seccionId:", length(intersect(seccionId_2023, seccionId_2025)), "\n")

# Check if we should aggregate at section level instead
cat("\n=== SECTION-LEVEL AGGREGATION POSSIBILITY ===\n")
sections_2023 <- unique(paste(data_2023$secprov, data_2023$seccionId, sep = "_"))
sections_2025 <- unique(paste(data_2025$secprov, data_2025$seccionId, sep = "_"))
common_sections <- intersect(sections_2023, sections_2025)

cat("Unique sections in 2023:", length(sections_2023), "\n")
cat("Unique sections in 2025:", length(sections_2025), "\n")
cat("Common sections:", length(common_sections), "\n")
cat("Section overlap percentage:", round(length(common_sections) / max(length(sections_2023), length(sections_2025)) * 100, 1), "%\n")

# Check data completeness per year by party
cat("\n=== PARTY COMPLETENESS CHECK ===\n")
parties_2023 <- unique(data_2023$nombreAgrupacion)
parties_2025 <- unique(data_2025$nombreAgrupacion)

cat("Parties 2023:", paste(parties_2023, collapse = ", "), "\n")
cat("Parties 2025:", paste(parties_2025, collapse = ", "), "\n")

# Check if all circuits have all parties
for(year in c("2023", "2025")) {
  data_year <- get(paste0("data_", year))
  parties_year <- get(paste0("parties_", year))
  
  cat(sprintf("\n%s party completeness by circuit:\n", year))
  party_counts <- data_year[, .N, by = .(circuit_key = paste(secprov, seccionId, circuitoId, sep = "_"))]
  expected_parties <- length(parties_year)
  complete_circuits <- sum(party_counts$N == expected_parties)
  
  cat(sprintf("Circuits with all %d parties: %d out of %d (%.1f%%)\n", 
              expected_parties, complete_circuits, nrow(party_counts),
              complete_circuits / nrow(party_counts) * 100))
}

cat("\n=== RECOMMENDATION ===\n")
if(length(common_sections) > length(common_circuits) * 3) {
  cat("RECOMMENDATION: Aggregate at SECTION level instead of circuit level\n")
  cat("This would give us", length(common_sections), "units instead of", length(common_circuits), "\n")
} else {
  cat("The issue appears to be genuine circuit mismatches, not data structure.\n")
}