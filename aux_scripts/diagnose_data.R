library(data.table)

# Load data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("=== DATA STRUCTURE DIAGNOSIS ===\n")
cat("2023 total rows:", nrow(data_2023), "\n")
cat("2025 total rows:", nrow(data_2025), "\n")

# Count unique circuits
circuits_2023 <- unique(paste(data_2023$secprov, data_2023$seccionId, data_2023$circuitoId, sep="_"))
circuits_2025 <- unique(paste(data_2025$secprov, data_2025$seccionId, data_2025$circuitoId, sep="_"))

cat("2023 unique circuits:", length(circuits_2023), "\n")
cat("2025 unique circuits:", length(circuits_2025), "\n")

common_circuits <- intersect(circuits_2023, circuits_2025)
cat("Common circuits before filtering:", length(common_circuits), "\n")

# Check if we have party data for all common circuits
cat("\n=== PARTY COVERAGE ===\n")
parties_2023 <- unique(data_2023$nombreAgrupacion)
parties_2025 <- unique(data_2025$nombreAgrupacion)
cat("Parties 2023:", paste(parties_2023, collapse=", "), "\n")
cat("Parties 2025:", paste(parties_2025, collapse=", "), "\n")

# Check vote magnitudes
cat("\n=== VOTE MAGNITUDES ===\n")
total_votes_2023 <- sum(data_2023[nombreAgrupacion %in% c("EN BLANCO", "JxC", "LLA", "OTROS", "UxP")]$votos)
total_votes_2025 <- sum(data_2025[nombreAgrupacion %in% c("EN BLANCO", "LLA", "OTROS", "UxP")]$votos)
cat("Total political votes 2023:", total_votes_2023, "\n")
cat("Total political votes 2025:", total_votes_2025, "\n")

# Sample some data to understand structure
cat("\n=== SAMPLE DATA ===\n")
cat("Sample 2023 data:\n")
print(head(data_2023[nombreAgrupacion == "UxP"], 3))
cat("Sample 2025 data:\n")
print(head(data_2025[nombreAgrupacion == "UxP"], 3))

# Check circuit-level statistics for a few circuits
cat("\n=== CIRCUIT-LEVEL EXAMPLE ===\n")
sample_circuit <- common_circuits[1]
cat("Sample circuit:", sample_circuit, "\n")

sample_2023 <- data_2023[paste(secprov, seccionId, circuitoId, sep="_") == sample_circuit & 
                         nombreAgrupacion %in% c("EN BLANCO", "JxC", "LLA", "OTROS", "UxP")]
sample_2025 <- data_2025[paste(secprov, seccionId, circuitoId, sep="_") == sample_circuit & 
                         nombreAgrupacion %in% c("EN BLANCO", "LLA", "OTROS", "UxP")]

cat("2023 votes in sample circuit:\n")
print(sample_2023[, .(nombreAgrupacion, votos)])
cat("2025 votes in sample circuit:\n")
print(sample_2025[, .(nombreAgrupacion, votos)])

cat("2023 total votes:", sum(sample_2023$votos), "\n")
cat("2025 total votes:", sum(sample_2025$votos), "\n")