# Deep investigation of proportion calculation issue
library(data.table)
library(dplyr)

# Load and examine the raw data structure
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("=== INVESTIGATING PROPORTION CALCULATION ISSUE ===\n\n")

# Check for one specific circuit
sample_circuit <- "00001"

cat("Sample circuit analysis:", sample_circuit, "\n")
cat("2023 data for circuit", sample_circuit, ":\n")
circuit_2023 <- data_2023[circuitoId == sample_circuit]
print(circuit_2023)

cat("\n2025 data for circuit", sample_circuit, ":\n")
circuit_2025 <- data_2025[circuitoId == sample_circuit]
print(circuit_2025)

# Process the data as we do in the script
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

cat("\nAfter dcast processing:\n")
cat("2023 circuit", sample_circuit, ":\n")
print(votes_2023[circuitoId == sample_circuit])

cat("\n2025 circuit", sample_circuit, ":\n")
print(votes_2025[circuitoId == sample_circuit])

# Check the VOTANTES vs individual parties relationship
political_parties_2023 <- c("EN_BLANCO", "JxC", "LLA", "NO_VOTANTES", "OTROS", "UxP")

sample_2023 <- votes_2023[circuitoId == sample_circuit]
individual_party_total <- sample_2023[, rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
votantes_total <- sample_2023$VOTANTES
cantidadElectores <- sample_2023$cantidadElectores

cat("\nCritical analysis for circuit", sample_circuit, ":\n")
cat("cantidadElectores:", cantidadElectores, "\n")
cat("Individual parties total:", individual_party_total, "\n") 
cat("VOTANTES column:", votantes_total, "\n")
cat("Difference (parties - VOTANTES):", individual_party_total - votantes_total, "\n")
cat("Missing votes (cantidadElectores - VOTANTES):", cantidadElectores - votantes_total, "\n")

# The key insight: Are VOTANTES the actual sum of individual votes?
# Or should we ignore VOTANTES and calculate NO_PADRON from cantidadElectores?

cat("\n=== TESTING DIFFERENT NO_PADRON CALCULATION METHODS ===\n")

# Method 1: NO_PADRON = cantidadElectores - sum(individual_parties)
method1_no_padron <- cantidadElectores - individual_party_total
cat("Method 1 (cantidadElectores - individual parties): NO_PADRON =", method1_no_padron, "\n")

# Method 2: NO_PADRON = cantidadElectores - VOTANTES  
method2_no_padron <- cantidadElectores - votantes_total
cat("Method 2 (cantidadElectores - VOTANTES): NO_PADRON =", method2_no_padron, "\n")

# Check which method gives proportions that sum to 1.0
cat("\n=== PROPORTION CHECK ===\n")

# Method 1 proportions
cat("Method 1 proportions:\n")
prop_1 <- c(
  EN_BLANCO = sample_2023$EN_BLANCO / cantidadElectores,
  JxC = sample_2023$JxC / cantidadElectores,
  LLA = sample_2023$LLA / cantidadElectores,
  NO_VOTANTES = sample_2023$NO_VOTANTES / cantidadElectores,
  OTROS = sample_2023$OTROS / cantidadElectores,
  UxP = sample_2023$UxP / cantidadElectores,
  NO_PADRON = method1_no_padron / cantidadElectores
)
print(round(prop_1, 4))
cat("Sum:", sum(prop_1), "\n")

# Method 2 proportions  
cat("\nMethod 2 proportions:\n")
prop_2 <- c(
  EN_BLANCO = sample_2023$EN_BLANCO / cantidadElectores,
  JxC = sample_2023$JxC / cantidadElectores,
  LLA = sample_2023$LLA / cantidadElectores,
  NO_VOTANTES = sample_2023$NO_VOTANTES / cantidadElectores,
  OTROS = sample_2023$OTROS / cantidadElectores,
  UxP = sample_2023$UxP / cantidadElectores,
  NO_PADRON = method2_no_padron / cantidadElectores
)
print(round(prop_2, 4))
cat("Sum:", sum(prop_2), "\n")

# Check if VOTANTES includes NO_VOTANTES or not
cat("\n=== CHECKING VOTANTES COMPOSITION ===\n")
active_parties_2023 <- c("EN_BLANCO", "JxC", "LLA", "OTROS", "UxP")  # Excluding NO_VOTANTES
active_total <- sample_2023[, rowSums(.SD, na.rm = TRUE), .SDcols = active_parties_2023]
cat("Active parties total (excluding NO_VOTANTES):", active_total, "\n")
cat("VOTANTES:", votantes_total, "\n")
cat("Difference (VOTANTES - active parties):", votantes_total - active_total, "\n")
cat("NO_VOTANTES value:", sample_2023$NO_VOTANTES, "\n")

if (abs(votantes_total - active_total) < 5) {
  cat("✅ VOTANTES appears to be sum of active parties (excluding NO_VOTANTES)\n")
} else if (abs(votantes_total - individual_party_total) < 5) {
  cat("✅ VOTANTES appears to be sum of all parties (including NO_VOTANTES)\n")
} else {
  cat("❌ VOTANTES doesn't match expected totals\n")
}