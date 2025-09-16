# Debug column names issue
library(data.table)
library(dplyr)

# Load and process data to see exact column structure
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

cat("2023 columns after cleaning:", colnames(votes_2023), "\n")
cat("2025 columns after cleaning:", colnames(votes_2025), "\n")

# Add NO_PADRON
political_parties_2023 <- c("EN_BLANCO", "JxC", "LLA", "NO_VOTANTES", "OTROS", "UxP")
political_parties_2025 <- c("EN_BLANCO", "LLA_JxC", "NO_VOTANTES", "OTROS", "UxP")

votes_2023[, total_votes := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
votes_2023[, NO_PADRON := cantidadElectores - total_votes]

votes_2025[, total_votes := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]
votes_2025[, NO_PADRON := cantidadElectores - total_votes]

# Rename before merge
setnames(votes_2023, "cantidadElectores", "cantidadElectores_2023")
setnames(votes_2025, "cantidadElectores", "cantidadElectores_2025")

# Merge
merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

cat("\nColumns after merge:\n")
print(colnames(merged_data))

cat("\nFirst few rows to check structure:\n")
print(head(merged_data[, 1:10], 2))