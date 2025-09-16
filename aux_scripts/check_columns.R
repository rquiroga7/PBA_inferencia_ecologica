# Check actual column names in the data
library(data.table)

# Load the corrected data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("=== DATA STRUCTURE ANALYSIS ===\n")
cat("2023 columns:", colnames(data_2023), "\n")
cat("2025 columns:", colnames(data_2025), "\n")

# Check unique parties
cat("\n2023 unique parties:", unique(data_2023$nombreAgrupacion), "\n")
cat("2025 unique parties:", unique(data_2025$nombreAgrupacion), "\n")

# Process data to see the actual structure after dcast
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

cat("\n=== AFTER DCAST ===\n")
cat("2023 columns:", colnames(votes_2023), "\n")
cat("2025 columns:", colnames(votes_2025), "\n")

cat("\nFirst few rows of 2023 data:\n")
print(head(votes_2023, 3))

cat("\nFirst few rows of 2025 data:\n")
print(head(votes_2025, 3))