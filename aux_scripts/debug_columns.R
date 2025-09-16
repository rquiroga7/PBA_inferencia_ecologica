# Debug column names in merged data
library(data.table)

# Load data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

# Reshape data
votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Clean column names
setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))

cat("2023 columns:", paste(colnames(votes_2023), collapse = ", "), "\n")
cat("2025 columns:", paste(colnames(votes_2025), collapse = ", "), "\n")

# Remove VOTANTES column as it's redundant
votes_2023[, VOTANTES := NULL]

# Merge datasets
merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))

cat("Merged columns:", paste(colnames(merged_data), collapse = ", "), "\n")