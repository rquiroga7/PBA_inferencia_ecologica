# Debug lambda extraction from tuneMD
library(eiPack)
library(data.table)

# Load and prepare a small sample of data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

votes_2023 <- dcast(data_2023, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

setnames(votes_2023, gsub(" ", "_", colnames(votes_2023)))
setnames(votes_2025, gsub(" ", "_", colnames(votes_2025)))
votes_2023[, VOTANTES := NULL]

merged_data <- merge(votes_2023, votes_2025, by = "circuitoId", suffixes = c("_2023", "_2025"))
merged_data <- merged_data[
  cantidadElectores_2025 <= 1.15 * cantidadElectores_2023 &
  cantidadElectores_2025 >= 0.85 * cantidadElectores_2023
]

political_parties_2023 <- c("EN_BLANCO_2023", "JxC", "LLA", "NO_VOTANTES_2023", "OTROS_2023", "UxP_2023")
political_parties_2025 <- c("EN_BLANCO_2025", "LLA_JxC", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025")

merged_data[, total_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2023]
merged_data[, total_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = political_parties_2025]
merged_data[, diff_2023 := abs(total_2023 - cantidadElectores_2023)]
merged_data[, diff_2025 := abs(total_2025 - cantidadElectores_2025)]

perfect_data <- merged_data[diff_2023 == 0 & diff_2025 == 0]

# Take just first 10 circuits for testing
test_data <- perfect_data[1:10]

test_data[, `:=`(
  x1 = EN_BLANCO_2023 / cantidadElectores_2023,
  x2 = JxC / cantidadElectores_2023,              
  x3 = LLA / cantidadElectores_2023,              
  x4 = NO_VOTANTES_2023 / cantidadElectores_2023,
  x5 = OTROS_2023 / cantidadElectores_2023,       
  x6 = UxP_2023 / cantidadElectores_2023,          
  
  t1 = EN_BLANCO_2025 / cantidadElectores_2025,   
  t2 = LLA_JxC / cantidadElectores_2025,          
  t3 = NO_VOTANTES_2025 / cantidadElectores_2025, 
  t4 = OTROS_2025 / cantidadElectores_2025,       
  t5 = UxP_2025 / cantidadElectores_2025          
)]

ei_data <- test_data[, .(
  circuito = circuitoId,
  x1, x2, x3, x4, x5, x6,
  n = cantidadElectores_2023,
  t1, t2, t3, t4, t5
)]

formula_ei <- cbind(t1, t2, t3, t4, t5) ~ cbind(x1, x2, x3, x4, x5, x6)

cat("Testing tuneMD with small dataset...\n")
cat("Data rows:", nrow(ei_data), "\n")

# Test tuneMD with single lambda values first
tune_result <- tuneMD(formula_ei, data = ei_data, total = ei_data$n,
                     totaldraws = 100, ntunes = 2,
                     lambda1 = 0.5, lambda2 = 0.1)

cat("tuneMD result structure:\n")
str(tune_result)
cat("Lambda1 value:", tune_result$lambda1, "\n")
cat("Lambda2 value:", tune_result$lambda2, "\n")