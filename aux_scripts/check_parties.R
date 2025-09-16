library(data.table)

# Check parties in each year
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("Parties in 2023:\n")
parties_2023 <- sort(unique(data_2023$nombreAgrupacion))
for(i in seq_along(parties_2023)) {
  cat(sprintf("%d. %s\n", i, parties_2023[i]))
}

cat("\nParties in 2025:\n")
parties_2025 <- sort(unique(data_2025$nombreAgrupacion))
for(i in seq_along(parties_2025)) {
  cat(sprintf("%d. %s\n", i, parties_2025[i]))
}

cat("\nCommon parties:", paste(intersect(parties_2023, parties_2025), collapse = ", "), "\n")
cat("Only in 2023:", paste(setdiff(parties_2023, parties_2025), collapse = ", "), "\n")
cat("Only in 2025:", paste(setdiff(parties_2025, parties_2023), collapse = ", "), "\n")