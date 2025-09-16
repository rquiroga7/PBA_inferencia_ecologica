# Debug script to check data structure
library(data.table)

cat("Checking data structure...\n")

# Load and inspect 2023 data
votes_2023 <- fread("filteredPBAcargo56_2023.csv")
electores_2023 <- fread("electores_circuitos_2023.csv")

cat("2023 votes columns:", colnames(votes_2023), "\n")
cat("2023 votes first few rows:\n")
print(head(votes_2023, 3))

cat("\n2023 electores columns:", colnames(electores_2023), "\n") 
cat("2023 electores first few rows:\n")
print(head(electores_2023, 3))

# Load and inspect 2025 data
votes_2025 <- fread("todo_2025.csv")
electores_2025 <- fread("electores_circuitos_2025.csv")

cat("\n2025 votes columns:", colnames(votes_2025), "\n")
cat("2025 votes first few rows:\n")
print(head(votes_2025, 3))

cat("\n2025 electores columns:", colnames(electores_2025), "\n")
cat("2025 electores first few rows:\n")
print(head(electores_2025, 3))

# Check unique provinces in each
cat("\nUnique provinces in votes_2023:", unique(votes_2023$provincia), "\n")
cat("Unique provinces in votes_2025:", unique(votes_2025$provincia), "\n")
cat("Unique provinces in electores_2023:", unique(electores_2023$provincia), "\n")
cat("Unique provinces in electores_2025:", unique(electores_2025$provincia), "\n")