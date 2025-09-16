# Simple diagnostic script to show X matrix data for 5 circuits
library(data.table)
library(dplyr)
library(jsonlite)

cat("Loading data to show X matrix examples...\n")

# Load 2023 data
votes_2023 <- fread("filteredPBAcargo56_2023.csv")
electores_2023 <- fread("electores_circuitos_2023.csv")

# Load 2025 data  
votes_2025 <- fread("todo_2025.csv")
electores_2025_df <- fread("electores_circuitos_2025.csv")

# Create circuit keys
votes_2023$circuit_key <- paste(votes_2023$provincia, votes_2023$circuito, sep = "_")
electores_2023$circuit_key <- paste(electores_2023$provincia, electores_2023$circuito, sep = "_")
votes_2025$circuit_key <- paste(votes_2025$provincia, votes_2025$circuito, sep = "_")
electores_2025_df$circuit_key <- paste(electores_2025_df$provincia, electores_2025_df$circuito, sep = "_")

# Get common circuits
common_circuits <- intersect(
  intersect(votes_2023$circuit_key, electores_2023$circuit_key),
  intersect(votes_2025$circuit_key, electores_2025_df$circuit_key)
)

cat("Total common circuits:", length(common_circuits), "\n")

# Process like main script - take first 5 circuits for examples
sample_circuits <- head(common_circuits, 5)

cat("\n=== 5 EXAMPLE CIRCUITS FOR X MATRIX ===\n")

for(i in 1:length(sample_circuits)) {
  circuit <- sample_circuits[i]
  cat("\n--- Circuit", i, "(", circuit, ") ---\n")
  
  # Get 2023 data for this circuit
  v2023 <- votes_2023[circuit_key == circuit]
  e2023 <- electores_2023[circuit_key == circuit]
  
  if(nrow(v2023) > 0 && nrow(e2023) > 0) {
    cat("cantidadElectores (2023):", e2023$cantidadElectores, "\n")
    
    # These are the values that go into X matrix
    cat("EN BLANCO:", v2023$`EN BLANCO`, "\n")
    cat("JxC:", v2023$JxC, "\n") 
    cat("LLA:", v2023$LLA, "\n")
    cat("NO_VOTANTES:", v2023$NO_VOTANTES, "\n")
    cat("OTROS:", v2023$OTROS, "\n")
    cat("UxP:", v2023$UxP, "\n")
    
    # Calculate actual votes sum
    actual_votes <- v2023$`EN BLANCO` + v2023$JxC + v2023$LLA + v2023$OTROS + v2023$UxP + v2023$NO_VOTANTES
    cat("Sum of actual votes:", actual_votes, "\n")
    
    # Calculate NO_PADRON as script does it
    no_padron_calculated <- e2023$cantidadElectores - actual_votes
    cat("NO_PADRON (calculated):", no_padron_calculated, "\n")
    
    # Show what would go in X matrix row
    cat("X matrix row would be: [", v2023$`EN BLANCO`, v2023$JxC, v2023$LLA, v2023$NO_VOTANTES, v2023$OTROS, v2023$UxP, no_padron_calculated, "]\n")
    cat("X matrix column names: [EN_BLANCO, JxC, LLA, NO_VOTANTES, OTROS, UxP, NO_PADRON]\n")
    cat("Row sum:", actual_votes + no_padron_calculated, "\n")
    
    # Check if this equals cantidadElectores
    if(actual_votes + no_padron_calculated == e2023$cantidadElectores) {
      cat("✓ Row sum equals cantidadElectores\n")
    } else {
      cat("✗ Row sum does NOT equal cantidadElectores\n")
    }
    
    # Check for negative NO_PADRON
    if(no_padron_calculated < 0) {
      cat("⚠️ WARNING: NO_PADRON is negative! This means more votes than electors.\n")
    }
  }
}