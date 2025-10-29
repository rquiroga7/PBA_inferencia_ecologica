library(data.table)
library(dplyr)

# Load October data only
cat("Loading October 2025 data...\n")
october_raw <- fread("data/resultados_Oct2025.csv")

# For October, analyze all country data
october_all <- october_raw %>%
  filter(cargo_nombre == "DIPUTADO NACIONAL")

cat(sprintf("October data loaded: %s rows\n", format(nrow(october_all), big.mark = ",")))

# Standardize party names
standardize_party <- function(party_names) {
  parties <- toupper(trimws(as.character(party_names)))
  result <- rep("OTROS", length(parties))
  
  # All Fuerza Patria coalition parties
  fp_pattern <- paste(c(
    "FUERZA PATRIA", "ALIANZA FUERZA PATRIA",
    "TUCUMAN PRIMERO", "TUCUMÁN PRIMERO",
    "FRENTE CIVICO POR SANTIAGO", "FRENTE CÍVICO POR SANTIAGO",
    "FUERZA JUSTICIALISTA MENDOZA",
    "FUERZA ENTRE RIOS", "FUERZA ENTRE RÍOS",
    "FRENTE DE LA VICTORIA",
    "FUERZA SAN JUAN",
    "FRENTE DEFENDEMOS LA PAMPA",
    "FEDERALES DEFENDAMOS LA RIOJA",
    "FRENTE UNIDOS PODEMOS",
    "FRENTE JUSTICIALISTA",
    "FUERZA SANTACRUCEÑA", "FUERZA SANTACRUCENA",
    "PARTIDO DE LA VICTORIA",
    "AHORA 503",
    "DEFENDAMOS TIERRA DEL FUEGO",
    "FRENTE PUEBLO"
  ), collapse = "|")
  
  result[grepl(fp_pattern, parties)] <- "Fuerza Patria"
  result[grepl("LIBERTAD AVANZA|ALIANZA LA LIBERTAD AVANZA", parties)] <- "La Libertad Avanza"
  result[grepl("^BLANCO$|^EN BLANCO$|NULO|IMPUGNADO|RECURRIDO", parties)] <- "EN BLANCO"
  return(result)
}

october_all$party_std <- standardize_party(october_all$agrupacion_nombre)

# Analyze zero-vote mesas
cat("\nAnalyzing zero votes by distrito_id, seccion_id, mesa_id...\n")

# Calculate total positive votes and party votes per unique mesa
mesa_summary <- october_all %>%
  group_by(distrito_id, seccion_id, mesa_id) %>%
  summarise(
    total_positive = sum(votos_cantidad[votos_tipo == "POSITIVO"], na.rm = TRUE),
    fp_votes = sum(votos_cantidad[party_std == "Fuerza Patria"], na.rm = TRUE),
    lla_votes = sum(votos_cantidad[party_std == "La Libertad Avanza"], na.rm = TRUE),
    .groups = "drop"
  )

# Count unique mesas
total_mesas <- nrow(mesa_summary)
cat(sprintf("Total unique mesas (distrito_id, seccion_id, mesa_id): %s\n", 
            format(total_mesas, big.mark = ",")))

# Count mesas with 0 votes
mesas_zero_all <- sum(mesa_summary$total_positive == 0)
mesas_zero_fp <- sum(mesa_summary$fp_votes == 0)
mesas_zero_lla <- sum(mesa_summary$lla_votes == 0)

cat("\n=== Octubre 2025 - TODO EL PAIS (DIPUTADO NACIONAL) ===\n")
cat(sprintf("Total mesas: %s\n", format(total_mesas, big.mark = ",")))
cat(sprintf("Mesas with 0 votes for ALL parties: %s (%.2f%%)\n", 
            format(mesas_zero_all, big.mark = ","),
            100 * mesas_zero_all / total_mesas))
cat(sprintf("Mesas with 0 votes for Fuerza Patria: %s (%.2f%%)\n", 
            format(mesas_zero_fp, big.mark = ","),
            100 * mesas_zero_fp / total_mesas))
cat(sprintf("Mesas with 0 votes for La Libertad Avanza: %s (%.2f%%)\n", 
            format(mesas_zero_lla, big.mark = ","),
            100 * mesas_zero_lla / total_mesas))

# Mesas with votes but 0 for both major parties
mesas_other_only <- mesa_summary %>%
  filter(total_positive > 0, fp_votes == 0, lla_votes == 0)

cat(sprintf("Mesas with positive votes but 0 for both FP and LLA: %s (%.2f%%)\n",
            format(nrow(mesas_other_only), big.mark = ","),
            100 * nrow(mesas_other_only) / total_mesas))

# Create summary table
results <- data.frame(
  total_mesas = total_mesas,
  zero_all_parties = mesas_zero_all,
  zero_fp = mesas_zero_fp,
  zero_lla = mesas_zero_lla,
  zero_both_fp_lla_but_has_votes = nrow(mesas_other_only),
  pct_zero_all = 100 * mesas_zero_all / total_mesas,
  pct_zero_fp = 100 * mesas_zero_fp / total_mesas,
  pct_zero_lla = 100 * mesas_zero_lla / total_mesas,
  pct_zero_both_but_has_votes = 100 * nrow(mesas_other_only) / total_mesas
)

# Save results
fwrite(results, "data/zero_votes_analysis.csv")
cat("\n=== Summary Table ===\n")
print(results)
cat("\nResults saved to data/zero_votes_analysis.csv\n")

cat("\n=== Done! ===\n")
