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

cat("\nAnalyzing zero votes by district...\n")

# Calculate votes per mesa for each party
mesa_party_votes <- october_all %>%
  group_by(distrito_id, distrito_nombre, seccion_id, mesa_id, party_std) %>%
  summarise(
    votes = sum(votos_cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# Count total mesas per district
total_mesas_per_district <- mesa_party_votes %>%
  select(distrito_id, distrito_nombre, seccion_id, mesa_id) %>%
  distinct() %>%
  group_by(distrito_id, distrito_nombre) %>%
  summarise(total_mesas = n(), .groups = "drop")

# Count zero-vote mesas by district and party
zero_votes_by_district <- mesa_party_votes %>%
  filter(votes == 0) %>%
  group_by(distrito_id, distrito_nombre, party_std) %>%
  summarise(zero_vote_mesas = n(), .groups = "drop")

# Reshape to wide format with one column per party using dcast
zero_votes_wide <- dcast(
  as.data.table(zero_votes_by_district),
  distrito_id + distrito_nombre ~ party_std,
  value.var = "zero_vote_mesas",
  fill = 0
)

# Join with total mesas
result <- total_mesas_per_district %>%
  left_join(as.data.frame(zero_votes_wide), by = c("distrito_id", "distrito_nombre")) %>%
  arrange(distrito_nombre)

# Reorder columns to have FP and LLA first
col_order <- c("distrito_id", "distrito_nombre", "total_mesas", 
               "Fuerza Patria", "La Libertad Avanza", "EN BLANCO", "OTROS")
# Only include columns that exist
col_order <- col_order[col_order %in% names(result)]
result <- result[, col_order]

# Add percentages
if ("Fuerza Patria" %in% names(result)) {
  result$pct_zero_FP <- round(100 * result$`Fuerza Patria` / result$total_mesas, 2)
}
if ("La Libertad Avanza" %in% names(result)) {
  result$pct_zero_LLA <- round(100 * result$`La Libertad Avanza` / result$total_mesas, 2)
}

# Add totals row
totals <- data.frame(
  distrito_id = NA,
  distrito_nombre = "TOTAL",
  total_mesas = sum(result$total_mesas),
  stringsAsFactors = FALSE
)

if ("Fuerza Patria" %in% names(result)) {
  totals$`Fuerza Patria` <- sum(result$`Fuerza Patria`)
}
if ("La Libertad Avanza" %in% names(result)) {
  totals$`La Libertad Avanza` <- sum(result$`La Libertad Avanza`)
}
if ("EN BLANCO" %in% names(result)) {
  totals$`EN BLANCO` <- sum(result$`EN BLANCO`)
}
if ("OTROS" %in% names(result)) {
  totals$OTROS <- sum(result$OTROS)
}
if ("pct_zero_FP" %in% names(result)) {
  totals$pct_zero_FP <- round(100 * totals$`Fuerza Patria` / totals$total_mesas, 2)
}
if ("pct_zero_LLA" %in% names(result)) {
  totals$pct_zero_LLA <- round(100 * totals$`La Libertad Avanza` / totals$total_mesas, 2)
}

result_with_totals <- bind_rows(result, totals)

# Save results
fwrite(result_with_totals, "data/zero_votes_by_district.csv")

cat("\n=== Zero-vote mesas by district ===\n")
print(result_with_totals, n = 30)

cat("\n=== Summary ===\n")
cat(sprintf("Total districts: %d\n", nrow(result)))
cat(sprintf("Total mesas: %s\n", format(sum(result$total_mesas), big.mark = ",")))

if ("Fuerza Patria" %in% names(result)) {
  cat(sprintf("Total zero-vote mesas for Fuerza Patria: %s (%.2f%%)\n",
              format(sum(result$`Fuerza Patria`), big.mark = ","),
              100 * sum(result$`Fuerza Patria`) / sum(result$total_mesas)))
  
  # Districts with highest percentage of zero-vote mesas for FP
  top_fp <- result %>%
    arrange(desc(pct_zero_FP)) %>%
    head(5)
  cat("\nTop 5 districts with highest % of zero-vote mesas for Fuerza Patria:\n")
  print(top_fp[, c("distrito_nombre", "total_mesas", "Fuerza Patria", "pct_zero_FP")])
}

if ("La Libertad Avanza" %in% names(result)) {
  cat(sprintf("\nTotal zero-vote mesas for La Libertad Avanza: %s (%.2f%%)\n",
              format(sum(result$`La Libertad Avanza`), big.mark = ","),
              100 * sum(result$`La Libertad Avanza`) / sum(result$total_mesas)))
  
  # Districts with highest percentage of zero-vote mesas for LLA
  top_lla <- result %>%
    arrange(desc(pct_zero_LLA)) %>%
    head(5)
  cat("\nTop 5 districts with highest % of zero-vote mesas for La Libertad Avanza:\n")
  print(top_lla[, c("distrito_nombre", "total_mesas", "La Libertad Avanza", "pct_zero_LLA")])
}

cat("\nResults saved to data/zero_votes_by_district.csv\n")

# Export mesas with zero votes for both FP and LLA
cat("\n=== Finding mesas with zero votes for both FP and LLA ===\n")

# Get FP votes per mesa
fp_mesa_votes <- october_all %>%
  filter(party_std == "Fuerza Patria") %>%
  group_by(distrito_id, distrito_nombre, seccion_id, circuito_id, mesa_id) %>%
  summarise(fp_votes = sum(votos_cantidad, na.rm = TRUE), .groups = "drop")

# Get LLA votes per mesa
lla_mesa_votes <- october_all %>%
  filter(party_std == "La Libertad Avanza") %>%
  group_by(distrito_id, distrito_nombre, seccion_id, circuito_id, mesa_id) %>%
  summarise(lla_votes = sum(votos_cantidad, na.rm = TRUE), .groups = "drop")

# Get all unique mesas
all_mesas <- october_all %>%
  select(distrito_id, distrito_nombre, seccion_id, circuito_id, mesa_id) %>%
  distinct()

# Join and find mesas with zero for both
mesas_zero_both <- all_mesas %>%
  left_join(fp_mesa_votes, by = c("distrito_id", "distrito_nombre", "seccion_id", "circuito_id", "mesa_id")) %>%
  left_join(lla_mesa_votes, by = c("distrito_id", "distrito_nombre", "seccion_id", "circuito_id", "mesa_id")) %>%
  mutate(
    fp_votes = ifelse(is.na(fp_votes), 0, fp_votes),
    lla_votes = ifelse(is.na(lla_votes), 0, lla_votes)
  ) %>%
  filter(fp_votes == 0 & lla_votes == 0) %>%
  arrange(distrito_nombre, seccion_id, circuito_id, mesa_id)

cat(sprintf("Found %s mesas with zero votes for both FP and LLA\n", 
            format(nrow(mesas_zero_both), big.mark = ",")))

# Save to CSV
fwrite(mesas_zero_both, "data/mesas_zero_fp_and_lla.csv")

cat("\nFirst 20 mesas:\n")
print(head(mesas_zero_both, 20))

cat("\nBreakdown by district:\n")
breakdown <- mesas_zero_both %>%
  group_by(distrito_id, distrito_nombre) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))
print(breakdown)

cat("\nMesa details saved to data/mesas_zero_fp_and_lla.csv\n")

cat("\n=== Done! ===\n")
