library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)

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

# Calculate votes per mesa for each party
cat("\nCalculating votes per mesa for each party...\n")
mesa_votes <- october_all %>%
  group_by(distrito_id, seccion_id, mesa_id, party_std) %>%
  summarise(
    votes = sum(votos_cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# Get total unique mesas
total_mesas <- october_all %>%
  select(distrito_id, seccion_id, mesa_id) %>%
  distinct() %>%
  nrow()

cat(sprintf("Total unique mesas: %s\n", format(total_mesas, big.mark = ",")))

# Function to create histogram for a specific vote range
create_histogram <- function(data, vote_min, vote_max, title_suffix) {
  filtered_data <- data %>%
    filter(votes >= vote_min & votes <= vote_max)
  
  n_mesas <- filtered_data %>%
    select(distrito_id, seccion_id, mesa_id) %>%
    distinct() %>%
    nrow()
  
  p <- ggplot(filtered_data, aes(x = votes, fill = party_std)) +
    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
    facet_wrap(~party_std, scales = "free_y", ncol = 1) +
    labs(
      title = paste0("Distribución de votos por mesa - ", title_suffix),
      subtitle = sprintf("%s mesas con votos en rango [%d, %d]", 
                        format(n_mesas, big.mark = ","), vote_min, vote_max),
      x = "Votos por mesa",
      y = "Frecuencia (número de mesas)"
    ) +
    scale_fill_manual(values = c(
      "Fuerza Patria" = "#4285F4",
      "La Libertad Avanza" = "#9333EA",
      "EN BLANCO" = "#808080",
      "OTROS" = "#FFA500"
    )) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10)
    )
  
  return(list(plot = p, n_mesas = n_mesas))
}

# Create histograms for different vote ranges
cat("\nCreating histograms...\n")

# 1. All mesas
cat("1. All vote ranges...\n")
h1 <- create_histogram(mesa_votes, 0, max(mesa_votes$votes), "Todos los votos")
ggsave("histogram_all_votes.png", h1$plot, width = 10, height = 12, dpi = 300)

# 2. Only 0 votes
cat("2. Zero votes only...\n")
h2 <- create_histogram(mesa_votes, 0, 0, "0 votos")
ggsave("histogram_0_votes.png", h2$plot, width = 10, height = 12, dpi = 300)

# 3. 1-10 votes
cat("3. 1-10 votes...\n")
h3 <- create_histogram(mesa_votes, 1, 10, "1-10 votos")
ggsave("histogram_1_10_votes.png", h3$plot, width = 10, height = 12, dpi = 300)

# 4. 11-20 votes
cat("4. 11-20 votes...\n")
h4 <- create_histogram(mesa_votes, 11, 20, "11-20 votos")
ggsave("histogram_11_20_votes.png", h4$plot, width = 10, height = 12, dpi = 300)

# Summary statistics
cat("\n=== Summary Statistics by Party ===\n")
summary_stats <- mesa_votes %>%
  group_by(party_std) %>%
  summarise(
    total_mesas_with_votes = n(),
    mesas_with_0_votes = sum(votes == 0),
    mesas_1_10_votes = sum(votes >= 1 & votes <= 10),
    mesas_11_20_votes = sum(votes >= 11 & votes <= 20),
    mesas_21plus_votes = sum(votes > 20),
    mean_votes = mean(votes, na.rm = TRUE),
    median_votes = median(votes, na.rm = TRUE),
    max_votes = max(votes, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# Save summary
fwrite(summary_stats, "data/vote_distribution_summary.csv")

cat("\n=== Histograms saved ===\n")
cat("- histogram_all_votes.png (all vote ranges)\n")
cat("- histogram_0_votes.png (0 votes only)\n")
cat("- histogram_1_10_votes.png (1-10 votes)\n")
cat("- histogram_11_20_votes.png (11-20 votes)\n")
cat("\nSummary statistics saved to data/vote_distribution_summary.csv\n")

cat("\n=== Done! ===\n")
