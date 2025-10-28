library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ============================================================================
# PART 1: Load September 2025 data (pre-processed CSV from JSONL)
# ============================================================================

cat("Loading September 2025 data from pre-processed CSV...\n")
september_df <- fread("data/resultados_Sept2025.csv")

cat(sprintf("Loaded %d rows from September data\n", nrow(september_df)))
cat(sprintf("Unique mesas: %d\n", length(unique(september_df$mesa_id))))
cat(sprintf("Unique secciones: %d\n", length(unique(september_df$seccion_id))))

# Check mesa_id distribution
cat(sprintf("Mesa IDs >= 9000: %d\n", sum(september_df$mesa_id >= 9000, na.rm = TRUE)))
cat(sprintf("Mesa IDs < 9000: %d\n", sum(september_df$mesa_id < 9000, na.rm = TRUE)))

# Filter out foreign voters (mesa_id >= 9000)
september_filtered <- september_df %>%
  filter(mesa_id < 9000)

cat(sprintf("\nAfter filtering mesa_id < 9000: %d rows (removed %d rows)\n", 
            nrow(september_filtered), nrow(september_df) - nrow(september_filtered)))

# Create unfiltered version for comparison
september_unfiltered <- september_df

# ============================================================================
# PART 2: Load and process October 2025 data (resultados_Oct2025.csv)
# ============================================================================

cat("\nLoading October 2025 data from CSV...\n")
october_raw <- fread("data/resultados_Oct2025.csv")
cat(sprintf("Loaded %d rows from October raw data\n", nrow(october_raw)))

# Filter for distrito_id == 2 (Buenos Aires Province) AND cargo_nombre == "DIPUTADO NACIONAL"
october_raw <- october_raw %>%
  filter(distrito_id == 2, cargo_nombre == "DIPUTADO NACIONAL")

cat(sprintf("After filtering distrito_id == 2 AND cargo == DIPUTADO NACIONAL: %d rows\n", nrow(october_raw)))

# Process October data - extract relevant fields and standardize
october_df <- october_raw %>%
  mutate(
    partido = ifelse(votos_tipo == "POSITIVO", agrupacion_nombre, votos_tipo),
    votos = votos_cantidad,
    mesa_id = mesa_id,
    seccion_id = seccion_id,
    circuito_id = circuito_id,
    cantidadElectores = mesa_electores
  ) %>%
  select(mesa_id, seccion_id, circuito_id, partido, votos, cantidadElectores)

cat(sprintf("Processed October data: %d rows\n", nrow(october_df)))

# ============================================================================
# PART 3: Standardize party names across datasets
# ============================================================================

# Function to standardize party names (vectorized)
standardize_party <- function(party_names) {
  # Convert to uppercase and trim whitespace
  parties <- toupper(trimws(as.character(party_names)))
  
  # Initialize result vector
  result <- rep("OTROS", length(parties))
  
  # Map to standard names
  result[grepl("FUERZA PATRIA|ALIANZA FUERZA PATRIA", parties)] <- "Fuerza Patria"
  result[grepl("LIBERTAD AVANZA|ALIANZA LA LIBERTAD AVANZA", parties)] <- "La Libertad Avanza"
  result[grepl("^BLANCO$|^EN BLANCO$|NULO|IMPUGNADO|RECURRIDO", parties)] <- "EN BLANCO"
  
  return(result)
}

# Apply standardization to both datasets
cat("\nStandardizing party names...\n")

september_filtered <- september_filtered %>%
  mutate(partido_std = standardize_party(partido))

september_unfiltered <- september_unfiltered %>%
  mutate(partido_std = standardize_party(partido))

october_df <- october_df %>%
  mutate(partido_std = standardize_party(partido))

# ============================================================================
# PART 4: Aggregate at seccion_id level
# ============================================================================

cat("\nAggregating data at seccion_id level...\n")

# September filtered aggregation by seccion
september_agg_filtered <- september_filtered %>%
  group_by(seccion_id, partido_std) %>%
  summarise(votos = sum(as.numeric(votos), na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

# September unfiltered aggregation by seccion
september_agg_unfiltered <- september_unfiltered %>%
  group_by(seccion_id, partido_std) %>%
  summarise(votos = sum(as.numeric(votos), na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

# October aggregation by seccion
october_agg <- october_df %>%
  group_by(seccion_id, partido_std) %>%
  summarise(votos = sum(as.numeric(votos), na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

cat(sprintf("September filtered aggregated: %d rows (%d secciones)\n", 
            nrow(september_agg_filtered), length(unique(september_agg_filtered$seccion_id))))
cat(sprintf("September unfiltered aggregated: %d rows (%d secciones)\n", 
            nrow(september_agg_unfiltered), length(unique(september_agg_unfiltered$seccion_id))))
cat(sprintf("October aggregated: %d rows (%d secciones)\n", 
            nrow(october_agg), length(unique(october_agg$seccion_id))))

# ============================================================================
# PART 5: Calculate fractions for Fuerza Patria and La Libertad Avanza
# ============================================================================

# Function to calculate party fraction
calc_party_fraction <- function(data, party_name, seccion_col = "seccion_id") {
  # Calculate total positive votes per seccion
  positive_votes <- data %>%
    filter(partido_std != "EN BLANCO") %>%
    group_by(across(all_of(seccion_col))) %>%
    summarise(total_positive = sum(votos, na.rm = TRUE), .groups = "drop")
  
  # Get party votes
  party_votes <- data %>%
    filter(partido_std == party_name) %>%
    select(all_of(seccion_col), votos_party = votos)
  
  # Join and calculate fraction
  result <- positive_votes %>%
    left_join(party_votes, by = seccion_col) %>%
    mutate(
      votos_party = ifelse(is.na(votos_party), 0, votos_party),
      fraction = ifelse(total_positive > 0, votos_party / total_positive, 0)
    ) %>%
    select(all_of(seccion_col), fraction, votos_party, total_positive)
  
  return(result)
}

cat("\nCalculating party fractions...\n")

# Calculate Fuerza Patria fractions
fp_september_filtered <- calc_party_fraction(september_agg_filtered, "Fuerza Patria") %>%
  rename(fp_fraction_sept_filt = fraction,
         fp_votes_sept_filt = votos_party,
         total_sept_filt = total_positive)

fp_september_unfiltered <- calc_party_fraction(september_agg_unfiltered, "Fuerza Patria") %>%
  rename(fp_fraction_sept_unfilt = fraction,
         fp_votes_sept_unfilt = votos_party,
         total_sept_unfilt = total_positive)

fp_october <- calc_party_fraction(october_agg, "Fuerza Patria") %>%
  rename(fp_fraction_oct = fraction,
         fp_votes_oct = votos_party,
         total_oct = total_positive)

# Calculate La Libertad Avanza fractions
lla_september_filtered <- calc_party_fraction(september_agg_filtered, "La Libertad Avanza") %>%
  rename(lla_fraction_sept_filt = fraction,
         lla_votes_sept_filt = votos_party)

lla_september_unfiltered <- calc_party_fraction(september_agg_unfiltered, "La Libertad Avanza") %>%
  rename(lla_fraction_sept_unfilt = fraction,
         lla_votes_sept_unfilt = votos_party)

lla_october <- calc_party_fraction(october_agg, "La Libertad Avanza") %>%
  rename(lla_fraction_oct = fraction,
         lla_votes_oct = votos_party)

# ============================================================================
# PART 6: Merge datasets for plotting
# ============================================================================

cat("\nMerging datasets for comparison...\n")

# Get seccion names from October data
seccion_names <- october_raw %>%
  select(seccion_id, seccion_nombre) %>%
  distinct()

# Merge Fuerza Patria data
fp_comparison <- fp_september_filtered %>%
  full_join(fp_october, by = "seccion_id") %>%
  full_join(fp_september_unfiltered %>% 
              select(seccion_id, fp_fraction_sept_unfilt, fp_votes_sept_unfilt, total_sept_unfilt), 
            by = "seccion_id") %>%
  left_join(seccion_names, by = "seccion_id")

# Merge La Libertad Avanza data
lla_comparison <- lla_september_filtered %>%
  select(seccion_id, lla_fraction_sept_filt, lla_votes_sept_filt) %>%
  full_join(lla_october %>% select(seccion_id, lla_fraction_oct, lla_votes_oct), 
            by = "seccion_id") %>%
  full_join(lla_september_unfiltered %>% 
              select(seccion_id, lla_fraction_sept_unfilt, lla_votes_sept_unfilt), 
            by = "seccion_id") %>%
  left_join(seccion_names, by = "seccion_id")

cat(sprintf("Fuerza Patria comparison: %d secciones\n", nrow(fp_comparison)))
cat(sprintf("La Libertad Avanza comparison: %d secciones\n", nrow(lla_comparison)))

# Filter for secciones with more than 50,000 total positive votes
cat("\nFiltering for secciones with > 50,000 total positive votes...\n")
fp_comparison_plot <- fp_comparison %>%
  filter(total_oct > 50000)

lla_comparison_plot <- lla_comparison %>%
  left_join(fp_comparison %>% select(seccion_id, total_oct), by = "seccion_id") %>%
  filter(total_oct > 50000)

cat(sprintf("Secciones after filtering (> 50k votes): %d\n", nrow(fp_comparison_plot)))

# ============================================================================
# PART 7: Create plots - VOTE DIFFERENCES AS PERCENTAGE OF OCTOBER VOTES
# ============================================================================

cat("Creating plots...\n")

# Calculate vote differences as percentage of October votes
fp_comparison <- fp_comparison %>%
  mutate(
    pct_diff_filtered = ifelse(fp_votes_oct > 0, (fp_votes_oct - fp_votes_sept_filt) / fp_votes_oct, NA),
    pct_diff_unfiltered = ifelse(fp_votes_oct > 0, (fp_votes_oct - fp_votes_sept_unfilt) / fp_votes_oct, NA)
  )

lla_comparison <- lla_comparison %>%
  mutate(
    pct_diff_filtered = ifelse(lla_votes_oct > 0, (lla_votes_oct - lla_votes_sept_filt) / lla_votes_oct, NA),
    pct_diff_unfiltered = ifelse(lla_votes_oct > 0, (lla_votes_oct - lla_votes_sept_unfilt) / lla_votes_oct, NA)
  )

# Also calculate for the plot-filtered datasets
fp_comparison_plot <- fp_comparison_plot %>%
  mutate(
    pct_diff_filtered = ifelse(fp_votes_oct > 0, (fp_votes_oct - fp_votes_sept_filt) / fp_votes_oct, NA),
    pct_diff_unfiltered = ifelse(fp_votes_oct > 0, (fp_votes_oct - fp_votes_sept_unfilt) / fp_votes_oct, NA)
  )

lla_comparison_plot <- lla_comparison_plot %>%
  mutate(
    pct_diff_filtered = ifelse(lla_votes_oct > 0, (lla_votes_oct - lla_votes_sept_filt) / lla_votes_oct, NA),
    pct_diff_unfiltered = ifelse(lla_votes_oct > 0, (lla_votes_oct - lla_votes_sept_unfilt) / lla_votes_oct, NA)
  )

# Determine common scales for all plots
# For fraction plots (x-axis: 0.25 to 0.55, y-axis: common for pct_diff)
x_fraction_limits <- c(0.25, 0.55)
y_pct_diff_limits <- range(c(fp_comparison_plot$pct_diff_filtered, 
                              fp_comparison_plot$pct_diff_unfiltered,
                              lla_comparison_plot$pct_diff_filtered, 
                              lla_comparison_plot$pct_diff_unfiltered), na.rm = TRUE)
# Add some padding
y_pct_diff_limits <- c(floor(y_pct_diff_limits[1] * 10) / 10, 
                       ceiling(y_pct_diff_limits[2] * 10) / 10)

# For absolute vote plots (both parties on same scale)
all_sept_votes <- c(fp_comparison_plot$fp_votes_sept_filt, lla_comparison_plot$lla_votes_sept_filt)
all_oct_votes <- c(fp_comparison_plot$fp_votes_oct, lla_comparison_plot$lla_votes_oct)
xy_absolute_limits <- c(0, max(c(all_sept_votes, all_oct_votes), na.rm = TRUE))

cat("Common scales:\n")
cat("  Fraction x-axis:", x_fraction_limits[1], "to", x_fraction_limits[2], "\n")
cat("  Pct diff y-axis:", y_pct_diff_limits[1], "to", y_pct_diff_limits[2], "\n")
cat("  Absolute votes:", xy_absolute_limits[1], "to", xy_absolute_limits[2], "\n")

# Plot 1: Fuerza Patria - Percentage Difference (filtered) vs October Fraction
p1 <- ggplot(fp_comparison_plot %>% filter(!is.na(pct_diff_filtered) & !is.na(fp_fraction_oct)), 
             aes(x = fp_fraction_oct, y = pct_diff_filtered)) +
  geom_point(alpha = 0.6, size = 3, color = "#2E86AB") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1, alpha = 0.2) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "Fuerza Patria: Cambio de Votos (Oct - Sept) / Oct vs Fracción de Votos en Octubre",
    subtitle = "Secciones con > 50,000 votos totales | Septiembre - sin votos extranjeros",
    x = "Fracción de Votos en Octubre (de votos positivos)",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/fp_pct_diff_filtered_vs_oct_fraction.png", p1, width = 10, height = 8, dpi = 300)
print(p1)

# Plot 2: Fuerza Patria - Percentage Difference (unfiltered) vs October Fraction
p2 <- ggplot(fp_comparison_plot %>% filter(!is.na(pct_diff_unfiltered) & !is.na(fp_fraction_oct)), 
             aes(x = fp_fraction_oct, y = pct_diff_unfiltered)) +
  geom_point(alpha = 0.6, size = 3, color = "#A23B72") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1, alpha = 0.2) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "Fuerza Patria: Cambio de Votos (Oct - Sept) / Oct vs Fracción de Votos en Octubre",
    subtitle = "Secciones con > 50,000 votos totales | Septiembre - votos totales",
    x = "Fracción de Votos en Octubre (de votos positivos)",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/fp_pct_diff_unfiltered_vs_oct_fraction.png", p2, width = 10, height = 8, dpi = 300)
print(p2)

# Plot 3: La Libertad Avanza - Percentage Difference (filtered) vs October Fraction
p3 <- ggplot(lla_comparison_plot %>% filter(!is.na(pct_diff_filtered) & !is.na(lla_fraction_oct)), 
             aes(x = lla_fraction_oct, y = pct_diff_filtered)) +
  geom_point(alpha = 0.6, size = 3, color = "#F18F01") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1, alpha = 0.2) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "La Libertad Avanza: Cambio de Votos (Oct - Sept) / Oct vs Fracción de Votos en Octubre",
    subtitle = "Secciones con > 50,000 votos totales | Septiembre - sin votos extranjeros",
    x = "Fracción de Votos en Octubre (de votos positivos)",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/lla_pct_diff_filtered_vs_oct_fraction.png", p3, width = 10, height = 8, dpi = 300)
print(p3)

# Plot 4: La Libertad Avanza - Percentage Difference (unfiltered) vs October Fraction
p4 <- ggplot(lla_comparison_plot %>% filter(!is.na(pct_diff_unfiltered) & !is.na(lla_fraction_oct)), 
             aes(x = lla_fraction_oct, y = pct_diff_unfiltered)) +
  geom_point(alpha = 0.6, size = 3, color = "#C73E1D") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1, alpha = 0.2) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "La Libertad Avanza: Cambio de Votos (Oct - Sept) / Oct vs Fracción de Votos en Octubre",
    subtitle = "Secciones con > 50,000 votos totales | Septiembre - votos totales",
    x = "Fracción de Votos en Octubre (de votos positivos)",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/lla_pct_diff_unfiltered_vs_oct_fraction.png", p4, width = 10, height = 8, dpi = 300)
print(p4)

# Additional plots: Comparison of absolute votes (original request)
# Plot 5: Fuerza Patria - Filtered September vs October (ABSOLUTE VOTES)
p5 <- ggplot(fp_comparison_plot %>% filter(!is.na(fp_votes_sept_filt) & !is.na(fp_votes_oct)), 
             aes(x = fp_votes_sept_filt, y = fp_votes_oct)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = xy_absolute_limits) +
  scale_y_continuous(limits = xy_absolute_limits) +
  labs(
    title = "Fuerza Patria: Septiembre vs Octubre 2025",
    subtitle = "Secciones con > 50,000 votos totales | Conteo absoluto de votos | Septiembre - sin votos extranjeros",
    x = "Votos en Septiembre",
    y = "Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  coord_fixed(ratio = 1)

ggsave("plots/fp_sept_filtered_vs_oct.png", p5, width = 10, height = 8, dpi = 300)
print(p5)

# Plot 6: La Libertad Avanza - Filtered September vs October (ABSOLUTE VOTES)
p6 <- ggplot(lla_comparison_plot %>% filter(!is.na(lla_votes_sept_filt) & !is.na(lla_votes_oct)), 
             aes(x = lla_votes_sept_filt, y = lla_votes_oct)) +
  geom_point(alpha = 0.5, size = 2, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = xy_absolute_limits) +
  scale_y_continuous(limits = xy_absolute_limits) +
  labs(
    title = "La Libertad Avanza: Septiembre vs Octubre 2025",
    subtitle = "Secciones con > 50,000 votos totales | Conteo absoluto de votos | Septiembre - sin votos extranjeros",
    x = "Votos en Septiembre",
    y = "Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  coord_fixed(ratio = 1)

ggsave("plots/lla_sept_filtered_vs_oct.png", p6, width = 10, height = 8, dpi = 300)
print(p6)

# NEW: Plot 7 - Fuerza Patria Paired Dots (Filtered vs Unfiltered) with Labels
fp_paired <- fp_comparison_plot %>%
  filter(!is.na(pct_diff_filtered) & !is.na(pct_diff_unfiltered) & !is.na(fp_fraction_oct)) %>%
  select(seccion_id, seccion_nombre, fp_fraction_oct, pct_diff_filtered, pct_diff_unfiltered)

p7 <- ggplot(fp_paired) +
  geom_segment(aes(x = fp_fraction_oct, xend = fp_fraction_oct, 
                   y = pct_diff_filtered, yend = pct_diff_unfiltered),
               alpha = 0.3, linewidth = 0.5, color = "gray50") +
  geom_point(aes(x = fp_fraction_oct, y = pct_diff_unfiltered), 
             color = "black", fill = "white", size = 2, alpha = 0.8, shape = 21, stroke = 1) +
  geom_point(aes(x = fp_fraction_oct, y = pct_diff_filtered), 
             color = "#2E86AB", size = 3, alpha = 0.7) +
  geom_text_repel(aes(x = fp_fraction_oct, y = pct_diff_filtered, label = seccion_nombre),
                  size = 2.5, max.overlaps = 15, box.padding = 0.5, 
                  segment.size = 0.2, segment.alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "Fuerza Patria: Diferencia de votos Octubre - Septiembre para votos totales o restando extranjeros",
    subtitle = "Azul (sólido) = sin votos extranjeros, Contorno negro (hueco) = votos totales | Las líneas conectan secciones pareadas",
    x = "Fracción de Votos en Octubre",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/fp_paired_filtered_unfiltered.png", p7, width = 12, height = 10, dpi = 300)
print(p7)

# NEW: Plot 8 - La Libertad Avanza Paired Dots (Filtered vs Unfiltered) with Labels
lla_paired <- lla_comparison_plot %>%
  filter(!is.na(pct_diff_filtered) & !is.na(pct_diff_unfiltered) & !is.na(lla_fraction_oct)) %>%
  select(seccion_id, seccion_nombre, lla_fraction_oct, pct_diff_filtered, pct_diff_unfiltered)

p8 <- ggplot(lla_paired) +
  geom_segment(aes(x = lla_fraction_oct, xend = lla_fraction_oct, 
                   y = pct_diff_filtered, yend = pct_diff_unfiltered),
               alpha = 0.3, linewidth = 0.5, color = "gray50") +
  geom_point(aes(x = lla_fraction_oct, y = pct_diff_unfiltered), 
             color = "black", fill = "white", size = 2, alpha = 0.8, shape = 21, stroke = 1) +
  geom_point(aes(x = lla_fraction_oct, y = pct_diff_filtered), 
             color = "#F18F01", size = 3, alpha = 0.7) +
  geom_text_repel(aes(x = lla_fraction_oct, y = pct_diff_filtered, label = seccion_nombre),
                  size = 2.5, max.overlaps = 15, box.padding = 0.5, 
                  segment.size = 0.2, segment.alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(limits = x_fraction_limits) +
  scale_y_continuous(limits = y_pct_diff_limits) +
  labs(
    title = "La Libertad Avanza: Diferencia de votos Octubre - Septiembre para votos totales o restando extranjeros",
    subtitle = "Naranja (sólido) = sin votos extranjeros, Contorno negro (hueco) = votos totales | Las líneas conectan secciones pareadas",
    x = "Fracción de Votos en Octubre",
    y = "(Octubre - Septiembre) / Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9))

ggsave("plots/lla_paired_filtered_unfiltered.png", p8, width = 12, height = 10, dpi = 300)
print(p8)

# ============================================================================
# PART 8: Save comparison datasets
# ============================================================================

cat("Saving comparison datasets...\n")

fwrite(fp_comparison, "data/fp_comparison_sept_oct_2025.csv")
fwrite(lla_comparison, "data/lla_comparison_sept_oct_2025.csv")

cat("\n=== Summary Statistics ===\n")
cat("\nFuerza Patria:\n")
cat("Secciones in September (filtered):", sum(!is.na(fp_comparison$fp_fraction_sept_filt)), "\n")
cat("Secciones in September (unfiltered):", sum(!is.na(fp_comparison$fp_fraction_sept_unfilt)), "\n")
cat("Secciones in October:", sum(!is.na(fp_comparison$fp_fraction_oct)), "\n")
cat("Secciones in both (filtered):", sum(!is.na(fp_comparison$fp_fraction_sept_filt) & !is.na(fp_comparison$fp_fraction_oct)), "\n")

cat("\nLa Libertad Avanza:\n")
cat("Secciones in September (filtered):", sum(!is.na(lla_comparison$lla_fraction_sept_filt)), "\n")
cat("Secciones in September (unfiltered):", sum(!is.na(lla_comparison$lla_fraction_sept_unfilt)), "\n")
cat("Secciones in October:", sum(!is.na(lla_comparison$lla_fraction_oct)), "\n")
cat("Secciones in both (filtered):", sum(!is.na(lla_comparison$lla_fraction_sept_filt) & !is.na(lla_comparison$lla_fraction_oct)), "\n")

cat("\n=== Done! ===\n")
cat("Plots saved in plots/ directory\n")
cat("Comparison datasets saved in data/ directory\n")
