library(data.table)
library(dplyr)
library(ggplot2)

# Load September data (already has filtered and unfiltered)
cat("Loading September 2025 mesa-level data...\n")
september_df <- fread("data/resultados_Sept2025.csv")

# Load October data
cat("Loading October 2025 data...\n")
october_raw <- fread("data/resultados_Oct2025.csv")
october_df <- october_raw %>%
  filter(distrito_id == 2, cargo_nombre == "DIPUTADO NACIONAL") %>%
  mutate(
    partido = ifelse(votos_tipo == "POSITIVO", agrupacion_nombre, votos_tipo),
    votos = votos_cantidad
  ) %>%
  select(mesa_id, seccion_id, partido, votos, votos_tipo)

# Standardize party names
standardize_party <- function(party_names) {
  parties <- toupper(trimws(as.character(party_names)))
  result <- rep("OTROS", length(parties))
  result[grepl("FUERZA PATRIA|ALIANZA FUERZA PATRIA", parties)] <- "Fuerza Patria"
  result[grepl("LIBERTAD AVANZA|ALIANZA LA LIBERTAD AVANZA", parties)] <- "La Libertad Avanza"
  result[grepl("^BLANCO$|^EN BLANCO$|NULO|IMPUGNADO|RECURRIDO", parties)] <- "EN BLANCO"
  return(result)
}

september_df$party_std <- standardize_party(september_df$partido)
october_df$party_std <- standardize_party(october_df$partido)

# Create filtered September data
september_filtered <- september_df %>% filter(mesa_id < 9000)

# Calculate totals and fractions at mesa level for each dataset
cat("Calculating mesa-level vote fractions...\n")

# September filtered - mesa level
sept_filt_mesa <- september_filtered %>%
  group_by(mesa_id, seccion_id) %>%
  summarise(
    total_positive = sum(votos[votos_tipo == "POSITIVO"]),
    fp_votes = sum(votos[party_std == "Fuerza Patria"]),
    lla_votes = sum(votos[party_std == "La Libertad Avanza"]),
    .groups = "drop"
  ) %>%
  mutate(
    fp_fraction = ifelse(total_positive > 0, fp_votes / total_positive, NA),
    lla_fraction = ifelse(total_positive > 0, lla_votes / total_positive, NA)
  )

# September unfiltered - mesa level
sept_unfilt_mesa <- september_df %>%
  group_by(mesa_id, seccion_id) %>%
  summarise(
    total_positive = sum(votos[votos_tipo == "POSITIVO"]),
    fp_votes = sum(votos[party_std == "Fuerza Patria"]),
    lla_votes = sum(votos[party_std == "La Libertad Avanza"]),
    .groups = "drop"
  ) %>%
  mutate(
    fp_fraction = ifelse(total_positive > 0, fp_votes / total_positive, NA),
    lla_fraction = ifelse(total_positive > 0, lla_votes / total_positive, NA)
  )

# October - mesa level
oct_mesa <- october_df %>%
  filter(votos_tipo == "POSITIVO" | party_std != "OTROS") %>%
  group_by(mesa_id, seccion_id) %>%
  summarise(
    total_positive = sum(votos),
    fp_votes = sum(votos[party_std == "Fuerza Patria"]),
    lla_votes = sum(votos[party_std == "La Libertad Avanza"]),
    .groups = "drop"
  ) %>%
  mutate(
    fp_fraction = ifelse(total_positive > 0, fp_votes / total_positive, NA),
    lla_fraction = ifelse(total_positive > 0, lla_votes / total_positive, NA)
  )

cat(sprintf("September filtered: %d mesas\n", nrow(sept_filt_mesa)))
cat(sprintf("September unfiltered: %d mesas\n", nrow(sept_unfilt_mesa)))
cat(sprintf("October: %d mesas\n", nrow(oct_mesa)))

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Common scales
x_limits <- c(0.25, 0.55)
y_limits <- c(0.25, 0.55)

# Plot 1: Fuerza Patria - Filtered September vs October (Mesa level heatmap)
cat("\nCreating Fuerza Patria mesa-level plots...\n")
fp_mesa_data <- sept_filt_mesa %>%
  select(mesa_id, seccion_id, fp_frac_sept = fp_fraction) %>%
  inner_join(
    oct_mesa %>% select(mesa_id, fp_frac_oct = fp_fraction),
    by = "mesa_id"
  ) %>%
  filter(!is.na(fp_frac_sept) & !is.na(fp_frac_oct))

p1 <- ggplot(fp_mesa_data, aes(x = fp_frac_sept, y = fp_frac_oct)) +
  geom_point(alpha = 0.02, size = 0.8, color = "#2E86AB") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits) +
  labs(
    title = "Fuerza Patria: Fracciones de Votos Septiembre vs Octubre (Nivel Mesa)",
    subtitle = sprintf("Cada punto = 1 mesa (n=%s) | Septiembre - sin votos extranjeros | Superposición transparente crea mapa de densidad", 
                      format(nrow(fp_mesa_data), big.mark = ",")),
    x = "Fracción de Votos en Septiembre",
    y = "Fracción de Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  coord_fixed(ratio = 1)

ggsave("plots/fp_mesa_filtered_vs_oct_heatmap.png", p1, width = 10, height = 10, dpi = 300)
print(p1)

# Plot 2: Fuerza Patria - Unfiltered September vs October
fp_mesa_unfilt_data <- sept_unfilt_mesa %>%
  select(mesa_id, seccion_id, fp_frac_sept = fp_fraction) %>%
  inner_join(
    oct_mesa %>% select(mesa_id, fp_frac_oct = fp_fraction),
    by = "mesa_id"
  ) %>%
  filter(!is.na(fp_frac_sept) & !is.na(fp_frac_oct))

p2 <- ggplot(fp_mesa_unfilt_data, aes(x = fp_frac_sept, y = fp_frac_oct)) +
  geom_point(alpha = 0.02, size = 0.8, color = "#A23B72") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits) +
  labs(
    title = "Fuerza Patria: Fracciones de Votos Septiembre vs Octubre (Nivel Mesa)",
    subtitle = sprintf("Cada punto = 1 mesa (n=%s) | Septiembre - votos totales | Superposición transparente crea mapa de densidad", 
                      format(nrow(fp_mesa_unfilt_data), big.mark = ",")),
    x = "Fracción de Votos en Septiembre",
    y = "Fracción de Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  coord_fixed(ratio = 1)

ggsave("plots/fp_mesa_unfiltered_vs_oct_heatmap.png", p2, width = 10, height = 10, dpi = 300)
print(p2)

# Plot 3: La Libertad Avanza - Filtered September vs October
cat("\nCreating La Libertad Avanza mesa-level plots...\n")
lla_mesa_data <- sept_filt_mesa %>%
  select(mesa_id, seccion_id, lla_frac_sept = lla_fraction) %>%
  inner_join(
    oct_mesa %>% select(mesa_id, lla_frac_oct = lla_fraction),
    by = "mesa_id"
  ) %>%
  filter(!is.na(lla_frac_sept) & !is.na(lla_frac_oct))

p3 <- ggplot(lla_mesa_data, aes(x = lla_frac_sept, y = lla_frac_oct)) +
  geom_point(alpha = 0.02, size = 0.8, color = "#F18F01") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits) +
  labs(
    title = "La Libertad Avanza: Fracciones de Votos Septiembre vs Octubre (Nivel Mesa)",
    subtitle = sprintf("Cada punto = 1 mesa (n=%s) | Septiembre - sin votos extranjeros | Superposición transparente crea mapa de densidad", 
                      format(nrow(lla_mesa_data), big.mark = ",")),
    x = "Fracción de Votos en Septiembre",
    y = "Fracción de Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  coord_fixed(ratio = 1)

ggsave("plots/lla_mesa_filtered_vs_oct_heatmap.png", p3, width = 10, height = 10, dpi = 300)
print(p3)

# Plot 4: La Libertad Avanza - Unfiltered September vs October
lla_mesa_unfilt_data <- sept_unfilt_mesa %>%
  select(mesa_id, seccion_id, lla_frac_sept = lla_fraction) %>%
  inner_join(
    oct_mesa %>% select(mesa_id, lla_frac_oct = lla_fraction),
    by = "mesa_id"
  ) %>%
  filter(!is.na(lla_frac_sept) & !is.na(lla_frac_oct))

p4 <- ggplot(lla_mesa_unfilt_data, aes(x = lla_frac_sept, y = lla_frac_oct)) +
  geom_point(alpha = 0.02, size = 0.8, color = "#C73E1D") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = x_limits) +
  scale_y_continuous(limits = y_limits) +
  labs(
    title = "La Libertad Avanza: Fracciones de Votos Septiembre vs Octubre (Nivel Mesa)",
    subtitle = sprintf("Cada punto = 1 mesa (n=%s) | Septiembre - votos totales | Superposición transparente crea mapa de densidad", 
                      format(nrow(lla_mesa_unfilt_data), big.mark = ",")),
    x = "Fracción de Votos en Septiembre",
    y = "Fracción de Votos en Octubre"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  coord_fixed(ratio = 1)

ggsave("plots/lla_mesa_unfiltered_vs_oct_heatmap.png", p4, width = 10, height = 10, dpi = 300)
print(p4)

cat("\n=== Done! ===\n")
cat("Mesa-level heatmap plots saved in plots/ directory\n")
