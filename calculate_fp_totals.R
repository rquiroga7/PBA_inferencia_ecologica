library(data.table)
library(dplyr)

# Load data
sept <- fread('data/resultados_Sept2025.csv')
oct <- fread('data/resultados_Oct2025.csv') %>% 
  filter(distrito_id == 2, cargo_nombre == 'DIPUTADO NACIONAL')

# Standardize party names
standardize_party <- function(party_name) {
  ifelse(grepl('FUERZA PATRIA|FRENTE RENOVADOR|UNION POR LA PATRIA', party_name, ignore.case = TRUE), 
         'Fuerza Patria',
         ifelse(grepl('LA LIBERTAD AVANZA|MILEI', party_name, ignore.case = TRUE), 
                'La Libertad Avanza',
                ifelse(grepl('BLANCO', party_name, ignore.case = TRUE), 
                       'EN BLANCO', 
                       'OTROS')))
}

# Note: September has 'partido' and 'votos', October has 'agrupacion_nombre' and 'votos_cantidad'
sept$party_std <- standardize_party(sept$partido)
oct$party_std <- standardize_party(oct$agrupacion_nombre)

# Filter September data
sept_filt <- sept %>% filter(mesa_id < 9000)

# Calculate Fuerza Patria totals
fp_sept_filt <- sept_filt %>% 
  filter(party_std == 'Fuerza Patria') %>% 
  summarise(total = sum(votos))

fp_sept_unfilt <- sept %>% 
  filter(party_std == 'Fuerza Patria') %>% 
  summarise(total = sum(votos))

fp_oct <- oct %>% 
  filter(party_std == 'Fuerza Patria') %>% 
  summarise(total = sum(votos_cantidad))

# Print results
cat('\n=== FUERZA PATRIA VOTE TOTALS ===\n')
cat('September (filtered, mesa_id < 9000):', format(fp_sept_filt$total, big.mark = ','), '\n')
cat('September (unfiltered, all mesas):   ', format(fp_sept_unfilt$total, big.mark = ','), '\n')
cat('October (DIPUTADO NACIONAL):        ', format(fp_oct$total, big.mark = ','), '\n')
cat('\n=== DIFFERENCES ===\n')
cat('Foreign votes (mesa_id >= 9000):    ', format(fp_sept_unfilt$total - fp_sept_filt$total, big.mark = ','), '\n')
cat('Difference (Oct - Sept filtered):   ', format(fp_oct$total - fp_sept_filt$total, big.mark = ','), '\n')
cat('Percentage change (filtered):       ', round(100 * (fp_oct$total - fp_sept_filt$total) / fp_sept_filt$total, 2), '%\n')
