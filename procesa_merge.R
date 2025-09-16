library(data.table)
library(dplyr)

# Paths
votes_file <- "data/filteredPBAcargo56_2023.csv"
out_file <- "data/electores_circuitos_2023.csv"

# Read votes
votes <- fread(votes_file, encoding = "UTF-8", header = TRUE)

# Remove any potential header rows that may have been read as data
votes <- votes[seccionprovincial_id != "seccionprovincial_id"]

# Remove accents from seccion_nombre only
remove_accents <- function(x) {
  # Handle specific encoding issues first
  x <- gsub("Ã¡", "a", x)  # á
  x <- gsub("Ã©", "e", x)  # é
  x <- gsub("Ã­", "i", x)  # í
  x <- gsub("Ã³", "o", x)  # ó
  x <- gsub("Ãº", "u", x)  # ú
  x <- gsub("Ã±", "n", x)  # ñ
  # Then handle normal accents
  x <- gsub("á|à|â|ä|ã", "a", x, ignore.case = TRUE)
  x <- gsub("é|è|ê|ë", "e", x, ignore.case = TRUE)
  x <- gsub("í|ì|î|ï", "i", x, ignore.case = TRUE)
  x <- gsub("ó|ò|ô|ö|õ", "o", x, ignore.case = TRUE)
  x <- gsub("ú|ù|û|ü", "u", x, ignore.case = TRUE)
  x <- gsub("ñ", "n", x, ignore.case = TRUE)
  x <- gsub("ç", "c", x, ignore.case = TRUE)
  return(x)
}

# Apply accent removal only to seccion_nombre column
votes[, seccion_nombre := remove_accents(seccion_nombre)]

# Where votos_tipo == "POSITIVO", replace with agrupacion_nombre
votes[votos_tipo == "POSITIVO", votos_tipo := agrupacion_nombre]

# Remove agrupacion_nombre column
votes[, agrupacion_nombre := NULL]

names(votes)
# Rename columns
setnames(votes, old = c("seccionprovincial_id", "seccion_id", "seccion_nombre",
						"circuito_id", "circuito_nombre", "mesa_id",
						"mesa_electores", "cargo_id", "agrupacion_id",
 						"votos_tipo", "votos_cantidad"), 
         		new = c("secprov", "seccionId", "name", 
		 				"circuitoId", "circuitoNombre", "mesaId",
                		"mesaElectores", "cargoId", "agrupacionId",
                		"nombreAgrupacion", "votos"))

# Convert circuitoId to character for merging
votes[, circuitoId := as.character(circuitoId)]

# Convert votos to numeric
votes[, votos := as.numeric(votos)]
votes[, mesaElectores := as.numeric(mesaElectores)]

# Aggregate by circuitoId, summing votos and counting total rows as mesasTotalizadas
agg <- votes %>% group_by(secprov, seccionId, name, 
		 				circuitoId, nombreAgrupacion) %>% 
						summarise(votos = sum(votos, na.rm = TRUE), cantidadElectores= sum(mesaElectores,na.rm = TRUE), mesasTotalizadas = n())

#Now we need to calculate cantidadVotantes for each circuitoId, as well as noVotantes, which will be new factor levels for the nombreAgrupacion column 

# Calculate totals per circuitoId
totals <- agg %>% 
  group_by(secprov, seccionId, name, circuitoId, cantidadElectores) %>% 
  summarise(cantidadVotantes = sum(votos), 
            mesasTotalizadas_mode = as.numeric(names(sort(table(mesasTotalizadas), decreasing = TRUE))[1]),
            .groups = "drop") %>%
  mutate(noVotantes = cantidadElectores - cantidadVotantes)

# Create new rows for VOTANTES and NO_VOTANTES
votantes_rows <- totals %>%
  select(secprov, seccionId, name, circuitoId, cantidadElectores, mesasTotalizadas_mode) %>%
  mutate(nombreAgrupacion = "VOTANTES",
         votos = totals$cantidadVotantes,
         mesasTotalizadas = mesasTotalizadas_mode) %>%
  select(-mesasTotalizadas_mode)

no_votantes_rows <- totals %>%
  select(secprov, seccionId, name, circuitoId, cantidadElectores, mesasTotalizadas_mode) %>%
  mutate(nombreAgrupacion = "NO_VOTANTES", 
         votos = totals$noVotantes,
         mesasTotalizadas = mesasTotalizadas_mode) %>%
  select(-mesasTotalizadas_mode)

# Combine original data with new rows
agg <- bind_rows(agg, votantes_rows, no_votantes_rows)

agg$nombreAgrupacion <- as.factor(agg$nombreAgrupacion)
#Rename 
levels(agg$nombreAgrupacion) 

#Rename level "JUNTOS POR EL CAMBIO" to JxC, "FRENTE DE TODOS" to FdT, "VOTANTES" to VOTANTES, "NO_VOTANTES" to NO_VOTANTES, "LA LIBERTAD AVANZA" to LLA.
levels(agg$nombreAgrupacion)[levels(agg$nombreAgrupacion) == "JUNTOS POR EL CAMBIO"] <- "JxC"
levels(agg$nombreAgrupacion)[levels(agg$nombreAgrupacion) == "UNION POR LA PATRIA"] <- "UxP"
levels(agg$nombreAgrupacion)[levels(agg$nombreAgrupacion) == "LA LIBERTAD AVANZA"] <- "LLA"

agg$nombreAgrupacion <- as.character(agg$nombreAgrupacion)
#group all levels that are not "EN BLANCO", "NO_VOTANTES", JxC, FdT, LLA into "OTROS"
agg$nombreAgrupacion[!agg$nombreAgrupacion %in% c("EN BLANCO", "NO_VOTANTES", "VOTANTES", "JxC", "UxP", "LLA")] <- "OTROS"
agg$nombreAgrupacion <- as.factor(agg$nombreAgrupacion)
levels(agg$nombreAgrupacion) 


#Now aggregate again to sum votos for the new "OTROS" category
agg <- agg %>% group_by(secprov, seccionId, name, circuitoId, nombreAgrupacion, mesasTotalizadas, cantidadElectores) %>%
		summarise(votos = sum(votos), .groups = "drop")

#Add column anioEleccion with value 2023
agg$anioEleccion <- 2023

#Set all secprov values to 2
agg$secprov <- 2

# Write output
fwrite(agg, out_file)

cat(sprintf("Wrote %s with %d rows\n", out_file, nrow(agg)))
