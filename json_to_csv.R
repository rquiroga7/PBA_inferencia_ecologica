library(data.table)
library(jsonlite)
library(dplyr)

# Paths
json_file <- "data/electores_circuitos_2025.json"
out_file <- "data/electores_circuitos_2025.csv"

# Read JSON file
json_data <- fromJSON(json_file, simplifyDataFrame = FALSE)

# Initialize empty list to store all rows
all_rows <- list()

# Process each circuit (json_data is a list/array)
for (i in seq_len(length(json_data))) {
  circuit <- json_data[[i]]
  
  # Skip if circuit is NULL
  if (is.null(circuit)) {
    next
  }
  
  # Extract metadata from circuit_metadata
  if (!is.null(circuit$circuit_metadata)) {
    secprov <- circuit$circuit_metadata$secprov
    seccionId <- circuit$circuit_metadata$seccionId
    name <- circuit$circuit_metadata$name
    circuitoId <- circuit$circuit_metadata$circuitoId
  } else {
    next
  }
  
  # Extract from estadoRecuento
  if (!is.null(circuit$estadoRecuento)) {
    mesasTotalizadas <- circuit$estadoRecuento$mesasTotalizadas
    cantidadElectores <- circuit$estadoRecuento$cantidadElectores
    cantidadVotantes <- circuit$estadoRecuento$cantidadVotantes
  } else {
    next
  }
  
  # Process positive votes (valoresTotalizadosPositivos)
  if (!is.null(circuit$valoresTotalizadosPositivos) && length(circuit$valoresTotalizadosPositivos) > 0) {
    positivos <- circuit$valoresTotalizadosPositivos
    for (j in seq_len(length(positivos))) {
      agrupacion <- positivos[[j]]
      nombreAgrupacion <- agrupacion$nombreAgrupacion
      votos <- agrupacion$votos
      
      # Apply same transformations as in procesa_merge.R
      if (nombreAgrupacion == "FUERZA PATRIA") nombreAgrupacion <- "UxP"
      if (nombreAgrupacion == "LA LIBERTAD AVANZA") nombreAgrupacion <- "LLA_JxC"  # Coalition in 2025
      if (!nombreAgrupacion %in% c("EN BLANCO", "UxP", "LLA_JxC")) nombreAgrupacion <- "OTROS"
      
      row <- data.frame(
        secprov = secprov,
        seccionId = seccionId,
        name = name,
        circuitoId = circuitoId,
        nombreAgrupacion = nombreAgrupacion,
        mesasTotalizadas = mesasTotalizadas,
        cantidadElectores = cantidadElectores,
        votos = votos,
        stringsAsFactors = FALSE
      )
      all_rows <- append(all_rows, list(row))
    }
  }
  
  # Add EN BLANCO row from valoresTotalizadosOtros
  # Include votosEnBlanco + votosNulos + votosRecurridosComandoImpugnados
  votos_blanco <- 0
  if (!is.null(circuit$valoresTotalizadosOtros$votosEnBlanco)) {
    votos_blanco <- votos_blanco + circuit$valoresTotalizadosOtros$votosEnBlanco
  }
  if (!is.null(circuit$valoresTotalizadosOtros$votosNulos)) {
    votos_blanco <- votos_blanco + circuit$valoresTotalizadosOtros$votosNulos
  }
  if (!is.null(circuit$valoresTotalizadosOtros$votosRecurridosComandoImpugnados)) {
    votos_blanco <- votos_blanco + circuit$valoresTotalizadosOtros$votosRecurridosComandoImpugnados
  }
  
  if (votos_blanco > 0) {
    row_blanco <- data.frame(
      secprov = secprov,
      seccionId = seccionId,
      name = name,
      circuitoId = circuitoId,
      nombreAgrupacion = "EN BLANCO",
      mesasTotalizadas = mesasTotalizadas,
      cantidadElectores = cantidadElectores,
      votos = votos_blanco,
      stringsAsFactors = FALSE
    )
    all_rows <- append(all_rows, list(row_blanco))
  }
  
  # Add NO_VOTANTES row (calculated as cantidadElectores - cantidadVotantes)
  # cantidadVotantes includes all votes (party votes + blank votes)
  # NO_VOTANTES represents abstention
  noVotantes <- cantidadElectores - cantidadVotantes
  row_no_votantes <- data.frame(
    secprov = secprov,
    seccionId = seccionId,
    name = name,
    circuitoId = circuitoId,
    nombreAgrupacion = "NO_VOTANTES",
    mesasTotalizadas = mesasTotalizadas,
    cantidadElectores = cantidadElectores,
    votos = noVotantes,
    stringsAsFactors = FALSE
  )
  all_rows <- append(all_rows, list(row_no_votantes))
}

# Combine all rows
result <- do.call(rbind, all_rows)

# Convert to data.table
result <- as.data.table(result)

# Aggregate by grouping variables (same as in procesa_merge.R)
final_result <- result %>% 
  group_by(secprov, seccionId, name, circuitoId, nombreAgrupacion, mesasTotalizadas, cantidadElectores) %>%
  summarise(votos = sum(as.numeric(votos)), .groups = "drop")

# Convert back to data.table for fwrite
final_result <- as.data.table(final_result)

# Write output
fwrite(final_result, out_file)

cat(sprintf("Wrote %s with %d rows\n", out_file, nrow(final_result)))