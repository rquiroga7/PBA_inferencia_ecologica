# Sankey Plot for Ecological Inference Vote Transfers
# This script creates a beautiful interactive Sankey plot showing vote flows from 2025 to Oct 2025

# Load required libraries
library(networkD3)
library(htmlwidgets)
library(dplyr)
library(data.table)
library(htmltools)

cat("=== CREATING SANKEY PLOT FOR VOTE TRANSFERS (2025 → Oct 2025) ===\n")

# Check if the required CSV files exist from ei_2025_to_oct2025.R
if(!file.exists("vote_flows_absolute_oct2025.csv")) {
  cat("❌ vote_flows_absolute_oct2025.csv not found. Please run ei_2025_to_oct2025.R first.\n")
  stop("Required data files not found")
}

# Load the data
vote_flows_data <- fread("vote_flows_absolute_oct2025.csv")

cat("✅ Data loaded successfully\n")
cat("Vote flows data dimensions:", nrow(vote_flows_data), "×", ncol(vote_flows_data), "\n")

# Clean the data - remove commas and convert to numeric
numeric_cols <- c("EN_BLANCO_Oct2025", "LLA_Oct2025", "NO_VOTANTES_Oct2025", "OTROS_Oct2025", "FP_Oct2025")

for(col in numeric_cols) {
  # Remove commas and convert to numeric
  vote_flows_data[[col]] <- as.numeric(gsub(",", "", vote_flows_data[[col]]))
}

# Create nodes data frame
# Source nodes (2025 parties)
source_nodes <- data.frame(
  name = c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "UxP_2025"),
  group = "2025",
  stringsAsFactors = FALSE
)

# Target nodes (Oct 2025 parties)
target_nodes <- data.frame(
  name = c("EN_BLANCO_Oct2025", "LLA_Oct2025", "NO_VOTANTES_Oct2025", "OTROS_Oct2025", "FP_Oct2025"),
  group = "Oct2025",
  stringsAsFactors = FALSE
)

# Combine all nodes
nodes <- rbind(source_nodes, target_nodes)
nodes$id <- 0:(nrow(nodes) - 1)  # networkD3 requires 0-based indexing

cat("Created", nrow(nodes), "nodes for Sankey plot\n")

# Create links data frame
links <- data.frame()

# Map source parties to their indices
source_mapping <- c(
  "EN_BLANCO_2025" = 0, "LLA_JxC_2025" = 1, "NO_VOTANTES_2025" = 2,
  "OTROS_2025" = 3, "UxP_2025" = 4
)

# Map target parties to their indices
target_mapping <- c(
  "EN_BLANCO_Oct2025" = 5, "LLA_Oct2025" = 6, "NO_VOTANTES_Oct2025" = 7,
  "OTROS_Oct2025" = 8, "FP_Oct2025" = 9
)

# Create links from the vote flows data
for(i in 1:nrow(vote_flows_data)) {
  source_party <- vote_flows_data$From_2025[i]
  source_idx <- source_mapping[source_party]

  # Create a link for each destination
  for(col in numeric_cols) {
    target_party <- col
    target_idx <- target_mapping[target_party]
    value <- vote_flows_data[[col]][i]

    # Only include links with significant vote flows (> 1000 votes)
    if(!is.na(value) && value > 1000) {
      links <- rbind(links, data.frame(
        source = source_idx,
        target = target_idx,
        value = value,
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat("Created", nrow(links), "links with significant vote flows (>1,000 votes)\n")

# Clean party names for better display
nodes$display_name <- gsub("_2025|_Oct2025", "", nodes$name)
nodes$display_name <- gsub("_", " ", nodes$display_name)

# Improve specific party names
nodes$display_name[nodes$display_name == "EN BLANCO"] <- "Blank Votes"
nodes$display_name[nodes$display_name == "NO VOTANTES"] <- "Non-Voters"
nodes$display_name[nodes$display_name == "LLA JxC"] <- "LLA+JxC Coalition"
nodes$display_name[nodes$display_name == "LLA"] <- "La Libertad Avanza"
nodes$display_name[nodes$display_name == "OTROS"] <- "Other Parties"
nodes$display_name[nodes$display_name == "UxP"] <- "Unión por la Patria"
nodes$display_name[nodes$display_name == "FP"] <- "Frente Patria"

# Define colors for each party/group
party_colors <- c(
  # 2025 parties (source)
  "#E8E8E8",  # EN_BLANCO_2025 - Light gray
  "#8A2BE2",  # LLA_JxC_2025 - Blue Violet (coalition)
  "#808080",  # NO_VOTANTES_2025 - Gray
  "#D2B48C",  # OTROS_2025 - Tan
  "#4169E1",  # UxP_2025 - Royal Blue

  # Oct 2025 parties (target)
  "#E8E8E8",  # EN_BLANCO_Oct2025 - Light gray
  "#9966CC",  # LLA_Oct2025 - Purple (no longer coalition)
  "#808080",  # NO_VOTANTES_Oct2025 - Gray
  "#D2B48C",  # OTROS_Oct2025 - Tan
  "#FF6347"   # FP_Oct2025 - Tomato (Frente Patria)
)

# Create the Sankey plot
cat("Creating Sankey plot...\n")

sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "display_name",
  units = "votes",
  fontSize = 14,
  nodeWidth = 20,
  nodePadding = 15,
  margin = list(top = 50, right = 150, bottom = 50, left = 150),
  height = 600,
  width = 1000,
  iterations = 100,  # More iterations for better layout
  sinksRight = TRUE,  # Place target nodes on the right
  colourScale = sprintf('d3.scaleOrdinal().range(["%s"])',
                       paste(party_colors, collapse = '","'))
)

# Add title and subtitle
sankey_plot <- sankey_plot %>%
  htmlwidgets::prependContent(
    tags$div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$h2("Buenos Aires Electoral Flows: 2025 → October 2025",
              style = "color: #2C3E50; font-family: 'Arial', sans-serif; margin-bottom: 5px;"),
      tags$h4("Estimated Voter Transitions Based on Ecological Inference",
              style = "color: #7F8C8D; font-family: 'Arial', sans-serif; font-weight: normal;"),
      tags$p(paste("Based on analysis of", nrow(vote_flows_data), "electoral circuits. ",
                   "Flow thickness represents number of voters (minimum 1,000 shown)."),
             style = "color: #95A5A6; font-family: 'Arial', sans-serif; font-size: 12px;")
    )
  )

# Save the plot as HTML
html_file <- "vote_transfers_sankey_oct2025.html"
saveWidget(sankey_plot, html_file, selfcontained = FALSE)
cat("✅ Sankey plot saved as:", html_file, "\n")

# Create a summary table of major flows
cat("\n=== MAJOR VOTE FLOWS SUMMARY ===\n")

# Calculate total flows and identify the largest ones
links$flow_label <- paste(
  nodes$display_name[links$source + 1], "→",
  nodes$display_name[links$target + 1]
)

# Sort by value and show top flows
major_flows <- links[order(links$value, decreasing = TRUE), ]
top_flows <- head(major_flows, 10)

cat("Los 10 mayores flujo de votos:\n")
for(i in 1:nrow(top_flows)) {
  cat(sprintf("%2d. %s: %s votos\n",
              i,
              top_flows$flow_label[i],
              format(round(top_flows$value[i]), big.mark = ",")))
}

# Calculate loyalty rates
cat("\n=== PARTY LOYALTY ANALYSIS ===\n")

# Find same-party flows (or close matches)
loyalty_flows <- list()

# UxP → FP loyalty (UxP 2025 → FP Oct 2025)
fp_loyalty <- links[links$source == 4 & links$target == 9, ]
if(nrow(fp_loyalty) > 0) {
  cat("UxP → FP: ", format(round(fp_loyalty$value), big.mark = ","), " votos\n")
  loyalty_flows$FP <- fp_loyalty$value
}

# Coalition split (LLA_JxC → LLA)
coalition_to_lla <- links[links$source == 1 & links$target == 6, ]
if(nrow(coalition_to_lla) > 0) {
  cat("LLA+JxC → LLA: ", format(round(coalition_to_lla$value), big.mark = ","), " votos\n")
  loyalty_flows$LLA_continuity <- coalition_to_lla$value
}

# Non-voter persistence
nonvoter_persistence <- links[links$source == 2 & links$target == 7, ]
if(nrow(nonvoter_persistence) > 0) {
  cat("Ausentes: ", format(round(nonvoter_persistence$value), big.mark = ","), " votos\n")
  loyalty_flows$NonVoter <- nonvoter_persistence$value
}

# Calculate total flows for percentage analysis
cat("\n=== FLOW PERCENTAGES ===\n")

# Load the original totals for percentage calculations
total_votes_per_source <- vote_flows_data[, .(
  total = as.numeric(gsub(",", "", Total_From))
), by = From_2025]

# Calculate percentages for major flows
for(i in 1:min(5, nrow(top_flows))) {
  source_party <- nodes$name[top_flows$source[i] + 1]
  source_total <- total_votes_per_source[From_2025 == source_party, total]

  if(length(source_total) > 0 && source_total > 0) {
    percentage <- (top_flows$value[i] / source_total) * 100
    cat(sprintf("%s: %.1f%% de los votantes originales\n",
                top_flows$flow_label[i], percentage))
  }
}

# Export summary data
summary_data <- data.frame(
  Flow = top_flows$flow_label,
  Votes = format(round(top_flows$value), big.mark = ","),
  Rank = 1:nrow(top_flows),
  stringsAsFactors = FALSE
)

fwrite(summary_data, "sankey_flow_summary_oct2025.csv")
cat("\n✅ Flow summary exported to: sankey_flow_summary_oct2025.csv\n")

cat("\n🎯 SANKEY PLOT CREATION COMPLETE!\n")
cat("📊 Interactive plot saved as:", html_file, "\n")
cat("📁 Open the HTML file in a web browser to view the interactive Sankey diagram\n")
cat("🔍 The plot shows all vote flows > 1,000 votes with proportional thickness\n")

# Additional visualization - create a static summary plot using base R
cat("\n=== CREATING STATIC SUMMARY PLOT ===\n")

# Create a simple bar plot of the top flows
png("top_vote_flows_oct2025.png", width = 1000, height = 600, res = 100)
par(mar = c(12, 5, 4, 2))

barplot(top_flows$value[1:8],
        names.arg = top_flows$flow_label[1:8],
        las = 2,
        col = rainbow(8, alpha = 0.7),
        main = "Los 8 mayores flujo de votos: 2025 → Oct 2025",
        ylab = "Número de votos",
        cex.names = 0.8)

# Add value labels on bars
text(x = 1:8 * 1.2 - 0.5,
     y = top_flows$value[1:8] + max(top_flows$value[1:8]) * 0.02,
     labels = format(round(top_flows$value[1:8]), big.mark = ","),
     cex = 0.8, adj = 0.5)

dev.off()
cat("✅ Static summary plot saved as: top_vote_flows_oct2025.png\n")

cat("\n📈 All visualizations created successfully!\n")
cat("Files created:\n")
cat("  1. ", html_file, " - Interactive Sankey plot\n")
cat("  2. top_vote_flows_oct2025.png - Static bar chart\n")
cat("  3. sankey_flow_summary_oct2025.csv - Summary data\n")
