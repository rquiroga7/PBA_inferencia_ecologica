# Simple Sankey Plot Creator for Vote Transfers
# Creates a basic Sankey plot without complex HTML dependencies

library(data.table)

cat("=== CREATING SIMPLE SANKEY VISUALIZATION ===\n")

# Load the data
vote_flows_data <- fread("vote_flows_absolute.csv")

# Clean the data - remove commas and convert to numeric
numeric_cols <- c("EN_BLANCO_2025", "LLA_JxC_2025", "NO_VOTANTES_2025", "OTROS_2025", "FP_2025")

for(col in numeric_cols) {
  vote_flows_data[[col]] <- as.numeric(gsub(",", "", vote_flows_data[[col]]))
}

cat("✅ Data loaded and cleaned\n")

# Create a matrix representation for easier analysis
transfer_matrix <- as.matrix(vote_flows_data[, ..numeric_cols])
rownames(transfer_matrix) <- vote_flows_data$From_2023

# Print the transfer matrix in a nice format
cat("\n=== VOTE TRANSFER MATRIX ===\n")
cat("Values show estimated number of voters moving from 2023 parties (rows) to 2025 options (columns)\n\n")

# Create formatted table
cat(sprintf("%-20s %12s %12s %12s %12s %12s\n", 
            "From \\ To", "EN_BLANCO", "LLA+JxC", "NO_VOTANTES", "OTROS", "FP"))
cat(paste(rep("-", 92), collapse = ""), "\n")

for(i in 1:nrow(transfer_matrix)) {
  cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
              gsub("_2023", "", rownames(transfer_matrix)[i]),
              format(transfer_matrix[i,1], big.mark = ","),
              format(transfer_matrix[i,2], big.mark = ","),
              format(transfer_matrix[i,3], big.mark = ","),
              format(transfer_matrix[i,4], big.mark = ","),
              format(transfer_matrix[i,5], big.mark = ",")))
}

# Calculate column totals
cat(paste(rep("-", 92), collapse = ""), "\n")
col_totals <- colSums(transfer_matrix)
cat(sprintf("%-20s %12s %12s %12s %12s %12s\n",
            "TOTAL RECEIVED:",
            format(col_totals[1], big.mark = ","),
            format(col_totals[2], big.mark = ","),
            format(col_totals[3], big.mark = ","),
            format(col_totals[4], big.mark = ","),
            format(col_totals[5], big.mark = ",")))

# Identify the biggest flows
cat("\n=== TOP 10 VOTE TRANSFERS ===\n")

# Create a list of all flows
all_flows <- data.frame()
party_names_2023 <- c("EN_BLANCO", "JxC", "LLA", "NO_VOTANTES", "OTROS", "UxP")
party_names_2025 <- c("EN_BLANCO", "LLA+JxC", "NO_VOTANTES", "OTROS", "FP")

for(i in 1:nrow(transfer_matrix)) {
  for(j in 1:ncol(transfer_matrix)) {
    flow_value <- transfer_matrix[i, j]
    if(flow_value > 0) {
      all_flows <- rbind(all_flows, data.frame(
        from = party_names_2023[i],
        to = party_names_2025[j],
        votes = flow_value,
        flow_label = paste(party_names_2023[i], "→", party_names_2025[j]),
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Sort by vote count and show top 10
top_flows <- all_flows[order(all_flows$votes, decreasing = TRUE), ]
top_10 <- head(top_flows, 10)

for(i in 1:nrow(top_10)) {
  cat(sprintf("%2d. %-30s: %10s votes\n", 
              i, 
              top_10$flow_label[i], 
              format(top_10$votes[i], big.mark = ",")))
}

# Key insights
cat("\n=== KEY POLITICAL INSIGHTS ===\n")

# Loyalty analysis
cat("PARTY LOYALTY:\n")
cat("• UxP → Frente Patriótico:", 
    sprintf("%.1f%% (%s votes)", 
            (transfer_matrix[6,5] / sum(transfer_matrix[6,])) * 100,
            format(transfer_matrix[6,5], big.mark = ",")), "\n")

cat("• Non-Voter persistence:", 
    sprintf("%.1f%% (%s votes)", 
            (transfer_matrix[4,3] / sum(transfer_matrix[4,])) * 100,
            format(transfer_matrix[4,3], big.mark = ",")), "\n")

# Coalition dynamics
jxc_to_coalition <- transfer_matrix[2,2]
lla_to_coalition <- transfer_matrix[3,2]
total_coalition_from_jxc_lla <- jxc_to_coalition + lla_to_coalition

cat("\nCOALITION FORMATION (JxC + LLA → LLA+JxC):\n")
cat("• JxC → Coalition:", format(jxc_to_coalition, big.mark = ","), "votes\n")
cat("• LLA → Coalition:", format(lla_to_coalition, big.mark = ","), "votes\n")
cat("• Total coalition building:", format(total_coalition_from_jxc_lla, big.mark = ","), "votes\n")

# Major defections
cat("\nMAJOR VOTER MOVEMENTS:\n")
cat("• LLA → Non-Voters:", format(transfer_matrix[3,3], big.mark = ","), "votes (abstention)\n")
cat("• JxC → Other Parties:", format(transfer_matrix[2,4], big.mark = ","), "votes (fragmentation)\n")
cat("• Blank Votes → Non-Voters:", format(transfer_matrix[1,3], big.mark = ","), "votes (disengagement)\n")

# Summary statistics
total_voters_2023 <- sum(transfer_matrix)


cat("\n=== SUMMARY ===\n")
cat("Total voters analyzed:", format(total_voters_2023, big.mark = ","), "\n")
cat("Most stable group:", most_stable, "\n")
cat("Biggest transformation:", biggest_change, "\n")
cat("Coalition success: JxC+LLA retained", 
    format(total_coalition_from_jxc_lla, big.mark = ","), "voters as LLA+JxC\n")

cat("\n🎯 ANALYSIS COMPLETE!\n")
cat("📊 For the interactive Sankey plot, open: vote_transfers_sankey.html in a web browser\n")
cat("📈 For the static chart, view: top_vote_flows.png\n")