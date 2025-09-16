# Check and install required packages
# Set CRAN mirror first
options(repos = c(CRAN = "https://cran.rstudio.com/"))

packages <- c("eiPack", "ggplot2", "reshape2")

for(pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("All required packages are available.\n")