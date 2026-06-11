required_packages <- c("DatabaseConnector","dplyr","DBI")

installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
