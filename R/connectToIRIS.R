#############################################
# Connect to IRIS
# Description: Connect to an IRIS namespace.
#############################################


# ---- Connection to DB ----

# Libraries
library(DatabaseConnector)

# Edit connection details
# 1. "namespace": replace with the name of the IRIS namespace
# 2. user: add your username
# 3. password: add your password

driver_path <- file.path("drivers")


connection_details <- DatabaseConnector::createConnectionDetails(
  dbms            = "iris",
  connectionString = "jdbc:IRIS://188.59.46.0:1972/PMM",
  user            = "_system",
  password        = "vmtest.25",
  pathToDriver = driver_path
)

# Execute connection
conn <- DatabaseConnector::connect(connection_details)

# Safety measure to disconnect on exit
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)


# --------------------------------------------------------------------
# ---- Disconnect when finished ----
DatabaseConnector::disconnect(conn)
