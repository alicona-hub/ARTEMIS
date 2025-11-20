#############################################
# artemis
# Author: Alisson Licona
# Description: Connect to an OMOP-CDM database
#              and load new artemis output tables.
#############################################


# ---- Connection to DB ----
library(dplyr)
library(DatabaseConnector)
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms            = "iris", 
  connectionString = "jdbc:IRIS://178.242.184.226:1972/BC",
  user            = "_SYSTEM",
  password        = "vmtest.25",
  pathToDriver = "/Users/alissonlicona/.iris_jdbc"
)

conn <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(conn), add = TRUE)



## Create and copy table in IRIS
names(regStats)[names(regStats) == "Count"] <- "CountValue"
dbWriteTable(conn, "artemis.regStats", regStats, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.processedEras_combined", processedEras_combined, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.processedAll_combined", processed_own, overwrite = TRUE, row.names = FALSE)
#dbWriteTable(conn, "artemis.processed_single", processed_single, overwrite = TRUE, row.names = FALSE)
#dbWriteTable(conn, "artemis.processedAll_multi", processedAll_multi, overwrite = TRUE, row.names = FALSE)
#dbWriteTable(conn, "artemis.output_single", output_single, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.output_own", output_own, overwrite = TRUE, row.names = FALSE)

names(regimens_total_groups_extended)[names(regimens_total_groups_extended) == "count"] <- "CountValue"
regimens_total_groups_extended2 <- regimens_total_groups_extended %>%
  select(regName,shortString,CountValue,avg_days,Groups,Groups2)
dbWriteTable(conn, "artemis.regimens_empirical", regimens_total_groups_extended2, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.stringDF", stringDF_fixed, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.con_df", con_df2, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.regGroups", regGroups, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.sankey_all", sankey_all, append = TRUE, row.names = FALSE)




# --------------------------------------------------------------------
# ---- Disconnect when finished ----
DatabaseConnector::disconnect(conn)
