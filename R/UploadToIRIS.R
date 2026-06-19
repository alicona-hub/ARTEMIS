############################################################
# Export ARTEMIS Results to IRIS
#
# Purpose:
#   Persist ARTEMIS analysis outputs to an IRIS database
#   for downstream reporting, visualization, and analysis.
#
# Description:
#   This script:
#     1. Connects to the target IRIS database.
#     2. Creates or replaces ARTEMIS output tables.
#     3. Uploads treatment pathway results, regimen
#        statistics, alignment outputs, and supporting
#        analysis datasets.
#
# Outputs:
#   artemis.regStats
#   artemis.processedEras_combined
#   artemis.processedAll_combined
#   artemis.output_own
#   artemis.regimens_empirical
#   artemis.stringDF
#   artemis.con_df
#   artemis.regGroups
#   artemis.sankey_all
############################################################


############################################################
# 1. Connect to IRIS Database
############################################################

# Establish connection to the target IRIS instance that
# will store ARTEMIS outputs and supporting analysis tables.

# ---- Connect to IRIS following the R/connectToIRIS.R file ----

############################################################
# 2. Upload Core ARTEMIS Outputs
############################################################

# Upload the primary outputs generated during treatment
# pathway analysis.
#
# These tables contain:
#   - Regimen statistics
#   - Processed treatment eras
#   - Post-processed alignments
#   - Raw alignment results

library(dplyr)
## Create and copy table in IRIS
names(regStats)[names(regStats) == "Count"] <- "CountValue"
dbWriteTable(conn, "artemis.regStats", regStats, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.processedEras_combined", processedEras_combined, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.processedAll_combined", processed_own, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.output_own", output_own, overwrite = TRUE, row.names = FALSE)

names(regimens_total_groups_extended)[names(regimens_total_groups_extended) == "count"] <- "CountValue"
regimens_total_groups_extended2 <- regimens_total_groups_extended %>%
  select(regName,shortString,CountValue,avg_days,Groups,Groups2)
dbWriteTable(conn, "artemis.regimens_empirical", regimens_total_groups_extended2, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.stringDF", stringDF_fixed, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.con_df", con_df2, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.regGroups", regGroups, overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn, "artemis.sankey_all", sankey_all, append = TRUE, row.names = FALSE)

