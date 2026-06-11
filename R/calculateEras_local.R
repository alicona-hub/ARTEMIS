calculateEras_local <- function(
    processedAll,
    same_regimen_gap,
    diff_regimen_gap
) {

  IDs_All <- unique(processedAll$personID)
  result_list <- vector("list", length(IDs_All))

  for (i in seq_along(IDs_All)) {

    tempDF <- processedAll[processedAll$personID == IDs_All[i], ]
    tempDF <- tempDF[order(tempDF$t_start), ]

    # Remove overlapping windows (safety step)
    toRemove <- c()
    if (nrow(tempDF) > 1) {
      for (ic in 2:nrow(tempDF)) {
        if (tempDF[ic, ]$t_start < tempDF[ic - 1, ]$t_end) {
          toRemove <- c(toRemove, ic)
        }
      }
    }
    if (length(toRemove) > 0) tempDF <- tempDF[-toRemove, ]

    # Gap between consecutive regimen windows
    tempDF <- tempDF %>%
      mutate(timeToNextRegimen = lead(t_start) - t_end)

    # Decide if two episodes belong to SAME line or NEW line
    tempDF <- tempDF %>%
      mutate(
        prev_component = lag(component),
        prev_gap      = lag(timeToNextRegimen),

        delete = case_when(
          # First episode → always NEW LINE
          is.na(prev_component) ~ "N",

          # SAME REGIMEN LOGIC (default threshold 146)
          component == prev_component & prev_gap < same_regimen_gap ~ "Y",
          component == prev_component & prev_gap >= same_regimen_gap ~ "N",

          # DIFFERENT REGIMEN LOGIC (default threshold 84)
          component != prev_component & prev_gap < diff_regimen_gap ~ "Y",
          component != prev_component & prev_gap >= diff_regimen_gap ~ "N"
        )
      )

    # Create line numbering
    tempDF1 <- tempDF %>%
      mutate(newLine = cumsum(delete == "N")) %>%
      summarise(
        adjustedS = sum(adjustedS * (t_end - t_start) / sum(t_end - t_start)),
        t_start  = min(t_start),
        t_end    = max(t_end),
        timeToEOD = min(timeToEOD),
        .by = c(component, newLine, personID)
      ) %>%
      mutate(
        regLength = t_end - t_start,
        timeToNextRegimen = lead(t_start) - t_end,
        First_Line = as.integer(row_number() == 1),
        Second_Line = as.integer(row_number() == 2),
        Third_Line = as.integer(row_number() == 3),
        Fourth_Line = as.integer(row_number() == 4),
        Fifth_Line = as.integer(row_number() == 5),
        Sixth_Line = as.integer(row_number() == 6),
        Seventh_Line = as.integer(row_number() == 7),
        Other      = as.integer(row_number() > 7)
      )

    result_list[[i]] <- tempDF1
  }

  bind_rows(result_list)
}
