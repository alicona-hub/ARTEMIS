postprocess_alignments <- function(df, regimenCombine = 30) {
  required_cols <- c("personID", "regName", "Regimen", 
                     "DrugRecord", "drugRec_Start", "drugRec_End")
  if (!all(required_cols %in% names(df))) {
    stop("Input data.frame must include: ", paste(required_cols, collapse = ", "))
  }
  
  df %>%
    arrange(personID, drugRec_Start) %>%
    group_by(personID) %>%
    mutate(
      # Compute simple duration (within-regimen)
      regLength = drugRec_End - drugRec_Start + 1,
      # Compute time until next regimen starts
      timeToNextRegimen = lead(drugRec_Start) - drugRec_End,
      # Ensure no negatives
      timeToNextRegimen = ifelse(is.na(timeToNextRegimen), NA, pmax(0, timeToNextRegimen)),
      # Compute time to end of data (within patient)
      timeToEOD = max(drugRec_End, na.rm = TRUE) - drugRec_End
    ) %>%
    ungroup() %>%
    transmute(
      t_start = drugRec_Start,
      t_end = drugRec_End,
      component = regName,
      regimen = Regimen,
      adjustedS = 1,
      personID = personID,
      timeToNextRegimen = timeToNextRegimen,
      timeToEOD = timeToEOD,
      regLength = regLength
    )
}
