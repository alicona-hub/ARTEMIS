postprocess_alignments <- function(df, regimenCombine) {

  #df %>%
  #  arrange(personID, regName, drugRec_Start) %>%
  #  group_by(personID, regName) %>%

  # Merge into 56-day regimen windows
  #mutate(
  #gap = drugRec_Start - lag(drugRec_End),
  #same_window = ifelse(is.na(gap) | gap <= regimenCombine, 0, 1),
  #window_id = cumsum(same_window)
  #)

  df %>%
    arrange(personID, drugRec_Start) %>%
    group_by(personID) %>%
    mutate(
      gap = drugRec_Start - lag(drugRec_End),
      same_window = ifelse(is.na(gap) | gap <= regimenCombine, 0, 1),
      window_id = cumsum(same_window)
    ) %>%

    group_by(personID, regName, window_id) %>%
    summarise(
      t_start = min(drugRec_Start),
      t_end = max(drugRec_End),
      regimen = first(Regimen),
      component = first(regName),
      adjustedS = 1,
      .groups = "drop_last"
    ) %>%

    ungroup() %>%
    arrange(personID, t_start) %>%
    group_by(personID) %>%
    mutate(
      timeToNextRegimen = lead(t_start) - t_end,
      timeToNextRegimen = ifelse(is.na(timeToNextRegimen), NA, pmax(0, timeToNextRegimen)),
      timeToEOD = max(t_end) - t_end,
      regLength = t_end - t_start
    ) %>%
    ungroup()
}
