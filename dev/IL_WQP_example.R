# download individual data profiles from WQX

# construct the WQP web service URL for each profile
  baseurl <- "https://www.waterqualitydata.us"
  profile_station <- "/data/Station"
  profile_result <- "/data/Result"
  profile_result_2 <- "&dataProfile=resultPhysChem"
  profile_project <- "/data/Project"
  filters <- "/search?statecode=US%3A17&characteristicType=Nutrient"
  dates <- "&startDateLo=04-01-2023&startDateHi=11-01-2023"
  type <- "&mimeType=csv&zip=yes"
  providers <- "&providers=NWIS&providers=STEWARDS&providers=STORET"

  # get Stations profile
  stationProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_station, filters, dates, type, providers)
  )

  # get full physical chemical Results profile
  physchemProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_result, filters, dates, type, profile_result_2, providers)
  )

  # get Project profile
  projectProfile <- TADA_ReadWQPWebServices(
    paste0(baseurl, profile_project, filters, dates, type, providers)
  )

  # Join all three profiles using TADA_JoinWQPProfiles
  TADAProfile <- TADA_JoinWQPProfiles(
    FullPhysChem = physchemProfile,
    Sites = stationProfile,
    Projects = projectProfile
  )


# create a list of the col names required for each profile

physchem.names <- names(physchemProfile)

project.names <-  names(projectProfile)

station.names <- names(stationProfile)


# in this section use TADA functions to make changes
qaqc_TADAProfile <- TADAProfile |>
  TADA_AutoClean()
# after this you would also run any additional QAQC TADA functions

# create function to retain required cols for each profile, prioritizing TADA-prefixed versions when present
TADA_SelectForWQXUpdate <- function(df, cols, prefix = "TADA.", rename_to_base = FALSE, warn = TRUE) {
  cn <- names(df)
  pref <- paste0(prefix, cols)

  # choose TADA.<colname> if present; otherwise choose <colname>
  chosen <- ifelse(pref %in% cn, pref, cols)

  # keep only those that actually exist
  exists <- chosen %in% cn
  if (warn && any(!exists)) {
    warning("Not found (neither prefixed nor base): ",
            paste(cols[!exists], collapse = ", "))
  }

  chosen_kept <- chosen[exists]
  base_kept   <- cols[exists]

  if (inherits(df, "data.table")) {
    # data.table: use .. to evaluate character vector in j
    out <- df[, ..chosen_kept]
    if (rename_to_base) {
      data.table::setnames(out, old = chosen_kept, new = base_kept)
    }
    return(out)
  } else {
    # data.frame / tibble
    out <- df[, chosen_kept, drop = FALSE]
    if (rename_to_base) {
      names(out) <- base_kept
    }
    return(out)
  }
}


# retain the required cols for WQX upload
# rename to base is set to TRUE which means that the TADA-prefixed cols that were
# retained drop the TADA-prefix and return to their original names
# however, the data reflect any changes made by TADA functions
qaqc_physchemProfile <- TADA_SelectForWQXUpdate(df = qaqc_TADAProfile,
                                                cols = physchem.names,
                                                rename_to_base = TRUE)

qaqc_projectProfile <- TADA_SelectForWQXUpdate(df = qaqc_TADAProfile,
                                               cols = project.names,
                                               rename_to_base = TRUE)

qaqc_stationProfile <- TADA_SelectForWQXUpdate(df = qaqc_TADAProfile,
                                               cols = station.names,
                                               rename_to_base = TRUE)

# the next step is likely to compare the "qaqc" profiles with the original ones
# this will allow you to see if any changes have been made (which would tell you
# if a specific profile needs to be updated/uploaded.
# it may be possible that some profiles will not have any changes so wouldn't need to be updated
