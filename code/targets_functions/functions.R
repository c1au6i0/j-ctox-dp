#' extr_inf
#'
#' Extract some basic info from list of dataframes
#'
#' @param dat A list of dataframes with a casrn and iupac_name column.
#'
#' @return Dataframe
extr_inf <- function(dat) {
  dat |>
    summarize(
      dataset_name = dplyr::first(dataset_name),
      unique_casrn = length(unique(casrn)),
      unique_names = length(unique(iupac_name)),
      nrows = length(casrn),
      duplicated_casrn = sum(duplicated(dat$casrn)),
      duplicated_iupac_names = sum(duplicated(dat$iupac_name))
    )
}


not_found_cid <- tibble::tribble(
  ~casrn, ~reason,
  "84649-99-0", "complex_ingr",
  "8016-21-5", "complex_ingr",
  "4940-11-08", "not_found",
  "27043-05-6", "26334",
  "8008-56-8", "complex_ingr",
  "8008-26-2", "complex_ingr",
  "8008-57-9", "complex_ingr",
  "2060-12-08", "not_found",
  "9000-05-09", "not_found",
  "8007-80-5", "complex_ingr",
  "8002-66-2", "5280443",
  "8015-91-6", "complex_ingr",
  "8007-08-07", "not_found",
  "2090-05-01", "not_found",
  "8008-45-5", "complex_ingr",
  "9000-50-4", "complex_ingr",
  "8014-09-03", "not_found",
  "8006-87-9", "complex_ingr",
  "8006-81-3", "complex_ingr",
  "2093-08-03", "not_found",
  "68917-18-0", "167312527",
  "8008-79-5", "complex_ingr",
)
