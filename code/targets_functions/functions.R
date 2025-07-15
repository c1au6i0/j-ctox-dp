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


