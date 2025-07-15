# _targets.R

library(targets)
library(tarchetypes)
library(here)
tar_source(here("code", "targets_functions", "functions.R"))

# @@@@@@@@@@@@
# Set Up -----
# @@@@@@@@@@@

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "janitor",
    "lubridate",
    "tidyverse",
    "vroom",
    "openxlsx"
  ),
  workspace_on_error = TRUE,
  garbage_collection = TRUE,
  memory = "transient"
)


list(
  tar_file_read(
    main_dat_wp,
    here::here("data", "other", "main_list_wp.xlsx"),
    readxl::read_xlsx(path = !!.x) |> 
      janitor::clean_names() |> 
      dplyr::select(1:4) |> 
      mutate(casrn = str_trim(casrn))
  ),
  tar_file_read(
    ex_data,
    here::here("data", "other", "ex_list.txt"),
    readr::read_delim(!!.x, 
                      delim = ":", 
                      escape_double = FALSE, 
                      trim_ws = TRUE,
                      show_col_types = FALSE
                      ) |> 
    mutate(casrn = str_trim(casrn)) |> 
    janitor::clean_names()
  ),
  # @@@@@@@@@@@@@@@
  # Main sheet ----
  # @@@@@@@@@@@@@@@

  tar_target(
    main_comptox,
    main_dat_wp |> 
      distinct(casrn, .keep_all = TRUE) |>
      dplyr::filter(!is.na(casrn)) |> 
      pull(casrn) |> 
      extractox::extr_comptox( download_items =  c("CASRN", "INCHIKEY", "IUPAC_NAME", "SMILES", "INCHI_STRING", "MS_READY_SMILES", "QSAR_READY_SMILES", "MOLECULAR_FORMULA") )
  ),
  tar_target(
    main_cid,
    main_dat_wp |> 
      filter(!is.na(casrn)) |> 
      mutate(casrn = str_trim(casrn)) |> 
      dplyr::distinct(casrn) |> 
      pull(casrn) |> 
      webchem::get_cid(match = "first",
                     verbose = TRUE)
  ),
  tar_target(
    to_add_main,
    tibble::tribble(
      ~query, ~cid,
      "8028-89-5", "61634",
      "8002-66-2", "5280443",
      "8000-41-7", "17100"
    )
  ),
  tar_target(
    to_remove_main,
    main_cid |> 
      dplyr::filter(is.na(cid)) |> 
      pull(query)
  ),
  tar_target(
    main_chem,
    main_cid |>
      dplyr::filter(!query %in% to_remove_main) |>
      dplyr::bind_rows(to_add_main) |>
      dplyr::pull(cid) |>
      webchem::pc_prop(verbose = TRUE)
      ),
  tar_file(
    main_chem_out,
    {
      path_out <- here::here("reports", "tabs_figs", "main_wp_clean.xlsx")
      main_chem |> 
        dplyr::relocate(IUPACName, 1) |> 
        write.xlsx(path_out)
      path_out
      
    }
  ),
  # @@@@@@@@@@@@@@@
  # ex_data ----
  # @@@@@@@@@@@@@@@
  tar_target(
    ex_cid,
    ex_data |> 
      filter(!is.na(casrn)) |> 
      mutate(casrn = str_trim(casrn)) |> 
      dplyr::distinct(casrn) |> 
      pull(casrn) |> 
      webchem::get_cid(match = "first",
                       verbose = TRUE)
  )
  
 
)
