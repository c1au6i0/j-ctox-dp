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
      dplyr::distinct(cid) |> 
      dplyr::pull(cid) |>
      webchem::pc_prop(verbose = TRUE) |> 
      dplyr::relocate(IUPACName, 1) 
      ),
  tar_target(
    main_chem_all,
    main_cid |> 
      dplyr::rename(CASRN = query, CID = cid) |>
      dplyr::mutate(CID = as.integer(CID)) |> 
      dplyr::right_join(main_chem, by ="CID") |> 
      dplyr::relocate(CASRN:CID, .after = IUPACName) |> 
      dplyr::distinct(CID, .keep_all = TRUE)
  ),
  tar_file(
    main_chem_out,
    {
      path_out <- here::here("reports", "tabs_figs", "main_wp_clean.xlsx")
      main_chem_all |> 
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
      tidyr::drop_na(casrn) |> 
      pull(casrn) |> 
      webchem::get_cid(match = "first",
                       verbose = TRUE)
  ),
  tar_target(
    to_add_ex,
    tibble::tribble(
      ~query, ~cid,
      "27043-05-6", "26334",
      "8002-66-2", "5280443",
      "68917-18-0", "167312527"
    )
  ),
  tar_target(
    ex_cid_chem,
    ex_cid |> 
      dplyr::bind_rows(to_add_ex) |>
      dplyr::distinct(cid) |> 
      tidyr::drop_na(cid) |>
      pull(cid) |> 
      webchem::pc_prop(verbose = TRUE) |> 
      dplyr::relocate(IUPACName, 1) 
  ),
  tar_target(
    ex_cid_chem_all,
    ex_cid |> 
      dplyr::rename(CASRN = query, CID = cid) |>
      dplyr::mutate(CID = as.integer(CID)) |> 
      dplyr::right_join(ex_cid_chem, by ="CID") |> 
      dplyr::relocate(CASRN:CID, .after = IUPACName) |> 
      dplyr::distinct(CID, .keep_all = TRUE) |> 
      dplyr::select(IUPACName:InChIKey) |> 
      dplyr::select(-MolecularWeight)
  ),
  tar_file(
    ex_cid_chem_out,
    {
      path_out <- here::here("reports", "tabs_figs", "ex_cid_chem.xlsx")
      ex_cid_chem_all |> 
        write.xlsx(path_out)
      path_out
    }
  ),
  # @@@@@@@@@@@@@@@@@@
  # Solution ----@@@@@
  # @@@@@@@@@@@@@@@@@@
  tar_target(
    solution,
    ex_cid_chem_all |> 
      dplyr::filter(!SMILES %in% main_chem_all$SMILES) |> 
      dplyr::select(IUPACName:InChIKey)
  ),
  tar_file(
    solution_out,
    {
      path_out <- here::here("reports", "tabs_figs", "outer_to_check.xlsx")
      solution |> 
        write.xlsx(path_out)
      path_out
      
    }
  ),
  # @@@@@@@@@@@@@@@@@@
  # orange ----@@@@@@@
  # @@@@@@@@@@@@@@@@@@
  tar_file_read(
    orange,
    here::here("data", "other", "orange.xlsx"),
    readxl::read_xlsx(path = !!.x) |> 
      janitor::clean_names() |> 
      dplyr::select(1:3) |> 
      mutate(casrn = str_trim(casrn))
  ),
  tar_target(
    orange_cid,
    orange |> 
      filter(!is.na(casrn)) |> 
      mutate(casrn = str_trim(casrn)) |> 
      dplyr::distinct(casrn) |> 
      pull(casrn) |> 
      webchem::get_cid(match = "first",
                       verbose = TRUE)
  ),
  tar_target(
    to_add_orange,
    tibble::tribble(
      ~query, ~cid,
      "27043-05-6", "26334"
    )
  ),
  tar_target(
    orange_cid_all,
    orange_cid |> 
      dplyr::filter(!is.na(cid)) |> 
      dplyr::bind_rows(to_add_orange) 
  ),
  tar_target(
    orange_chem,
    orange_cid_all |>
      dplyr::bind_rows(to_add_orange) |>
      dplyr::distinct(cid) |>
      tidyr::drop_na(cid) |>
      pull(cid) |>
      webchem::pc_prop(verbose = TRUE) |>
      dplyr::relocate(IUPACName, 1) 
  ),
  tar_target(
    orange_chem_all,
    orange_cid_all |> 
      dplyr::rename(CASRN = query, CID = cid) |> 
      dplyr::mutate(CID = as.numeric(CID)) |> 
      dplyr::right_join(orange_chem, by ="CID") |> 
      dplyr::relocate(CASRN:CID, .after = IUPACName) |>
      dplyr::distinct(CID, .keep_all = TRUE) |>
      dplyr::select(IUPACName:InChIKey)
  ),
  tar_target(
    orange_plus,
    orange_chem_all |> 
      bind_rows(solution)  |> 
      distinct(CID, .keep_all = TRUE)  
    
  )

)
