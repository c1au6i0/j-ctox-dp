
<!-- README.md is generated from README.Rmd. Please edit that file -->

# J-ctox-db

<!-- badges: start -->

<!-- badges: end -->

Analysis asked by J Fu, 7-14-25:

1.  DPS Review (PDF) o Contains the new ingredient list
2.  “New list EX-clean version” (Excel) o Table extracted from the DPS
    review for easier reference
3.  Master List (Excel) o EX bundles that have completed CompTox
    evaluation and literature search Requested Analysis: Could you
    please compare these lists to identify how many ingredients from the
    new list are not included in the master list? This comparison will
    help us determine which ingredients require further evaluation.

Project uses `{targets}`and `{renv}` and `{here}`.

`data\raw` : raw data given by JF. `New List Ex-clean version.xlsx` cis
the new
`Master list_waterpipe chemical database (for the EX program and waterpipe fillers only)_2025.xlsx`
is the one that they created.

`data\other`: cleaned up version of the files.

`code\` : functions called by `_targets.R` in
`here::here("code", "targets_functions"))`

`reports\`: output files

## Reports

Master list_waterpipe chemical database (for the EX program and
waterpipe fillers only)\_2025.xlsx = A = main_dat = `main_wp_clean.xlsx`

New List EX-clean version.xlsx = B = ex_cid_chem = `ex_cid_chem.xlsx`

solution = ex_dat - main_dat = 33 = `outer_to_check.xlsx`

orange_plus = solution + orange = `orange_plux.xlsx`

We want: chem_to_eval = Orange_plus – main_dat (pretty much we check the
one that are not in main_dat) Actually after discussion, this will just
be a double check.

# Howto

Initialize and install packages with:

    renv::restore()

Run the pipeline.

    targets::tar_make()

Check manifest with `targets::tar_manifest()` and Load targets with
`targets::tar_load("name_of_target)`
