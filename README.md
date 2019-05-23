zhaoy provides functions for 5 data tasks: transfer, tidy, transform, summarize, and check.

Some advantages of zhaoy functions:

- Data-transfer functions convert relative file-paths to absolute file-paths, so there's no need to 1) hard-code file-paths or 2) set working directories.

- First arguments are data, so functions are compatible with the [tidyverse](https://www.tidyverse.org) pipe.

- Within data tasks, function names and arguments are consistent.

# transfer

Microsoft Excel: `import_excel` (deprecates `i_excel`) and `export_excel`

[feather](https://github.com/wesm/feather): `import_feather` (deprecates `i_feather`) and `export_feather`

R-specific binary files: `import_rds` and `export_rds`

`file_path` converts relative file-paths to absolute file-paths

# tidy

`tidy_activity`

`tidy_lab_pe`

# transform

`lc_df` (deprecates `i_df`)

`lz_id`

# summarize

`s_cross_n`, `s_cross_p`, `s_cross_np`

`s_mode`

`s_s`

`s_unique`

# check

`c_assert`