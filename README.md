zhaoy provides functions for 5 data tasks: transfer, tidy, transform, summarize, and check.

Some advantages of zhaoy functions:

- Data-transfer functions convert relative file-paths to absolute file-paths, so there's no need to set working directories.

- First arguments are data, so functions are compatible with the [tidyverse](https://www.tidyverse.org) pipe.

- Within data tasks, function names and arguments are consistent.

# transfer

Microsoft Excel: `import_excel` (deprecates `i_excel`) and `export_excel`

[feather](https://github.com/wesm/feather): `import_feather` (deprecates `i_feather`) and `export_feather`

R-specific binary files: `import_rds` and `export_rds`

`path`: convert relative file-paths to absolute

# tidy

`tidy_activity`: tidy the Provide Enterprise "Activity Summary by Provider by Client by Date" report

`tidy_lab_pe`: tidy the Provide Enterprise "Test Results by Client with ID" report

# transform

`lc_df` (deprecates `i_df`): convert upper-case English characters to lower-case, in data-frames

`lz_id`: include or exclude leading zeros in medical record numbers (MRNs)

# summarize

`s_cross_n`: cross-tabulate counts of unique values, including missing-data, in 2 or 3 variables 

`s_cross_p`: cross-tabulate percents of unique values, including missing-data, in 2 or 3 variables

`s_cross_np`: cross-tabulate counts and percents of unique values, including missing-data, in 2 or 3 variables

`s_mode`: statistical mode 

`s_s`: tabulate summary statistics

`s_unique`: tabulate counts and percents of unique values, including missing-data, in vectors

# check

`c_assert`: check assertions about data