# hmdbconc

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/wilsontom/hmdbconc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wilsontom/hmdbconc/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/wilsontom/hmdbconc/branch/master/graph/badge.svg)](https://app.codecov.io/gh/wilsontom/hmdbconc?branch=master)![License](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg "GNU GPL v3.0") 

## Getting Started

`hmdbconc` can be installed directly from GitHub using the remotes package.

```r
remotes::install_github('wilsontom/hmdbconc')
```
To retrieve the "Normal Concentration" table for a single accession. 

```r
methyl_histidine <- hmdbconc::get_concentrations('0000001')

str(methyl_histidine)

tibble [49 × 9] (S3: tbl_df/tbl/data.frame)
 $ name               : chr [1:49] "1-Methylhistidine" "1-Methylhistidine" "1-Methylhistidine" "1-Methylhistidine" ...
 $ accession          : 'glue' chr [1:49] "HMDB0000001" "HMDB0000001" "HMDB0000001" "HMDB0000001" ...
 $ biospecimen        : chr [1:49] "Blood" "Blood" "Blood" "Blood" ...
 $ concentration_value: chr [1:49] NA "7.7 +/- 1.9" "14.4 +/- 2.3" "19.6 +/- 2.6" ...
 $ concentration_units: chr [1:49] NA "uM" "uM" "uM" ...
 $ subject_age        : chr [1:49] "Adult (>18 years old)" "Adult (>18 years old)" "Adult (>18 years old)" "Adult (>18 years old)" ...
 $ subject_sex        : chr [1:49] "Both" "Both" "Both" "Both" ...
 $ subject_condition  : chr [1:49] "Normal" "Normal" "Normal" "Normal" ...
 $ pubmed             : chr [1:49] "32966057" "7061274" "7061274" "7061274" ...
 
 ```

To iterate over multiple accessions and combine the results

```r
accession_ids <- c('0000001', '0000002', '0000005')

concentration_tables <-
  purrr::map(accession_ids, hmdbconc::get_concentrations) %>%
  dplyr::bind_rows()


str(concentration_tables)

tibble [71 × 9] (S3: tbl_df/tbl/data.frame)
 $ name               : chr [1:71] "1-Methylhistidine" "1-Methylhistidine" "1-Methylhistidine" "1-Methylhistidine" ...
 $ accession          : 'glue' chr [1:71] "HMDB0000001" "HMDB0000001" "HMDB0000001" "HMDB0000001" ...
 $ biospecimen        : chr [1:71] "Blood" "Blood" "Blood" "Blood" ...
 $ concentration_value: chr [1:71] NA "7.7 +/- 1.9" "14.4 +/- 2.3" "19.6 +/- 2.6" ...
 $ concentration_units: chr [1:71] NA "uM" "uM" "uM" ...
 $ subject_age        : chr [1:71] "Adult (>18 years old)" "Adult (>18 years old)" "Adult (>18 years old)" "Adult (>18 years old)" ...
 $ subject_sex        : chr [1:71] "Both" "Both" "Both" "Both" ...
 $ subject_condition  : chr [1:71] "Normal" "Normal" "Normal" "Normal" ...
 $ pubmed             : chr [1:71] "32966057" "7061274" "7061274" "7061274" ...

```


