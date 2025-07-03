# datachangelog

<!-- badges: start -->
[![R-CMD-check](https://github.com/ancientoceanslab/datachangelog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ancientoceanslab/datachangelog/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ancientoceanslab/datachangelog/graph/badge.svg)](https://app.codecov.io/gh/ancientoceanslab/datachangelog)
[![CRAN status](https://www.r-pkg.org/badges/version/datachangelog)](https://CRAN.R-project.org/package=datachangelog)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/datachangelog)](https://cran.r-project.org/package=datachangelog)
[![R-CMD-check](https://github.com/AncientOceansLab/datachangelog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AncientOceansLab/datachangelog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `datachangelog` is to facilitate easy comparison between two datasets, evaluate whether any changes in data records or fields has taken place, and generate a data changelog.

## Developer

- [Lewis A. Jones](mailto:Lewis.Jones@ucl.ac.uk), University College London

## Installation

The stable version of `datachangelog` can be installed from CRAN using:

```r
install.packages("datachangelog")
```

The development version of `datachangelog` can be installed via GitHub using:

```r
# install.packages("devtools")
devtools::install_github("ancientoceanslab/datachangelog")
```

## Example usage

A minimal example evaluating two datasets and generating a changelog report:

```r
# Load packages
library(datachangelog)

# Create original dataset
x <- mtcars
x$car <- row.names(x)

# Imitate changes in replacement dataset
y <- x
# Drop some fields (columns)
y <- y[, 4:ncol(y)]
# Drop some records (rows)
y <- y[6:nrow(y), 4:ncol(y)]
# Change some record values
y$qsec[5:8] <- y$qsec[5:8] * 2
# Evaluate changes
evaluate(x = x, y = y, by = "car", report = TRUE, 
         metadata = list(Title = "A really cool dataset",
                         Author = "Bob", 
                         Date = Sys.Date(),
                         Version = "0.0.1"))
```

Console output

```
Comparing common records between `x` and `y`.
Comparing common fields between `x` and `y`.
         car column original replacement
1   Merc 280   qsec     18.3        36.6
2  Merc 280C   qsec     18.9        37.8
3 Merc 450SE   qsec     17.4        34.8
4 Merc 450SL   qsec     17.6        35.2
Warning messages:
1: In evaluate(x = x, y = y, by = "car", report = TRUE, metadata = list(Title = "A really cool dataset",  :
  Records present in `x` but not `y`: Mazda RX4, Mazda RX4 Wag, Datsun 710, Hornet 4 Drive, Hornet Sportabout
2: In evaluate(x = x, y = y, by = "car", report = TRUE, metadata = list(Title = "A really cool dataset",  :
  Fields present in `x` but not `y`: mpg, cyl, disp, hp, drat, wt
3: In evaluate(x = x, y = y, by = "car", report = TRUE, metadata = list(Title = "A really cool dataset",  :
  Record values have changed between `x` and `y`.
```

Markdown report output

```
# Changelog 

Title: A really cool dataset
Author: Bob
Date: 20272
Version: 0.0.1 

## The following fields have been removed: 

-  mpg
-  cyl
-  disp
-  hp
-  drat
-  wt 

## The following records have been removed: 

-  Mazda RX4
-  Mazda RX4 Wag
-  Datsun 710
-  Hornet 4 Drive
-  Hornet Sportabout 

## The following record values have been changed:

Merc 280 

- qsec: 18.3 -> 36.6

Merc 280C 

- qsec: 18.9 -> 37.8

Merc 450SE 

- qsec: 17.4 -> 34.8

Merc 450SL 

- qsec: 17.6 -> 35.2

```

## Funding

The development of this R package was supported by a NERC Independent Research Fellowship (UKRI185) awarded to Lewis A. Jones.


