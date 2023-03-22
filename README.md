# wolberg-std-lib
R code for working with voter data, and some other useful tools.

* SSH database connection in R (for Windows)
* SQL query wrappers
* Zip code cleanup
* Misc ETL functions

# Installing
Generate a personal access token: https://github.com/settings/tokens

You need the 'repo' access levels assigned.

```R
install.packages("remotes")
remotes::install_github(
  repo = 'tshynik/wolberg-std-lib',
  auth_token = '' # personal access token here, between quote marks
)

library(wolberg)
```

# Iterating
Some useful stuff for building + developing packages. HOT TIP: Create an RStudio project in the directory above where your package folder lives to run this stuff + (re)install from github when needed.

```R
# library(roxygen2)
roxygen2::roxygenize("workingameRica-pub")

# library(devtools)
devtools::document("wolberg-std-lib")
devtools::check("wolberg-std-lib", document=F)
devtools::check("wolberg-std-lib", document=T)

# compile package locally:
devtools::install("wolberg-std-lib")
```

TODO: figure out how to format and document data per https://r-pkgs.org/data.html#sec-data-data

# Connecting to miscellaneous PostgreSQL databases
Requires packages ```DBI``` and ```RPostgres```, which aren't loaded by ```wolberg```.

```R
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = '', # URL
  port = 5432, # the default for PSQL
  dbname = '',
  user = '',
  password = ''
)
```