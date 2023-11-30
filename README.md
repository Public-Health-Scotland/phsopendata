<!-- README.md is generated from README.Rmd. Please edit that file -->

phsopendata
===========

<!-- badges: start -->

[![R-CMD-check](https://github.com/Public-Health-Scotland/phsopendata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Public-Health-Scotland/phsopendata/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Public-Health-Scotland/phsopendata/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Public-Health-Scotland/phsopendata?branch=master)
<!-- badges: end -->

`phsopendata` contains functions to interact with open data from the
[Scottish Health and Social Care Open Data
platform](https://www.opendata.nhs.scot/) via the CKAN API.

-   `get_resource()` extracts a single resource from an open dataset by
    resource id
-   `get_dataset()` extracts multiple resources from an open dataset by
    dataset name

For extracting metadata and search functionality, we recommend using the
[ckanr package](https://docs.ropensci.org/ckanr/).

`phsopendata` can be used on both the
[server](https://rstudio.nhsnss.scot.nhs.uk/) and desktop versions of
RStudio. However, depending on firewall settings, proxy use may need to
be configured with `use_proxy()`.

Installation
------------

You need to install `phsopendata` from GitHub, which requires a package
like `remotes` or `devtools`.

Using `remotes` you run this to install the package:

    remotes::install_github("Public-Health-Scotland/phsopendata",
      upgrade = "never"
    )

Examples
--------

### Downloading a data table with `get_resource()`

To extract a specific resource, you will need it’s unique identifier -
resource id. This can be found in the dataset metadata, the URL of a
resource’s page on
<a href="https://www.opendata.nhs.scot/" class="uri">https://www.opendata.nhs.scot/</a>,
or extracted using `ckanr::package_show`.

    library(phsopendata)

    # define a resource ID
    res_id <- "a794d603-95ab-4309-8c92-b48970478c14"

    # download the data from the CKAN database
    data <- get_resource(res_id = "a794d603-95ab-4309-8c92-b48970478c14")

### Querying/filtering data with `get_resource()`

You can define a row limit with the `rows` argument to get the first *N*
rows of a table.

    # get first 100 rows
    get_resource(
      res_id = "a794d603-95ab-4309-8c92-b48970478c14",
      rows = 100
    )

You can use `col_select` and `row_filters` to query the data server-side
(i.e., the data is filtered before it is downloaded to your machine).

    # get first 100 rows
    get_resource(
      res_id = "a794d603-95ab-4309-8c92-b48970478c14",
      col_select = c("GPPracticeName", "TelephoneNumber"),
      row_filters = list(
        HB = "S08000017",
        Dispensing = "Y"
      )
    )

### Downloading multiple tables with `get_dataset()`

To extract all resources from a dataset, you will need to use the
*dataset name*. Note that this will differ from the *dataset title* that
displays on the website. This can be found in the dataset metadata
extracted using `ckanr::package_show`, or taken from the dataset URL.

In this example, we are downloading GP Practice Population Demographics
from:
[opendata.nhs.scot/dataset/*gp-practice-populations*](https://www.opendata.nhs.scot/dataset/gp-practice-populations),
so the dataset name will be gp-practice-populations.

    # if max_resources is not set, all resources will be returned by default. 
    # Here we pull 10 rows from the first 2 resources only
    get_dataset("gp-practice-populations", max_resources = 2, rows = 10)

Contributing to phsopendata
---------------------------

At present, this package is maintained by the [PHS Open Data
team](phs.opendata@phs.scot).

If you have requests or suggestions for additional functionality, please
contact the package maintainer and/or the [PHS Open Data
team](phs.opendata@phs.scot).

If you would like to share examples of how you work with open data, you
can also do so in the [Open Data
repository](https://github.com/Public-Health-Scotland/Open-Data), where
example scripts and resources are collated.
