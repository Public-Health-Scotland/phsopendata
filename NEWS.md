# phsopendata (development version)

# phsopendata 1.0.1 (2025-11-10)

- No user-facing changes. Fixes some tests that were failing when the open data platform is offline, and fixes tests that were failing due to a change in the open data resource. Tests are now more robust against changes to the data.  

# phsopendata 1.0.0 (2025-09-03)

- Initial CRAN release 🥳 - [CRAN.R-project.org/package=phsopendata](https://CRAN.R-project.org/package=phsopendata).

# phsopendata 0.4.0 (2025-02-13)

- Multiple filters can now be passed to the `row_filter` arguments of  [`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.html) and [`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.html). (#54)
- Improved error reporting. (#53)

# phsopendata 0.3.0 (2024-11-21)

- [`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.html) now takes the `col_select` and `row_filter` arguments allowing filtering of data. (#46)
- Failed calls will be retried up to 3 times using [`httr::RETRY()`](https://httr.r-lib.org/reference/RETRY.html). (#45 and #47)
- Improvements to [`get_resource_sql()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource_sql.html) error checks and messages and better handling of spaces and new lines. (#41)

# phsopendata 0.2.0 (2024-09-18)

- [`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.html) and [`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.html) gain a new parameter `include_context` 
which allows adding context such as the resource ID and modified / created 
dates to returned data (#24).
- The magrittr pipe (`%>%`) is now exported (#29).
- [`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.html) will now suggest multiple dataset names, when the dataset 
you've asked for doesn't exist (i.e. there's a typo) and there are multiple 
likely candidates  (#28).
- Two new functions [`list_datasets()`](https://public-health-scotland.github.io/phsopendata/reference/list_datasets.html) and [`list_resources()`](https://public-health-scotland.github.io/phsopendata/reference/list_resources.html) allow browsing
available datasets and resources (#10).
- The new function [`get_latest_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_latest_resource.html) retrieves the most recent resource from a dataset with additional context such as the resource ID and modified / created dates (#36).

# phsopendata 0.1.0 (2021-07-22)

- Initial package release. 
- [`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.html), [`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.html) and [`get_resource_sql()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource_sql.html) functions added. 
