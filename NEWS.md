# phsopendata 0.3.0 (2024-11-21)

- `get_dataset()` now takes the `col_select` and `row_filter` arguments allowing filtering of data.
- Failed calls will be retried up to 3 times using httr::RETRY.
- Improvements to `get_resource_sql` error checks and messages and better handling of spaces and new lines.

# phsopendata 0.2.0 (2024-09-18)

- `get_dataset()` and `get_resource()` gain a new parameter `include_context` 
which allows adding context such as the resource ID and modified / created 
dates to returned data (#24).
- The magrittr pipe (`%>%`) is now exported (#29).
- `get_dataset()` will now suggest multiple dataset names, when the dataset 
you've asked for doesn't exist (i.e. there's a typo) and there are multiple 
likely candidates  (#28).
- Two new functions `list_datasets()` and `list_resources()` allow browsing
available datasets and resources (#10).
- The new function `get_latest_resource()` retrieves the most recent resource from a dataset with additional context such as the resource ID and modified / created dates (#36).

# phsopendata 0.1.0 (2021-07-22)

- Initial package release. 
- `get_dataset()`, `get_resource()` and `get_resource_sql()` functions added. 
