# phsopendata (development version)

- `get_dataset()` and `get_resource()` gain a new parameter `include_context` 
which allows adding context such as the resource ID and modified / created 
dates to returned data (#24).
- The magrittr pipe (`%>%`) is now exported (#29).
- `get_dataset()` will now suggest multiple dataset names, when the dataset 
you've asked for doesn't exist (i.e. there's a typo) and there are multiple 
likely candidates  (#28).

# phsopendata 0.1.0 (2021-07-22)

- Initial package release. 
- `get_dataset()`, `get_resource()` and `get_resource_sql()` functions added. 
