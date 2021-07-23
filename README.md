
<!-- README.md is generated from README.Rmd. Please edit that file -->
phsopendata
===========

<!-- badges: start -->
<!-- badges: end -->
`phsopendata` contains functions to interact with open data from the [Scottish Health and Social Care Open Data platform](https://www.opendata.nhs.scot/) via the CKAN API.

-   `get-resource` extracts a single resource from an open dataset by resource id
-   `get-dataset` extracts multiple resources from an open dataset by dataset name

For extracting metadata and search functionality, we recommend using the [ckanr package](https://docs.ropensci.org/ckanr/).

`phsopendata` can be used on both the [server](https://rstudio.nhsnss.scot.nhs.uk/) and desktop versions of RStudio. However, depending on firewall settings, proxy use may need to be configured with use\_proxy().

Installation
------------

To install `phsopendata`, the package `remotes` is required, and can be installed with `install.packages("remotes")`.

You can then install `phsopendata` on RStudio server from GitHub with:

``` r
remotes::install_github("Public-Health-Scotland/phsopendata",
  upgrade = "never"
)
```

Examples
--------

### get\_resource

To extract a specific resource, you will need it\`s unique identifier - resource id. This can be found in the dataset metadata by visiting the website, or extracted using ckanr::package\_show.

``` r
library(phsopendata)

#by default the full resource is returned
opendata_get_resource(res_id = "a794d603-95ab-4309-8c92-b48970478c14")
#> 
#> ── Column specification ───────────────────────────────────────────────────
#> cols(
#>   `_id` = col_double(),
#>   PracticeCode = col_double(),
#>   GPPracticeName = col_character(),
#>   PracticeListSize = col_double(),
#>   AddressLine1 = col_character(),
#>   AddressLine2 = col_character(),
#>   AddressLine3 = col_character(),
#>   AddressLine4 = col_character(),
#>   Postcode = col_character(),
#>   TelephoneNumber = col_character(),
#>   PracticeType = col_character(),
#>   Dispensing = col_character(),
#>   HB = col_character(),
#>   HSCP = col_character(),
#>   DataZone = col_character(),
#>   GPCluster = col_character()
#> )
#> # A tibble: 926 x 15
#>    PracticeCode GPPracticeName  PracticeListSize AddressLine1  AddressLine2
#>           <dbl> <chr>                      <dbl> <chr>         <chr>       
#>  1        10002 Muirhead Medic…             7980 Muirhead Med… Liff Road   
#>  2        10017 The Blue Pract…             6555 The Blue Pra… Crieff Medi…
#>  3        10036 Aberfeldy And …             4680 Aberfeldy An… Taybridge R…
#>  4        10106 Grove Health C…             6079 Grove Health… 129 Dundee …
#>  5        10125 Alyth Health C…             4346 Alyth Health… New Alyth R…
#>  6        10182 Arbroath Medic…             9127 Arbroath Med… 7 Hill Place
#>  7        10233 The Abbey Prac…             6203 Abbey Health… East Abbey …
#>  8        10286 East Practice,…             4454 East Practic… 30 Ponderla…
#>  9        10322 West Practice,…             7719 West Practic… 30 Ponderla…
#> 10        10361 St Margaret's …            10466 St Margaret'… St Margaret…
#> # … with 916 more rows, and 10 more variables: AddressLine3 <chr>,
#> #   AddressLine4 <chr>, Postcode <chr>, TelephoneNumber <chr>,
#> #   PracticeType <chr>, Dispensing <chr>, HB <chr>, HSCP <chr>,
#> #   DataZone <chr>, GPCluster <chr>

#but you can set the number of rows to return
opendata_get_resource(res_id = "a794d603-95ab-4309-8c92-b48970478c14", rows = 10)
#> # A tibble: 10 x 16
#>    `_id` PracticeCode GPPracticeName      PracticeListSize AddressLine1    
#>    <int>        <int> <chr>                          <int> <chr>           
#>  1     1        10002 Muirhead Medical C…             7980 Muirhead Medica…
#>  2     2        10017 The Blue Practice               6555 The Blue Practi…
#>  3     3        10036 Aberfeldy And Kinl…             4680 Aberfeldy And K…
#>  4     4        10106 Grove Health Centre             6079 Grove Health Ce…
#>  5     5        10125 Alyth Health Centre             4346 Alyth Health Ce…
#>  6     6        10182 Arbroath Medical C…             9127 Arbroath Medica…
#>  7     7        10233 The Abbey Practice              6203 Abbey Health Ce…
#>  8     8        10286 East Practice, Spr…             4454 East Practice, …
#>  9     9        10322 West Practice, Spr…             7719 West Practice, …
#> 10    10        10361 St Margaret's Heal…            10466 St Margaret's H…
#> # … with 11 more variables: AddressLine2 <chr>, AddressLine3 <chr>,
#> #   AddressLine4 <chr>, Postcode <chr>, TelephoneNumber <chr>,
#> #   PracticeType <chr>, Dispensing <chr>, HB <chr>, HSCP <chr>,
#> #   DataZone <chr>, GPCluster <chr>
```

### get\_dataset

To extract all resources from a dataset, you will need to use the dataset name. Note that this will differ from the dataset Title that displays on the website. This can be found in the dataset metadata extracted using ckanr::package\_show, or taken from the dataset URL.

In this example, we are downloading GP Practice Population Demographics from: opendata.nhs.scot/dataset/gp-practice-populations, so the dataset name will be gp-practice-populations.

``` r
#if max_resources is not set, all resources will be returned by default. Here we pull 10 rows from the first 2 #resources only
opendata_get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
#> # A tibble: 20 x 24
#>    `_id`     Date PracticeCode HB      HSCP    Sex   SexQF AllAges Ages0to4
#>    <int>    <int>        <int> <chr>   <chr>   <chr> <chr>   <int>    <int>
#>  1     1 20210401        10002 S08000… S37000… All   "d"      7980      354
#>  2     2 20210401        10002 S08000… S37000… Male  ""       3941      179
#>  3     3 20210401        10002 S08000… S37000… Fema… ""       4039      175
#>  4     4 20210401        10017 S08000… S37000… All   "d"      6555      221
#>  5     5 20210401        10017 S08000… S37000… Male  ""       3312      123
#>  6     6 20210401        10017 S08000… S37000… Fema… ""       3243       98
#>  7     7 20210401        10036 S08000… S37000… All   "d"      4680      151
#>  8     8 20210401        10036 S08000… S37000… Male  ""       2346       83
#>  9     9 20210401        10036 S08000… S37000… Fema… ""       2334       68
#> 10    10 20210401        10106 S08000… S37000… All   "d"      6079      170
#> 11     1 20210101        10002 S08000… S37000… All   "d"      7955      347
#> 12     2 20210101        10002 S08000… S37000… Male  ""       3926      175
#> 13     3 20210101        10002 S08000… S37000… Fema… ""       4029      172
#> 14     4 20210101        10017 S08000… S37000… All   "d"      6559      211
#> 15     5 20210101        10017 S08000… S37000… Male  ""       3310      114
#> 16     6 20210101        10017 S08000… S37000… Fema… ""       3249       97
#> 17     7 20210101        10036 S08000… S37000… All   "d"      4676      149
#> 18     8 20210101        10036 S08000… S37000… Male  ""       2359       81
#> 19     9 20210101        10036 S08000… S37000… Fema… ""       2317       68
#> 20    10 20210101        10106 S08000… S37000… All   "d"      6070      169
#> # … with 15 more variables: Ages0To4QF <chr>, Ages5to14 <int>,
#> #   Ages5To14QF <chr>, Ages15to24 <int>, Ages15To24QF <chr>,
#> #   Ages25to44 <int>, Ages25To44QF <chr>, Ages45to64 <int>,
#> #   Ages45To64QF <chr>, Ages65to74 <int>, Ages65To74QF <chr>,
#> #   Ages75to84 <int>, Ages75To84QF <chr>, Ages85plus <int>,
#> #   Ages85PlusQF <chr>
```

Contributing to phsopendata
---------------------------

At present, this package is maintained by [Csilla Scharle](https://github.com/csillasch).

If you have requests or suggestions for additional functionality, please contact the package maintainer and/or the [PHS Open Data team](phs.opendata@phs.scot).

If you would like to share examples of how you work with open data, you can also do so in the <https://github.com/Public-Health-Scotland/Open-Data> repository, where example scripts and resources are collated.
