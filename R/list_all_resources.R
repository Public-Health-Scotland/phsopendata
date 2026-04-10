#' Deprecated: please use `list_resources()` instead.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `list_all_resources()` was renamed to [list_resources()] for naming consistency.
#'
#' @param dataset_contains A character string containing an expression to be
#'  used as search criteria against the dataset 'title' field.
#' @param resource_contains A character string containing a regular expression
#'  to be matched against available resource names.
#'
#' @inherit list_resources return examples
#'
#' @keywords internal
#' @export
list_all_resources <- function(
    dataset_contains = NULL,
    resource_contains = NULL
) {
  lifecycle::deprecate_warn(
    when = "1.0.3",
    what = "list_all_resources()",
    with = "list_resources()"
  )
  list_resources(
    dataset_contains = dataset_contains,
    resource_contains = resource_contains
  )
}
