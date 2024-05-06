show_datasets <- function(){
  data_sets <- phs_GET("package_list", "")$result

  return(data_sets)
}
