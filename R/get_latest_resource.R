
get_latest_resource <- function(resrouce_id){
  ckanr::ckanr_setup("www.opendata.nhs.scot")
  ckanr::resource_show(resrouce_id)
}
