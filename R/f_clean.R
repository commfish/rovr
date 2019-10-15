#' Clean up raw data
#'
#' @param data
#'
#' @return
#' @export f_clean
#'
#' @examples
#'
#' f_clean(raw_data)
f_clean <- function(data){
  data %>%
    mutate(Dive = as.numeric(gsub(".*_", "", DIVE_NO))) %>%
    full_join(qc_sseo) %>%
    fill(Family) %>%
    filter(!is.na(Family))%>%
    dplyr::select(Seconds = SECONDS, Dive, ROV_X, ROV_Y, Family) %>%
    mutate(Dive = factor(Dive))
}
