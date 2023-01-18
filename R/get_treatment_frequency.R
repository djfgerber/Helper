#' Get treatment frequency for a given disease for situation type
#' At the moment implemented:
#' - Categorize treatment frequency of STH, according to
#' World Health Organization. 2030 targets for soil-transmitted helminthiases control programmes.
#' Geneva: World Health Organization; 2020. Available: https://apps.who.int/iris/handle/10665/330611
#'
#' @param prevalence The prevalence needed to determine a treatment frequency
#' @param doc One of "sth_targets2030"
#' @param type One of "baseline" or "reassessment" in the case of STH.
#'
#' @return
#' @export
#'
#' @examples
get_treatment_frequency <- function(prevalence, doc, type){
  stopifnot(doc == "sth_targets2030")
  stopifnot(doc == "sth_targets2030" & (
    type %in% c("baseline", "reassessment")
  ))
  if(doc == "sth_targets2030"){
    if(type == "baseline"){
      if (prevalence < .2) {
        0
      }else if (prevalence < .5) {
        1
      }else {
        2
      }
    }else if(type == "reassessment"){
      if (prevalence < .02) {
        0
      }else if (prevalence < .1) {
        .5
      }else if (prevalence < .2) {
        1
      }else if (prevalence < .5) {
        2
      }else {
        3
      }
    }
  }

}
