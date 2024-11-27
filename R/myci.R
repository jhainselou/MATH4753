#' Confidence Interval
#'
#' @param x a sample
#'
#' @return a confidence interval
#' @export
#'
#' @examples myci(c(1,2,3,4,5,6,7,8,9))
myci <- function(x){
  t.test(x,conf.level=0.95)$conf
}



