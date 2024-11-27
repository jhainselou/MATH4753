#' Title Normal Distribution Plot
#'
#' @param a upper limit of lower tail probability
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return plot shaded with lower tail probability
#' @export
#'
#' @examples
#' myncurve(1,1,1)
myncurve = function(mu,sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve <- seq((mu-(100*sigma)),a,length=10000)
  ycurve <- dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-(100*sigma),xcurve,a),c(0,ycurve,0),c="red")
  prob <- pnorm(a,mu,sigma)
  prob <- round(prob,4)
  list(mu = mu, sigma = sigma, area = prob)
}
