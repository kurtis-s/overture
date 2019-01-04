#' Determine if a Metropolis–Hastings step should be accepted
#'
#' \code{AcceptProposal} is a utility function to determine if a proposal should
#' be accepted in a Metropolis or Metropolis-Hastings step.
#'
#' The function uses the Metropolis choice for a Metropolis/Metropolis-Hastings
#' sampler, which accepts a proposed value \eqn{x'} with probability \deqn{
#' A(x', x) = min(1, P(x')/P(x) g(x|x')/g(x'|x)) } where \eqn{P(x)} is the
#' target distribution and \eqn{g(x'|x)} is the proposal distribution.
#'
#' @param log.curr log density of the target at the current value,
#'   \eqn{log(P(x))}
#' @param log.prop log density of the target at the proposed value,
#'   \eqn{log(P(x'))}
#' @param log.curr.to.prop log of transition distribution from current value to
#'   proposed value, \eqn{log(g(x'|x))}
#' @param log.prop.to.curr log of transition distribution from proposed value to
#'   current value, \eqn{log(g(x|x'))}
#' @return \code{TRUE/FALSE} for whether the proposal should be accepted or
#'   rejected, respectively
#' @example examples/example-AcceptProposal.R
#' @export
AcceptProposal <- function(log.curr, log.prop, log.curr.to.prop=0,
                           log.prop.to.curr=0) {
    u <- stats::runif(1)
    log(u) <= (log.prop - log.curr + log.prop.to.curr - log.curr.to.prop)
}
