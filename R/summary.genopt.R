
summary.genopt <- function(object, ...)
{
  answer <- list(call=object$call, summary.objectives=summary(object$objective),
                 best.solution=object$population[, 1L])
}
