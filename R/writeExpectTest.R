
writeExpectTest <- function(expr, filename="", ...)
{
  tfile <- tempfile()
  on.exit(unlink(tfile))
  exsub <- deparse(substitute(expr))
  result <- paste(deparse(dput(expr, file=tfile)), collapse="\n")
  output <- paste0("expect_equal(", exsub, ",\n",
                   result, "\n)")
  cat(output, file=filename, ...)
}
