
scriptSearch <- function(pattern, path=".", subdirs=TRUE, suffix="\\.[rR]$",
                         commentsIncluded=FALSE, ..., verbose=FALSE)
{
  grepFile <- function(x, commentsIncluded, ...) {
    if(!file.exists(x)) {
      warning("non-existent file: ", x)
      return(NULL)
    }
    suppressWarnings(theText <- readLines(x))
    subans <- trimws(grep(pattern, value=TRUE, x=theText, ...))
    if(!commentsIncluded && length(subans)) {
      subans <- subans[substr(subans, 1, 1) != "#"]
    }
    subans
  }

  # start of main function

  if(!file.exists(path)) {
    path <- substr(path, 1, nchar(path) - 1)
    if(!file.exists(path)) {
      stop("'path' does not exist, it seems")
    }
  }
  fileList <- list.files(path=path, pattern=suffix, full.names=TRUE,
                         recursive=subdirs)
  answer <- setNames(vector("list", length(fileList)), fileList)
  for(i in seq_along(fileList)) {
    if(verbose) {
      cat("checking:", fileList[i], "\n")
    }
    answer[[i]] <- grepFile(fileList[i], commentsIncluded=commentsIncluded,
                            ...)
  }
  answer[lengths(answer) > 0L]
}
