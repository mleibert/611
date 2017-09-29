pmatrix = function(x, digits=NULL, ...) {
  library(xtable)
  default_args = list(include.colnames=FALSE, only.contents=TRUE,
                      include.rownames=FALSE, hline.after=NULL, comment=FALSE,
                      print.results=FALSE)
  passed_args = list(...)
  calling_args = c(list(x=xtable(x, digits=digits)),
                   c(passed_args,
                     default_args[setdiff(names(default_args), names(passed_args))]))
  cat("\\begin{pmatrix}\n",
      do.call(print.xtable, calling_args),
      "\\end{pmatrix}\n")
}
