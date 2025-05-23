

#' @title Additional S3 Methods for \link[multcomp]{glht} Object
#' 
#' @param x a \link[multcomp]{glht} object
#' 
#' @param level ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(multcomp)
#' m0 = lm(breaks ~ tension + wool, data = warpbreaks)
#' cp = mcp(tension = 'Tukey', wool = 'Dunnett')
#' m1 = aov(breaks ~ tension + wool, data = warpbreaks) |> glht(linfct = cp)
#' m2 = m0 |> glht(linfct = cp)
#' summary(m0)
#' summary(m1)
#' summary(m1, test = adjusted('none'))
#' .pval.glht(m1)
#' 
#' @name S3_glht
#' @importFrom stats confint
#' @export
confint_.glht <- function(x, level = .95, ...) {
  x |>
    confint(level = level, ...) |> # ?multcomp:::confint.glht
    confint_.confint.glht()
}

#' @rdname S3_glht
#' @export
confint_.confint.glht <- function(x, ...) {
  ci_ <- x$confint
  ci <- ci_[, -1L, drop = FALSE] # three columns, 'Estimate', 'lwr' and 'upr'
  attr(ci, which = 'conf.level') <- attr(ci_, which = 'conf.level', exact = TRUE)
  return(ci)
}

#' @rdname S3_glht
#' @importFrom multcomp adjusted
#' @export
.pval.glht <- function(x) {
  # different from tzh::.pval.default !!!
  x |>
    summary(test = adjusted(type = 'none')) |> # ?multcomp:::summary.glht
    .pval.summary.glht()
  # adjusted p-values simply confuses collaborators
}

#' @rdname S3_glht
#' @export
.pval.summary.glht <- function(x) (x$test) |> .pval.mtest()

#' @rdname S3_glht
#' @export
.pval.summary.gtest <- function(x) (x$test) |> .pval.gtest()

#' @rdname S3_glht
#' @export
.pval.mtest <- function(x) {
  p <- x$pvalues # unnamed ..
  names(p) <- names(x$coefficients)
  return(p)
}


#' @rdname S3_glht
#' @export
.pval.gtest <- function(x) stop('not programmed yet (should be easy)')


#' @rdname S3_glht
#' @export
Sprintf.glht <- function(x) {
  'Select linear contrast(s) are created using <u>**`R`**</u> package <u>**`multcomp`**</u>.'
}




# below: might not need anymore

# @importFrom stats terms
# @export
# terms.glht <- function(x, ...) terms(x$model, ...)

# @importFrom stats formula
# @export
# formula.glht <- function(x, ...) formula(x$model, ...)

# @export
# getLinfct.glht <- function(x, ...) x$linfct





