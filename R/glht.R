
#' @title \link[multcomp]{glht} Object
#' 
#' @examples
#' library(ecip); list(
#'  '`aov`' = aov(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett'))
#' ) |> fastmd::render2html()
#' 
#' m = lm(breaks ~ tension + wool, data = warpbreaks)
#' library(ecip); list(
#'  '`lm`, single `$focus`' = m |> 
#'    glht(linfct = mcp(tension = 'Tukey')),
#'  '`lm`, multiple `$focus`' = m |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett'))
#' ) |> fastmd::render2html()
#' 
#' @name glht
NULL

#' @importFrom fastmd md_ md_flextable_
#' @export
md_.glht <- function(x, xnm, ...) {
  
  if (!is.character(xnm)) xnm <- deparse1(xnm)
  
  z1 <- md_(x = x$model, xnm = paste0(xnm, '$model'), ...)
  
  z2 <- 'Select linear contrast(s) are created using <u>**`R`**</u> package <u>**`multcomp`**</u>.' |>
    new(Class = 'md_lines', package = 'multcomp')
  
  z3 <- md_flextable_(xnm = xnm, ...)
  
  c(z1, z2, z3) # fastmd::c.md_lines
}









#' @importFrom ecip confint_
#' @export
confint_.glht <- function(x, level = .95, ...) {
  x |>
    confint(level = level, ...) |> # ?multcomp:::confint.glht
    confint_.confint.glht()
}

#' @importFrom ecip confint_
#' @method confint_ confint.glht
#' @export
confint_.confint.glht <- function(x, ...) {
  ci_ <- x$confint
  ci <- ci_[, -1L, drop = FALSE] # three columns, 'Estimate', 'lwr' and 'upr'
  attr(ci, which = 'conf.level') <- attr(ci_, which = 'conf.level', exact = TRUE)
  return(ci)
}

#' @importFrom multcomp adjusted
#' @importFrom ecip .pval
#' @export
.pval.glht <- function(x) {
  # different from tzh::.pval.default !!!
  x |>
    summary(test = adjusted(type = 'none')) |> # ?multcomp:::summary.glht
    .pval.summary.glht()
  # adjusted p-values simply confuses collaborators
}

#' @importFrom ecip .pval
#' @method .pval summary.glht
#' @export
.pval.summary.glht <- function(x) (x$test) |> .pval.mtest()

#' @importFrom ecip .pval
#' @method .pval summary.gtest
#' @export
.pval.summary.gtest <- function(x) (x$test) |> .pval.gtest()

#' @importFrom ecip .pval
#' @export
.pval.mtest <- function(x) {
  p <- x$pvalues # unnamed ..
  names(p) <- names(x$coefficients)
  return(p)
}

#' @importFrom ecip .pval
#' @export
.pval.gtest <- function(x) stop('not programmed yet (should be easy)')




#' @importFrom ecip estnm
#' @export
estnm.glht <- function(x) estnm(x$model)



#' @importFrom ecip expcoef
#' @export
expcoef.glht <- function(x) expcoef(x$model)


#' @importFrom ecip endpoint
#' @export
endpoint.glht <- function(x) endpoint(x$model)


#' @importFrom ecip nobsText
#' @export
nobsText.glht <- function(x) nobsText(x$model)







# below: might not need anymore

# @importFrom stats terms
# @export
# terms.glht <- function(x, ...) terms(x$model, ...)

# @importFrom stats formula
# @export
# formula.glht <- function(x, ...) formula(x$model, ...)

# @export
# getLinfct.glht <- function(x, ...) x$linfct





