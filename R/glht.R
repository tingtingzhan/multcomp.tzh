

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
#' @importFrom ecip confint_
#' @export confint_.glht
#' @export
confint_.glht <- function(x, level = .95, ...) {
  x |>
    confint(level = level, ...) |> # ?multcomp:::confint.glht
    confint_.confint.glht()
}



#' @rdname S3_glht
#' @importFrom ecip confint_
#' @method confint_ confint.glht
#' @export confint_.confint.glht
#' @export
confint_.confint.glht <- function(x, ...) {
  ci_ <- x$confint
  ci <- ci_[, -1L, drop = FALSE] # three columns, 'Estimate', 'lwr' and 'upr'
  attr(ci, which = 'conf.level') <- attr(ci_, which = 'conf.level', exact = TRUE)
  return(ci)
}

#' @rdname S3_glht
#' @importFrom multcomp adjusted
#' @importFrom ecip .pval
#' @export .pval.glht
#' @export
.pval.glht <- function(x) {
  # different from tzh::.pval.default !!!
  x |>
    summary(test = adjusted(type = 'none')) |> # ?multcomp:::summary.glht
    .pval.summary.glht()
  # adjusted p-values simply confuses collaborators
}

#' @rdname S3_glht
#' @importFrom ecip .pval
#' @method .pval summary.glht
#' @export .pval.summary.glht
#' @export
.pval.summary.glht <- function(x) (x$test) |> .pval.mtest()

#' @rdname S3_glht
#' @importFrom ecip .pval
#' @method .pval summary.gtest
#' @export .pval.summary.gtest
#' @export
.pval.summary.gtest <- function(x) (x$test) |> .pval.gtest()

#' @rdname S3_glht
#' @importFrom ecip .pval
#' @export .pval.mtest
#' @export
.pval.mtest <- function(x) {
  p <- x$pvalues # unnamed ..
  names(p) <- names(x$coefficients)
  return(p)
}

#' @rdname S3_glht
#' @importFrom ecip .pval
#' @export .pval.gtest
#' @export
.pval.gtest <- function(x) stop('not programmed yet (should be easy)')



#' @rdname S3_glht
#' @importFrom ecip estnm
#' @export estnm.glht
#' @export
estnm.glht <- function(x) estnm(x$model)



#' @rdname S3_glht
#' @importFrom ecip expcoef
#' @export expcoef.glht
#' @export
expcoef.glht <- function(x) expcoef(x$model)


#' @rdname S3_glht
#' @importFrom ecip endpoint
#' @export endpoint.glht
#' @export
endpoint.glht <- function(x) endpoint(x$model)


#' @rdname S3_glht
#' @importFrom ecip nobsText
#' @export nobsText.glht
#' @export
nobsText.glht <- function(x) nobsText(x$model)


#' @title R Markdown Lines for \link[multcomp]{glht} Object
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(ecip)
#' 
#' list(
#'  '`aov`' = aov(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett')),
#'  '`lm`, single `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey')),
#'  '`glht` via `lm`, multiple `$focus`' = lm(breaks ~ tension + wool, data = warpbreaks) |> 
#'    glht(linfct = mcp(tension = 'Tukey', wool = 'Dunnett'))
#' ) |> rmd.tzh::render_(file = 'glht')
#' @keywords internal
#' @importFrom methods new
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom rmd.tzh md_
#' @importFrom flextable.tzh md_.aov
#' @export md_.glht
#' @export
md_.glht <- function(x, xnm, ...) {
  
  if (!is.character(xnm)) xnm <- deparse1(xnm)
  
  z0 <- md_(x$model, xnm = paste0(xnm, '$model'), ...)
  # must import ?flextable.tzh::md_.aov
  # !!!!!!!
  
  z1 <- 'Select linear contrast(s) are created using <u>**`R`**</u> package <u>**`multcomp`**</u>.' |>
    new(Class = 'md_lines', package = 'multcomp')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z0, z1, z2) # rmd.tzh::c.md_lines
  
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





