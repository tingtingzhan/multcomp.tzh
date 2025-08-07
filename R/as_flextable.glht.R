


#' @title Convert \link[multcomp]{glht} Object to \link[flextable]{flextable}
#' 
#' @description ..
#'  
#' @param x \link[multcomp]{glht} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' Function [as_flextable.glht()] add additional \link[flextable]{hline}(s)
#' for multiple `$focus` in the input \link[multcomp]{glht} object.
#' 
#' @returns
#' Function [as_flextable.glht()] returns a \link[flextable]{flextable}.
#'  
#' @keywords internal  
#' @importFrom flextable as_flextable hline
#' @importFrom ecip ecip as_flextable.ecip
#' @export as_flextable.glht
#' @export
as_flextable.glht <- function(x, ...) {
  
  z <- x |> ecip() |> as_flextable.ecip(...)
  if (length(x$focus) == 1L) return(z)
  
  nm <- x |> coef() |> names() # multcomp:::coef.glht 
  
  id <- paste0('^', x$focus, '\\:') |>
    lapply(FUN = grep, x = nm)
  
  c1 <- id |> vapply(FUN = \(i) {
    if (length(i) == 1L) return(TRUE)
    identical(1L, i |> diff.default() |> unique.default())
  }, FUN.VALUE = NA) # each consecutive
  if (any(!c1)) return(z)
  
  c2 <- identical(1L, unlist(id) |> diff.default() |> unique.default()) # all consecutive
  if (!c2) return(z)
  
  h <- id |> lengths() |> cumsum()
  h <- h[-length(id)]
  z |> 
    hline(i = h)
  
}
