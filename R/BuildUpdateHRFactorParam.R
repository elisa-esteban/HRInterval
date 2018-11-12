#' @title Constructor de objetos de clase \linkS4class{UpdateHRFactorParam}
#'
#' @description Este constructor devuelve un objeto de clase \linkS4class{UpdateHRFactorParam}.
#' a partir de un conjunto de parametros.
#'
#' @param EditName \code{Vector} de tipo \code{character} de longitud 1 con el nombre
#' del Edit para el que se establece la condicion.
#'
#' @param Units \code{data.table} con el conjunto de unidades cuyos parametros se
#' desea actualizar.
#'
#' @param Values  \code{list} con los nuevos valores de los distintos parametros del
#' HitRate que se quieren actualizar.
#'
#'
#' @return Un objeto de clase \linkS4class{UpdateHRFactorParam} with components specified in the input
#' parameter Data. Depending on the specified components, the output object will be
#' of classes \code{GenConditionParam}, \code{FixedConditionParam} or \code{TSConditionParam}.
#'
#'
#' @examples
#' \dontrun{
#' UpdateHRFactorParam <- BuildUpdateHRFactorParam(EditName = 'LCN_W_1',
#'                                                 )
#' UpdateHRFactorParam
#' }
#'
#' @include UpdateHRFactorParam-class.R
#'
#' @import data.table StQ
#'
#' @export
BuildUpdateHRFactorParam <- function(EditName = character(0),
                                     Units = data.table(),
                                     Values = list()){

  if (missing(EditName)) stop("[HRInterval:: BuildUpdateHRFactorParam] 'EditName' must be a character vector of length 1.")

  if (missing(Units)) stop("[HRInterval:: BuildUpdateHRFactorParam] 'Units' must be a data.table with the units to update.")

  UpdateHRFactorParam <- new(Class = 'UpdateHRFactorParam',
                             EditName = EditName,
                             Units = Units,
                             Values = Values)

  return(UpdateHRFactorParam)

}

