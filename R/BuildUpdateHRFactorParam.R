#' @title Constructor de objetos de clase \linkS4class{UpdateHRFactorParam}
#'
#' @description Este constructor devuelve un objeto de clase \linkS4class{UpdateHRFactorParam} a
#' partir de un conjunto de parametros.
#'
#' @param EditName \code{Vector} de tipo \code{character} de longitud 1 con el nombre del Edit para
#' el que se establece la condicion.
#'
#' @param Units \code{data.table} con el conjunto de unidades cuyos parametros se desean actualizar.
#'
#' @param Values lista con nombres que contiene los valores que actualizaran los parametros que 
#' intervienen en el calculo del HitRate por unidad.
#'
#'
#' @return Un objeto de clase \linkS4class{UpdateHRFactorParam} .
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

