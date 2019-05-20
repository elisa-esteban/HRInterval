#' Actualizacion de los parametros para el calculo del factor hit rate.
#'
#' \code{UpdateParamHRFactor} Actualiza la lista de parametros para el calculo del factor hit rate
#' con el metodo \code{\link{HRFactor}}.
#'
#' @param object \linkS4class{data.table} con los valores de los parametros que intervienen en el
#' calculo del HitRate para un edit concreto, o lista de \linkS4class{data.table} con dichos
#' parametros para cada variable y cada edit.
#'
#' @param Param Objeto de clase \linkS4class{UpdateHRFactorParam} con las unidades y valores de los
#' parametros que se desean actualizar en el objeto de entrada \code{object}, o lista de objetos de
#' este tipo cuando el objeto de entrada \code{object} es una lista.
#'
#' @return \linkS4class{data.table} o lista de \linkS4class{data.table}, segun sea el objeto de
#' entrada \code{object}, con los parametros para el calculo de los factores hit rate del edit y
#' unidades seleccionados actualizados.
#'
#' @examples
#' \dontrun{
#' UpdateHRFactorParam(object, Param)
#' }
#'
#' @export
setGeneric("UpdateParamHRFactor", function(object, Param){standardGeneric("UpdateParamHRFactor")})
#' @rdname UpdateParamHRFactor
#'
#' @import data.table StQ
#'
#' @include UpdateHRFactorParam-class.R
#'
#' @export
setMethod(
  f = "UpdateParamHRFactor",
  signature = c("data.table", "UpdateHRFactorParam"),
  function(object, Param){

    Values <- Param@Values
    Units <- Param@Units

    if (length(Param) == 0L) return(object)

    IDQual <- names(Units)

    out <- subset(object, !(get(IDQual) %in% Units[[IDQual]])) #Unidades que no se van a actualizar

    oldParams <- subset(object, get(IDQual) %in% Units[[IDQual]]) #Unidades a actualizar
    newParams <- copy(Units)[, (names(Values)) := Values]

    colOrder <- names(oldParams)
    NoModifCols <- setdiff(names(oldParams), names(newParams))

    if (length(NoModifCols) > 0) {
      oldParams <- oldParams[, c(IDQual, NoModifCols), with = FALSE]
      newParams <- merge(newParams, oldParams, by = (IDQual))
      setcolorder(newParams, colOrder)
    }

    out <- rbindlist(list(out, newParams))
    setkeyv(out, IDQual)

    return(out)

  })
#' @rdname UpdateParamHRFactor
#'
#' @import data.table StQ
#'
#' @include UpdateHRFactorParam-class.R
#'
#' @export
setMethod(
  f = "UpdateParamHRFactor",
  signature = c("list", "UpdateHRFactorParam"),
  function(object, Param){

    EditName <- Param@EditName
    VarName <- unlist(strsplit(EditName, '_'))[1]

    dt <- object[[VarName]][[EditName]]
    object[[VarName]][[EditName]] <- UpdateParamHRFactor(dt, Param)

    return(object)

})
#' @rdname UpdateParamHRFactor
#'
#' @import data.table StQ
#'
#' @include UpdateHRFactorParam-class.R
#'
#' @export
setMethod(
  f = "UpdateParamHRFactor",
  signature = c("list", "list"),
  function(object, Param){
     for (i in seq(along = Param)){
       object <- UpdateParamHRFactor(object, Param[[i]])
     }
    return(object)
  })


