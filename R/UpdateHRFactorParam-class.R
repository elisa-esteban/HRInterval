#' @title Clase S4 para la actualización de los parámetros del Hit Rate por unidad
#'
#' @description Definición de la clase S4 llamada \code{UpdateHRFactorParam} para
#' la actualización de los parámetros utilizados en el cálculo del Hit Rate por
#' unidad en la fase longitudinal de la depuración selectiva.
#'
#' @slot EditName Vector carácter con los nombres de los edits cuyos parámetros
#'  queremos actualizar.
#'
#' @slot Units data.table con las unidades para las que se van a modificar los
#' parámetros..
#'
#' @slot Values lista con los valores de los parámetros que intervienen en el
#' cálculo del HitRate que se desean actualizar.
#' .
#'
#' @examples
#' new(Class = 'UpdateHRFactorParam')
#'
#' @import data.table
#'
#' @export
setClass(Class = "UpdateHRFactorParam",
         slots = c(EditName = 'character',
                   Units = 'data.table',
                   Values = 'list'),
         prototype = list(EditName = character(0),
                          Units = data.table(),
                          Values = list()),
         validity = function(object){

           if (!is.null(object@Values) & !all(names(object@Values) %in% c('MinFactor', 'MaxFactor', 'HRUnit', 'CHRUnit', 'HRDomain', 'CHRDomain', 'HRlambda', 'CHRlambda'))) {
             stop('[UpdateHRFactorParam::validity] Los valores en el slot Values deben ser MinFactor, MaxFactor, HRUnit, CHRUnit, HRDomain, CHRDomain, HRlambda o CHRlambda.')
           }

           return(TRUE)
         }
)
