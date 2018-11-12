#' @title Clase S4 para la actualizacion de los parametros del Hit Rate por unidad
#'
#' @description Definicion de la clase S4 llamada \code{UpdateHRFactorParam} para
#' la actualizacion de los parametros utilizados en el calculo del Hit Rate por
#' unidad en la fase longitudinal de la depuracion selectiva.
#'
#' @slot EditName Vector caracter con los nombres de los edits cuyos parametros
#'  queremos actualizar.
#'
#' @slot Units data.table con las unidades para las que se van a modificar los
#' parametros.
#'
#' @slot Values lista con los valores de los parametros que intervienen en el
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
