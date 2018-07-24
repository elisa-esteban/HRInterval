setClassUnion('characterOrNULL', c('character', 'NULL'))
#' Clase S4 HRFactorParam para los parámetros de la función IntervalUnitHitRate
#'
#' Definición de la clase S4 \code{HRUnitParam} que contiene los parámetros que utiliza la función
#' \code{\link{IntervalUnitHitRate}} para el cálculo del hit rate de un conjunto de unidades.
#'
#' @slot VarRoles lista con las siguientes componentes:
#'
#' \itemize{
#' \item \code{Units} objeto de clase data.table con las unidades para las que se van a calcular
#' los intervalos.
#' \item \code{ObjVariable} Vector de tipo caracter de longitud 1 con el nombre de la variable
#' para la que se están construyendo los intervalos de validación.
#' \item \code{IntervalsLimits} Vector de longitud 2 con los nombres utilizados en el repositorio
#' para los extremos inferior y superior de los intervalos de validación.
#'\item \code{EditName} Vector de tipo caracter de longitud 1 con el nombre del edit para el
#' que se está construyendo los intervalos de validación.
#' }
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'HRUnitParam')
#'
#' @import data.table StQ
#'
#' @export
setClass(Class = 'HRUnitParam',
         slots = c(VarRoles = 'list'),
         prototype = list(VarRoles = list(Units = character(0),
                                          ObjVariable = character(0),
                                          IntervalsLimits =  c('Parametro_07._5.1.1.3.','Parametro_07._5.1.1.4.'),
                                          EditName = character(0))),

         validity = function(object){

           VarRoles <- slot(object, 'VarRoles')
           if (!all(names(VarRoles) %in% c('Units', 'ObjVariable', 'IntervalsLimits', 'EditName'))) {

             stop('[HRUnitParam: validity] All components of VarRoles must be one of these: Units, ObjVariable, IntervalsLimist, EditName')

           }

           if (length(object@VarRoles[['EditName']]) != 1) stop('[HRUnitParam validation] The component EditName of VarRoles must be a character vector of length 1.')
           if (length(object@VarRoles[['ObjVariable']]) != 1) stop('[HRUnitParam validation] The component ObjVariable of VarRoles must be a character vector of length 1.')
           if (length(object@VarRoles[['IntervalsLimits']]) != 2) stop('[HRUnitParam validation] The component IntervalsLimits of VarRoles must be a character vector of length 2 with the names of interval limits.')
           if (!all(object@VarRoles[['IntervalsLimits']] %in% c('Parametro_07._5.1.1.3.', 'Parametro_07._5.1.1.4.', 'Parametro_07.__5.1.1.3.', 'Parametro_07.__5.1.1.4.'))) stop('[HRUnitParam validation] Los nombres especificados en la componente IntervalsLimits de VarRoles no se corresponden con los nombres de los intervalos en el slot IntervalData.')

           return(TRUE)
      }

    )
