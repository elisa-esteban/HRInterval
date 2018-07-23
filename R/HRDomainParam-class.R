setClassUnion('characterOrNULL', c('character', 'NULL'))
#' Clase S4 HRFactorParam para los parámetros de la función IntervalUnitHitRate
#'
#' Definición de la clase S4 \code{HRDomainParam} que contiene los parámetros que utiliza la función
#' \code{\link{IntervalDomainHitRate}} para el cálculo del hit rate de un conjunto de intervalos
#' para un determinado dominio.
#' @slot VarRoles lista con las siguientes componentes:
#' \item Units objeto de clase \code{data.table} con las unidades para las que se van a calcular
#' los intervalos.
#' \item ObjVariable \code{Vector} de tipo \code{character} de longitud 1 con el nombre de la variable
#' para la que se están construyendo los intervalos de validación.
#' \item Domains \code{Vector} con los nombres de las variables que actúan como factores.
#' \item IntervalsLimits \code{vector} de longitud 2 con los nombres utilizados en el repositorio
#' para los extremos inferior y superior de los intervalos de validación.
#' \item EditName \code{Vector} de tipo \code{character} de longitud 1 con el nombre del edit para el
#' que se está construyendo los intervalos de validación.
#'
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'HRDomainParam')
#'
#' @import data.table StQ
#'
#' @export
setClass(Class = 'HRDomainParam',
         slots = c(VarRoles = 'list'),
         prototype = list(VarRoles = list(Units = character(0),
                                          ObjVariable = character(0),
                                          Domains = character(0),
                                          IntervalsLimits = c('Parametro_07._5.1.1.3.','Parametro_07._5.1.1.4.'),
                                          EditName = character(0))),

  validity = function(object){

    VarRoles <- slot(object, 'VarRoles')
    if (!all(names(VarRoles) %in% c('Units', 'ObjVariable', 'Domains', 'IntervalsLimits', 'EditName'))) {

      stop('[HRDomainParam: validity] All components of VarRoles must be one of these: Units, ObjVariable, Domains, IntervalsLimist, EditName')

    }

    if (length(object@VarRoles[['EditName']]) != 1) stop('[HRDomainParam validation] The component EditName of VarRoles must be a character vector of length 1.')
    if (length(object@VarRoles[['ObjVariable']]) != 1) stop('[HRDomainParam validation] The component ObjVariable of VarRoles must be a character vector of length 1.')
    if (length(object@VarRoles[['IntervalsLimits']]) != 2) stop('[HRDomainParam validation] The component IntervalsLimits of VarRoles must be a character vector of length 2 with the names of interval limits.')
    if (!all(object@VarRoles[['IntervalsLimits']] %in% c('Parametro_07._5.1.1.3.', 'Parametro_07._5.1.1.4.', 'Parametro_07.__5.1.1.3.', 'Parametro_07.__5.1.1.4.'))) stop('[HRDomainParam validation] Los nombres especificados en la componente IntervalsLimits de VarRoles no se corresponden con los nombres de los intervalos en el slot IntervalData.')

    # IDQuals <- object@VarRoles[['Units']]
    # if (length(intersect(IDQuals, unique(unlist(getIDQual(object@RawData))))) != length(IDQuals)) stop('[HRDomainParam validation] Los calificadores de unidad del slot RawData no se corresponden con las unidades especificadas en Units.')
    # if (length(intersect(IDQuals, unique(unlist(getIDQual(object@EdData))))) != length(IDQuals)) stop('[HRDomainParam validation] Los calificadores de unidad del slot EdData no se corresponden con las unidades especificadas en Units.')
    # if (length(intersect(IDQuals, unique(unlist(getIDQual(object@IntervalData))))) != length(IDQuals)) stop('[HRDomainParam validation] Los calificadores de unidad del slot EdData no se corresponden con las unidades especificadas en IntervalData.')
    #
    # IDDD_RawData <- getIDDD(object@RawData)
    # if (!ExtractNames(object@VarRoles[['ObjVariable']]) %in% IDDD_RawData) stop('[HRDomainParam validation] El slot RawData no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles.')
    # if (!all(ExtractNames(object@VarRoles[['Domains']]) %in% IDDD_RawData)) stop('[HRDomainParam validation] El parámetro RawData no contiene datos sobre alguna de las variables especificadas en la componente Domains del slot VarRoles.')
    #
    # IDDD_EdData <- getIDDD(object@EdData)
    # if (!ExtractNames(object@VarRoles[['ObjVariable']]) %in% IDDD_EdData) stop('[HRDomainParam validation] El slot EdData no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles.')
    # if (!all(ExtractNames(object@VarRoles[['Domains']]) %in% IDDD_EdData)) stop('[HRDomainParam validation] El slot EdData no contiene datos sobre alguna de las variables especificadas en la compontente Domains del slot VarRoles.')
    #
    # Edit_IntervalData <- unique(getData(object@IntervalData)[['IDEdit']])
    # if (!object@VarRoles[['EditName']] %in% Edit_IntervalData) stop('[HRDomainParam validation] El slot IntervalData no contiene datos sobre el edit especificado en la componente EditName del slot VarRoles.')

    return(TRUE)
  }
)
