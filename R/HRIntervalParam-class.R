#' Clase S4 HRIntervalParam para los parametros del metodo HRInterval
#'
#' Definicion de la clase S4 \code{HRIntervalParam} que contiene los parametros que utiliza el metodo
#' \code{\link{HRInterval}}.
#'
#' @slot Units objeto de clase \code{data.table} con las unidades para las que se van a calcular
#' los intervalos.
#'
#' @slot VarName \code{Vector} de tipo \code{character} de longitud 1 con el nombre de la variable
#' para la que se estan construyendo los intervalos de validacion.
#'
#' @slot Edit \code{Vector} de tipo \code{character} de longitud 1 con el nombre del edit para el
#' que se estan construyendo los intervalos de validacion.
#'
#' @slot DomainNames \code{Vector} de tipo \code{character} con los nombres de la variables que
#' establecen las celdas (particion de la muestra).
#'
#' @param IntervalsLimits \code{vector} de longitud 2 con los nombres utilizados en el repositorio
#' para los extremos inferior y superior de los intervalos de validacion.
#'
#' @slot LastFactor objeto de clase \code{data.table} con los valores del factor hit rate del edit
#' en el ultimo periodo para cada unidad.
#'
#' @slot BestTSPred Objeto de clase \linkS4class{data.table} con la mejor prediccion de entre un
#' conjunto de modelos para la variable considerada.
#'
#' @slot MinFactor objeto de clase \code{data.table} con los valores minimos del factor hit rate
#' para cada unidad.
#'
#' @slot MaxFactor objeto de clase \code{data.table} con los valores maximos del factor hit rate
#' para cada unidad.
#'
#' @slot HRUnit objeto de clase \code{data.table} con los valores del hit rate de los intervalos
#' para cada unidad.
#'
#' @slot CHRUnit objeto de clase \code{data.table} con los valores optimos de las unidades que no
#' han sido marcadas y efectivamente no tenian error para cada unidad.
#'
#' @slot HRDomain objeto de clase \code{data.table} con los valores del hit rate de los intervalos
#' para cada celda (particion de la muestra).
#'
#' @slot CHRDomain objeto de clase \code{data.table} con los valores optimos de las unidades que no
#' han sido marcadas y efectivamente no tenian error para cada celda (particion de la muestra).
#'
#' @slot HRlambda objeto de clase \code{data.table} para la estabilizacion del factor asociado al
#' \emph{hit-rate}. Debe ser, para cada unidad, un valor en el intervalo [0, 1].
#'
#' @slot CHRlambda objeto de clase \code{data.table} para la estabilizacion del factor asociado al
#' \emph{hit-rate}. Debe ser, para cada unidad, un valor en el intervalo [0, 1] y esta asociado al
#' numero de unidades no marcadas y sin error.
#'
#' @slot Lower objeto de clase \code{data.table}.
#'
#' @slot Upper objeto de clase \code{data.table}.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'HRIntervalParam')
#'
#' @import data.table StQ
#'
#' @export
setClass(
  Class = 'HRIntervalParam',
  slots = c(Units = 'data.table',
            VarName = 'character',
            Edit = 'character',
            DomainNames = 'characterOrNULL',
            IntervalsLimits = 'character',
            LastFactor = 'data.table',
            BestTSPred = 'data.table',
            MinFactor = 'data.table',
            MaxFactor = 'data.table',
            HRUnit = 'data.table',
            CHRUnit = 'data.table',
            HRDomain = 'data.table',
            CHRDomain = 'data.table',
            HRlambda = 'data.table',
            CHRlambda = 'data.table',
            Lower = 'data.table',
            Upper = 'data.table'
  ),
  validity = function(object){

    if (length(object@Edit) != 1) stop('[HRIntervalParam validation] Edit debe ser un vector de tipo character de longitud 1.')
    if (length(object@VarName) != 1) stop('[HRIntervalParam validation] VarName debe ser un vector de tipo character de longitud 1.')
    if (length(IntervalsLimits) != 2) stop('[HRIntervalParam validation] IntervalsLimits debe ser un vector de tipo character de longitud 2 con los nombres de los limites de los intervalos.')

    IDQuals <- names(object@Units)
    if (!identical(names(object@LastFactor), c(IDQuals, 'LastFactor'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot LastFactor han de ser: ', IDQuals, ' y ', 'LastFactor')
    if (!identical(names(object@MinFactor), c(IDQuals, 'MinFactor'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot MinFactor han de ser: ', IDQuals,  ' y ', 'MinFactor')
    if (!identical(names(object@MaxFactor), c(IDQuals, 'MaxFactor'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot MaxFactor han de ser: ', IDQuals, ' y ', 'MaxFactor')
    if (!identical(names(object@HRUnit), c(IDQuals, 'HRUnit'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot HRUnit han de ser: ', IDQuals, ' y ', 'HRUnit')
    if (!identical(names(object@CHRUnit), c(IDQuals, 'CHRUnit'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot HRUnit han de ser: ', IDQuals, ' y ', 'CHRUnit')
    if (!identical(names(object@HRDomain), c(IDQuals, 'HRDomain'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot HRDomain han de ser: ', IDQuals, ' y ', 'HRDomain')
    if (!identical(names(object@CHRDomain), c(IDQuals, 'CHRDomain'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot HRDomain han de ser: ', IDQuals, ' y ', 'CHRDomain')
    if (!identical(names(object@HRlambda), c(IDQuals, 'HRlambda'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot HRlambda han de ser: ', IDQuals, ' y ', 'HRlambda')
    if (!identical(names(object@CHRlambda), c(IDQuals, 'CHRlambda'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot CHRlambda han de ser: ', IDQuals, ' y ', 'CHRlambda')
    if (!identical(names(object@Lower), c(IDQuals, 'Lower'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot Lower han de ser: ', IDQuals, ' y ', 'Lower')
    if (!identical(names(object@Upper), c(IDQuals, 'Upper'))) stop('[HRIntervalParam validation] Los nombres de la data.table del slot Upper han de ser: ', IDQuals, ' y ', 'Upper')

    if (dim(merge(object@LastFactor, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y LastFactor no coinciden.')
    if (dim(merge(object@MinFactor, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y MinFactor no coinciden.')
    if (dim(merge(object@MaxFactor, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y MaxFactor no coinciden.')
    if (dim(merge(object@HRUnit, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y HRUnit no coinciden.')
    if (dim(merge(object@CHRUnit, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y CHRUnit no coinciden.')
    if (dim(merge(object@HRDomain, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y HRDomain no coinciden.')
    if (dim(merge(object@CHRDomain, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y CHRDomain no coinciden.')
    if (dim(merge(object@HRlambda, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y HRlambda no coinciden.')
    if (dim(merge(object@CHRlambda, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y CHRlambda no coinciden.')
    if (dim(merge(object@Lower, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y Lower no coinciden.')
    if (dim(merge(object@Upper, object@Units, by = IDQuals))[1] != dim(Units)[1]) stop('[HRIntervalParam validation] Las unidades de los slots Units y Upper no coinciden.')


    return(TRUE)
  }
)
