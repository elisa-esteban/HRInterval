setClassUnion('characterOrNULL', c('character', 'NULL'))
#' Clase S4 HRFactorParam para los parametros del método HRFactor
#'
#' Definicion de la clase S4 \code{HRFactorParam} que contiene los parametros que utiliza el metodo
#' \code{\link{HRFactor}} para el calculo del factor hit rate en la obtencion de los radios de los
#' intervalos de validacion con el metodo \code{\link{HRInterval}}.
#'
#' @slot Units objeto de clase \code{data.table} con las unidades para las que se van a calcular
#' los intervalos.
#'
#' @slot VarName \code{Vector} de tipo \code{character} de longitud 1 con el nombre de la variable
#' para la que se estan construyendo los intervalos de validacion.
#'
#' @slot Edit \code{Vector} de tipo \code{character} de longitud 1 con el nombre del edit para el
#' que se esta construyendo los intervalos de validacion.
#'
#' @slot DomainNames \code{Vector} de tipo \code{character} con los nombres de la variables que
#' establecen las celdas (partición de la muestra).
#'
#' @param IntervalsLimits \code{vector} de longitud 2 con los nombres utilizados en el repositorio
#' para los extremos inferior y superior de los intervalos de validacion.
#'
#' @slot LastFactor objeto de clase \code{data.table} con los valores del factor hit rate del edit
#' en el ultimo periodo, para cada unidad.
#'
#' @slot MinFactor objeto de clase \code{data.table} con los valores minimos del factor hit rate
#' de cada edit para cada unidad.
#'
#' @slot MaxFactor objeto de clase \code{data.table} con los valores maximos del factor hit rate
#' de cada edit para cada unidad.
#'
#' @slot HRUnit objeto de clase \code{data.table} con los valores del hit rate asociado al edit
#' especificado, para cada unidad.
#'
#' @slot CHRUnit objeto de clase \code{data.table} con los valores optimos de las unidades que no
#' han sido marcadas y efectivamente no tenian error, para el edit especificado y para cada unidad.
#'
#' @slot HRDomain objeto de clase \code{data.table} con los valores del hit rate asociado al edit
#' especificado, para cada celda (particion de la muestra).
#'
#' @slot CHRDomain objeto de clase \code{data.table} con los valores optimos de las unidades que no
#' han sido marcadas y efectivamente no tenian error, para el edit especificado y para cada celda
#' (partición de la muestra).
#'
#' @slot HRlambda objeto de clase \code{data.table} para la estabilizacion del factor asociado al
#' \emph{hit-rate}. Debe ser, para cada edit y cada unidad, un valor en el intervalo [0, 1] y esta
#' asociado al numero de unidades marcadas correctamente.
#'
#' @slot CHRlambda objeto de clase \code{data.table} para la estabilizacion del factor asociado al
#' \emph{hit-rate}. Debe ser, para cada edit y cada unidad, un valor en el intervalo [0, 1] y esta
#' asociado al numero de unidades no marcadas y sin error.
#'
#' @examples
#' # Un prototipo vacío
#' new(Class = 'HRFactorParam')
#'
#' @import data.table StQ
#'
#' @export
setClass(
  Class = 'HRFactorParam',
  slots = c(Units = 'data.table',
            VarName = 'character',
            Edit = 'character',
            DomainNames = 'characterOrNULL',
            IntervalsLimits = 'character',
            LastFactor = 'data.table',
            MinFactor = 'data.table',
            MaxFactor = 'data.table',
            HRUnit = 'data.table',
            CHRUnit = 'data.table',
            HRDomain = 'data.table',
            CHRDomain = 'data.table',
            HRlambda = 'data.table',
            CHRlambda = 'data.table'
  ),
  validity = function(object){

    if (length(object@Edit) != 1) stop('[HRFactorParam validation] Edit debe ser un vector de tipo character de longitud 1.')
    if (length(object@VarName) != 1) stop('[HRFactorParam validation] VarName debe ser un vector de tipo character de longitud 1.')
    if (length(object@IntervalsLimits) != 2) stop('[HRFactorParam validation] IntervalsLimits debe ser un vector de tipo character de longitud 2 con los nombres de los limites de los intervalos.')

    IDQuals <- names(object@Units)
    if (!identical(names(object@LastFactor), c(IDQuals, 'LastFactor'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot LastFactor han de ser: ', c(IDQuals, 'LastFactor'))
    if (!identical(names(object@MinFactor), c(IDQuals, 'MinFactor'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot MinFactor han de ser: ', c(IDQuals, 'MinFactor'))
    if (!identical(names(object@MaxFactor), c(IDQuals, 'MaxFactor'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot MaxFactor han de ser: ', c(IDQuals, 'MaxFactor'))
    if (!identical(names(object@HRUnit), c(IDQuals, 'HRUnit'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot HRUnit han de ser: ', c(IDQuals, 'HRUnit'))
    if (!identical(names(object@CHRUnit), c(IDQuals, 'CHRUnit'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot CHRUnit han de ser: ', c(IDQuals, 'CHRUnit'))
    if (!identical(names(object@HRDomain), c(IDQuals, 'HRDomain'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot HRDomain han de ser: ', c(IDQuals, 'HRDomain'))
    if (!identical(names(object@CHRDomain), c(IDQuals, 'CHRDomain'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot CHRDomain han de ser: ', c(IDQuals, 'CHRDomain'))
    if (!identical(names(object@HRlambda), c(IDQuals, 'HRlambda'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot HRlambda han de ser: ', c(IDQuals, 'HRlambda'))
    if (!identical(names(object@CHRlambda), c(IDQuals, 'CHRlambda'))) stop('[HRFactorParam validation] Los nombres de la data.table del slot CHRlambda han de ser: ', c(IDQuals, 'CHRlambda'))

    if (dim(merge(object@LastFactor, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y LastFactor no coinciden.')
    if (dim(merge(object@MinFactor, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y MinFactor no coinciden.')
    if (dim(merge(object@MaxFactor, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y MaxFactor no coinciden.')
    if (dim(merge(object@HRUnit, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y HRUnit no coinciden.')
    if (dim(merge(object@CHRUnit, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y CHRUnit no coinciden.')
    if (dim(merge(object@HRDomain, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y HRDomain no coinciden.')
    if (dim(merge(object@CHRDomain, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y CHRDomain no coinciden.')
    if (dim(merge(object@HRlambda, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y HRlambda no coinciden.')
    if (dim(merge(object@CHRlambda, object@Units, by = IDQuals))[1] != dim(object@Units)[1]) stop('[HRFactorParam validation] Las unidades de los slots Units y CHRlambda no coinciden.')

    return(TRUE)
  }
)
