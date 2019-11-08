#' Calculo del factor hit rate
#'
#' \code{HRFactor} calcula el factor hit rate para obtener el radio de los intervalos de validacion
#' con el método \code{\link{HRInterval}}.
#'
#' @param object Objeto de clase \linkS4class{StQ} con las unidades para las que se quieren calcular
#' los factores hit rate.
#'
#' @param EdData objeto de clase \linkS4class{StQList} con el historico de datos depurados de las
#' unidades.
#'
#' @param RawData objeto de clase \linkS4class{StQList} con el historico de datos sin depurar de las
#' unidades.
#'
#' @param IntervalData objeto de clase \linkS4class{StQList} con el historico de intervalos de
#' validacion de las unidades.
#'
#' @param Param Objeto de clase \linkS4class{HRFactorParam} con los parametros necesarios para
#' calcular el factor hit rate.
#'
#' @return \code{data.table} con los factores hit rate de la variable.
#'
#' @examples
#' \dontrun{
#' HRFactor(object, Param)
#' }
#'
#' @export
setGeneric("HRFactor", function(object, EdData, RawData, IntervalData, Param){standardGeneric("HRFactor")})
#' @rdname HRFactor
#'
#' @import data.table StQ
#'
#' @include HRFactorParam-class.R HRUnitParam-class.R HRDomainParam-class.R
#'
#' @export
setMethod(
  f = "HRFactor",
  signature = c("StQ", "StQList", "StQList", "StQList", "HRFactorParam"),
  function(object, EdData, RawData, IntervalData, Param){

    LastRawData <- getData(RawData)
    LastRawData <- LastRawData[[length(LastRawData)]]
    LastIntervalData <- getData(IntervalData)
    LastIntervalData <- LastIntervalData[[length(LastIntervalData)]]
    IDQuals <- intersect(getIDQual(object), names(Param@Units))

    ### Validaciones ###
    IDQuals <- names(Param@Units)
    if (length(intersect(IDQuals, unique(unlist(getIDQual(RawData))))) != length(IDQuals)) stop('[HRFactor validation] Los calificadores de unidad del parametro RawData no se corresponden con las unidades especificadas en el slot Units de Param.')
    if (length(intersect(IDQuals, unique(unlist(getIDQual(EdData))))) != length(IDQuals)) stop('[HRFactor validation] Los calificadores de unidad del parametro EdData no se corresponden con las unidades especificadas en el slot Units de Param.')
    if (length(intersect(IDQuals, unique(unlist(getIDQual(IntervalData))))) != length(IDQuals)) stop('[HRFactor validation] Los calificadores de unidad del parametro IntervalData no se corresponden con las unidades especificadas en el slot Units de Param.')

    Periods.RawData <- getPeriods(RawData)
    Periods.EdData <- getPeriods(EdData)
    if (length(intersect(Periods.RawData, Periods.EdData)) == 0) stop('[HRFactor validation] Los parametros RawData y EdData no tienen ningun periodo en comun.')

    IDDD_RawData <- unique(unlist(lapply(getData(RawData), getIDDD)))
    if (!ExtractNames(Param@VarName) %in% IDDD_RawData) stop('[HRFactor validation] El parametro RawData no contiene datos sobre la variable especificada en el slot VarName de Param.')
    if (!all(ExtractNames(Param@DomainNames) %in% IDDD_RawData)) stop('[HRFactor validation] El parametro RawData no contiene datos sobre alguna de las variables especificadas en el slot DomainNames de Param.')

    IDDD_EdData <- unique(unlist(lapply(getData(EdData), getIDDD)))
    if (!ExtractNames(Param@VarName) %in% IDDD_EdData) stop('[HRFactor validation] El parametro EdData no contiene datos sobre la variable especificada en el slot VarName de Param.')
    if (!all(ExtractNames(Param@DomainNames) %in% IDDD_EdData)) stop('[HRFactor validation] El parametro EdData no contiene datos sobre alguna de las variables especificadas en el slot DomainNames de Param.')

    Units_RawData <- getUnits(RawData)
    Units_RawData[, Period := NULL]
    if (dim(merge(unique(Units_RawData), Param@Units, by = IDQuals))[1] == 0) stop('[HRFactor validation] En el parametro RawData no existe ninguna unidad en el slot Units de Param .')
    Units_EdData <- getUnits(EdData)
    Units_EdData[, Period := NULL]
    if (dim(merge(unique(Units_EdData), Param@Units, by = IDQuals))[1] == 0) stop('[HRFactor validation] En el parametro EdData no existe ninguna unidad en el slot Units de Param.')
    Units_IntervalData <- getUnits(IntervalData)
    Units_IntervalData <- Units_IntervalData[, Period := NULL]
    if (dim(merge(unique(Units_IntervalData), Param@Units, by = IDQuals))[1] == 0) stop('[HRFactor validation] En el parametro IntervalData no exite ninguna unidad en el slot Units de Param')


    ### Fin Validaciones ###

    HRUnitParam <- new(Class = 'HRUnitParam', VarRoles = list(Units = IDQuals,
                                                              ObjVariable = Param@VarName,
                                                              IntervalsLimits = Param@IntervalsLimits,
                                                              EditName = Param@Edit))

    HRDomainParam <- new(Class = 'HRDomainParam', VarRoles = list(Units = IDQuals,
                                                                  ObjVariable = Param@VarName,
                                                                  Domains = Param@DomainNames,
                                                                  IntervalsLimits = Param@IntervalsLimits,
                                                                  EditName = Param@Edit))

    HRUnit <- IntervalUnitHitRate(object, EdData, RawData, IntervalData, HRUnitParam)
    HRDomain <- IntervalDomainHitRate(object, LastRawData, LastIntervalData, HRDomainParam)

    if (dim(HRUnit)[1] == 0 | dim(HRDomain)[1] == 0) {

      output <- Param@MaxFactor
      setnames(output, 'MaxFactor', 'HRFactor')

    } else {

      auxData <- list(HRUnit, HRDomain, Param@LastFactor, Param@MinFactor, Param@MaxFactor, Param@HRUnit, Param@CHRUnit, Param@HRDomain, Param@CHRDomain, Param@HRlambda, Param@CHRlambda)
      auxData <- Reduce(function(x, y){merge(x, y, by = IDQuals)}, auxData)[
        , IntervHRUnit := as.numeric(IntervHRUnit)][
        , IntervCHRUnit := as.numeric(IntervCHRUnit)][
        , IntervHRDomain := as.numeric(IntervHRDomain)][
        , IntervCHRDomain := as.numeric(IntervCHRDomain)][
        , LastFactor := as.numeric(LastFactor)][
        , MinFactor := as.numeric(MinFactor)][
        , MaxFactor := as.numeric(MaxFactor)][
        , HRUnit := as.numeric(HRUnit)][
        , CHRUnit := as.numeric(CHRUnit)][
        , HRDomain :=  as.numeric(HRDomain)][
        , CHRDomain :=  as.numeric(CHRDomain)][
        , HRlambda := as.numeric(HRlambda)][
        , CHRlambda := as.numeric(CHRlambda)][
        , HRFactor := LastFactor + MaxFactor * (1 - (1 - HRlambda) * IntervHRUnit / HRUnit - HRlambda * IntervHRDomain / HRDomain) -
                      MaxFactor * (1 - (1 - CHRlambda) * IntervCHRUnit / CHRUnit - CHRlambda * IntervCHRDomain / CHRDomain), by = IDQuals]
      output <- auxData[, HRFactor := max(MinFactor, HRFactor), by = IDQuals]
      output <- auxData[, HRFactor := min(MaxFactor, HRFactor), by = IDQuals]
      output <- output[, c(IDQuals, 'HRFactor'), with = FALSE]
    }

    gc()

    return(output)
})
