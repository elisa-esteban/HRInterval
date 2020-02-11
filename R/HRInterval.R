#' Calculo de intervalos de validacion
#'
#' \code{HRInterval} construye intervalos de validacion tomando como centro de los intervalos la
#' predicción de la variable considerada y como radio el producto de la desviación del error de
#' predicción por el factor hit rate asociado a la variable.
#'
#' @param object Objeto de clase \linkS4class{StQList} con el historico de datos depurados de las
#' unidades.
#'
#' @param RawData objeto de clase \linkS4class{StQList} con el historico de datos sin depurar de las
#' unidades.
#'
#' @param IntervalData objeto de clase \linkS4class{StQList} con el historico de los intervalos de
#' validacion de las unidades.
#'
#' @param Param Objeto de clase \linkS4class{HRIntervalParam} con los parametros necesarios para
#' determinar los intervalos.
#'
#' @return \code{data.table} con los extremos inferior y superior de los intervalos de validacion
#' de cada unidad, asi como el factor hit rate asociado a la variable considerada para cada unidad.
#'
#' @examples
#' \dontrun{
#' HRInterval(object, Param)
#' }
#'
#' @export
setGeneric("HRInterval", function(object, RawData, IntervalData, Param) {standardGeneric("HRInterval")})
#' @rdname HRInterval
#'
#' @import data.table StQ BestTSPred
#'
#' @include HRIntervalParam-class.R
#'
#' @export
setMethod(
  f = "HRInterval",
  signature = c("StQList", "StQList", "StQList", "HRIntervalParam"),
  function(object, RawData, IntervalData, Param){

    Units <- Param@Units
    BestTS <- Param@BestTSPred
    VarName <- Param@VarName
    Lower <- copy(Param@Lower)
    if (all(is.na(Lower[['Lower']]))) Lower[, Lower := NULL]
    Upper <- copy(Param@Upper)
    if (all(is.na(Upper[['Upper']]))) Upper[, Upper := NULL]

    Times <- intersect(getPeriods(RawData), getPeriods(object))
    if (length(Times) == 0) {

      stop('[HRInterval: HRIntervalParam] No hay series de tiempo coincidentes entre el parametro de entrada object y el parametro Param@RawList.')
    }

    EdList <- subPeriods(object, Times)
    RawList <- subPeriods(RawData, Times)
    setUnits(EdList) <- Units
    setUnits(RawList) <- Units
    setUnits(RawList) <- Units
    StQ <- getData(EdList)
    StQ <- StQ[[length(StQ)]]
    IDQuals <- intersect(getIDQual(StQ), names(Units))

    HRFactorParam <- new(Class = 'HRFactorParam', Units = Units,
                                                  VarName = VarName,
                                                  Edit = Param@Edit,
                                                  DomainNames = Param@DomainNames,
                                                  IntervalsLimits = Param@IntervalsLimits,
                                                  LastFactor = Param@LastFactor,
                                                  MinFactor = Param@MinFactor,
                                                  MaxFactor = Param@MaxFactor,
                                                  HRUnit = Param@HRUnit,
                                                  CHRUnit = Param@CHRUnit,
                                                  HRDomain = Param@HRDomain,
                                                  CHRDomain = Param@CHRDomain,
                                                  HRlambda = Param@HRlambda,
                                                  CHRlambda = Param@CHRlambda)

    HRFactor <-  HRFactor(StQ, EdList, RawList, IntervalData, HRFactorParam)

    aux <- data.table(setdiff(Units[[IDQuals]], HRFactor[[IDQuals]]))
    setnames(aux, names(aux), IDQuals)
    aux <- merge(aux, Param@MaxFactor, by = IDQuals)
    setnames(aux, 'MaxFactor', 'HRFactor')
    HRFactor <- rbind(HRFactor, aux)
    setkeyv(HRFactor, IDQuals)

    setnames(BestTS, setdiff(names(BestTS), IDQuals), c('CentPred', 'STDPred'))
    BestTS <- merge(BestTS, HRFactor, by = IDQuals, all.x = TRUE)

    BestTS[, Lower := ifelse(dim(Lower)[2] == dim(Units)[2], CentPred - HRFactor * STDPred, Lower[['Lower']]), by = IDQuals]
    BestTS[, Upper := ifelse(dim(Upper)[2] == dim(Units)[2], CentPred + HRFactor * STDPred, Upper[['Upper']]), by = IDQuals]

    output <- BestTS[, c(IDQuals, 'HRFactor', 'Lower', 'Upper'), with = FALSE]

    return(output)
})
