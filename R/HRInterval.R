#' Cálculo de intervalos de validación
#'
#' \code{HRInterval} construye intervalos de validación determinando los valores de sus centros
#' y sus radios.
#'
#' @param object Objeto de clase \linkS4class{StQList} con los datos y unidades de las series de
#' tiempo con los que vamos a calcular los intervalos.
#'
#' @param Param Objeto de clase \linkS4class{HRIntervalParam} con los parámetros necesarios para
#' determinar los intervalos.
#'
#' @return Objeto de clase \linkS4class{IntervalsDT} con los intervalos de validación para los datos
#' del objeto de entrada \code{object}.
#'
#' @examples
#' \dontrun{
#' HRInterval(object, Param)
#' }
#'
#' @export
setGeneric("HRInterval", function(object, RawData, IntervalData, Param) {standardGeneric("HRInterval")})
#'
#'
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

    Times <- intersect(getPeriods(RawData), getPeriods(object))
    if (length(Times) == 0) {

      stop('[HRInterval: HRIntervalParam] No hay series de tiempo coincidentes entre el parámetro de entrada object y el parámetro Param@RawList.')
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

    BestTS[, Lower := CentPred - HRFactor * STDPred, by = IDQuals]
    BestTS[, Upper := CentPred + HRFactor * STDPred, by = IDQuals]

    output <- BestTS[, c(IDQuals, 'HRFactor', 'Lower', 'Upper'), with = FALSE]

    if (dim(output[is.na(Lower) | is.infinite(Lower)])[1] > 0) output[is.na(Lower) | is.infinite(Lower)][['Lower']] <- -99.99
    if (dim(output[is.na(Upper) | is.infinite(Upper)])[1] > 0) output[is.na(Upper) | is.infinite(Upper)][['Upper']] <- 99.99

    return(output)
})
