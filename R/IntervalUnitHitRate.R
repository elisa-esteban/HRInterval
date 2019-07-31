#' Calculo del hit rate para unidades
#'
#' \code{IntervalUnitHitRate} calcula el hit rate de un conjunto de unidades.
#'
#' @param object objeto de clase \linkS4class{StQ} con las unidades para las que se desea calcular
#' el hit rate.
#'
#' @param EdData objeto de clase \linkS4class{StQList} con el historico de datos depurados de las
#' unidades.
#'
#' @param RawData objeto de clase \linkS4class{StQList} con el historico de datos sin depurar de las
#' unidades.
#'
#' @param IntervalData objeto de clase \linkS4class{StQList} con el historico de los intervalos de
#' validacion de las unidades.
#'
#' @param HRUnitParam Objeto de clase \linkS4class{HRUnitParam} con los parametros necesarios para
#' calcular el hit rate de un conjunto de unidades.
#'
#' @return \code{data.table} con el hit rate de los intervalos de entrada para cada unidad.
#'
#' @examples
#' \dontrun{
#' IntervalUnitHitRate(HRUnitParam)
#' }
#'
#' @export
setGeneric("IntervalUnitHitRate", function(object, EdData, RawData, IntervalData, HRUnitParam){standardGeneric("IntervalUnitHitRate")})
#' @rdname IntervalUnitHitRate
#'
#' @import data.table StQ
#'
#' @include HRUnitParam-class.R
#'
#' @export
setMethod(
   f = "IntervalUnitHitRate",
   signature = c("StQ", "StQList", "StQList", "StQList", "HRUnitParam"),
   function(object, EdData, RawData, IntervalData, HRUnitParam) {

  VarName <- HRUnitParam@VarRoles[['ObjVariable']]
  IntervalsLimits <- HRUnitParam@VarRoles[['IntervalsLimits']]

  Units <- getUnits(object)
  IDQuals <- names(Units)
  EdUnits <- getUnits(EdData)
  EdUnits[, ('Period') := NULL]
  RawUnits <- getUnits(RawData)
  RawUnits[, ('Period') := NULL]


  # IDQualsParam <- HRUnitParam@VarRoles[['Units']]

  Periods.RawData <- getPeriods(RawData)
  Periods.EdData <- getPeriods(EdData)
  if (length(intersect(Periods.RawData, Periods.EdData)) == 0) stop('[HRInterval::IntervalUnitHitRate] Los parametros RawData y EdData no tienen ningun periodo en comun.')

  VarName <- HRUnitParam@VarRoles[['ObjVariable']]
  IDDD_RawData <- unique(unlist(lapply(getData(RawData), getIDDD)))
  if (!ExtractNames(VarName) %in% IDDD_RawData) stop('[HRInterval::IntervalUnitHitRate] El parametro RawData no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles.')

  IDDD_EdData <- unique(unlist(lapply(getData(EdData), getIDDD)))
  if (!ExtractNames(VarName) %in% IDDD_EdData) stop('[HRInterval::IntervalUnitHitRate] El parametro EdData no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles.')


  DataInterval <- getData(IntervalData)
  Edit_IntervalData <- lapply(DataInterval, function(StQ){

    out <- unique(getData(StQ)[Value != VarName][['IDEdit']])

  })

  Edit_IntervalData <- unique(unlist(Edit_IntervalData))
  Units <- fintersect(Units, EdUnits)
  Units <- fintersect(Units, RawUnits)


  if (!HRUnitParam@VarRoles[['EditName']] %in% Edit_IntervalData) {

    warning('[HRInterval::IntervalUnitHitRate] El parametro IntervalData no contiene datos sobre el edit especificado en la componente EditName de VarRoles.')
    IntervalsTable <- copy(Units)[, (IntervalsLimits) := NA_real_]
    IDQuals <- c(IDQuals, 'Period')

  }else {

    Intervals.StQ <- StQListToStQ(IntervalData)
    Intervals.StQ <- Intervals.StQ[IDEdit == HRUnitParam@VarRoles[['EditName']]]
    IntervalsTable <- dcast_StQ(Intervals.StQ)
    IntervalsTable <- merge(Units, IntervalsTable, by = IDQuals)
    IDQuals <- c(IDQuals, 'Period')
    IntervalsTable <- IntervalsTable[, c(IDQuals, IntervalsLimits), with = FALSE]

  }

  EdTable <- getValues(EdData, VarName, Units)
  RawTable <- getValues(RawData, VarName, Units)

  ErrorTable <- merge(EdTable, RawTable, by = IDQuals, suffixes = c('.ed', '.raw'))
  ErrorTable <- merge(ErrorTable, IntervalsTable, by = intersect(IDQuals, names(IntervalsTable)))

  ErrorTable[, Error := (abs(as.numeric(get(paste0(VarName,'.ed'))) - as.numeric(get(paste0(VarName,'.raw')))) > .Machine$double.eps) * 1L ]
  ErrorTable[, Flagged := (as.numeric(get(paste0(VarName,'.raw'))) < as.numeric(get(IntervalsLimits[1])) | as.numeric(get(paste0(VarName,'.raw'))) > as.numeric(get(IntervalsLimits[2]))) * 1L]

  ErrorTable[, CorrectFlagged := (Flagged == 1L & Error == 1L) * 1L]
  ErrorTable[, CorrectNonFlagged := (Flagged == 0L & Error == 0L) * 1L]
  ErrorTable[, TotalReg := ifelse(is.na(get(paste0(VarName,'.raw'))), NA_integer_, 1L)]

  IDQuals <- names(Units)

  output <- ErrorTable[, lapply(.SD, sum, na.rm = TRUE), by = IDQuals, .SDcols = c('CorrectFlagged', 'CorrectNonFlagged', 'Flagged','TotalReg')]
  output[, IntervHRUnit := CorrectFlagged/Flagged]
  output[, IntervCHRUnit := CorrectNonFlagged/(TotalReg - Flagged)]
  output[, (c('CorrectFlagged', 'CorrectNonFlagged', 'Flagged','TotalReg')) := NULL]

  if (dim(output[is.nan(IntervHRUnit)])[1] > 0) output[is.nan(IntervHRUnit)][['IntervHRUnit']] <- 1
  if (dim(output[is.nan(IntervCHRUnit)])[1] > 0) output[is.nan(IntervCHRUnit)][['IntervCHRUnit']] <- 1

  setkeyv(output, IDQuals)

  gc()

  return(output)
})

