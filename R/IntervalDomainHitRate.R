#' Cálculo del hit rate en dominios
#'
#' \code{IntervalDomainHitRate} calcula el hit rate de un conjunto de intervalos para un determinado
#' dominio.
#'
#' @param HRDomainParam Objeto de clase \linkS4class{HRDomainParam} con los parámetros necesarios
#' para calcular el hit rate en un determinado dominio.
#'
#' @return \code{data.table} con el hit rate de los intervalos de entrada para los dominios
#' especificados como input.
#'
#' @examples
#' \dontrun{
#' IntervalDomainHitRate(HRDomainParam)
#' }
#'
#' @export
setGeneric("IntervalDomainHitRate", function(object, RawData, IntervalData, HRDomainParam){standardGeneric("IntervalDomainHitRate")})
#' @rdname IntervalDomainHitRate
#'
#' @import data.table StQ
#'
#' @include HRDomainParam-class.R
#'
#' @export
setMethod(
  f = "IntervalDomainHitRate",
  signature = c("StQ", "StQ", "StQ","HRDomainParam"),
  function(object, RawData, IntervalData, HRDomainParam){

  VarName <- HRDomainParam@VarRoles[['ObjVariable']]
  DomainNames <- HRDomainParam@VarRoles[['Domains']]
  IntervalsLimits <- HRDomainParam@VarRoles[['IntervalsLimits']]


  ## Validaciones
  IDQuals <- HRDomainParam@VarRoles[['Units']]

  if (length(intersect(IDQuals, unique(unlist(getIDQual(RawData))))) != length(IDQuals)) stop('[HRInterval::IntervalDomainHitRate] Los calificadores de unidad del slot RawData no se corresponden con las unidades especificadas en Units del slot VarRoles de Param.')
  if (length(intersect(IDQuals, unique(unlist(getIDQual(object))))) != length(IDQuals)) stop('[HRInterval::IntervalDomainHitRate] Los calificadores de unidad del slot EdData no se corresponden con las unidades especificadas en Units del slot VarRoles de Param.')
  if (length(intersect(IDQuals, unique(unlist(getIDQual(IntervalData))))) != length(IDQuals)) stop('[HRInterval::IntervalDomainHitRate] Los calificadores de unidad del parametro IntervalData no se corresponden con las unidades especificadas en Units del slot VarRoles de Param.')

  IDDD_RawData <- getIDDD(RawData)
  if (!ExtractNames(HRDomainParam@VarRoles[['ObjVariable']]) %in% IDDD_RawData) stop('[HRInterval::IntervalDomainHitRate] El parametro RawData no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles de Param.')
  if (!all(ExtractNames(HRDomainParam@VarRoles[['Domains']]) %in% IDDD_RawData)) stop('[HRInterval::IntervalDomainHitRate] El parametro RawData no contiene datos sobre alguna de las variables especificadas en la componente Domains del slot VarRoles de Param.')

  IDDD_EdData <- getIDDD(object)
  if (!ExtractNames(HRDomainParam@VarRoles[['ObjVariable']]) %in% IDDD_EdData) stop('[HRInterval::IntervalDomainHitRate] El parametro object no contiene datos sobre la variable especificada en la componente ObjVariable del slot VarRoles de Param.')
  if (!all(ExtractNames(HRDomainParam@VarRoles[['Domains']]) %in% IDDD_EdData)) stop('[HRInterval::IntervalDomainHitRate] El parametro object no contiene datos sobre alguna de las variables especificadas en la compontente Domains del slot VarRoles de Param.')


  Units <- getUnits(object)
  IDQuals <- names(Units)
  EdUnits <- getUnits(object)
  RawUnits <- getUnits(RawData)

  if (!identical(HRDomainParam@VarRoles[['Units']], names(Units))) stop('[HRInterval::IntervalDomainHitRate] Los calificadores de unidad especificados en la componente Units del slot VarRoles del parametro HRUnitParam no se corresponden con las unidades del objeto de entrada.')
  Units <- fintersect(Units, EdUnits)
  Units <- fintersect(Units, RawUnits)

  Edit_IntervalData <- unique(getData(IntervalData)[['IDEdit']])

  if (!HRDomainParam@VarRoles[['EditName']] %in% Edit_IntervalData) {

    warning('[HRInterval::IntervalDomainHitRate] El parametro IntervalData no contiene datos sobre el edit especificado en la componente EditName del slot VarRoles de Param.')
    IntervalTable <- copy(Units)[, (IntervalsLimits) := NA_real_]

  }else {

    IntervalTable <- IntervalData[IDEdit == HRDomainParam@VarRoles[['EditName']]]
    #  if (dim(IntervalTable)[1] == 0) return(data.table())
    IntervalTable <- dcast_StQ(IntervalTable)
    IntervalTable <- merge(Units, IntervalTable, by = IDQuals)
    IntervalTable <- IntervalTable[, c(IDQuals, IntervalsLimits), with = FALSE]

  }

  Vars <- c(DomainNames, VarName)

  EdVars <- lapply(Vars, function(Var){out <- getValues(object, Var, Units)})
  EdTable <- Reduce(merge, EdVars)

  RawTable <- getValues(RawData, VarName, Units)

  ErrorTable <- merge(EdTable, RawTable, by = IDQuals, suffixes = c('.ed', '.raw'))
  ErrorTable <- merge(ErrorTable, IntervalTable, by = IDQuals)

  ErrorTable[, Error := (abs(as.numeric(get(paste0(VarName,'.ed'))) - as.numeric(get(paste0(VarName,'.raw')))) > .Machine$double.eps) * 1L ]
  ErrorTable[, Flagged := (as.numeric(get(paste0(VarName,'.raw'))) < as.numeric(get(IntervalsLimits[1])) | as.numeric(get(paste0(VarName,'.raw'))) > as.numeric(get(IntervalsLimits[2]))) * 1L]

  ErrorTable[, CorrectFlagged := (Flagged == 1L & Error == 1L) * 1L]
  ErrorTable[, CorrectNonFlagged := (Flagged == 0L & Error == 0) * 1L]
  ErrorTable[, TotalReg := ifelse(is.na(get(paste0(VarName,'.raw'))), NA_integer_, 1L)]

  output <- ErrorTable[, lapply(.SD, sum, na.rm = TRUE), by = DomainNames, .SDcols = c('CorrectFlagged', 'CorrectNonFlagged', 'Flagged','TotalReg')]
  output[, IntervHRDomain := CorrectFlagged/Flagged]
  output[, IntervCHRDomain := CorrectNonFlagged/(TotalReg - Flagged)]

  output[, (c('CorrectFlagged', 'CorrectNonFlagged', 'Flagged','TotalReg')) := NULL]

  if (dim(output[is.nan(IntervHRDomain)])[1] > 0) output[is.nan(IntervHRDomain)][['IntervHRDomain']] <- 1
  if (dim(output[is.nan(IntervCHRDomain)])[1] > 0) output[is.nan(IntervCHRDomain)][['IntervCHRDomain']] <- 1

  output <- merge(EdTable, output, by = DomainNames, all.x = TRUE)
  output[, (VarName) := NULL]

  setcolorder(output, c(IDQuals, DomainNames, 'IntervHRDomain', 'IntervCHRDomain'))

  gc()

  return(output)
})

