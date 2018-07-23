% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HRIntervalParam-class.R
\docType{class}
\name{HRIntervalParam-class}
\alias{HRIntervalParam-class}
\title{método \code{\link{HRInterval}} para la obtención de intervalos de validación mediante el
cálculo de sus centros y radios.}
\arguments{
\item{IntervalsLimits}{\code{vector} de longitud 2 con los nombres utilizados en el repositorio
para los extremos inferior y superior de los intervalos de validación.}
}
\description{
método \code{\link{HRInterval}} para la obtención de intervalos de validación mediante el
cálculo de sus centros y radios.
}
\section{Slots}{

\describe{
\item{\code{Units}}{objeto de clase \code{data.table} con las unidades para las que se van a calcular
los intervalos.}

\item{\code{VarName}}{\code{Vector} de tipo \code{character} de longitud 1 con el nombre de la variable
para la que se están construyendo los intervalos de validación.}

\item{\code{Edit}}{\code{Vector} de tipo \code{character} de longitud 1 con el nombre del edit para el
que se están construyendo los intervalos de validación.}

\item{\code{DomainNames}}{\code{Vector} de tipo \code{character} con los nombres de la variables que
establecen las celdas (partición de la muestra).}

\item{\code{LastFactor}}{objeto de clase \code{data.table} con los valores del factor hit rate del edit
en el último periodo para cada unidad.}

\item{\code{BestTSPred}}{Objeto de clase \linkS4class{data.table} con la mejor predicción de entre un
conjunto de modelos para la variable considerada.}

\item{\code{MinFactor}}{objeto de clase \code{data.table} con los valores mínimos del factor hit rate
para cada unidad.}

\item{\code{MaxFactor}}{objeto de clase \code{data.table} con los valores máximos del factor hit rate
para cada unidad.}

\item{\code{HRUnit}}{objeto de clase \code{data.table} con los valores del hit rate de los intervalos
para cada unidad.}

\item{\code{HRDomain}}{objeto de clase \code{data.table} con los valores del hit rate de los intervalos
para cada celda (partición de la muestra).}

\item{\code{HRlambda}}{objeto de clase \code{data.table} para la estabilización del factor asociado al
\emph{hit-rate}. Debe ser, para cada unidad, un valor en el intervalo [0, 1].}

\item{\code{Scale}}{\code{Vector} de tipo \code{numeric} de longitud 1 con el valor del parámetro de
escala aplicado a la longitud del intervalo. Así, por ejemplo, si \eqn{Scale <- 2}, los
intervalos obtenidos pasan a tener el doble de longitud con el mismo centro. Debe tomar un valor
positivo. Por defecto toma el valor 1L.}
}}

\examples{
# Un prototipo vacío
new(Class = 'HRIntervalParam')

}