% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IntervalDomainHitRate.R
\docType{methods}
\name{IntervalDomainHitRate}
\alias{IntervalDomainHitRate}
\alias{IntervalDomainHitRate,StQ,StQ,StQ,HRDomainParam-method}
\title{Calculo del hit rate en dominios}
\usage{
IntervalDomainHitRate(object, RawData, IntervalData, HRDomainParam)

\S4method{IntervalDomainHitRate}{StQ,StQ,StQ,HRDomainParam}(object, RawData,
  IntervalData, HRDomainParam)
}
\arguments{
\item{object}{objeto de clase \linkS4class{StQ} con las unidades para las que se desea calcular
el hit rate.}

\item{RawData}{objeto de clase \linkS4class{StQ} con los datos sin depurar de las unidades.}

\item{IntervalData}{objeto de clase \linkS4class{StQ} los intervalos de validacion de las
unidades.}

\item{HRDomainParam}{Objeto de clase \linkS4class{HRDomainParam} con los parametros necesarios
para calcular el hit rate en un determinado dominio.}
}
\value{
\code{data.table} con el hit rate de los intervalos de entrada para los dominios
especificados como input.
}
\description{
\code{IntervalDomainHitRate} calcula el hit rate de un conjunto de intervalos para un determinado
dominio.
}
\examples{
\dontrun{
IntervalDomainHitRate(HRDomainParam)
}

}
