% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildUpdateHRFactorParam.R
\name{BuildUpdateHRFactorParam}
\alias{BuildUpdateHRFactorParam}
\title{Constructor de objetos de clase \linkS4class{UpdateHRFactorParam}}
\usage{
BuildUpdateHRFactorParam(EditName = character(0), Units = data.table(),
  Values = list())
}
\arguments{
\item{EditName}{\code{Vector} de tipo \code{character} de longitud 1 con el nombre del Edit para
el que se establece la condicion.}

\item{Units}{\code{data.table} con el conjunto de unidades cuyos parametros se desean actualizar.}

\item{Values}{lista con nombres que contiene los valores que actualizaran los parametros que 
intervienen en el calculo del HitRate por unidad.}
}
\value{
Un objeto de clase \linkS4class{UpdateHRFactorParam} .
}
\description{
Este constructor devuelve un objeto de clase \linkS4class{UpdateHRFactorParam} a
partir de un conjunto de parametros.
}
\examples{
\dontrun{
UpdateHRFactorParam <- BuildUpdateHRFactorParam(EditName = 'LCN_W_1',
                                                )
UpdateHRFactorParam
}

}
