% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/difference_funs.R
\name{cross_fun}
\alias{cross_fun}
\title{Berechnung der Stimmbeteiligungsdifferenz zwischen zwei Vorlagen}
\usage{
cross_fun(df, vorl1, vorl2)
}
\arguments{
\item{df}{Datensatz mit Gemeinde-ID und den Stimmbeteiligungen für verschiedene Vorlagen (eigene Spalten, mit Vorlagen-ID als Spaltenname)}

\item{vorl1}{Vektor mit Vorlagen-ID (Ebene und Index: Beispiel eidg1, kant2 etc.)}

\item{vorl2}{Vektor mit Vorlagen-ID (Ebene und Index: Beispiel eidg1, kant2 etc.)}
}
\value{
Stimmbeteiligungsdifferenz zwischen zwei Vorlagen in einem tibble
}
\description{
Berechnung der Stimmbeteiligungsdifferenz zwischen zwei Vorlagen
}
\examples{

testdata <- tibble(gemwkid = c(13,49,41,43,44),
eidg1 = c(60.90,61.18,65.27,55.36,57.68),
eidg2 = c(62.16,62.54,66.95,56.65,58.68),
kant1 = c(57.73,60.27,63.31,51.93,54.49))

cross_fun(testdata,"eidg1","eidg2")

 # generate combinations
combinations <-as.data.frame(t(combn(c("eidg1","eidg2","kant1"),2)))

# difference between columns named as the first combination
cross_fun(crosscheckdata,combinations$V1[1],combinations$V2[1])

}
