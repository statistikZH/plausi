#' Berechnung der Stimmbeteiligungsdifferenz zwischen zwei Vorlagen
#'
#' @param df Datensatz mit Gemeinde-ID und den Stimmbeteiligungen für verschiedene Vorlagen (eigene Spalten, mit Vorlagen-ID als Spaltenname)
#' @param vorl1 Vektor mit Vorlagen-ID (Ebene und Index: Beispiel eidg1, kant2 etc.)
#' @param vorl2 Vektor mit Vorlagen-ID (Ebene und Index: Beispiel eidg1, kant2 etc.)
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#'
#' @return Stimmbeteiligungsdifferenz zwischen zwei Vorlagen in einem tibble
#' @export
#'
#' @examples
#'
#' testdata <- tibble(gemwkid = c(13,49,41,43,44),
#' eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#' eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#' kant1 = c(57.73,60.27,63.31,51.93,54.49))
#'
#' cross_fun(testdata,"eidg1","eidg2")
#'
#'  # generate combinations
#' combinations <-as.data.frame(t(combn(c("eidg1","eidg2","kant1"),2)))
#'
#' # difference between columns named as the first combination
#' cross_fun(crosscheckdata,combinations$V1[1],combinations$V2[1])
#'
cross_fun <- function(df, vorl1, vorl2){

  varname <- paste0(vorl1,"_",vorl2)

  df %>% dplyr::mutate(!!varname :=.data[[as.character(vorl1)]] -.data[[as.character(vorl2)]]) %>%
    dplyr::select(gemwkid,gemeinde,!!varname)
}


#' Berechnung der Stimmbeteiligungsdifferenz zwischen allen Vorlagen
#'
#' @param df Datensatz mit Gemeinde-ID und den Stimmbeteiligungen für verschiedene Vorlagen (eigene Spalten)
#' @param vorl1 Vektor mit Vorlagen-ID, die als Spaltennamen im Datensatz vorhanden sind (Ebene und Index: Beispiel eidg1, kant2 etc.)
#' @param vorl2 Vektor mit Vorlagen-ID (Ebene und Index: Beispiel eidg1, kant2 etc.)
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr contains
#' @importFrom dplyr ends_with
#' @importFrom purrr map2
#'
#' @return Stimmbeteiligungsdifferenzen zwischen zwei Vorlagen in einem tibble
#' @export
#'
#' @examples
#' library(tibble)
#'
#' testdata <- tibble(gemwkid = c(13,49,41,43,44),
#' eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#' eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#' kant1 = c(57.73,60.27,63.31,51.93,54.49))
#'
#' single_difference(testdata,"eidg1","eidg2")
#'
#'  # generate combinations
#' combinations <-as.data.frame(t(combn(c("eidg1","eidg2","kant1"),2)))
#'
#' # calculate all possible differences between columns
#'
#' get_differences(testdata,combinations$V1,combinations$V2)
#'
get_differences <- function(df, vorl1,vorl2){

  crosscheckdata_new <- purrr::map2(vorl1,vorl2,~cross_fun(df,.x,.y))

  crosscheckdata_new %>% purrr::map(tidyr::gather, key = "combination", value = "difference", dplyr::contains("_")) %>% dplyr::bind_rows()
}

