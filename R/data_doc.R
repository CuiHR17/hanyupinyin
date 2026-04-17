#' Unihan Pinyin Dictionary
#'
#' A data frame containing Chinese characters and their Hanyu Pinyin readings
#' extracted from the Unicode Unihan Database (`kMandarin` field, Version 17.0).
#'
#' @format A data frame with 44348 rows and 4 variables:
#' \describe{
#'   \item{char}{The Chinese character.}
#'   \item{pinyin}{Pinyin with tone marks (e.g. `qiū`). Multiple readings are
#'     space-separated.}
#'   \item{pinyin_tone}{Pinyin with numeric tones (e.g. `qiu1`). Multiple
#'     readings are space-separated.}
#'   \item{pinyin_toneless}{Toneless Pinyin (e.g. `qiu`). Multiple readings are
#'     space-separated.}
#' }
#' @source Unicode Consortium, Unihan Database,
#'   <https://www.unicode.org/reports/tr38/>
"unihan_pinyin"
