#' Convert Chinese Characters to Hanyu Pinyin
#'
#' Converts a character vector of Chinese strings into Pinyin romanization.
#' The function is fully vectorized and uses the Unicode Unihan database
#' (`kMandarin`) as its authoritative source.
#'
#' @param x A character vector.
#' @param sep Separator between syllables. Default is `"_"`.
#' @param tone If `TRUE` (default), returns Pinyin with numeric tones (e.g.
#'   `qiu1`). If `FALSE`, returns toneless Pinyin (e.g. `qiu`). If `"marks"`,
#'   returns Pinyin with diacritic tone marks (e.g. `qiū`).
#' @param polyphone If `FALSE` (default), each character is converted
#'   independently using its most common reading. If `TRUE`, a built-in phrase
#'   table is used to resolve common polyphones via greedy longest-match
#'   segmentation.
#' @param other_replace How to handle non-Chinese characters. `NULL` means
#'   leave them as-is. A single character string replaces them.
#'
#' @return A character vector of the same length as `x`.
#' @export
#'
#' @examples
#' to_pinyin("\u6625\u7720\u4e0d\u89c9\u6653")
#' to_pinyin("Hello \u4e16\u754c", sep = " ", other_replace = "?")
#' to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE)
#' to_pinyin("\u6625\u7720\u4e0d\u89c9\u6653", tone = "marks")
to_pinyin <- function(x,
                      sep = "_",
                      tone = TRUE,
                      polyphone = FALSE,
                      other_replace = NULL) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  style <- if (identical(tone, "marks")) {
    "marks"
  } else if (isTRUE(tone)) {
    "tone"
  } else {
    "toneless"
  }
  env <- switch(style,
    marks = .pinyin_marks_env,
    tone = .pinyin_env,
    .pinyin_toneless_env
  )

  if (polyphone) {
    vapply(x, to_pinyin_poly, character(1),
           sep = sep, env = env, style = style,
           other_replace = other_replace,
           USE.NAMES = FALSE
    )
  } else {
    vapply(x, to_pinyin_single, character(1),
           sep = sep, env = env, other_replace = other_replace,
           USE.NAMES = FALSE
    )
  }
}

#' @noRd
is_cjk <- function(x) {
  grepl("^[\u4e00-\u9fff\u3400-\u4dbf]$", x, perl = TRUE)
}

#' @noRd
is_space_token <- function(x) {
  grepl("^\\s+$", x)
}

#' @noRd
segment_text <- function(x) {
  if (is.na(x) || nchar(x) == 0) {
    return(character(0))
  }
  out <- stringi::stri_extract_all_regex(
    x,
    "[\u4e00-\u9fff\u3400-\u4dbf]|[^\u4e00-\u9fff\u3400-\u4dbf\\s]+|\\s+"
  )[[1]]
  out[!is.na(out)]
}

#' @noRd
join_tokens <- function(converted, was_cjk, sep, other_replace) {
  if (length(converted) == 0) return("")
  if (length(converted) == 1) return(converted[1])
  result <- converted[1]
  for (i in seq.int(2, length(converted))) {
    prev_space <- grepl("^\\s+$", converted[i - 1])
    curr_space <- grepl("^\\s+$", converted[i])
    prev_cjk <- was_cjk[i - 1]
    curr_cjk <- was_cjk[i]
    add_sep <- TRUE
    if (prev_space || curr_space) add_sep <- FALSE
    if (!prev_cjk && !curr_cjk && is.null(other_replace)) add_sep <- FALSE
    if (add_sep) {
      result <- paste0(result, sep, converted[i])
    } else {
      result <- paste0(result, converted[i])
    }
  }
  result
}

#' @noRd
to_pinyin_single <- function(x, sep, env, other_replace) {
  if (is.na(x) || nchar(x) == 0) {
    return(x)
  }

  tokens <- segment_text(x)

  out <- vapply(tokens, function(tok) {
    if (is_cjk(tok)) {
      py <- env[[tok]]
      if (is.null(py)) tok else py
    } else if (is_space_token(tok)) {
      tok
    } else {
      if (is.null(other_replace)) tok else paste(rep(other_replace, nchar(tok)), collapse = "")
    }
  }, character(1), USE.NAMES = FALSE)

  was_cjk <- vapply(tokens, is_cjk, logical(1), USE.NAMES = FALSE)
  join_tokens(out, was_cjk, sep, other_replace)
}

#' @noRd
to_pinyin_poly <- function(x, sep, env, style, other_replace) {
  if (is.na(x) || nchar(x) == 0) {
    return(x)
  }

  tokens <- segment_text(x)
  phrases <- get_phrase_env()
  phrase_keys <- ls(phrases, all.names = TRUE)
  phrase_keys <- phrase_keys[order(nchar(phrase_keys), decreasing = TRUE)]

  n <- length(tokens)
  i <- 1
  result <- character(0)
  result_is_cjk <- logical(0)

  while (i <= n) {
    tok <- tokens[i]
    if (!is_cjk(tok)) {
      if (is_space_token(tok)) {
        result <- c(result, tok)
      } else {
        result <- c(result, if (is.null(other_replace)) tok else paste(rep(other_replace, nchar(tok)), collapse = ""))
      }
      result_is_cjk <- c(result_is_cjk, FALSE)
      i <- i + 1
      next
    }

    # Find consecutive CJK run
    j <- i
    while (j <= n && is_cjk(tokens[j])) j <- j + 1
    cjk_run <- tokens[i:(j - 1)]

    k <- 1
    while (k <= length(cjk_run)) {
      matched <- FALSE
      for (pk in phrase_keys) {
        plen <- nchar(pk)
        if (k + plen - 1 <= length(cjk_run)) {
          seg <- paste(cjk_run[k:(k + plen - 1)], collapse = "")
          if (identical(seg, pk)) {
            reading_obj <- phrases[[pk]]
            if (is.list(reading_obj)) {
              reading <- switch(style,
                marks = reading_obj$marks,
                toneless = strip_tone(reading_obj$tone),
                reading_obj$tone
              )
            } else {
              # backward compatibility for old string storage
              reading <- if (identical(style, "toneless")) strip_tone(reading_obj) else reading_obj
            }
            reading <- gsub(" ", sep, reading, fixed = TRUE)
            result <- c(result, reading)
            result_is_cjk <- c(result_is_cjk, TRUE)
            k <- k + plen
            matched <- TRUE
            break
          }
        }
      }
      if (!matched) {
        py <- env[[cjk_run[k]]]
        result <- c(result, if (is.null(py)) cjk_run[k] else py)
        result_is_cjk <- c(result_is_cjk, TRUE)
        k <- k + 1
      }
    }
    i <- j
  }

  join_tokens(result, result_is_cjk, sep, other_replace)
}

#' Convert to Toneless Pinyin
#'
#' A convenience wrapper around [to_pinyin()] with `tone = FALSE`.
#'
#' @param x A character vector.
#' @param sep Separator between syllables. Default is `"_"`.
#' @inheritParams to_pinyin
#' @return A character vector of the same length as `x`.
#' @export
#' @examples
#' to_pinyin_toneless("\u6625\u7720\u4e0d\u89c9\u6653")
to_pinyin_toneless <- function(x, sep = "_", polyphone = FALSE, other_replace = NULL) {
  to_pinyin(x, sep = sep, tone = FALSE, polyphone = polyphone, other_replace = other_replace)
}

#' Convert to Pinyin with Tone Marks
#'
#' A convenience wrapper around [to_pinyin()] with `tone = "marks"`.
#'
#' @param x A character vector.
#' @param sep Separator between syllables. Default is `"_"`.
#' @inheritParams to_pinyin
#' @return A character vector of the same length as `x`.
#' @export
#' @examples
#' to_pinyin_marks("\u6625\u7720\u4e0d\u89c9\u6653")
#' to_pinyin_marks("Hello \u4e16\u754c", sep = " ")
to_pinyin_marks <- function(x, sep = "_", polyphone = FALSE, other_replace = NULL) {
  to_pinyin(x, sep = sep, tone = "marks", polyphone = polyphone, other_replace = other_replace)
}

#' Extract Pinyin Initials
#'
#' Returns only the first letter of each syllable.
#'
#' @param x A character vector.
#' @inheritParams to_pinyin
#' @return A character vector of the same length as `x`.
#' @export
#' @examples
#' to_pinyin_initials("\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd")
to_pinyin_initials <- function(x, polyphone = FALSE, other_replace = NULL) {
  py <- to_pinyin_toneless(x, sep = " ", polyphone = polyphone, other_replace = other_replace)
  # split by spaces, take first letter, collapse without sep
  vapply(py, function(s) {
    if (is.na(s) || nchar(s) == 0) return(s)
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    paste(substr(words, 1, 1), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Create URL-Friendly Slug from Chinese Text
#'
#' @param x A character vector.
#' @inheritParams to_pinyin
#' @return A character vector of URL-friendly slug strings.
#' @export
#' @examples
#' to_slug("2026\u5e74\u62a5\u544a")
to_slug <- function(x, polyphone = FALSE, other_replace = NULL) {
  py <- to_pinyin_toneless(x, sep = "-", polyphone = polyphone, other_replace = other_replace)
  # lowercase, keep alnum and hyphen, collapse multiple hyphens
  py <- tolower(py)
  py <- gsub("[^a-z0-9-]+", "-", py)
  py <- gsub("^-+|-+$", "", py)
  py <- gsub("-+", "-", py)
  py
}

#' Generate Valid R Variable Names from Chinese Text
#'
#' Useful when cleaning imported data (e.g. from SAS or Excel) where column
#' labels are in Chinese.
#'
#' @param x A character vector.
#' @param unique If `TRUE` (default), appends `.1`, `.2`, etc. to duplicates
#'   via [make.names()].
#' @param abbrev If not `NULL`, an integer giving the maximum length of each
#'   syllable (e.g. `abbrev = 4` truncates `zhong` to `zhon`).
#' @inheritParams to_pinyin
#' @return A character vector of valid R variable names.
#' @export
#' @examples
#' to_varname(c("\u59d3\u540d", "\u5e74\u9f84", "\u6027\u522b"))
#' to_varname("\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd", abbrev = 4)
to_varname <- function(x,
                       unique = TRUE,
                       abbrev = NULL,
                       polyphone = FALSE,
                       other_replace = NULL) {
  py <- to_pinyin_toneless(x, sep = "_", polyphone = polyphone, other_replace = other_replace)

  if (!is.null(abbrev)) {
    py <- vapply(py, function(s) {
      if (is.na(s) || nchar(s) == 0) return(s)
      parts <- strsplit(s, "_", fixed = TRUE)[[1]]
      parts <- substr(parts, 1, abbrev)
      paste(parts, collapse = "_")
    }, character(1), USE.NAMES = FALSE)
  }

  # ensure it starts with a letter or dot
  py <- gsub("^([^a-zA-Z])", "X\\1", py)
  py <- make.names(py, unique = unique)
  py
}

#' Add a Custom Polyphone Phrase
#'
#' Allows users to extend the built-in phrase table with their own
#' multi-character phrases and readings. The reading can be given in either
#' numeric tone form (e.g. `"hang2 zhang3"`) or tone-mark form
#' (e.g. `"háng zhǎng"`); both forms are stored internally so the phrase
#' works correctly with all `tone` settings.
#'
#' @param phrase A Chinese character string (e.g. `"\u884c\u957f"`).
#' @param reading The corresponding Pinyin reading as a single string
#'   (e.g. `"hang2 zhang3"` or `"háng zhǎng"`).
#'
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' add_phrase("\u884c\u957f", "hang2 zhang3")
#' to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE)
#' to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE, tone = "marks")
add_phrase <- function(phrase, reading) {
  if (!is.character(phrase) || length(phrase) != 1 || nchar(phrase) < 2) {
    stop("`phrase` must be a single string of at least 2 Chinese characters.", call. = FALSE)
  }
  if (!is.character(reading) || length(reading) != 1) {
    stop("`reading` must be a single string.", call. = FALSE)
  }
  env <- get_phrase_env()
  style <- detect_reading_style(reading)
  if (style == "tone") {
    env[[phrase]] <- list(tone = reading, marks = num_to_mark(reading))
  } else if (style == "marks") {
    env[[phrase]] <- list(tone = mark_to_num(reading), marks = reading)
  } else {
    env[[phrase]] <- list(tone = reading, marks = reading)
  }
  invisible(NULL)
}

#' List Custom Polyphone Phrases
#'
#' @return A data frame with columns `phrase`, `tone`, and `marks`.
#' @export
#' @examples
#' list_phrases()
list_phrases <- function() {
  env <- get_phrase_env()
  keys <- ls(env, all.names = TRUE)
  if (length(keys) == 0) {
    return(data.frame(
      phrase = character(0), tone = character(0), marks = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    phrase = keys,
    tone = vapply(keys, function(k) {
      obj <- env[[k]]
      if (is.list(obj)) obj$tone else obj
    }, character(1), USE.NAMES = FALSE),
    marks = vapply(keys, function(k) {
      obj <- env[[k]]
      if (is.list(obj)) obj$marks else obj
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @noRd
get_phrase_env <- function() {
  if (!exists(".hanyupinyin_phrases", envir = .hanyupinyin_env, inherits = FALSE)) {
    assign(".hanyupinyin_phrases", new.env(hash = TRUE, parent = emptyenv()), envir = .hanyupinyin_env)
    init_default_phrases()
  }
  get(".hanyupinyin_phrases", envir = .hanyupinyin_env, inherits = FALSE)
}

#' @noRd
init_default_phrases <- function() {
  env <- get_phrase_env()
  defaults <- list(
    "\u884c\u957f" = "hang2 zhang3",
    "\u94f6\u884c" = "yin2 hang2",
    "\u884c\u8d70" = "xing2 zou3",
    "\u8fd8\u884c" = "hai2 xing2",
    "\u91cd\u8981" = "zhong4 yao4",
    "\u91cd\u65b0" = "chong2 xin1",
    "\u957f\u5927" = "zhang3 da4",
    "\u76f8\u5f53" = "xiang1 dang1",
    "\u4e0a\u8c03" = "shang4 diao4",
    "\u4eba\u53c2" = "ren2 shen1",
    "\u6570\u5b57" = "shu4 zi4",
    "\u6570\u4e0d\u6e05" = "shu3 bu4 qing1",
    "\u5927\u592b" = "dai4 fu1",
    "\u7a7a\u5730" = "kong4 di4",
    "\u7a7a\u95f4" = "kong1 jian1",
    "\u5904\u7406" = "chu3 li3",
    "\u5230\u5904" = "dao4 chu4",
    "\u4fbf\u5b9c" = "pian2 yi2",
    "\u65b9\u4fbf" = "fang1 bian4",
    "\u97f3\u4e50" = "yin1 yue4",
    "\u5feb\u4e50" = "kuai4 le4",
    "\u7761\u89c9" = "shui4 jiao4",
    "\u89c9\u5f97" = "jue2 de2",
    "\u5dee\u522b" = "cha1 bie2",
    "\u5dee\u52b2" = "cha4 jin4",
    "\u51fa\u5dee" = "chu1 chai1",
    "\u65f6\u4ee3" = "shi2 dai4",
    "\u5927\u738b" = "dai4 wang2",
    "\u90fd\u5e02" = "du1 shi4",
    "\u90fd\u662f" = "dou1 shi4",
    "\u9732\u51fa" = "lu4 chu1",
    "\u9732\u73e0" = "lu4 zhu1",
    "\u76f8\u58f0" = "xiang4 sheng1",
    "\u548c\u5c1a" = "he2 shang1",
    "\u9644\u548c" = "fu4 he4",
    "\u6311\u6218" = "tiao3 zhan4",
    "\u8c03\u76ae" = "tiao2 pi2",
    "\u8fd8\u662f" = "hai2 shi4",
    "\u8fd8\u6709" = "hai2 you3",
    "\u8fd8\u7ed9" = "huan2 gei3",
    "\u7231\u597d" = "ai4 hao4",
    "\u597d\u5403" = "hao3 chi1",
    "\u591a\u5c11" = "duo1 shao3",
    "\u5c11\u5e74" = "shao4 nian2",
    "\u5c11\u6570" = "shao3 shu4",
    "\u5f3a\u5236" = "qiang2 zhi4",
    "\u52c9\u5f3a" = "mian3 qiang3",
    "\u5f3a\u58ee" = "qiang2 zhuang4",
    "\u4e2d\u5fc3" = "zhong1 xin1",
    "\u4e2d\u5956" = "zhong4 jiang3",
    "\u4e2d\u95f4" = "zhong1 jian1",
    "\u957f\u77ed" = "chang2 duan3",
    "\u751f\u957f" = "sheng1 zhang3",
    "\u957f\u76f8" = "zhang3 xiang4",
    "\u88ab\u5b50" = "bei4 zi5",
    "\u88ab\u52a8" = "bei4 dong4",
    "\u611f\u89c9" = "gan3 jue2",
    "\u76f8\u4fe1" = "xiang1 xin4",
    "\u548c\u5e73" = "he2 ping2",
    "\u4e92\u76f8" = "hu4 xiang1"
  )
  for (nm in names(defaults)) {
    if (!exists(nm, envir = env, inherits = FALSE)) {
      reading <- defaults[[nm]]
      env[[nm]] <- list(tone = reading, marks = num_to_mark(reading))
    }
  }
}

# ---------------------------------------------------------------------------
# Internal tone conversion utilities
# ---------------------------------------------------------------------------

#' @noRd
strip_tone <- function(s) {
  gsub("[1-5]", "", s)
}

#' @noRd
detect_reading_style <- function(reading) {
  if (grepl("[1-5]", reading)) {
    "tone"
  } else if (grepl("[\u0101\u00e1\u01ce\u00e0\u0113\u00e9\u011b\u00e8\u012b\u00ed\u01d0\u00ec\u014d\u00f3\u01d2\u00f2\u016b\u00fa\u01d4\u00f9\u01d6\u01d8\u01da\u01dc\u00fc\u0144\u0148\u01f9\u1e3f]", reading, perl = TRUE)) {
    "marks"
  } else {
    "toneless"
  }
}

#' @noRd
num_to_mark_syllable <- function(syl) {
  tone <- as.integer(sub(".*([1-5])$", "\\1", syl))
  if (is.na(tone)) return(syl)

  base <- sub("[1-5]$", "", syl)
  if (tone == 5) return(base)

  chars <- strsplit(base, "")[[1]]

  # Special standalone nasal syllables
  if (identical(base, "m") && tone == 2) return("\u1e3f")
  if (identical(base, "n")) {
    if (tone == 2) return("\u0144")
    if (tone == 3) return("\u0148")
    if (tone == 4) return("\u01f9")
    return(base)
  }

  target <- NA_integer_

  pos_a <- which(chars == "a")
  if (length(pos_a) > 0) {
    target <- pos_a[1]
  } else {
    pos_e <- which(chars == "e")
    pos_o <- which(chars == "o")
    if (length(pos_e) > 0) {
      target <- pos_e[1]
    } else if (length(pos_o) > 0) {
      target <- pos_o[1]
    } else {
      pos_i <- which(chars == "i")
      pos_u <- which(chars == "u")
      pos_v <- which(chars == "v" | chars == "\u00fc")
      if (length(pos_i) > 0 && length(pos_u) > 0) {
        # iu -> mark u (later one); ui -> mark i (later one)
        target <- max(pos_i[1], pos_u[1])
      } else if (length(pos_v) > 0) {
        target <- pos_v[1]
      } else {
        vowels <- c("i", "u")
        pos <- which(chars %in% vowels)
        if (length(pos) > 0) target <- pos[1]
      }
    }
  }

  if (is.na(target)) return(base)

  tone_map <- list(
    a = c("\u0101", "\u00e1", "\u01ce", "\u00e0"),
    e = c("\u0113", "\u00e9", "\u011b", "\u00e8"),
    i = c("\u012b", "\u00ed", "\u01d0", "\u00ec"),
    o = c("\u014d", "\u00f3", "\u01d2", "\u00f2"),
    u = c("\u016b", "\u00fa", "\u01d4", "\u00f9"),
    v = c("\u01d6", "\u01d8", "\u01da", "\u01dc"),
    "\u00fc" = c("\u01d6", "\u01d8", "\u01da", "\u01dc")
  )

  ch <- chars[target]
  if (ch %in% names(tone_map)) {
    chars[target] <- tone_map[[ch]][tone]
  }
  paste(chars, collapse = "")
}

#' @noRd
mark_to_num_syllable <- function(syl) {
  mark_map <- c(
    "\u0101" = "a1", "\u00e1" = "a2", "\u01ce" = "a3", "\u00e0" = "a4",
    "\u0113" = "e1", "\u00e9" = "e2", "\u011b" = "e3", "\u00e8" = "e4",
    "\u012b" = "i1", "\u00ed" = "i2", "\u01d0" = "i3", "\u00ec" = "i4",
    "\u014d" = "o1", "\u00f3" = "o2", "\u01d2" = "o3", "\u00f2" = "o4",
    "\u016b" = "u1", "\u00fa" = "u2", "\u01d4" = "u3", "\u00f9" = "u4",
    "\u01d6" = "v1", "\u01d8" = "v2", "\u01da" = "v3", "\u01dc" = "v4",
    "\u00fc" = "v0",
    "\u0144" = "n2", "\u0148" = "n3", "\u01f9" = "n4",
    "\u1e3f" = "m2"
  )

  out <- syl
  tone_num <- "5"
  for (mk in names(mark_map)) {
    if (grepl(mk, out, fixed = TRUE)) {
      out <- gsub(mk, substr(mark_map[mk], 1, 1), out, fixed = TRUE)
      tone_num <- substr(mark_map[mk], 2, 2)
    }
  }
  # If no tone mark was found and no trailing digit, return as-is
  if (tone_num == "5" && !grepl("[1-5]$", syl)) {
    return(out)
  }
  paste0(out, tone_num)
}

#' @noRd
num_to_mark <- function(s) {
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  paste(vapply(words, num_to_mark_syllable, character(1), USE.NAMES = FALSE), collapse = " ")
}

#' @noRd
mark_to_num <- function(s) {
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  paste(vapply(words, mark_to_num_syllable, character(1), USE.NAMES = FALSE), collapse = " ")
}
