# Build dictionary data from Unicode Unihan Database
# This script is run once during package development to generate:
#   - R/sysdata.rda   (internal fast lookup tables)
#   - data/unihan_pinyin.rda (exported dataset)

# Step 1: Download Unicode Unihan data
url <- "https://www.unicode.org/Public/UCD/latest/ucd/Unihan.zip"
dest <- tempfile(fileext = ".zip")
download.file(url, dest, mode = "wb")
unzip(dest, exdir = tempdir())

readings_file <- file.path(tempdir(), "Unihan_Readings.txt")

# Step 2: Parse kMandarin field
lines <- readLines(readings_file, encoding = "UTF-8")
lines <- lines[!grepl("^#", lines)]
lines <- lines[nchar(lines) > 0]

# Split tab-separated lines
parts <- strsplit(lines, "\t", fixed = TRUE)
parts <- parts[vapply(parts, length, integer(1)) == 3]

km <- do.call(rbind, parts)
km <- km[km[, 2] == "kMandarin", ]
codepoints <- as.integer(strtoi(substr(km[, 1], 3, nchar(km[, 1])), base = 16L))
chars <- intToUtf8(codepoints, multiple = TRUE)
readings <- km[, 3]

# Step 3: Convert tone marks to numeric tones
tone_map <- c(
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

convert_tone <- function(py) {
  out <- py
  tone_num <- "5"
  for (tm in names(tone_map)) {
    if (grepl(tm, out, fixed = TRUE)) {
      out <- gsub(tm, substr(tone_map[tm], 1, 1), out, fixed = TRUE)
      tone_num <- substr(tone_map[tm], 2, 2)
    }
  }
  list(
    pinyin = py,
    pinyin_tone = paste0(out, tone_num),
    pinyin_toneless = out
  )
}

results <- lapply(readings, function(r) {
  py_list <- strsplit(r, " ", fixed = TRUE)[[1]]
  conv <- lapply(py_list, convert_tone)
  data.frame(
    char = chars[1],
    pinyin = paste(vapply(conv, `[[`, character(1), "pinyin"), collapse = " "),
    pinyin_tone = paste(vapply(conv, `[[`, character(1), "pinyin_tone"), collapse = " "),
    pinyin_toneless = paste(vapply(conv, `[[`, character(1), "pinyin_toneless"), collapse = " "),
    stringsAsFactors = FALSE
  )
})

# This would continue to build sysdata.rda and unihan_pinyin.rda
# (see /tmp/build_dict.R for the complete production script used)
