test_that("basic conversion works", {
  expect_equal(to_pinyin("\u6625\u7720\u4e0d\u89c9\u6653"), "chun1_mian2_bu4_jue2_xiao3")
  expect_equal(to_pinyin("Hello \u4e16\u754c", sep = " "), "Hello shi4 jie4")
})

test_that("vectorized input works", {
  x <- c("\u4f60\u597d", "\u4e16\u754c")
  expect_equal(to_pinyin(x), c("ni3_hao3", "shi4_jie4"))
})

test_that("toneless mode works", {
  expect_equal(to_pinyin_toneless("\u6625\u7720\u4e0d\u89c9\u6653"), "chun_mian_bu_jue_xiao")
})

test_that("tone marks mode via to_pinyin works", {
  expect_equal(to_pinyin("\u6625\u7720\u4e0d\u89c9\u6653", tone = "marks"), "ch\u016bn_mi\u00e1n_b\u00f9_ju\u00e9_xi\u01ceo")
  expect_equal(to_pinyin("Hello \u4e16\u754c", sep = " ", tone = "marks"), "Hello sh\u00ec ji\u00e8")
})

test_that("tone marks wrapper works", {
  expect_equal(to_pinyin_marks("\u6625\u7720\u4e0d\u89c9\u6653"), "ch\u016bn_mi\u00e1n_b\u00f9_ju\u00e9_xi\u01ceo")
})

test_that("tone marks handles neutral tone", {
  expect_equal(to_pinyin("\u597d\u4e86", tone = "marks"), "h\u01ceo_le")
})

test_that("tone marks with polyphone works for built-in phrases", {
  # Built-in phrase "yin2 hang2" should auto-convert to marks
  expect_equal(to_pinyin("\u94f6\u884c", polyphone = TRUE, tone = "marks"), "y\u00edn_h\u00e1ng")
  expect_equal(to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE, tone = "marks"), "y\u00edn_h\u00e1ng_h\u00e1ng_zh\u01ceng")
})

test_that("initials extraction works", {
  expect_equal(to_pinyin_initials("\u4e2d\u534e\u4eba\u6c11\u5171\u548c\u56fd"), "zhrmghg")
})

test_that("slug generation works", {
  expect_equal(to_slug("2026\u5e74\u62a5\u544a"), "2026-nian-bao-gao")
})

test_that("variable name generation works", {
  expect_equal(to_varname(c("\u59d3\u540d", "\u5e74\u9f84")), c("xing_ming", "nian_ling"))
  expect_equal(to_varname("1\u5f00\u59cb"), c("X1_kai_shi"))
})

test_that("polyphone phrase table works in numeric mode", {
  expect_equal(to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE), "yin2_hang2_hang2_zhang3")
})

test_that("polyphone phrase table works in marks mode", {
  expect_equal(to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE, tone = "marks"), "y\u00edn_h\u00e1ng_h\u00e1ng_zh\u01ceng")
})

test_that("user-defined phrases with numeric reading work in both modes", {
  add_phrase("\u6d4b\u8bd5\u77ed\u8bed", "ce4 shi4 duan3 yu3")
  expect_equal(to_pinyin("\u6d4b\u8bd5\u77ed\u8bed", polyphone = TRUE), "ce4_shi4_duan3_yu3")
  expect_equal(to_pinyin("\u6d4b\u8bd5\u77ed\u8bed", polyphone = TRUE, tone = "marks"), "c\u00e8_sh\u00ec_du\u01cen_y\u01d4")
})

test_that("user-defined phrases with mark reading work in both modes", {
  add_phrase("\u81ea\u5b9a\u4e49\u8bcd", "z\u00ec d\u00ecng y\u00ec c\u00ed")
  expect_equal(to_pinyin("\u81ea\u5b9a\u4e49\u8bcd", polyphone = TRUE, tone = "marks"), "z\u00ec_d\u00ecng_y\u00ec_c\u00ed")
  expect_equal(to_pinyin("\u81ea\u5b9a\u4e49\u8bcd", polyphone = TRUE), "zi4_ding4_yi4_ci2")
})

test_that("list_phrases returns both tone and marks columns", {
  df <- list_phrases()
  expect_true("tone" %in% names(df))
  expect_true("marks" %in% names(df))
  expect_type(df$tone, "character")
  expect_type(df$marks, "character")
})

test_that("non-Chinese characters are handled", {
  expect_equal(to_pinyin("abc", other_replace = NULL), "abc")
  expect_equal(to_pinyin("abc", other_replace = "-"), "---")
})

test_that("empty and NA inputs are handled", {
  expect_equal(to_pinyin(""), "")
  expect_equal(to_pinyin(NA_character_), NA_character_)
})
