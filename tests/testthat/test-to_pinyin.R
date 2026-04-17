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

test_that("polyphone phrase table works", {
  # default sep is "_", so spaces inside phrase readings are replaced by sep
  expect_equal(to_pinyin("\u94f6\u884c\u884c\u957f", polyphone = TRUE), "yin2_hang2_hang2_zhang3")
})

test_that("user-defined phrases work", {
  add_phrase("\u6d4b\u8bd5\u77ed\u8bed", "ce4 shi4 duan3 yu3")
  expect_equal(to_pinyin("\u6d4b\u8bd5\u77ed\u8bed", polyphone = TRUE), "ce4_shi4_duan3_yu3")
})

test_that("non-Chinese characters are handled", {
  expect_equal(to_pinyin("abc", other_replace = NULL), "abc")
  expect_equal(to_pinyin("abc", other_replace = "-"), "---")
})

test_that("empty and NA inputs are handled", {
  expect_equal(to_pinyin(""), "")
  expect_equal(to_pinyin(NA_character_), NA_character_)
})
