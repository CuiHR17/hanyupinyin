# hanyupinyin

<!-- badges: start -->
<!-- badges: end -->

**hanyupinyin** converts Chinese characters into Hanyu Pinyin in R. It is a
modern, vectorized, and self-contained package inspired by the now-orphaned
CRAN package `pinyin`.

## Why hanyupinyin?

- **Authoritative data** – dictionary sourced from the Unicode Unihan Database
  (`kMandarin`), covering ~44k unique characters.
- **Fully vectorized** – no slow `sapply` loops; works natively on character
  vectors.
- **Polyphone aware** – built-in phrase table for common ambiguous words (e.g.
  银行, 行长) with user-extensible overrides.
- **Data-cleaning friendly** – helpers for generating valid R variable names,
  URL slugs, initials, and toneless output.

## Installation

You can install the development version of **hanyupinyin** from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/hanyupinyin")
```

## Usage

``` r
library(hanyupinyin)

# Basic conversion
to_pinyin("春眠不觉晓")
#> [1] "chun1_mian2_bu4_jue2_xiao3"

# Toneless
to_pinyin_toneless("中华人民共和国")
#> [1] "zhong_hua_ren_min_gong_he_guo"

# Initials only
to_pinyin_initials("中华人民共和国")
#> [1] "zhrmghg"

# Polyphone handling
to_pinyin("银行行长", polyphone = TRUE)
#> [1] "yin2 hang2 hang2 zhang3"

# Generate valid R variable names from Chinese labels
to_varname(c("姓名", "年龄", "性别"))
#> [1] "xing_ming"  "nian_ling"  "xing_bie"

# URL-friendly slug
to_slug("2026年报告")
#> [1] "2026-nian-bao-gao"
```

## Custom polyphone phrases

``` r
add_phrase("测试短语", "ce4 shi4 duan3 yu3")
to_pinyin("测试短语", polyphone = TRUE)
#> [1] "ce4 shi4 duan3 yu3"

list_phrases()
```

## Acknowledgements

This package was inspired by the CRAN package `pinyin` (Peng Zhao et al.),
which was archived in April 2026 after the maintainer became unreachable.
`hanyupinyin` is a ground-up rewrite using standard Unicode data and modern R
practices.

Dictionary data are derived from the Unicode Unihan Database
(<https://www.unicode.org/reports/tr38/>) and used under the Unicode License.
