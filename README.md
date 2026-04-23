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
- **Flexible tone output** – numeric tones (`qiu1`), toneless (`qiu`), or
  diacritic marks (`qiū`) via a single function.
- **Data-cleaning friendly** – helpers for generating valid R variable names,
  URL slugs, initials, and toneless output.

## Installation

You can install the development version of **hanyupinyin** from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("CuiHR17/hanyupinyin")
```

## Usage

``` r
library(hanyupinyin)

# Basic conversion (numeric tones)
to_pinyin("春眠不觉晓")
#> [1] "chun1_mian2_bu4_jue2_xiao3"

# Tone marks via the unified interface
to_pinyin("春眠不觉晓", tone = "marks")
#> [1] "chūn_mián_bù_jué_xiǎo"

# Convenience wrappers
to_pinyin_toneless("中华人民共和国")
#> [1] "zhong_hua_ren_min_gong_he_guo"

to_pinyin_marks("春眠不觉晓")
#> [1] "chūn_mián_bù_jué_xiǎo"

# Initials only
to_pinyin_initials("中华人民共和国")
#> [1] "zhrmghg"

# Polyphone handling
to_pinyin("银行行长", polyphone = TRUE)
#> [1] "yin2_hang2_hang2_zhang3"

to_pinyin("银行行长", polyphone = TRUE, tone = "marks")
#> [1] "yín_háng_háng_zhǎng"

# Generate valid R variable names from Chinese labels
to_varname(c("姓名", "年龄", "性别"))
#> [1] "xing_ming"  "nian_ling"  "xing_bie"

# URL-friendly slug
to_slug("2026年报告")
#> [1] "2026-nian-bao-gao"
```

## Custom polyphone phrases

Users can extend the built-in phrase table. The `reading` argument accepts
numeric tones, tone marks, or even toneless syllables. Syllables should be
separated by spaces (underscores and hyphens are also accepted and normalised
automatically).

``` r
# Numeric input -- both tone and mark outputs work automatically
add_phrase("测试短语", "ce4 shi4 duan3 yu3")
to_pinyin("测试短语", polyphone = TRUE)
#> [1] "ce4_shi4_duan3_yu3"
to_pinyin("测试短语", polyphone = TRUE, tone = "marks")
#> [1] "cè_shì_duǎn_yǔ"

# Tone-mark input -- numeric tones are derived automatically
add_phrase("和平", "hé píng")
to_pinyin("和平", polyphone = TRUE, tone = "marks")
#> [1] "hé_píng"

# Underscore separators are also accepted
add_phrase("行长", "hang2_zhang3")
to_pinyin("行长", polyphone = TRUE)
#> [1] "hang2_zhang3"

# Inspect stored phrases
list_phrases()
#>      phrase       tone      marks
#> 1    测试短语 ce4 shi4 duan3 yu3 cè shì duǎn yǔ
#> 2      和平     hé píng    hé píng
#> 3      行长   hang2 zhang3 háng zhǎng
```

## Acknowledgements

This package was inspired by the CRAN package `pinyin` (Peng Zhao et al.),
which was archived in April 2026 after the maintainer became unreachable.
`hanyupinyin` is a ground-up rewrite using standard Unicode data and modern R
practices.

Dictionary data are derived from the Unicode Unihan Database
(<https://www.unicode.org/reports/tr38/>) and used under the Unicode License.
