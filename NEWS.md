# hanyupinyin 0.1.2 (development)

* New `to_pinyin_marks()` returns Pinyin with diacritic tone marks (e.g. `qiū`)
  directly from the Unicode Unihan `kMandarin` field, correctly handling the
  neutral tone (tone 5) where the source data marks it.

# hanyupinyin 0.1.1

* Added citation to Unicode Unihan Database in DESCRIPTION.
* Added missing `\value` tags to Rd documentation.

# hanyupinyin 0.1.0

* Initial CRAN release.
* Vectorized conversion of Chinese characters to Hanyu Pinyin.
* Dictionary sourced from the Unicode Unihan Database (`kMandarin`).
* Support for tones, toneless output, initials, URL slugs, and valid R variable names.
* Built-in phrase table for common polyphones with user-extensible overrides.
