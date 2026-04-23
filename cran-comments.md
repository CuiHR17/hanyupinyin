## CRAN submission comments

### Submission (v0.1.2)

This is a minor update adding tone-mark output and improving the polyphone phrase table.

**New features**

* `to_pinyin()` gains a new `tone = "marks"` option that returns Pinyin with
diacritic tone marks (e.g. `qiū`) directly from the Unicode Unihan `kMandarin`
field.
* New convenience wrapper `to_pinyin_marks()` is equivalent to
`to_pinyin(..., tone = "marks")`.

**Improvements**

* The built-in polyphone phrase table now stores both numeric-tone and tone-mark
readings internally, so phrases work correctly with all `tone` settings
(including `tone = FALSE`).
* `add_phrase()` auto-detects whether the user supplies a numeric or tone-mark
reading and derives the other format automatically. Underscores and hyphens are
accepted as syllable separators and normalised to spaces.
* `list_phrases()` now returns three columns (`phrase`, `tone`, `marks`)
instead of two. This is a minor API change for a recently-released package
with no known reverse dependencies.

### R CMD check results

There were no ERRORs, no WARNINGs, and no NOTEs.

### Win-builder results

* R-release (Windows): 0 ERROR, 0 WARNING, 0 NOTE
* R-devel (Windows): 0 ERROR, 0 WARNING, 0 NOTE

### Downstream dependencies

There are no downstream dependencies.
