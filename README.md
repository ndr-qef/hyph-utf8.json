# hyph-utf8

The TeX `hyph-utf8` hyphenation patterns in JSON format, serialized as pattern-score pairs for fast sequential reading.

## Usage

```bash
stack build
stack exec hyph-json -- --mode FORM
```

where `FORM` may be any of [`Data.Text.ICU.Normalize.NormalizationMode`](https://hackage.haskell.org/package/text-icu-0.7.0.1/docs/Data-Text-ICU-Normalize.html)
