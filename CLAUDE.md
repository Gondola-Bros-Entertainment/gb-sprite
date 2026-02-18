# CLAUDE.md

## Rules

- **Only commit and push when explicitly instructed.** Never amend commits. Never add `Co-Authored-By` headers.
- Run `cabal test` and `cabal build --ghc-options="-Werror"` before considering any change complete.
- **`-Wall -Wcompat` clean.** All code compiles without warnings.

## Haskell Style

- **Pure by default.** IO only in BMP.hs and Export.hs for file writing.
- **Strict by default.** Bang patterns on all data fields and accumulators.
- **Total functions only.** No `head`, `tail`, `!!`, `fromJust`, `read`.
- **Named constants.** No magic numbers.
- **No prime-mark variables.** Use descriptive names.
- Ormolu for formatting, HLint for linting.

## Context

Procedural 2D sprite and VFX generation library for GB games. Pure Haskell — no external image libraries, no assets, no tools. Just math. Optional `juicy-pixels` cabal flag for PNG export.
