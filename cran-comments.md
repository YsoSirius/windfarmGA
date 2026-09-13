## Resubmission (5.0.0)

This is a resubmission of 5.0.0. The previous incoming check failed
on the PDF manual (Unicode `≈` in `selection.Rd`) and reported a 404
CRAN-checks URL in README (package is archived, so that page does
not exist). Both are fixed: Rd text is ASCII, the badge link is gone.

windfarmGA 4.0.0 was archived on 2025-11-07 because R-devel checks
failed (ggplot2 >= 4.0 / S7: `plot_windrose()` returned
`ggplot2::ggplot`, tests expected class `"gg"`) and the issues were
not fixed in time.

`plot_windrose()` now uses `inherits(., c("ggplot", "ggplot2::ggplot"))`.
Tests cover ggplot2 3.x and 4.x.

This is also a major version. Layouts are `n` unique grid-cell IDs, not
a 0/1 chromosome. `selection()` returns an ID matrix; the GA uses
`set_crossover()` and `swap_mutation()`. The old binary helpers
`crossover()`, `mutation()` and `trimton()` stay exported.

## Test environments

* GitHub Actions: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1), `--as-cran`
* local Windows, R 4.4

## R CMD check results

0 errors | 0 warnings | 0 notes

Incoming feasibility will still NOTE that this is a new submission
of an archived package. That is expected.

There is an INFO (not a NOTE) on installed size (~6 MB:
`data`, `img`, compiled `libs`). `resultrect` is the first 50
generations of the old example run (`xz`, ~90 KB). `experimental/`
is not in the tarball.

Parallel and terrain-download tests use `skip_on_cran()`.

## Reverse dependencies

The package was archived, so there are no CRAN reverse dependencies
to check.
