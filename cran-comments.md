## Submission (5.0.0)

Major version: the layout chromosome is `n` unique grid-cell IDs instead of
a 0/1 string. `selection()` returns an ID matrix; `genetic_algorithm` uses
set-crossover and swap-mutation. Binary helpers `crossover()`, `mutation()`
and `trimton()` remain for old code.

Also fixes the ggplot2 >= 4.0 test failure that archived 4.0.0 on r-devel
(`plot_windrose()` now uses `inherits(., c("ggplot", "ggplot2::ggplot"))`).

## R CMD check results

0 errors | 0 warnings | 0 notes
