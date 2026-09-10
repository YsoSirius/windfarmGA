## Resubmission

Archived because tests failed on r-devel with ggplot2 >= 4.0.0.
ggplot2 4.0 represents plots as S7 objects, so they are no longer recursive
lists with `class(.)[1] == "gg"`.

`plot_windrose()` tests now use `inherits(., c("ggplot", "ggplot2::ggplot"))`,
which works with ggplot2 3.x and 4.x.

## R CMD check results

0 errors | 0 warnings | 0 notes
