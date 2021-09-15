
Overview
--------

`repel` helps repel overlapping text labels. It is forked from `ggrepel` to
help in a non `ggplot2` context. Work in progress.



Installation
------------

```r

# install.packages("remotes")
remotes::install_github("hms-dbmi/repel")
```

Usage
-----

```
library(repel)

# plot points
plot(mpg ~ wt, data = mtcars)

# construct label_coords
label_coords <- mtcars[, c('wt', 'mpg')]
colnames(label_coords) <- c('x', 'y')
label_coords$label <- row.names(mtcars)

# get coords of repelled labels
repels <- repel_text(label_coords)

# plot labels and segments
segments(label_coords$x, label_coords$y, repels$x, repels$y, col = 'blue', lty=2)
text(repels$x, repels$y, labels = repels$label)
```

