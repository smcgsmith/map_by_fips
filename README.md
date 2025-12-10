# map_by_fips

R code for fast and flexible **county-level mapping** in the United States. Adapted from the original ``map_by_fips()`` package written by Clay Hallman.

This version of ``map_by_fips_tidy()`` now supports:

- **Univariate choropleth maps**
- **Bivariate maps** using ``biscale``
- **Log-scale transformation**
- **Discrete or continuous color scales**
- **Returns a ``ggplot2`` object for easy figure manipulation**

---

## Installation

Clone the repository:

```sh
git clone https://github.com/smcgsmith/map_by_fips.git
```
---

## Mapping one variable

```
# Input must contain: FIPS, value
data <- tibble(
  FIPS = c("01001", "01003", "01005"),
  value = c(0.001, 0.02, 0.15)
)

p <- map_by_fips_tidy(
  data.to.map = data,
  log_scale = TRUE,
  discrete_scale = TRUE,
  color.break.type = "values",
  color.break.values = log_breaks(data$value, n = 6, min_val = 0, digits = 6), # log scale is optional
  color.break.digits = 6,
  color.sequence = c("white", "darkred"),
  county.border.col = NA
)

p # one ggplot2 object to view or edit
```

## Bivariate mapping

```
df <- tibble(
  FIPS   = c(1001, 1003, 1005),
  risk   = c(0.02, 0.15, 0.40),
  impact = c(0.5, 2.3, 4.8)
)

biv_data <- bi_class(
  df,
  x    = "risk",
  y    = "impact",
  style = "fisher",
  dim   = 4
) %>%
  select(FIPS, bi_class)

break_vals <- bi_class_breaks(
  df,
  x = "risk",
  y = "impact",
  style = "fisher",
  dim = 4,
  split = TRUE
)

p <- map_by_fips_tidy(
  data.to.map     = biv_data,
  bivariate       = TRUE,
  pal             = "BlueOr",
  dim             = 4,
  bi_break_values = break_vals,
  xlab            = "Risk (X)",
  ylab            = "Impact (Y)",
  state.border.col  = "grey30",
  state.border.width = 0.75,
  county.border.col = NA
)

p
```