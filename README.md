# map_by_fips

A lightweight, reusable R utility for fast and flexible **county-level mapping** in the United States.

`map_by_fips` provides a single high-level function — **`map_by_fips_tidy()`** — that supports:

- **Univariate choropleth maps**
- **Bivariate maps** using *biscale*
- **Log-scale transformation**
- **Discrete or continuous color scales**

---

## 🔧 Installation

Clone the repository:

```sh
git clone https://github.com/smcgsmith/map_by_fips.git

---

# Example usage

``# Input must contain: FIPS, value
data <- tibble(
  FIPS = c("01001", "01003", "01005"),
  value = c(0.001, 0.02, 0.15)
)

# Create a univariate map with a log-scaled legend
p <- map_by_fips_tidy(
  data.to.map = data,
  log_scale = TRUE,
  discrete_scale = TRUE,
  color.break.type = "values",
  color.break.values = log_breaks(data$value, n = 6, min_val = 0, digits = 6),
  color.break.digits = 6,
  color.sequence = c("white", "darkred"),
  county.border.col = NA
)

p ``