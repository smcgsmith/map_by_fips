# map_by_fips

A lightweight, reusable R utility for fast and flexible **county-level mapping** in the United States.

`map_by_fips` provides a single high-level function — **`map_by_fips_tidy()`** — that supports:

- **Univariate choropleth maps**
- **Bivariate maps** using *biscale*
- **Optional log-scale transformation**
- **Optional discrete or continuous color scales**
- **Automatic shapefile loading**
- **Optional plotting of point features** (e.g., slaughter facilities)
- **Dependency auto-installation via `utils/packages.R`**

This repo is designed to be cloned into future projects so you never rewrite mapping code again.

---

## 🔧 Installation

Clone the repository:

```sh
git clone https://github.com/<your-username>/map_by_fips.git
