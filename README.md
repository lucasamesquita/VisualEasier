# QMap — VisualEasier

## Introduction

*QMap — VisualEasier* is an interactive web application for the processing, visualization, and quantitative interpretation of spatially resolved geochemical and mineral chemistry datasets. Built with R and the Shiny framework, the tool runs entirely in the browser and requires no coding experience.

Version 1.0.0 introduces a complete analytical workflow: from raw elemental matrix loading and data transformation, through false-colour and RGB composite mapping, k-means phase segmentation with entropy-based uncertainty mapping, and multivariate data analysis, to a full-session PDF report export.

The application supports data acquired by SEM-EDS, Electron Microprobe (EPMA), MicroXRF, LA-ICP-MS, and any other spatially resolved technique whose output can be structured as per-element numeric matrices.

## Features

- **Input Data** — load one CSV matrix per element; define normalization values, sample ID, and physical map dimensions (mm); save and reload sessions as `.veproj.rds` project files
- **Data Processing** — apply session-wide transformations before any downstream analysis:
  - Pixel-wise CPS → Wt% (requires normalization values)
  - Raw value (no transformation)
  - Centered Log-Ratio (CLR) — recommended for compositional data
  - Natural logarithm (ln)
  - MinMax scaling
  - Standard-deviation scaling
- **Map Processing** — per-element false-colour maps with calibrated mm axis and scale bar; RGB ternary composites from any three elements; optional image filters (Median, Gaussian blur, Gradient); element clipping editor; cluster-based spatial filter
- **Cluster Analysis** — k-means segmentation with optional PCA pre-reduction; customisable cluster colours; entropy map (IDW kernel) for pixel-level uncertainty; post-processing operations: rename, merge, delete, border erosion (statistics only), and moving-mode spatial filter
- **Data Analysis** — interactive scatter plots and ternary diagrams (Plotly); Spearman/Pearson correlogram; stratified sampling to maintain cluster proportions at scale
- **Create Report** — configurable multi-page A4 PDF export covering all active session outputs
- Fully browser-based; compatible with Docker deployment on a web server

## Input Data Format

Each element must be provided as a plain-text CSV or TXT file containing a numeric matrix with **no header row**. All files in a session must share identical dimensions (same number of rows and columns). Values may be in counts per second (CPS) or in any other numeric unit; the chosen data transformation is applied uniformly across all elements after loading.

Optionally, a normalization percentage (weight percent of the element in a reference standard) may be entered per element to enable the CPS → Wt% transformation.

This structure is directly compatible with export formats from MicroXRF, SEM-EDS, EPMA, and LA-ICP-MS mapping software.

## Installation and Usage

### Requirements

R ≥ 4.3 and the following packages (all available on CRAN):

### Imports
```r
c(
  "shiny", "shinythemes", "data.table", "dplyr", "tidyr", "stringr",
  "ggplot2", "patchwork", "Cairo", "imager", "pals", "ggpubr",
  "shinycssloaders", "scales", "png", "colourpicker", "RColorBrewer",
  "plotly", "corrplot", "htmlwidgets", "viridisLite", "GGally"
))
```

### Running locally

```r
shiny::runApp("app.R")
```

The application opens automatically in the default browser (`options(shiny.launch.browser = TRUE)` is set internally).

### Docker deployment

The application is designed to run as a containerised Shiny web app. A `Dockerfile` targeting `rocker/shiny` or `rocker/r-ver` can be used to build a portable image for deployment on any server or cloud environment.

```dockerfile
FROM rocker/shiny:latest
RUN R -e "install.packages(c('shiny','shinythemes','data.table','dplyr','tidyr','stringr','ggplot2','patchwork','Cairo','imager','pals','ggpubr','shinycssloaders','scales','png','colourpicker','RColorBrewer','plotly','corrplot','htmlwidgets','viridisLite','GGally'))"
COPY app.R /srv/shiny-server/
EXPOSE 3838
```

The application will be available at **[https://apps.sgb.gov.br](https://apps.sgb.gov.br)** (coming soon).

## Project File

Saving a session produces a `.veproj.rds` file that stores the input matrices, element metadata, normalization values, selected variables, element clipping settings, and the active processing method. Cluster results, rendered maps, and report outputs are not stored and must be regenerated after reloading.

![Example output](documents/example_output.png)

## Important Notes

> ⚠️ The quality of visual and statistical outputs depends directly on the quality and consistency of the input data. Ensure all matrices are complete, share identical dimensions, and use a consistent decimal separator before loading.

> ⚠️ The CPS → Wt% transformation assumes that the normalization percentages entered in Input Data are accurate reference values for the acquisition conditions. Errors in these values propagate to all weight-percent estimates and downstream statistics.

> ⚠️ K-means clustering results depend on the random initialisation. The application uses 10 independent restarts (nstart = 10) with a fixed internal seed (123) to ensure reproducibility within the same session and dataset.

> ⚠️ QMap — VisualEasier is under active development. Interface elements, file structures, and output formats may change in future releases.

## Demo and Documentation

A detailed user guide and video tutorials (in Portuguese) will be made available shortly. Example datasets are included in the `/example_data/` folder.

## Contributors

- Lucas Abud de Mesquita — [lucas.mesquita@sgb.gov.br](mailto:lucas.mesquita@sgb.gov.br)
- Guilherme Ferreira da Silva — [guilherme.ferreira@sgb.gov.br](mailto:guilherme.ferreira@sgb.gov.br)

## Citation

If you use QMap — VisualEasier in published work, please cite:

> VisualEasier: An interactive tool for geochemical and mineral chemistry data visualization and analysis. *Journal of the Geological Survey of Brazil.*

## License

The source code is licensed under the **BSD 3-Clause License**. See `LICENSE` for details.
