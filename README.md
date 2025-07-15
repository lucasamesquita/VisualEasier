# VisualEasier

![Example output](documents/example_output.png)

## Introduction  
*VisualEasier* is an interactive application designed for the visualization and interpretation of spatially resolved mineral chemistry data. Built using the R language and the Shiny framework, the tool enables the generation of high-quality single-element maps, RGB composites, and clustering-based segmentation of samples. It supports datasets from a variety of analytical methods such as SEM-EDS, Electron Microprobe (EPMA), MicroXRF, and LA-ICP-MS, as long as the input data is structured in comma-separated files (.csv) with pixel-based spatial reference.

The application aims to promote accessible and standardized visualization of geochemical imaging data, even for users with no coding experience.

## Features  
- Generation of pixel-by-pixel normalized single-element maps  
- RGB ternary composites using any combination of three elements  
- Application of image filters (median and gradient) to highlight chemical/textural patterns  
- k-means clustering to identify compositional domains  
- Export of all outputs in high-quality vectorized PDF format  
- Fully interactive and user-friendly interface

## Input Data  
To function properly, the application requires a set of CSV files where each file represents a single element. The values must be expressed in **counts per second (cps)** or in **concentration**, and each file should include positional information for spatial reconstruction. Additionally, a reference file with bulk composition may be used to convert cps values into relative concentrations (when applicable).  

This structure supports data exported from MicroXRF, SEM/EDS, EPMA, and other spatially resolved techniques.

## Building  
The application was developed in the R programming language (version 4.3.2), using the Shiny framework for the user interface. The following R packages were used to support data handling, image processing, and visualization tasks:

- `shiny`  
- `shinythemes`  
- `tidyverse`  
- `readr`  
- `reshape2`  
- `raster`  
- `data.table`  
- `patchwork`  
- `Cairo`  
- `imager`  
- `pals`  
- `rsconnect`  
- `rasterVis`  
- `ggpubr`  

All required packages can be installed directly from CRAN. The application is organized into a single R script containing both UI and server functions, and it can be launched locally in RStudio or published to a Shiny server.

## Usage  
The application can be launched locally through RStudio or accessed via web browser (coming soon at **[https://apps.sgb.gov.br](https://apps.sgb.gov.br)**).  

## Important Notes  
⚠️ VisualEasier is under active development. Interface elements, file structures, and output formats may change in future updates.  
⚠️ The quality of the visual output depends on the quality and consistency of the input data. Ensure that the dataset is complete and properly formatted before use.  
⚠️ File naming must be consistent for proper recognition (e.g., sample_element.csv).  

## Demo and Documentation  
A detailed user guide and video tutorials (in Portuguese) will be made available shortly. Example datasets and test cases are included in the `/example_data/` folder.

## Contributors  
- Lucas Abud de Mesquita (lucas.mesquita@sgb.gov.br)
- Guilherme Ferreira da Silva (guilherme.ferreira@sgb.gov.br)  

## License  
The source code for *VisualEasier* is licensed under the BSD 3-Clause License. See `LICENSE` for more details.
