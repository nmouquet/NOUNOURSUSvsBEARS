# Too cute to be wild

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Research compendium to reproduce analyses and figures of the following article:

> Too cute to be wild: what teddy bears reveal about our disconnection from nature, by Mouquet N., Blanc, N., Brassac T., Casajus N. & Tribot A.-S. Submited to Trends in Ecology and Evolution


## Content

This repository is structured as follow:

- [`data/`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/data):
contains data required to reproduce figures and tables

- [`analysis/`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/):
contains subfolders organized by theme. Each folder contains R scripts to run 
specific analysis

- [`results/`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/results):
follows the structure of analyses. Contains intermediate results and the 
numeric results used to produce the figures

- [`outputs/`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/outputs):
contains the figures produced for the article

- [`DESCRIPTION`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/DESCRIPTION):
contains project metadata (author, date, dependencies, etc.)

## Workflow
    
The script [`analysis/Cludy_toys.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/Cludy_toys.R) compute functional insurance with simulated metacommunities.

The script [`analysis/Bear_vs_nounoursus.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/Bear_vs_nounoursus.R) get the morphometric and colorimetric features from the analysis of images for Teddy bears and Real bears; and perform the main analysis

The script [`analysis/features/cluster.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/features/cluster.R) extract information on color clusters heterogeneity in images.

The script [`analysis/features/lumsat.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/features/lumsatl.R) extract information on lightness and saturation in images.

The script [`analysis/morpho/morpho.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/morpho/morpho.R) compute the morphometric measures from the analysis of teady and real bears images.


## Big files 

  Some files were not uploaded to GitHub due to their large size. They are available upon request:

  data/illustrations_figures
  data/images_real
  data/images_real_croped
  data/images_real_tocrop
  data/nounoursus


##Figures are stored in `outputs/`.

The following Figures and Tables can be reproduced with the script indicated in brackets (all in [`analysis/`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/)):

- [cuddly.tiff](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/outputs), was produced by [`Cludy_toys.R`](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/analysis/Cludy_toys.R)
- [fig_pca](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/outputs) wad produced by [`Bear`_vs_nounoursus.R`](https://github.com/nmouquet/OUNOURSUSvsBEARS/tree/master/analysis/Bear`_vs_nounoursus.R)
- [fig1-sup](https://github.com/nmouquet/NOUNOURSUSvsBEARS/tree/master/outputs), [fig2-sup](https://github.com/nmouquet/SAFE/tree/master/outputs), were produced by [`Bear`_vs_nounoursus.R`](https://github.com/nmouquet/OUNOURSUSvsBEARS/tree/master/analysis/Bear`_vs_nounoursus.R)


## Installation

To install this compendium:

- [Fork](https://docs.github.com/en/get-started/quickstart/contributing-to-projects) 
this repository using the GitHub interface.
- [Clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) 
your fork using `git clone fork-url` (replace `fork-url` by the URL of your fork). 
Alternatively, open [RStudio IDE](https://posit.co/products/open-source/rstudio/) 
and create a New Project from Version Control.


## Contributing

All types of contributions are encouraged and valued. For more information, 
check out our [Contributor Guidelines](https://github.com/nmouquet/NOUNOURSUSvsBEARS/blob/main/CONTRIBUTING.md).

Please note that this project is released with a 
[Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). 
By contributing to this project, you agree to abide by its terms.
