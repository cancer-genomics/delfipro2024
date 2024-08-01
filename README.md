# delfipro2024

This project is a workflowr repository containing code needed to generate all figures and analyses in "Early detection of ovarian cancer using cell-free DNA
fragmentomes and protein biomarkers".

Upstream preprocessing code needed to run the DELFI pipeline and generate features used in modelling is available at https://github.com/cancer-genomics/delfi3

A summary of the available resources in this repository is available in /docs/index.html - please view this first.

There are 5 folders of interest in this workflowr.

(1) analysis - contains code needed to generate each figure and supplementary figure of the paper.

(2) code - This contains code needed to train DELFI-Pro models and compile the model scores, as well as some packages with functions used by other code in this repository. This folder contains a separate README describing each file.

(3) data - This contains raw data used to generate the figures and tables, as well as data compiled by scripts in the code folder.

(4) docs - This contains html of the markdown files in analysis, as well as the generated figures.

(5) output - This contains additional data compiled by scripts in the code folder.

SessionInfo.Rmd in the analysis folder, and the corresponding html in the docs folder document the versions of packages used to compile this repo. 

This repository is available on Github, and may be run as a workflowr project to generate a webpage with all code and figures linked. 
[workflowr]: https://github.com/jdblischak/workflowr
