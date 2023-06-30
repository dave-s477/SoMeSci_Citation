# SoMeSci Formal Software Citation

**A multi-level analysis of data quality for formal software citation** by 

David Schindler, Tazin Hossain, Sascha Spors, and Frank Krüger

## Abstract of the paper

Software is a central part of modern science, and knowledge of its use is crucial for the scientific community with respect to reproducibility and attribution of its developers.
Several studies have investigated in-text mentions of software and its quality, while the quality of formal software citations has only been analysed superficially. 
This study performs an in-depth evaluation of formal software citation based on a set of manually annotated software references.
It examines which resources are cited for software usage, to what extend they allow proper identification of software and its specific version---a central aspect of research provenance, how this information is made available by scientific publishers, and how well it is represented in large-scale bibliographic databases.
The results of the investigation show that software articles are the most cited resource for software, while direct software citations are better suited for identification of software versions. 
However, we found that current practices by both, publishers and bibliographic databases, are unsuited to represent these direct software citations, hindering large-scale analyses such as assessing software impact.
We argue that current practices for representing software citations---the recommended way to cite software by current citation standards---stand in the way of their further adaption by the scientific community, and urge providers of bibliographic data to explicitly model scientific software. 

## Repository

This repository contains all data and code to repeat the analyses outlined in the article and requires the `git submodule` [SoMeSci](), which contains the data required in the folder `Formal_Citation`, as well as further data with respect to software mentions in scientific articles. 

## Repeating the analysis

To repeat the analyses the following steps are necessary:
* get the sub modules `git submodule update --init`
* install required R(=4.3.0) packages: `tidyverse`(=2.0.0), `magrittr`(=2.0.3), `patchwork`(=1.1.2), `DescTools`(=0.99.48), `ggalluvial`(=0.12.5), `easyalluvial`(=0.3.1), `xtable`(=1.8-4), `rcompanion`(=2.4.30)
* execute `analyses.qmd`

[![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a [Creative Commons Attribution 4.0 International
License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg