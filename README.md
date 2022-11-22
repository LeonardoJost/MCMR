[![DOI](https://zenodo.org/badge/288438147.svg)](https://zenodo.org/badge/latestdoi/288438147)

# MCMR
Repository for multiple choice mental rotation tests

This project contains code for conducting and analyzing two experiments testing effects of multiple choice mental rotation tests and the data obtained in both studies.

The rationale and preregistration for the first study is available at https://osf.io/b78yx . The results are published in Jost (2022).
The rationale and preregistration for the second study is available at https://doi.org/10.23668/psycharchives.5394

## Experiment
The experiments are programmed with the OpenSesame Software (Mathôt et al., 2012; https://osdoc.cogsci.nl/) and the code is in the OpenSesame folder.
Due to the large number of stimuli, these are not included. They can be generated by the code at https://github.com/LeonardoJost/MRlibrary .
The folder contains both online and offline versions of the experiment.

## Analysis
The MCMR.R script and the functions folder contain all R code (R Core Team, 2022) to read and transform the raw data and generate figures. The Julia folders contain the code for the statistical analysis for each experiment using MixedModels package (Bates et al., 2021) in Julia (Bezanson et al., 2017).

## Literature
Bates, D., Alday, P., Kleinschmidt, D., José Bayoán Santiago Calderón, P., Zhan, L., Noack, A., Arslan, A., Bouchet-Valat, M., Kelman, T., Baldassari, A., Ehinger, B., Karrasch, D., Saba, E., Quinn, J., Hatherly, M., Piibeleht, M., Mogensen, P. K., Babayan, S., & Gagnon, Y. L. (2021). JuliaStats/MixedModels.jl: v4.0.0. Zenodo. https://doi.org/10.5281/zenodo.596435

Bezanson, J., Edelman, A., Karpinski, S., & Shah, V. B. (2017). Julia: A fresh approach to numerical computing. SIAM Review, 59(1), 65–98. https://doi.org/10.1137/141000671

Jost, L. (2022). Mental Rotation - The Test Design, Sex Differences, and the Link to Physical Activity. https://doi.org/10.5283/epub.51432

Mathôt, S., Schreij, D., & Theeuwes, J. (2012). OpenSesame: An open-source, graphical experiment builder for the social sciences. Behavior Research Methods, 44(2), 314-324. doi:10.3758/s13428-011-0168-7

R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. URL https://www.R-project.org/.
