# README #

JASPAR plots

18-10-2021
    

This repository contains two scripts to generate plots summarising the number of motifs stored in [JASPAR](http://jaspar.genereg.net/) and the number of citations across the time (since 2004).

  * A :
  * B :



The list of all required R packages is listed below:

```R
required.packages = c("data.table",
                      "dplyr",
                      "ggplot2",
                      "lubridate",
                      "optparse",
                      "plotly",
                      "purrr",
                      "rcartocolor",
                      "reshape2",
                      "scholar",
                      "tidyr")


for (lib in required.packages) {
    if (!require(lib, character.only = TRUE)) {
        install.packages(lib)
        suppressPackageStartupMessages(library(lib, character.only = TRUE))
    }
}
```
