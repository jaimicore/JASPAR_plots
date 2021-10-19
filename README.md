# README #

JASPAR plots

19-10-2021
    

This repository contains two scripts to generate plots summarising the number of motifs stored in [JASPAR](http://jaspar.genereg.net/) and the number of citations across the time (since 2004).

  * JASPAR_collections_summary_plots.R : draws barplots containing the growth of JASPAR CORE and UNVALIDATED collections
  * JASPAR_citations.R                 : draws a barplot showing the citation of all JASPAR releases since 2004

Both scripts generate static (pdf) and interactive (html) barplots using ggplot and plotly, respectively. Additionally, the interactive plots can be directly uploaded to plotly server by providing a user name and a valid API key, for more details see [plotly's website](https://chart-studio.plotly.com/feed/#/).


How to run these scripts:

```R
## Export citation barplots locally
Rscript JASPAR_citations.R -o JASPAR_citations

## Exports citation barplots locally and thorugh the plotly website of the given user
Rscript JASPAR_citations.R -o JASPAR_citations -p User_Name -k API_Key


## Export JASPAR growth barplots locally
Rscript Jaspar_collections_summary_plots.R -i data/Motifs_per_taxon_per_release.csv -o JASPAR_growth

## Exports JASPAR growth barplots locally and thorugh the plotly website of the given user
Rscript Jaspar_collections_summary_plots.R -i data/Motifs_per_taxon_per_release.csv -o JASPAR_growth -p User_Name -k API_Key
```


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
