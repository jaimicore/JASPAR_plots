#####################
## Load R packages ##
#####################
required.libraries <- c("data.table",
                        "dplyr",
                        "ggplot2",
                        "optparse",
                        "plotly",
                        "reshape2",
                        "rcartocolor",
                        "tidyr")

for (lib in required.libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    suppressPackageStartupMessages(library(lib, character.only = TRUE))
  }
}


## How to run:
##
## When user provides plotly username and API key (results are uploaded to plotly user's website)
## Rscript Jaspar_collections_summary_plots.R -i Motifs_per_taxon_per_release.csv -o JASPAR_growth -p User_Name -k API_Key
##
## When user does not provides plotly username and API key (results are stored in the provided directory)
## Rscript Jaspar_collections_summary_plots.R -i Motifs_per_taxon_per_release.csv -o JASPAR_growth


####################
## Read arguments ##
####################
option_list = list(
  
  make_option(c("-i", "--input_table"), type = "character", default = NULL, 
              help = "Table with the number of motifs on each JASPAR release. (Mandatory) ", metavar = "character"),
  
  make_option(c("-o", "--output_directory"), type = "character", default = NULL, 
              help = "Output directory to export the results (Mandatory)", metavar = "character"),

  make_option(c("-k", "--plotly_api_key"), type = "character", default = NULL,
              help = "Plotly API key, use this to upload the interactive barplot to your pltoly account. Requires to have a plotly account (https://chart-studio.plotly.com/feed/#/) with a valid API key. (Optional) ", metavar = "character"),
  
  make_option(c("-p", "--plotly_user_name"), type = "character", default = NULL,
              help = "Plotly username, use this to upload the interactive barplot to your pltoly account. Requires to have a plotly account (https://chart-studio.plotly.com/feed/#/). (Optional) ", metavar = "character")
  
);
message("; Reading arguments from command-line")
opt_parser = OptionParser(option_list = option_list);
opt = parse_args(opt_parser);


## Set variable names
out.dir                   <- opt$output_directory
motifs.per.taxon.tab.file <- opt$input_table
plotly.api.key            <- opt$plotly_api_key
plotly.username           <- opt$plotly_user_name


#######################
## Plotly parameters ##
#######################

## In case the API is not provided, set the flag to 0 to avoid uploading the plots to the plotly website
plotly.api.key.flag <- ifelse(is.null(plotly.api.key), yes = 0, no = plotly.api.key)

## In case the API is not provided, set the flag to 0 to avoid uploading the plots to the plotly website
plotly.api.username.flag <- ifelse(is.null(plotly.username), yes = 0, no = plotly.username)

## Check the api-key and username are provided 
plotly.web.flag <- all(as.logical(plotly.api.key.flag), as.logical(plotly.api.username.flag), na.rm = T)



###########################
## Create output folders ##
###########################
message("; Creating output folder")
dir.create(out.dir, recursive = T)


##########################################
## Read JASPAR motifs per release table ##
##########################################
motifs.per.taxon.tab <- fread(motifs.per.taxon.tab.file, header = T, sep = ",")

## Count the motifs of each release
nb.motifs.per.release <- colSums(motifs.per.taxon.tab[,c(-1,-2)], na.rm = T)

## Get the year of the latest release
releases     <- as.numeric(names(nb.motifs.per.release))
release.year <- max(releases, na.rm = T)

## Convert the matrix in a data.frame
nb.motfs.per.release <- melt(motifs.per.taxon.tab)
nb.motfs.per.release <- nb.motfs.per.release %>% 
                          rename(Year     = variable,
                                 Nb_motifs = value)
                          
nb.taxa <- length(unique(nb.motfs.per.release$Taxon))


##########################
## Create color palette ##
##########################

## Determine order of taxon (descending, depending on the number of motifs)
taxon.order.CORE        <- subset(nb.motfs.per.release, Year == release.year & Collection == "CORE" ) %>%
                            drop_na(Year, Nb_motifs) %>% 
                            arrange(desc(Nb_motifs))
taxon.order.CORE        <- as.vector(taxon.order.CORE$Taxon)


taxon.order.UNVALIDATED <- subset(nb.motfs.per.release, Year == release.year & Collection == "UNVALIDATED") %>%
  drop_na(Year, Nb_motifs) %>% 
  arrange(desc(Nb_motifs))
taxon.order.UNVALIDATED <- as.vector(taxon.order.UNVALIDATED$Taxon)

taxon.order <- unique(c(taxon.order.CORE, taxon.order.UNVALIDATED))

tax.cols     <- carto_pal(length(taxon.order), "Bold")
cols         <- c( "#666666", tax.cols)
names(cols)  <- c("All_taxa", taxon.order)

## Order factors by number of motifs
nb.motfs.per.release$Taxon <- factor(nb.motfs.per.release$Taxon, levels = as.vector(c("All_taxa",
                                                                                      taxon.order)))

######################################################
## Generate plots: CORE and UNVALIDATED collections ##
######################################################

for (collection in c("CORE", "UNVALIDATED")) {
# for (collection in c("CORE")) {
  
  message("; Plots for JASPAR ", collection, " collection")
  
  ##############
  ## Bar plot ##
  ##############
  nb.motifs.per.release.subset <- nb.motfs.per.release %>% 
                                    dplyr::filter(Collection == collection) %>% 
                                    drop_na(Year, Nb_motifs) %>% 
                                    group_by(Year) %>% 
                                    mutate(Sum_release = sum(Nb_motifs))
  
  max.y       <-  max(nb.motifs.per.release.subset$Sum_release)
  jaspar.bars <-  ggplot(nb.motifs.per.release.subset, aes(x = Year, y = Nb_motifs, fill = Taxon)) +
                    geom_bar(stat = "identity", position = "stack") +
                    scale_fill_manual(values = cols) +
                    theme_classic() +
                    theme(text               = element_text(size = 15),
                          axis.text.x        = element_text(angle = 45, hjust = 1, size = 20),
                          axis.text.y         = element_text(hjust = 1, size = 20),
                          panel.grid.major.y = element_line(color = "#969696",
                                                            size = 0.25,
                                                            linetype = 2)) +
                    labs(title = paste0("JASPAR ", collection, " data growth"), y = "# Profiles", x = "Year of release") +
                    theme(plot.title = element_text(hjust = 0.5)) +
                    scale_y_continuous(limits = c(0, max.y), expand = c(0.015,0), breaks = seq(0, max.y, by = 500)[-1])
  
  
  ggsave(plot     = jaspar.bars,
         filename = file.path(out.dir, paste0("Jaspar_", collection, "_", release.year,"_growth_barplot.pdf")),
         width    = 9.5,
         height   = 7.5)
  
  jaspar.bars.plotly <- ggplotly(jaspar.bars,
                          tooltip = c("y", "x", "fill"))
  
  
  htmlwidgets::saveWidget(jaspar.bars.plotly, file.path(out.dir, paste0("Jaspar_", collection, "_growth_barplot.html")))
  message("; Bar chart ready")
  
  
  #################################
  ## Create a link to the charts ##
  #################################
  # Set up API credentials: https://plot.ly/r/getting-started
  if (plotly.web.flag) {
    
    message("; Exporting barplot thorugh the plotly wesbite, user: ", plotly.username)
    
    Sys.setenv("plotly_username" = plotly.username)
    Sys.setenv("plotly_api_key"  = plotly.api.key)  ## https://plot.ly/settings/api
    
    api_create(jaspar.bars.plotly, filename = paste0("Jaspar_", collection, "_growth_barplot.html"))
    message("; Interactive barplot created online, available through plotly account")
  }
}


## End of script