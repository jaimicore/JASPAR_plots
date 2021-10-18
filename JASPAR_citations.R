#############################
## Load required libraries ##
#############################
required.packages = c("data.table",
                      "dplyr",
                      "ggplot2",
                      "lubridate",
                      "optparse",
                      "plotly",
                      "purrr",
                      "rcartocolor",
                      "scholar")


for (lib in required.packages) {
    if (!require(lib, character.only = TRUE)) {
        install.packages(lib)
        suppressPackageStartupMessages(library(lib, character.only = TRUE))
    }
}


## How to run:
##
## When user provides plotly username and API key (results are uploaded to plotly user's website)
## Rscript JASPAR_citations.R -o JASPAR_citations -p User_Name -k API_Key
##
## When user does not provides plotly username and API key (results are stored in the provided directory)
## Rscript JASPAR_citations.R -o JASPAR_citations


####################
## Read arguments ##
####################
option_list = list(
    
    make_option(c("-k", "--plotly_api_key"), type = "character", default = NULL,
                help = "Plotly API key, use this to upload the interactive barplot to your pltoly account. Requires to have a plotly account (https://chart-studio.plotly.com/feed/#/) with a valid API key. (Optional) ", metavar = "character"),
    
    make_option(c("-p", "--plotly_user_name"), type = "character", default = NULL,
                help = "Plotly username, use this to upload the interactive barplot to your pltoly account. Requires to have a plotly account (https://chart-studio.plotly.com/feed/#/). (Optional) ", metavar = "character"),
    
    make_option(c("-o", "--output_directory"), type = "character", default = 0,
                help = "Output directory where the resulting tables will be exported. (Mandatory) ", metavar = "character")
);
message("; Reading arguments from command-line")
opt_parser = OptionParser(option_list = option_list);
opt = parse_args(opt_parser);


## Set variable names
out.dir         <- opt$output_directory
plotly.api.key  <- opt$plotly_api_key
plotly.username <- opt$plotly_user_name


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


#############################
## JASPAR publications IDs ##
#############################

## Retrieve author X scholar ID information
## NOTE: we use Boris Lenhard google scholar profile ID because he is an author of the 9 jaspar releases
##       in the future, in case he is not an author this section of the code should be updated.
##       For example retrieving the profiles of two co-authors and merge them
BL.ID               <- "mIt9KSMAAAAJ" ## Boris Lenhard
message("; Retrieving information form Google Scholar Profile ID ", BL.ID)
BL.all.publications <- get_publications(BL.ID, pagesize = 500, sortby = "citation")

## Boris Lenhard's publications 
## With JASPAR in title and from NAR
JASPAR.publications <- BL.all.publications %>% 
                        dplyr::filter(journal == "Nucleic acids research" & grepl(title, pattern = "jaspar", ignore.case = T)) %>% 
                        mutate(author_ID = BL.ID) %>% 
                        arrange(year) %>% 
                        mutate(release = paste0("Release ", rowid(journal), " (", year, ")"))


## Retrieve the number of citations per year of each publication
JASPAR.publications.per.year <- map2(JASPAR.publications$author_ID, JASPAR.publications$pubid, get_article_cite_history) %>% 
                                    rbindlist()

JASPAR.publications.per.year <- merge(JASPAR.publications.per.year, JASPAR.publications, by = "pubid") %>% 
                                    select(year.x, cites.x, pubid, release) %>% 
                                    rename(year  = year.x,
                                           cites = cites.x) %>% 
                                    arrange(release)


##################
## Draw barplot ##
##################
nb.releases <- length(unique(JASPAR.publications.per.year$release))

nb.classes.palette <- 12  ## We want to use the first 12 colors of the Safe palette
nb.seed.colors     <- ifelse(nb.releases < nb.classes.palette,
                             yes = nb.releases,
                             no = nb.classes.palette)

## Generate a carto palette (remove gray value)
carto.pal.classes  <- carto_pal(nb.seed.colors, "Safe")
carto.pal.classes  <- carto.pal.classes[which(carto.pal.classes != "#888888")]


## Expand the color palette and add the gray color at the end
class.colors        <- colorRampPalette(carto.pal.classes, space = "Lab")(nb.seed.colors)

max.y <- JASPAR.publications.per.year %>% 
            group_by(year) %>% 
            summarise(Y_axis = max(sum(cites)))
max.y <- max(max.y$Y_axis, na.rm = T)

message("; Generating JASPAR citations barplot")
jaspar.cite.plot <- ggplot(JASPAR.publications.per.year, aes(x    = factor(year),
                                                             y    = cites,
                                                             fill = release)) +
                        geom_bar(stat     = "identity",
                                 position = position_stack(reverse = T)) +
                        theme_bw() +
                        scale_fill_manual(values = class.colors,
                                          name   = "Publication year") +
                        labs(x        = "",
                             y        = "Citations",
                             subtitle = paste("All citations: ", sum(JASPAR.publications.per.year$cites, na.rm = T)),  
                             title    = paste("Google Scholar JASPAR citations by ", lubridate::today())) +
                        theme(plot.title         = element_text(hjust = 0.5),
                              plot.subtitle      = element_text(hjust = 0.5),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.y = element_line(color    = "#969696",
                                                                size     = 0.25,
                                                                linetype = 2),
                              axis.text.y        = element_text(angle = 0,
                                                                hjust = 1,
                                                                size  = 15),
                              axis.text.x        = element_text(angle = 45,
                                                                hjust = 1,
                                                                size  = 15)) +
                        scale_y_continuous(limits = c(0, max.y),
                                           expand = c(0.01,1),
                                           breaks = seq(0, max.y, by = 100)[-1]) +
                        guides(fill = guide_legend(reverse = T))


## Convert ggplot to plotly
message("; Generating interactive JASPAR citations barplot using plotly")
jaspar.cite.plotly <- ggplotly(jaspar.cite.plot,
                               tooltip = c("fill", "y")) %>% 
                      config(displayModeBar = F)

   
###################################################
## Export barplot: interactive and static format ##
###################################################
jaspar.cite.plotly.html <- file.path(out.dir, "JASPAR_citations_barplot.html")
htmlwidgets::saveWidget(widget = jaspar.cite.plotly,
                        file   = jaspar.cite.plotly.html, selfcontained = F)
message("; Interactive barplot created: ", jaspar.cite.plotly.html)


jaspar.cite.plot.pdf <- file.path(out.dir, "JASPAR_citations_barplot.pdf")
ggsave(filename = jaspar.cite.plot.pdf,
       plot     = jaspar.cite.plot)
message("; Static barplot created: ", jaspar.cite.plot.pdf)


#################################
## Create a link to the charts ##
#################################
# Set up API credentials: https://plot.ly/r/getting-started
if (plotly.web.flag) {
    
    message("; Exporting barplot thorugh the plotly wesbite, user: ", plotly.username)
    
    Sys.setenv("plotly_username" = plotly.username)
    Sys.setenv("plotly_api_key"  = plotly.api.key)  ## https://plot.ly/settings/api
    
    api_create(jaspar.cite.plotly, filename = "JASPAR_citations_barplot.html")
    message("; Interactive barplot created online, available through plotly account")
}



## End of script