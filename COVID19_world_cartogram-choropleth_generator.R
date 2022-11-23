## -----------------------------------------------------------------------------
## Purpose of script: to generate many choropleth + cartogram maps which will
## (when combined into a video) display the relationship between preventative
## COVID19 measures and COVID19 health outcomes, internationally.
##
## Author: Luke Hebert
##
## Date Created: 2022-11-21 (some work done prior, but same year)
##
## Copyright (c) Luke Hebert, 2022
## Email: luke.s.hebert@gmail.com
##
## -----------------------------------------------------------------------------
##
## Notes:
##
## USE CASE EXAMPLE:
##
## Rscript COVID19_world_cartogram-choropleth_generator.R owid-covid-data.csv \
## distort_column color_column
##
##
##
## Here's a link to a PDF of the flagship paper where cartogram maps were first 
## developed. https://www.pnas.org/doi/epdf/10.1073/pnas.0400280101
##
## An introduction to spatial data in R:
## http://rstudio-pubs-static.s3.amazonaws.com/
## 417601_236011cd7d4c40f1a980436f41873f6d.html#22_sf_package
##
##
## -----------------------------------------------------------------------------

#load necessary libraries
suppressWarnings(library(tidyverse)) #general data wrangling
suppressWarnings(library(sf)) #for cartogram plot-ability
suppressWarnings(library(spData)) #provides spatial data for the world
suppressWarnings(library(cartogram)) #for making the cartogram
suppressWarnings(library(ggplot2)) #to realize the plots

#get user's commands (from command line or terminal/shell)
args <- commandArgs(trailingOnly=TRUE)

#load the Our World in Data (OWID) COVID19 data
owid_path <- args[1]
owid <- as_tibble(read_csv(owid_path))

#get an ordered vector of all unique date values within owid's date column
owid <- owid[order(owid$date),]
dates <- unique(owid$date)

#in the "world" geographical dataset:
#get rid of Antarctica; has no countries & looks too large on plot if kept
noPenguins <- world[world$region_un != "Antarctica",]

#user defined distortion (cartogram) and color variabes (choropleth)
distort <- args[2]
colorvar <- args[3]

#these min & max chropleth values will come in handy when plotting later
#must remain outside the for loop
mx <- max(owid[[colorvar]], na.rm=TRUE)
mn <- min(owid[[colorvar]], na.rm=TRUE)

#loop through the dates
image_path <- gsub('owid-covid-data.csv', '', owid_path)
for (x in dates){
  #x is an integer since Jan 1st, 1970; x_read is YYYY-MM-DD
  x_read <- as.Date(x, origin="1970-01-01")
  
  #select only the OWID data from a single date
  sub_owid <- owid[owid$date==x,]

  #first rename a column to enable merging
  colnames(sub_owid)[3] <- "name_long"
  #keep only columns of interest going forward
  sub_owid <- select(sub_owid, c('name_long', distort, colorvar))
  #map this COVID data subset to the world sf object via country
  wrld_sars <- merge(noPenguins, sub_owid, by="name_long", all.x=TRUE)
  
  #replacing NA values with a placeholder integer
  #otherwise, creating a cartogram generates errors for some dates; 0 not good
  wrld_sars[is.na(wrld_sars)] <- 1
  
  print(x_read)
  if (sd(wrld_sars[[distort]])>0 || sd(wrld_sars[[colorvar]])>0){
    
    #project the sf geom data to an appropriate Coordinate Reference System
    world_proj = st_transform(wrld_sars, crs=3785)
    
    #create the cartogram (the distorted map)
    world_cartogram <- cartogram_cont(x=world_proj, 
                                      weight=distort,
                                      3)
    
    #plot the choropleth + cartogram
    ggplot() +
      geom_sf(data=world_cartogram, aes(fill=world_cartogram[[colorvar]])) +
      scale_fill_gradientn(limits = c(mn,mx),
                           colours=c("navyblue", "darkmagenta", "darkorange1"),
                           breaks=c(mn,mx), labels=format(c(mn,mx))) +
      annotate("text",  x=Inf, y = Inf, 
               label = x_read, 
               vjust=1, hjust=1,
               color='maroon',
               size=5) +
      annotate("text",  x=Inf, y = Inf, 
               label = paste("DISTORTED BY", distort, sep=' '), 
               vjust=3, hjust=1,
               color='gray',
               size=3) +
      xlab("") + 
      ylab("") +
      labs(fill=colorvar)
    
    #save the plot
    ggsave(paste(image_path, paste(distort, colorvar, x_read, sep='-'),'.png'), 
           dpi=800)
  }
  else{
    print('Both variables had no standard deviation. I.e. not worth plotting.')
    }
}