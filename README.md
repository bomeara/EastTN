# EastTN

This is the code gather data and render the easttn.info site about the local covid situation. It runs based on the R package `targets`. 

To see what it makes, look at https://github.com/bomeara/EastTN/blob/main/_targets.R. 

The functions used to parse the data are in https://github.com/bomeara/EastTN/blob/main/R/functions.R.

The *Rmd files have the code used to generate the images and the individual pages.

This is all run by sourcing https://github.com/bomeara/EastTN/blob/main/run.R; note it is hardcoded to the path on my computer where the site code resides; it also has code to copy UTK data from my downloads folder into this folder (I cannot find a way to get UT data other than by manually downloading it). 

The `scripts` directory has some of the quick R scripts used in cron jobs to download the data; I also use scrapingant for parsing some sites.