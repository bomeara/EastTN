setwd("/Users/bomeara/Documents/MyDocuments/GitClones/EastTN")
system("/usr/local/bin/git pull")
Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
#Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
system("export PATH=$PATH:/usr/local/bin")

library(targets)
source("_packages.R")
source("R/functions.R")

try(system('mv ~/Downloads/7\\ LIVE_saliva_test_data_Page\\ 1_Table.csv .'))
try(system('mv ~/Downloads/8\\ LIVE_SHC_test_data_Page\\ 1_Table.csv .'))
try(system('mv ~/Downloads/2\\ new\\ cases_Page\\ 1_Time\\ series.csv .'))
try(system('mv ~/Downloads/LIVE\\ saliva\\ positivity\\ rate_Page\\ 1_Bar\\ chart.csv .'))
try(system('mv ~/Downloads/LIVE\\ spring\\ 2021\\ saliva\\ table_Page\\ 1_Table.csv .'))
try(system('mv ~/Downloads/1\\ active\\ cases_Page\\ 1_Line\\ chart.csv .'))
try(system('mv ~/Downloads/3\\ active\\ self_isolations_group_Page\\ 1_Bar\\ chart.csv .'))

#options(clustermq.scheduler = "multiprocess")
#tar_make_clustermq(workers = parallel::detectCores()-1)
rerun <- TRUE
if(rerun) {
	try(tar_invalidate(contains("sum")))
	#try(tar_invalidate(contains("utk")))
	try(tar_invalidate(contains("sewage")))
	try(tar_invalidate(contains("cdc_reports_new_2022")))
	#try(tar_invalidate(contains("cdc_all_reports")))
	try(tar_invalidate(contains("daily")))
	try(tar_invalidate(contains("hhs")))
	try(tar_invalidate(contains("tn")))
	#try(tar_invalidate(contains("schools")))
	try(tar_invalidate(contains("dash")))
	try(tar_invalidate(contains("flight")))
	try(tar_invalidate(contains("tsa")))
	try(tar_invalidate(contains("microcovid")))
	try(tar_invalidate(contains("hospital")))
	#try(tar_invalidate(contains("knox")))
	#try(tar_invalidate(contains("anderson")))
} 
tar_make()
Sys.setenv(RSTUDIO_PANDOC="/usr/local/bin/pandoc")
Sys.setenv(PANDOC="/usr/local/bin/pandoc")
print(rmarkdown::find_pandoc())
#to get around path issues
pandoc_available <- function(...) {
	return(TRUE)
}

#only render if need to update page organization
rmarkdown::render_site()

Sys.sleep(10)
system("cp /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/data/*csv /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/docs")

#system("open docs/index.html")


try(system("git add docs"))
try(system("git commit -m'updated data' -a"))
try(system('git push'))
