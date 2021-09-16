setwd("/Users/bomeara/Documents/MyDocuments/GitClones/EastTN")
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
	tar_invalidate(contains("_"))
	tar_invalidate(contains("sum"))
	tar_invalidate(contains("utk"))
}
tar_make()
rmarkdown::render_site()
Sys.sleep(10)
system("cp /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/data/*csv /Users/bomeara/Documents/MyDocuments/GitClones/EastTN/docs")
#system("open docs/index.html")
system("git commit -m'updated data' -a")
system('git push')
