library(targets)
source("_packages.R")
source("R/functions.R")
#options(clustermq.scheduler = "multiprocess")
#tar_make_clustermq(workers = parallel::detectCores()-1)
rerun <- FALSE
if(rerun) {
	tar_invalidate(contains("_"))
	tar_invalidate(contains("sum"))
	tar_invalidate(contains("utk"))
}
tar_make()
rmarkdown::render_site()
#system("open docs/index.html")
#system("git commit -m'updated data' -a")
#system('git push')