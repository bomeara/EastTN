library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/functions.R")
source("_packages.R")
options(timeout=600) # let things download for at least ten minutes
options(download.file.method = "libcurl")



# End this file with a list of target objects.
list(
  tar_target(daily_focal, CreateDailyFocal()),
  tar_target(schoolkids_daily, CreateSchoolkidsDaily()),
  tar_target(hospital_knox, CreateHospitalKnox()),
  tar_target(schools_oakridge, CreateSchoolsOakRidge()),
  tar_target(hhs_capacity_tn, CreateHHSDataTN()),
  tar_target(hhs_capacity_tn_focal_latest, CreateHHSDataFocalCities(hhs_capacity_tn)),
  tar_target(hhs_capacity_tn_focal_latest_pretty, CreateHHSDataFocalCitiesPretty(hhs_capacity_tn_focal_latest)),
  tar_render(yearbyyear, "yearbyyear.Rmd", params = list(daily_focal=daily_focal, schoolkids_daily=schoolkids_daily))

)
