options(gargle_oauth_email = "omeara.brian@gmail.com")
write.csv(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1uxTwLbhWQgATiFbLU3fym1MGAI8q487kAjgxOpBZ2_4/edit#gid=0"), file=paste0("~/Dropbox/OakRidgeCovid/", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), "-oak_ridge_schools.csv"))
