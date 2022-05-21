FocalCountiesHospitalKnoxArea <- function() {
    return(c("Knox", "Anderson", "Roane", "Scott", "Blount", "Claiborne", "Jefferson", "Campbell", "Sevier", "Loudon", "Hamblen", "Cocke", "Monroe", "McMinn"))
}

FocalCountiesEastTN <- function() {
    return(c("Anderson", "Bledsoe", "Blount", "Bradley", "Campbell", "Carter", "Claiborne", "Cocke", "Cumberland", "Grainger", "Greene", "Hamblen", "Hamilton", "Hancock", "Hawkins", "Jefferson", "Johnson", "Knox", "Loudon", "Marion", "McMinn", "Meigs", "Monroe", "Morgan", "Polk", "Rhea", "Roane", "Scott", "Sevier", "Sullivan", "Unicoi", "Union", "Washington"))
}

CreateDailyFocal <- function(counties_in_east_tn=FocalCountiesEastTN()) {
    #These come from aggregating info from:
    # us <- COVID19::covid19(country="US", level=3, verbose=FALSE)
    # tn <- subset(us, administrative_area_level_2=="Tennessee")
    # knox <- subset(us, administrative_area_level_3=="Knox" & administrative_area_level_2=="Tennessee")
    # oakridge <- subset(us, administrative_area_level_3 %in% c("Anderson") & administrative_area_level_2=="Tennessee")
    # region <-  subset(us, administrative_area_level_3 %in% counties_in_hospital_region & administrative_area_level_2=="Tennessee")
    knox_pop <- 470313
    oakridge_pop <- 76978
    region_pop <- 2455501
    hospital_region_pop <- 1235720

    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-County-New.XLSX"
    download.file(dataURL, destfile=temp, mode='wb')

    daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("text", "date", rep("numeric",18)))

    daily_knox <- subset(daily, COUNTY=="Knox") %>% select(-"COUNTY")
    daily_knox$Region <- "Knox County"
    daily_knox$Population <- knox_pop
    daily_knox$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_knox$NEW_CASES, k=7, align="right", fill=NA)/knox_pop
    daily_knox$PositivityRate_per_week <- zoo::rollsum(daily_knox$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_knox$NEW_TESTS, k=7, align="right", fill=NA)



    daily_oakridge <- subset(daily, COUNTY %in% c("Anderson")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)
    daily_oakridge$Region <- "Anderson"
    daily_oakridge$Population <- oakridge_pop
    daily_oakridge$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_oakridge$NEW_CASES, k=7, align="right", fill=NA)/oakridge_pop
    daily_oakridge$PositivityRate_per_week <- zoo::rollsum(daily_oakridge$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_oakridge$NEW_TESTS, k=7, align="right", fill=NA)

    #daily_oakridge$New_cases_per_100k <- 100000*(daily_oakridge$NEW_CASES/daily_oakridge$Population)



    daily_region<- subset(daily, COUNTY %in% counties_in_east_tn) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

    daily_region$Region <- "East TN"
    daily_region$Population <- region_pop
    daily_region$New_cases_per_100k_per_week <- 100000*zoo::rollsum(daily_region$NEW_CASES, k=7, align="right", fill=NA)/region_pop
    daily_region$PositivityRate_per_week <- zoo::rollsum(daily_region$NEW_POS_TESTS, k=7, align="right", fill=NA) / zoo::rollsum(daily_region$NEW_TESTS, k=7, align="right", fill=NA)


    daily_focal <- dplyr::bind_rows(daily_knox, daily_oakridge, daily_region)
    daily_focal$Tests_per_100k <- 100000*(daily_focal$NEW_TESTS/daily_focal$Population)
    daily_focal$New_cases_per_100k <- 100000*(daily_focal$NEW_CASES/daily_focal$Population)
    #daily_focal$Active_cases_per_100k <- 100000*(daily_focal$TOTAL_ACTIVE/daily_focal$Population)
    daily_focal$PositivityPercentage_per_week <- 100*daily_focal$PositivityRate_per_week

    return(daily_focal)
}

CreateSchoolkidsDaily <- function() {
    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-5-18-Years.XLSX"
    download.file(dataURL, destfile=temp, mode='wb')

    schoolkids <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",2)))

    schoolkids_region<- subset(schoolkids, COUNTY %in% FocalCountiesEastTN()) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

    schoolkids_oakridge<- subset(schoolkids, COUNTY %in% c("Anderson")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

    schoolkids_knox<- subset(schoolkids, COUNTY %in% c("Knox")) %>% group_by(DATE) %>% select(-"COUNTY") %>% summarise_all(sum)

    schoolkids_region$Region <- "East TN"
    schoolkids_knox$Region <- "Knox County"
    schoolkids_oakridge$Region <- "Anderson"
    schoolkids_daily <- rbind(schoolkids_knox, schoolkids_oakridge, schoolkids_region)
    return(schoolkids_daily)
}


CreateHospitalKnox <- function() {
    hospital_knox_files <- list.files(path="/Users/bomeara/Dropbox/KnoxCovid", pattern="*covid_bed_capacity.csv", full.names=TRUE)
    hospital_knox <- data.frame()
    for (i in seq_along(hospital_knox_files)) {
        local_beds <- NA
        try({
			local_beds <- suppressWarnings(read.csv(hospital_knox_files[i]))
        if(nrow(local_beds)>0) {
            local_beds$East.Region.Hospitals <- gsub('All Hospital Beds*', 'All Hospital Beds *', gsub('All Hospital Beds *', 'All Hospital Beds', local_beds$East.Region.Hospitals, fixed=TRUE), fixed=TRUE)
            local_beds$Total.Capacity <- as.numeric(gsub(",",'', local_beds$Total.Capacity))
            local_beds$Current.Census <- as.numeric(gsub(",",'', local_beds$Current.Census))
            local_beds$Current.Utilization <- as.numeric(gsub('%','', local_beds$Current.Utilization))
            local_beds$Available.Capacity <- as.numeric(gsub('%','', local_beds$Available.Capacity))
            local_beds$Date <- anytime::anytime(stringr::str_extract(hospital_knox_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
            colnames(local_beds) <- gsub("\\.$", "", colnames(local_beds))
            hospital_knox <- rbind(hospital_knox, local_beds)
        }
		}, silent=TRUE)
		#print(i)
		#print(nrow(hospital_knox))
    }
	hospital_knox<<- hospital_knox
    hospital_knox <- subset(hospital_knox, East.Region.Hospitals != "Adult Floor Beds/Non-ICU")
    hospital_knox <- hospital_knox[which(nchar(hospital_knox$East.Region.Hospitals)>0),]
    hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)] <- hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)]/100 #to fix two days of data where Knox County was multiplying these by 100, getting 7935% utilization
    hospital_knox <- hospital_knox[!grepl('\\*', hospital_knox$East.Region.Hospitals),]
    colnames(hospital_knox) <- gsub("East.Region.Hospitals", "Resource", colnames(hospital_knox))
    return(hospital_knox)
}

CreateSchoolsOakRidge <- function() {
    oakridge_school_files <- list.files(path="/Users/bomeara/Dropbox/OakRidgeCovid", pattern="*oak_ridge_schools.csv", full.names=TRUE)
    schools_oakridge <- data.frame()
    for (i in seq_along(oakridge_school_files)) {
        school_oakridge_info <- NULL
		try({
			try(school_oakridge_info <- read.csv(oakridge_school_files[i]), silent=TRUE)
			if(!is.null(school_oakridge_info)) {
				for (col_index in 3:9) {
					school_oakridge_info[,col_index] <- as.numeric(school_oakridge_info[,col_index])
				}
				school_oakridge_info[is.na(school_oakridge_info)] <- 0
				colnames(school_oakridge_info)[9] <- "student.population"
				colnames(school_oakridge_info)[2] <- "School"

				local_info <- data.frame(Date=rep(anytime::anytime(stringr::str_extract(oakridge_school_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+")), nrow(school_oakridge_info)), School=school_oakridge_info$School, PercentPositiveStudentsYearToDate=100*school_oakridge_info$YTD.Student.Cases/school_oakridge_info$student.population, PercentActiveCovidStudents=100*school_oakridge_info$Current.Student.Cases/school_oakridge_info$student.population, StudentPopulation=school_oakridge_info$student.population)
				if (i==1) {
					schools_oakridge <- local_info
				} else {
					previous_most_recent <- subset(schools_oakridge, Date==max(schools_oakridge$Date))
					if(any(c(previous_most_recent$PercentPositiveStudentsYearToDate!=local_info$PercentPositiveStudentsYearToDate, previous_most_recent$PercentActiveCovidStudents!=local_info$PercentActiveCovidStudents), na.rm=TRUE)){
						#(paste0(unique(local_info$Date), " DOESN'T match ", max(schools_oakridge$Date)))
						schools_oakridge <- rbind(schools_oakridge, local_info) # only add new data
					} else {
						#print(paste0(unique(local_info$Date), " matches ", max(schools_oakridge$Date)))
					}
				}
			}
		})
    }
    schools_oakridge <- schools_oakridge[is.finite(schools_oakridge$PercentPositiveStudentsYearToDate),]
	breakdays <- c(as.Date(paste0("2021-10-", c(4:15))), as.Date(paste0("2021-12-", c(20:31))), as.Date(paste0("2022-03-", c(14:25)))) 
	breakrows <- (as.Date(schools_oakridge$Date) %in% breakdays)

	schools_oakridge$PercentPositiveStudentsYearToDate[breakrows] <- NA
	schools_oakridge$PercentActiveCovidStudents[breakrows] <- NA
	
	
    return(schools_oakridge)
}

CreateSchoolsKnox <- function() {
    staff_total <-  8000 #"more than" this from knoxschools.org
    student_total <- 60000 #"more than" this from knoxschools.org
    school_knox_info <- read.csv("/Users/bomeara/Dropbox/KnoxSchoolsCovid/KCS COVID Status Dashboard 21-22_Page 1_Bar chart.csv")
    colnames(school_knox_info) <- c("Date", "Active Staff", "Active Student")
    knox_school_files <- list.files(path="/Users/bomeara/Dropbox/KnoxSchoolsCovid", pattern="*knox_schools.html", full.names=TRUE)
    for (i in seq_along(knox_school_files)) {

        try(input_file <- readChar(knox_school_files[i], file.info(knox_school_files[i])$size), silent=TRUE)
        try(input_file_html <- rvest::read_html(input_file), silent=TRUE)
        try(tbl <- as.data.frame(rvest::html_table(rvest::html_nodes(input_file_html, "table"))[[1]]), silent=TRUE)
        try(school_knox_info <- rbind(school_knox_info, tbl), silent=TRUE)
    }
    school_knox_info <- school_knox_info[!duplicated(school_knox_info$Date),]
    school_knox_info$Date <- as.Date(school_knox_info$Date,"%b %d, %Y")
    colnames(school_knox_info) <- c("Date", "Active_Staff_Count", "Active_Students_Count")
    school_knox_info$Active_Staff_Percent <- 100*school_knox_info$Active_Staff_Count/staff_total
    school_knox_info$Active_Students_Percent <- 100*school_knox_info$Active_Students_Count/student_total
	
	# breakdays <- c(as.Date(paste0("2021-10-", c(11:15))), as.Date(paste0("2021-11-", c(24:26))), as.Date(paste0("2021-12-", c(20:31))), as.Date(paste0("2022-03-", c(14:18)))) 
	# breakrows <- (as.Date(school_knox_info$Date) %in% breakdays)

	# school_knox_info$Active_Staff_Count[breakrows] <- NA
	# school_knox_info$Active_Students_Count[breakrows] <- NA
	# school_knox_info$Active_Staff_Percent[breakrows] <- NA
	# school_knox_info$Active_Students_Percent[breakrows] <- NA

	
    return(school_knox_info)
}

CreateIndividualSchoolsKnox <- function() {
	knox_school_files <- list.files(path="/Users/bomeara/Dropbox/CovidData/KnoxSchoolsCovidBySchool", pattern="*knox_schools_by_school.html", full.names=TRUE)
	individual_schools_knox <- data.frame()
    for (i in seq_along(knox_school_files)) {
		try({
			schools_day <- data.frame()
			day_info <- NULL
			input_file <- readChar(knox_school_files[i], file.info(knox_school_files[i])$size)
			input_file_html <- rvest::read_html(input_file)
			day_info <- input_file_html %>% html_elements("lego-table") %>% html_elements("div")
			row_info <- html_text2(day_info) %>% str_split(pattern="\n")
			row_info_schools <- row_info[sapply(row_info, length)==9]
			schools_day <- data.frame(do.call(rbind, row_info_schools))
			colnames(schools_day) <- c("School", "Student_Enrolled", "Student_Present", "Student_Present_Percent", "Student_Active_Cases", "Staff_Employed", "Staff_Present", "Staff_Present_Percent", "Staff_Active_Cases")
			schools_day$Student_Present_Percent <- as.numeric(gsub('%', "", schools_day$Student_Present_Percent))
			schools_day$Staff_Present_Percent <- as.numeric(gsub('%', "", schools_day$Staff_Present_Percent))
			actual_time <- anytime::anytime(stringr::str_extract(knox_school_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
			schools_day$Date <- actual_time
			if(nrow(schools_day)>0) {
				if(nrow(individual_schools_knox)==0) {
					individual_schools_knox <- plyr::rbind.fill(individual_schools_knox, schools_day)	
				} else {
					previous_most_recent <- subset(individual_schools_knox, Date==max(individual_schools_knox$Date))
					if(any(c(
						previous_most_recent$Student_Present!=schools_day$Student_Present,
						previous_most_recent$Student_Active_Cases!=schools_day$Student_Active_Cases,
						previous_most_recent$Staff_Present!=schools_day$Staff_Present,
						previous_most_recent$Staff_Active_Cases!=schools_day$Staff_Active_Cases
						), na.rm=TRUE)){
						individual_schools_knox <- plyr::rbind.fill(individual_schools_knox, schools_day)	
					} else {
						#print(paste0(unique(schools_day$Date), " matches ", max(individual_schools_knox$Date)))
					}
				}
			}
		}, silent=TRUE)
    }
	individual_schools_knox$Level <- "Other"
	individual_schools_knox$Level[grepl("Elementary", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Middle", individual_schools_knox$School)] <- "Middle"
	individual_schools_knox$Level[grepl("High", individual_schools_knox$School)] <- "High"
	individual_schools_knox$Level[grepl("Sarah Moore Greene", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Ridgedale Alternative", individual_schools_knox$School)] <- "Middle"
	individual_schools_knox$Level[grepl("Paul L. Kelley Volunteer Academy", individual_schools_knox$School)] <- "High"
	individual_schools_knox$Level[grepl("L & N Stem Academy", individual_schools_knox$School)] <- "High"
	individual_schools_knox$Level[grepl("Hardin Valley Academy", individual_schools_knox$School)] <- "High"
	individual_schools_knox$Level[grepl("Hardin Valley Elememntary School", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Green Magnet Academy School", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Farragut Primary School", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Farragut Intermediate School", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Chilhowee Intermediate School", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Level[grepl("Career Magnet Academy At Pellissippi State", individual_schools_knox$School)] <- "High"
	individual_schools_knox$Level[grepl("Beaumont Magnet Academy", individual_schools_knox$School)] <- "Elementary"
	individual_schools_knox$Student_Maximum_Active_Cases <- as.numeric(gsub("≤", "", individual_schools_knox$Student_Active_Cases))
	individual_schools_knox$Staff_Maximum_Active_Cases <- as.numeric(gsub("≤", "", individual_schools_knox$Staff_Active_Cases))
	
	
	# breakdays <- c(as.Date(paste0("2021-10-", c(11:15))), as.Date(paste0("2021-11-", c(24:26))), as.Date(paste0("2021-12-", c(20:31))), as.Date(paste0("2022-03-", c(14:18)))) 
	# breakrows <- (as.Date(individual_schools_knox$Date) %in% breakdays)

	# individual_schools_knox$Student_Enrolled[breakrows] <- NA
	# individual_schools_knox$Student_Present[breakrows] <- NA
	# individual_schools_knox$Student_Present_Percent[breakrows] <- NA
	# individual_schools_knox$Student_Active_Cases[breakrows] <- NA
	# individual_schools_knox$Staff_Employed[breakrows] <- NA
	# individual_schools_knox$Staff_Present[breakrows] <- NA
	# individual_schools_knox$Staff_Present_Percent[breakrows] <- NA
	# individual_schools_knox$Staff_Active_Cases[breakrows] <- NA
	
	
	return(individual_schools_knox)
}

CreateHHSDataTN <- function() {

    # hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
    # capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility", hhs_sources$dataset$title)


    # capacity_by_facility_url <- hhs_sources$dataset$distribution[capacity_by_facility_number][[1]]$downloadURL[1] #often a week behind though
    # temp = tempfile(fileext = ".csv")

    # utils::download.file(capacity_by_facility_url, temp, method="libcurl")

    # hhs_capacity <- read.csv(file=temp)
	
	temp = tempfile(fileext = ".csv")
 	dataURL <- "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD&api_foundry=true"
 	download.file(dataURL, destfile=temp, mode='wb')

 	hhs_capacity <- read.csv(temp, header=TRUE)
    hhs_capacity_tn <- subset(hhs_capacity, state=="TN")
	hhs_capacity_tn[hhs_capacity_tn==-999999] <- NA
	hhs_capacity_tn[hhs_capacity_tn=="-999999"] <- NA

    hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied <- 100 * hhs_capacity_tn$total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg

    hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds <- 100 * hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg / hhs_capacity_tn$all_adult_hospital_inpatient_beds_7_day_avg

    hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds <- 100 - hhs_capacity_tn$percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds

    hhs_capacity_tn$number_unoccupied_adult_hospital_inpatient_beds <- hhs_capacity_tn$all_adult_hospital_inpatient_beds_7_day_avg - hhs_capacity_tn$all_adult_hospital_inpatient_bed_occupied_7_day_avg



    hhs_capacity_tn$percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied <- 100 * hhs_capacity_tn$staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg / hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg

    hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds <- 100 * hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg / hhs_capacity_tn$total_staffed_adult_icu_beds_7_day_avg

    hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds <- 100 - hhs_capacity_tn$percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds

    hhs_capacity_tn$number_unoccupied_adult_hospital_ICU_beds <- hhs_capacity_tn$total_staffed_adult_icu_beds_7_day_avg - hhs_capacity_tn$staffed_adult_icu_bed_occupancy_7_day_avg
    return(hhs_capacity_tn)
}


CreateHHSDataFocalCities <- function(hhs_capacity_tn) {

    focal_cities <- toupper(c("Oak Ridge", "Knoxville", "Lenoir City", "Maryville", "Sweetwater", "Harriman", "Powell", "Jefferson City", "Athens", "Morristown", "Sevierville", "Tazewell", "La Follette", "Jellico", "Sneedville", "Oneida"))
    hhs_capacity_tn_focal <- hhs_capacity_tn[hhs_capacity_tn$city%in%focal_cities,]

    hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, 
        !is.na(all_adult_hospital_inpatient_bed_occupied_7_day_avg) & 
        !is.na(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg) & 
        !is.na(all_adult_hospital_inpatient_beds_7_day_avg) &
        !is.na(total_staffed_adult_icu_beds_7_day_avg) &
        !is.na(staffed_adult_icu_bed_occupancy_7_day_avg) &
        !is.na(staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg) & 
        !is.na(percentage_adult_hospital_inpatient_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_occupied) &
        !is.na(percentage_adult_hospital_inpatient_bed_occupied_of_all_inpatient_beds) &
        !is.na(percentage_adult_hospital_ICU_bed_occupied_covid_confirmed_or_suspected_7_day_avg_of_all_ICU_occupied) &
        !is.na(percentage_adult_hospital_inpatient_ICU_bed_occupied_of_all_inpatient_ICU_beds)
    )


    hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, 
        (all_adult_hospital_inpatient_bed_occupied_7_day_avg >= 0) & 
        (total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg >= 0) & 
        (all_adult_hospital_inpatient_beds_7_day_avg >= 0) &
        (total_staffed_adult_icu_beds_7_day_avg >= 0) &
        (staffed_adult_icu_bed_occupancy_7_day_avg >= 0) &
        (staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_avg >= 0)
    )

    #hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, all_adult_hospital_inpatient_beds_7_day_avg>=100)

    few_update_hospitals <- names(which(table(hhs_capacity_tn_focal$hospital_name)<=2))

    if(length(few_update_hospitals)>0) {
        hhs_capacity_tn_focal <- subset(hhs_capacity_tn_focal, !(hospital_name %in% few_update_hospitals)) #deleting the tiny hospitals that don't update
    }

    hhs_capacity_tn_focal$hospital_name <- gsub("TENNOVA HEALTHCARE", "TENNOVA", hhs_capacity_tn_focal$hospital_name )
    hhs_capacity_tn_focal$hospital_name <- stringr::str_to_title(hhs_capacity_tn_focal$hospital_name)
    hhs_capacity_tn_focal$hospital_name[grepl("University Of Tn", hhs_capacity_tn_focal$hospital_name, ignore.case=TRUE)] <- "University of TN Medical Center"




    hhs_capacity_tn_focal$DATE <- as.Date(hhs_capacity_tn_focal$collection_week)

    # hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal

    # hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal %>% group_by(city, DATE) %>% summarize_at(vars(number_unoccupied_adult_hospital_inpatient_beds, number_unoccupied_adult_hospital_ICU_beds), list(sum = ~sum(., na.rm=TRUE)))

    # combinations <- expand.grid(DATE = unique(hhs_capacity_tn_focal_cities$DATE), city = unique(hhs_capacity_tn_focal_cities$city))

    # hhs_capacity_tn_focal_cities <- full_join(hhs_capacity_tn_focal_cities, combinations, by = c("DATE" = "DATE", "city" = "city")) %>% mutate(number_unoccupied_adult_hospital_inpatient_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_inpatient_beds_sum), 0, number_unoccupied_adult_hospital_inpatient_beds_sum)) %>% mutate(number_unoccupied_adult_hospital_ICU_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_ICU_beds_sum), 0, number_unoccupied_adult_hospital_ICU_beds_sum))

    # hhs_capacity_tn_focal_cities$city <- stringr::str_to_title(hhs_capacity_tn_focal_cities$city)

    return(hhs_capacity_tn_focal)
}

CreateHHSDataFocalCitiesPretty <- function(hhs_capacity_tn_focal) {
	
    hhs_capacity_tn_focal_latest <- subset(hhs_capacity_tn_focal, DATE==max(DATE))
    hhs_capacity_tn_focal_latest <- hhs_capacity_tn_focal_latest[order(hhs_capacity_tn_focal_latest$all_adult_hospital_inpatient_beds_7_day_avg, decreasing=TRUE),]
    hhs_capacity_tn_focal_latest_pretty <- hhs_capacity_tn_focal_latest[,c(
        "hospital_name", 
        "city", 
        "all_adult_hospital_inpatient_beds_7_day_avg", 
        "number_unoccupied_adult_hospital_inpatient_beds", 
        "percentage_adult_hospital_inpatient_bed_unoccupied_of_all_inpatient_beds", 
        "total_staffed_adult_icu_beds_7_day_avg", 
        "number_unoccupied_adult_hospital_ICU_beds", 
        "percentage_adult_hospital_inpatient_ICU_bed_unoccupied_of_all_inpatient_ICU_beds"
    )]
    colnames(hhs_capacity_tn_focal_latest_pretty) <- c(
        "Hospital", 
        "City", 
        "Adult beds total", 
        "Adult beds number avail", 
        "Adult beds % avail", 
        "Adult ICU total", 
        "Adult ICU number avail", 
        "Adult ICU % avail"
    )

    for (i in 3:ncol(hhs_capacity_tn_focal_latest_pretty)) {
        hhs_capacity_tn_focal_latest_pretty[,i]<- round(hhs_capacity_tn_focal_latest_pretty[,i])
    }

    hhs_capacity_tn_focal_latest_pretty$City <- stringr::str_to_title(hhs_capacity_tn_focal_latest_pretty$City)
    rownames(hhs_capacity_tn_focal_latest_pretty) <- NULL

    return(hhs_capacity_tn_focal_latest_pretty)
}

CreateDemographicDataInTN <- function() {
    # these hardcoded values come from the census; they are copied in another function, as well
    population_total <- 6829174

    population_female <- population_total*0.512
    population_male <- population_total*(1-0.512)

    population_white <- population_total*0.784
    population_black_africanamerican <- population_total*0.171
    population_asian <- population_total*0.02
    population_americanindian_alaskannative <- population_total*0.005
    population_nativehawaiian_other_pacificislander <- population_total*0.001
    population_twoormoreraces <- population_total*0.02

    population_hispanic <- population_total*0.057
    population_not_hispanic <- population_total*(1-0.057)


    population_age_under_5 <- population_total*0.06
    population_age_under_18 <- population_total*0.221
    population_age_65_and_over <- population_total*0.167



    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/COVID_VACCINE_DEMOGRAPHICS.XLSX"
    download.file(dataURL, destfile=temp, mode='wb')
    demographics_vaccine <- readxl::read_xlsx(temp, sheet =1)
    demographics_vaccine$RECIPIENT_COUNT[is.na(demographics_vaccine$RECIPIENT_COUNT)] <- demographics_vaccine$VACCINE_COUNT[is.na(demographics_vaccine$RECIPIENT_COUNT)]
    demographics_vaccine$RECIP_FULLY_VACC[is.na(demographics_vaccine$RECIP_FULLY_VACC)] <- 0





    race_vaccine <- subset(demographics_vaccine, CATEGORY=="RACE")
    race_vaccine$Race <- stringr::str_to_title(race_vaccine$CAT_DETAIL)
    race_vaccine$PercentFullyVaccinated <- NA
    race_vaccine$PercentFullyVaccinated <- as.numeric(race_vaccine$PercentFullyVaccinated)

    race_vaccine[which(race_vaccine$Race=="White"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="White"),]$RECIP_FULLY_VACC/population_white
    race_vaccine[which(race_vaccine$Race=="Black Or African American"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="Black Or African American"),]$RECIP_FULLY_VACC/population_black_africanamerican
    race_vaccine[which(race_vaccine$Race=="Asian"),]$PercentFullyVaccinated <- 100*race_vaccine[which(race_vaccine$Race=="Asian"),]$RECIP_FULLY_VACC/population_asian

    race_vaccine <- race_vaccine %>% group_by(Race) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
    race_vaccine$IncreasePercentFullyVaccinated <- race_vaccine$PercentFullyVaccinated - race_vaccine$PreviousPercentFullyVaccinated
    race_vaccine$IncreasePercentFullyVaccinated[race_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

    race_vaccine <- race_vaccine %>% group_by(Race) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))

    race_vaccine <- subset(race_vaccine, !is.na(PercentFullyVaccinated))

    ethnicity_vaccine <- subset(demographics_vaccine, CATEGORY=="ETHN")
    ethnicity_vaccine$Ethnicity <- stringr::str_to_title(ethnicity_vaccine$CAT_DETAIL)

    ethnicity_vaccine$PercentFullyVaccinated <- NA
    ethnicity_vaccine$PercentFullyVaccinated <- as.numeric(ethnicity_vaccine$PercentFullyVaccinated)

    ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Hispanic Or Latino"),]$PercentFullyVaccinated <- 100*ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Hispanic Or Latino"),]$RECIP_FULLY_VACC/population_hispanic
    ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Not Hispanic Or Latino"),]$PercentFullyVaccinated <- 100*ethnicity_vaccine[which(ethnicity_vaccine$Ethnicity=="Not Hispanic Or Latino"),]$RECIP_FULLY_VACC/population_not_hispanic


    ethnicity_vaccine <- ethnicity_vaccine %>% group_by(Ethnicity) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
    ethnicity_vaccine$IncreasePercentFullyVaccinated <- ethnicity_vaccine$PercentFullyVaccinated - ethnicity_vaccine$PreviousPercentFullyVaccinated
    ethnicity_vaccine$IncreasePercentFullyVaccinated[ethnicity_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

    ethnicity_vaccine <- ethnicity_vaccine %>% group_by(Ethnicity) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))

    ethnicity_vaccine <- subset(ethnicity_vaccine, !is.na(PercentFullyVaccinated))


    sex_vaccine <- subset(demographics_vaccine, CATEGORY=="SEX")
    sex_vaccine$Sex <- stringr::str_to_title(sex_vaccine$CAT_DETAIL)
    sex_vaccine$Sex <- gsub("F", "Female", sex_vaccine$Sex)
    sex_vaccine$Sex <- gsub("M", "Male", sex_vaccine$Sex)
    sex_vaccine$Sex <- gsub("U", "Unknown", sex_vaccine$Sex)
    sex_vaccine$Sex <- gsub("O", "Other", sex_vaccine$Sex)
    sex_vaccine$PercentFullyVaccinated <- NA
    sex_vaccine$PercentFullyVaccinated <- as.numeric(sex_vaccine$PercentFullyVaccinated)
    sex_vaccine[which(sex_vaccine$Sex=="Female"),]$PercentFullyVaccinated <- 100*sex_vaccine[which(sex_vaccine$Sex=="Female"),]$RECIP_FULLY_VACC/population_female
    sex_vaccine[which(sex_vaccine$Sex=="Male"),]$PercentFullyVaccinated <- 100*sex_vaccine[which(sex_vaccine$Sex=="Male"),]$RECIP_FULLY_VACC/population_male



    sex_vaccine <- sex_vaccine %>% group_by(Sex) %>% mutate(PreviousPercentFullyVaccinated=dplyr::lag(PercentFullyVaccinated, n=1, default=0))
    sex_vaccine$IncreasePercentFullyVaccinated <- sex_vaccine$PercentFullyVaccinated - sex_vaccine$PreviousPercentFullyVaccinated
    sex_vaccine$IncreasePercentFullyVaccinated[sex_vaccine$IncreasePercentFullyVaccinated<0] <- 0 # for cases with sudden drops, probably due to issues in the original data

    sex_vaccine <- sex_vaccine %>% group_by(Sex) %>% mutate(IncreasePercentFullyVaccinated7Day=zoo::rollmean(IncreasePercentFullyVaccinated, k=7, align="right", fill=0))


    sex_vaccine <- subset(sex_vaccine, !is.na(PercentFullyVaccinated))







    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-RaceEthSex.XLSX"
    download.file(dataURL, destfile=temp, mode='wb')
    demographics <- readxl::read_xlsx(temp, sheet =1)
    demographics$percent_of_demographic_with_cases <- 0
    demographics$percent_of_demographic_dead <- 0
    demographics$Date <- demographics$DATE
    demographics <- subset(demographics, as.character(demographics$DATE)!="2020-07-28") #there was an error that day for Native Hawaiian or Other Pacific Islander: it went from 2 to 15 recorded deaths. Remove date from all data just to be safe.



    race <- subset(demographics, CATEGORY=="RACE")
    race <- subset(race, CAT_DETAIL != "Pending")


    race[which(race$CAT_DETAIL=="White"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="White"),]$CAT_TOTALCASES/population_white
    race[which(race$CAT_DETAIL=="White"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="White"),]$CAT_TOTALDEATHS/population_white
    race[which(race$CAT_DETAIL=="Black or African American"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Black or African American"),]$CAT_TOTALCASES/population_black_africanamerican
    race[which(race$CAT_DETAIL=="Black or African American"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Black or African American"),]$CAT_TOTALDEATHS/population_black_africanamerican
    race[which(race$CAT_DETAIL=="Asian"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Asian"),]$CAT_TOTALCASES/population_asian
    race[which(race$CAT_DETAIL=="Asian"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Asian"),]$CAT_TOTALDEATHS/population_asian
    race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$CAT_TOTALCASES/population_americanindian_alaskannative
    race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="American Indian or Alaska Native"),]$CAT_TOTALDEATHS/population_americanindian_alaskannative
    race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$CAT_TOTALCASES/population_nativehawaiian_other_pacificislander
    race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Native Hawaiian or Other Pacific Islander"),]$CAT_TOTALDEATHS/population_nativehawaiian_other_pacificislander
    race[which(race$CAT_DETAIL=="Other/Multiracial"),]$percent_of_demographic_with_cases <- 100*race[which(race$CAT_DETAIL=="Other/Multiracial"),]$CAT_TOTALCASES/population_twoormoreraces
    race[which(race$CAT_DETAIL=="Other/Multiracial"),]$percent_of_demographic_dead <- 100*race[which(race$CAT_DETAIL=="Other/Multiracial"),]$CAT_TOTALDEATHS/population_twoormoreraces
    race <- subset(race, CAT_DETAIL!="Other/Multiracial") # see note above
    race <- subset(race, CAT_DETAIL!="Other/ Multiracial") # see note above

    race$Race <- race$CAT_DETAIL


    ethnicity <- subset(demographics, CATEGORY=="ETHNICITY")
    ethnicity <- subset(ethnicity, CAT_DETAIL != "Pending")
    ethnicity$Ethnicity <- ethnicity$CAT_DETAIL


    ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$percent_of_demographic_with_cases <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$CAT_TOTALCASES/population_hispanic
    ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$percent_of_demographic_dead <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Hispanic"),]$CAT_TOTALDEATHS/population_hispanic


    ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$percent_of_demographic_with_cases <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$CAT_TOTALCASES/population_not_hispanic
    ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$percent_of_demographic_dead <- 100*ethnicity[which(ethnicity$CAT_DETAIL=="Not Hispanic or Latino"),]$CAT_TOTALDEATHS/population_not_hispanic



    sex <- subset(demographics, CATEGORY=="SEX")
    sex <- subset(sex, CAT_DETAIL != "Pending")


    sex[which(sex$CAT_DETAIL=="Female"),]$percent_of_demographic_with_cases <- 100*sex[which(sex$CAT_DETAIL=="Female"),]$CAT_TOTALCASES/population_female
    sex[which(sex$CAT_DETAIL=="Female"),]$percent_of_demographic_dead <- 100*sex[which(sex$CAT_DETAIL=="Female"),]$CAT_TOTALDEATHS/population_female

    sex[which(sex$CAT_DETAIL=="Male"),]$percent_of_demographic_with_cases <- 100*sex[which(sex$CAT_DETAIL=="Male"),]$CAT_TOTALCASES/population_male
    sex[which(sex$CAT_DETAIL=="Male"),]$percent_of_demographic_dead <- 100*sex[which(sex$CAT_DETAIL=="Male"),]$CAT_TOTALDEATHS/population_male
    sex$Sex <- sex$CAT_DETAIL

    return(list(race_vaccine=race_vaccine, ethnicity_vaccine=ethnicity_vaccine, sex_vaccine=sex_vaccine, race=race, ethnicity=ethnicity, sex=sex))
}

CreateSummaryTable <- function(covid_by_demographic_in_tn) {
       # these hardcoded values come from the census; they are copied in another function, as well
    population_total <- 6829174

    population_female <- population_total*0.512
    population_male <- population_total*(1-0.512)

    population_white <- population_total*0.784
    population_black_africanamerican <- population_total*0.171
    population_asian <- population_total*0.02
    population_americanindian_alaskannative <- population_total*0.005
    population_nativehawaiian_other_pacificislander <- population_total*0.001
    population_twoormoreraces <- population_total*0.02

    population_hispanic <- population_total*0.057
    population_not_hispanic <- population_total*(1-0.057)


    population_age_under_5 <- population_total*0.06
    population_age_under_18 <- population_total*0.221
    population_age_65_and_over <- population_total*0.167



    race <- covid_by_demographic_in_tn$race
    ethnicity <- covid_by_demographic_in_tn$ethnicity
    sex <- covid_by_demographic_in_tn$sex
    race_vaccine <- covid_by_demographic_in_tn$race_vaccine
    ethnicity_vaccine <- covid_by_demographic_in_tn$ethnicity_vaccine
    sex_vaccine <- covid_by_demographic_in_tn$sex_vaccine
    # I'm doing this poorly using hard coding, but I don't know of a better way
    racesum <- data.frame(Category="Race", Group=c("American Indian or Alaska Native", "Asian", "Black or African American", "Native Hawaiian or Other Pacific Islander", "White"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
    race_recent <- subset(race, Date==max(race$Date))
    for (i in seq_along(racesum$Group)) {
        racesum$Positive_Covid_Test[i] <- race_recent$percent_of_demographic_with_cases[which(race_recent$CAT_DETAIL==racesum$Group[i])]
        racesum$Covid_Death[i] <- race_recent$percent_of_demographic_dead[which(race_recent$CAT_DETAIL==racesum$Group[i])]

    }
    race_vaccine_recent <- subset(race_vaccine, DATE==max(race_vaccine$DATE))

    racesum[which(racesum$Group=="Asian"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Asian"),]$RECIPIENT_COUNT / population_asian

    racesum[which(racesum$Group=="Asian"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Asian"),]$RECIP_FULLY_VACC / population_asian


    racesum[which(racesum$Group=="Black or African American"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Black Or African American"),]$RECIPIENT_COUNT / population_black_africanamerican

    racesum[which(racesum$Group=="Black or African American"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="Black Or African American"),]$RECIP_FULLY_VACC / population_black_africanamerican


    racesum[which(racesum$Group=="White"),]$At_Least_One_Vaccination <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="White"),]$RECIPIENT_COUNT / population_white

    racesum[which(racesum$Group=="White"),]$Fully_Vaccinated <- 100*race_vaccine_recent[which(race_vaccine_recent$Race=="White"),]$RECIP_FULLY_VACC / population_white

    sumtab <- racesum


    ethnicitysum <- data.frame(Category="Ethnicity", Group=c("Hispanic", "Not Hispanic or Latino"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
    ethnicity_recent <- subset(ethnicity, Date==max(ethnicity$Date))
    for (i in seq_along(ethnicitysum$Group)) {
        ethnicitysum$Positive_Covid_Test[i] <- ethnicity_recent$percent_of_demographic_with_cases[which(ethnicity_recent$CAT_DETAIL==ethnicitysum$Group[i])]
        ethnicitysum$Covid_Death[i] <- ethnicity_recent$percent_of_demographic_dead[which(ethnicity_recent$CAT_DETAIL==ethnicitysum$Group[i])]

    }
    ethnicity_vaccine_recent <- subset(ethnicity_vaccine, DATE==max(ethnicity_vaccine$DATE))

    ethnicitysum[which(ethnicitysum$Group=="Hispanic"),]$At_Least_One_Vaccination <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Hispanic Or Latino"),]$RECIPIENT_COUNT / population_hispanic

    ethnicitysum[which(ethnicitysum$Group=="Hispanic"),]$Fully_Vaccinated <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Hispanic Or Latino"),]$RECIP_FULLY_VACC / population_hispanic


    ethnicitysum[which(ethnicitysum$Group=="Not Hispanic or Latino"),]$At_Least_One_Vaccination <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Not Hispanic Or Latino"),]$RECIPIENT_COUNT / population_not_hispanic

    ethnicitysum[which(ethnicitysum$Group=="Not Hispanic or Latino"),]$Fully_Vaccinated <- 100*ethnicity_vaccine_recent[which(ethnicity_vaccine_recent$Ethnicity=="Not Hispanic Or Latino"),]$RECIP_FULLY_VACC / population_not_hispanic

    sumtab <- rbind(sumtab, ethnicitysum)

    sexsum <- data.frame(Category="Sex", Group=c("Female", "Male"), Positive_Covid_Test=NA, Covid_Death=NA, At_Least_One_Vaccination=NA, Fully_Vaccinated=NA)
    sex_recent <- subset(sex, Date==max(sex$Date))
    for (i in seq_along(sexsum$Group)) {
        sexsum$Positive_Covid_Test[i] <- sex_recent$percent_of_demographic_with_cases[which(sex_recent$CAT_DETAIL==sexsum$Group[i])]
        sexsum$Covid_Death[i] <- sex_recent$percent_of_demographic_dead[which(sex_recent$CAT_DETAIL==sexsum$Group[i])]

    }
    sex_vaccine_recent <- subset(sex_vaccine, DATE==max(sex_vaccine$DATE))

    sexsum[which(sexsum$Group=="Female"),]$At_Least_One_Vaccination <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Female"),]$RECIPIENT_COUNT / population_female

    sexsum[which(sexsum$Group=="Female"),]$Fully_Vaccinated <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Female"),]$RECIP_FULLY_VACC / population_female

    sexsum[which(sexsum$Group=="Male"),]$At_Least_One_Vaccination <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Male"),]$RECIPIENT_COUNT / population_male

    sexsum[which(sexsum$Group=="Male"),]$Fully_Vaccinated <- 100*sex_vaccine_recent[which(sex_vaccine_recent$Sex=="Male"),]$RECIP_FULLY_VACC / population_male

    sumtab <- rbind(sumtab, sexsum)

    sumtab[,c(3,5,6)] <- round(sumtab[,c(3,5,6)], 1)
    sumtab[,4] <- round(sumtab[,4], 2)

    for(i in seq(from=3, to=6, by=1)) {
    sumtab[,i] <- paste0(sumtab[,i], "%")
    }
    colnames(sumtab) <-gsub("_", " ",colnames(sumtab) )
    for (i in sequence(nrow(sumtab))) {
    for (j in sequence(ncol(sumtab))) {
        if(sumtab[i,j]=="NA%") {
        sumtab[i,j]<- ""
        }
    }
    }
    sumtab[which(sumtab=="NA%")] <- " "
    return(sumtab)
}

ComputeSummaryTableFraction <- function(sumtab){
    sumtab$'Positive Covid Test' <- paste0("1/",round(100/as.numeric(gsub("%", "", sumtab$'Positive Covid Test')),0))
    sumtab$'Covid Death' <- paste0("1/",round(100/as.numeric(gsub("%", "", sumtab$'Covid Death')),0))
    #sumtab$'At Least One Vaccination' <- paste0("10/",round(1000/as.numeric(gsub("%", "", sumtab$'At Least One Vaccination')),0))
   # sumtab$'Fully Vaccinated' <- paste0("10/",round(1000/as.numeric(gsub("%", "", sumtab$'Fully Vaccinated')),0))
    for (i in sequence(nrow(sumtab))) {
        for (j in sequence(ncol(sumtab))) {
            if(grepl("/NA", sumtab[i,j])) {
                sumtab[i,j] <- ""
            }
        }
    }
    return(sumtab)
}

CreateSalivaData <- function() {

    saliva_data <- read.csv(file="7 LIVE_saliva_test_data_Page 1_Table.csv", stringsAsFactors=FALSE)
    saliva_data2 <- read.csv(file="LIVE spring 2021 saliva table_Page 1_Table.csv", stringsAsFactors=FALSE)
    saliva_data2 <- subset(saliva_data2, Category=="Residents")
    saliva_data2$Locations <- "Residents"
    saliva_data2$Week <- saliva_data2$Week.ending
    saliva_data2$Positive.diagnostic.tests. <- saliva_data2$Positive.diagnostic.tests
    saliva_data2$Positive.pools <- saliva_data2$Number.of.positive.pools

    saliva_data2$Participation.rate <- as.numeric(gsub('%', '', saliva_data2$Participation.rate))/100
    saliva_data <- plyr::rbind.fill(saliva_data, saliva_data2)

    saliva_data$Active_cases_per_100k = 100000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
    saliva_data$Active_cases_per_30k = 30000*saliva_data$Positive.diagnostic.tests./saliva_data$Samples
    saliva_data$New_cases_per_100k = saliva_data$Active_cases_per_100k / 14
    saliva_data$DATE <- as.Date(saliva_data$Week,"%b %d, %Y")

    saliva_data$New_cases_per_100k_lower <- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$lower/14
    saliva_data$New_cases_per_100k_upper<- 100000*binom::binom.confint(saliva_data$Positive.diagnostic.tests., saliva_data$Samples, method="exact")$upper/14
    return(saliva_data)
}

ComputeUTKCasesOld <- function() {

    webfiles <- list.files(path="/Users/bomeara/Dropbox/UTKCovid", pattern="*html", full.names =TRUE)
    utk.cases <- data.frame()
    for(i in seq_along(webfiles)) {
    raw <- paste0(readLines(webfiles[i]),collapse=" ")
    raw <- gsub('\\x3c', '', raw, fixed=TRUE)
    raw <- gsub('\\x3d', '', raw, fixed=TRUE)
    raw <- gsub('\\x3e', '', raw, fixed=TRUE)
    raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
    raw <- gsub('/span/tdtd style\"width: \\d+\\.*\\d*%; text-align: left;\"span style\"font-size: 18px;\"', '', raw, fixed=FALSE)
    students <- as.numeric(gsub("Students", "", stringr::str_extract(raw, "Students\\d+")))
        faculty <- as.numeric(gsub("Faculty", "", stringr::str_extract(raw, "Faculty\\d+")))
        staff <- as.numeric(gsub("Staff", "", stringr::str_extract(raw, "Staff\\d+")))
    actual_time <- anytime::anytime(stringr::str_extract(webfiles[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
    result <- data.frame(date=rep(actual_time, 3), count=c(students, faculty, staff), group=c("students", "faculty", "staff"))
    if(i==1) {
        utk.cases <- result
    } else {
        utk.cases <- rbind(utk.cases, result)
    }
    }
    utk.cases$group <- as.factor(utk.cases$group)
    return(utk.cases)
}

ComputeUTKActiveCasesReported <- function() {

    utk_active_cases_reported <- read.csv("1 active cases_Page 1_Line chart.csv")
    utk_active_cases_reported$DATE <- as.Date(utk_active_cases_reported[,1], "%B %d, %Y")
    utk_active_cases_reported$ProportionFacultyStaff <- utk_active_cases_reported$Employees/6600 # Using size of pool from Chancellor update of Aug 19, 2021
    utk_active_cases_reported$ProportionStudents <- utk_active_cases_reported$Students/30000 # Using size of pool from Chancellor update of Aug 19, 2021
    utk_active_cases_reported$StudentProportionOverEmployeeProportion <- utk_active_cases_reported$ProportionStudents/utk_active_cases_reported$ProportionFacultyStaff
    return(utk_active_cases_reported)
}

ComputeUTKIsolationsReported <- function() {
    utk_isolations_reported_raw <- read.csv("3 active self_isolations_group_Page 1_Bar chart.csv")
    utk_isolations_reported_raw$DATE <- as.Date(utk_isolations_reported_raw[,1], "%B %d, %Y")
    utk_isolations_reported <- rbind(
        data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Employees/6600, Population="Employees"),
        data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Students..residential./8000, Population="Students (residential)"),
        data.frame(DATE=utk_isolations_reported_raw$DATE, Percentage=100*utk_isolations_reported_raw$Students..non.residential./22000, Population="Students (nonresidential)")
    )
    return(utk_isolations_reported)
}

GetZukowskiData <- function() {
	zukowski_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRRoX-icFl5a6T5OVpSllMJ3QGVplgdDEKPHtuvEjcDKNEvw5X6dcgGYSMGmynFcdxUwH2u4kZjBTiT/pub?gid=1053792052&single=true&output=csv")
	zukowski_data$DATE <- as.Date(zukowski_data$DATE, "%Y-%m-%d")
	return(zukowski_data)
}

Get17To25Knox <- function() {

    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.tn.gov/content/dam/tn/health/documents/cedep/novel-coronavirus/datasets/Public-Dataset-Daily-County-Cases-17-25-Years.XLSX"
    download.file(dataURL, destfile=temp, mode='wb')

    daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",7)))
    daily_knox <- subset(daily, COUNTY=="Knox")
    daily_knox$DATE <- as.Date(daily_knox$DATE)
    return(daily_knox)
}

GetTNDeathPredictions <- function() {
  fdat <- data.frame()
  offset <- 0
  while(nrow(fdat)==0) {
	fdat <- covidHubUtils::load_forecasts(
			models = c("COVIDhub-ensemble"),
			dates = as.character(Sys.Date()-offset),
			source = "zoltar",
			date_window_size = 6,
			locations = c("47"),
			types = c("quantile", "point"),
			targets = paste(1:4, "wk ahead inc death"),
			verbose = FALSE
		)
		offset <- offset + 1
		print(offset)
		print(fdat)
  }
  fdat <- subset(fdat, target_end_date>Sys.Date())
	return(fdat)
}

GetTNDeathRecord <- function() {
	truth_data <- covidHubUtils::load_truth(
		truth_source = "JHU",
		target_variable = "cum death",
		locations = "47"
	)	
	return(truth_data)
}

target_save_csv <- function(x, filename) {
	write.csv(x, file=filename, row.names=FALSE)	
	return(filename)
}

GetTSAThroughput <- function() {
	tsa <- read_html("https://www.tsa.gov/coronavirus/passenger-throughput")
	tsa_screening <- tsa %>% html_elements("table") %>% html_table()
	tsa_screening <- tsa_screening[[1]]
	tsa_screening$Day <- format(as.Date(tsa_screening$Date, format="%m/%d/%Y"), "%m/%d")
	tsa_summary <- data.frame(Date=as.Date(tsa_screening$Date, format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2022`)))
	tsa_summary <- rbind(tsa_summary, data.frame(Date=as.Date(paste0(tsa_screening$Day, "/2021"), format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2021`))))
	tsa_summary <- rbind(tsa_summary, data.frame(Date=as.Date(paste0(tsa_screening$Day, "/2020"), format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2020`))))
	tsa_summary <- rbind(tsa_summary, data.frame(Date=as.Date(paste0(tsa_screening$Day, "/2019"), format="%m/%d/%Y"), Throughput = as.numeric(gsub(",", "", tsa_screening$`2019`))))
	tsa_summary <- tsa_summary[order(tsa_summary$Date),]
	tsa_summary <- tsa_summary[!is.na(tsa_summary$Throughput),]	
	return(tsa_summary)
}

GetTYSFlights <- function() {
	#flights <- read.csv("~/Dropbox/Flight/208423505_T_DB1B_COUPON.csv")	
	flight_files <- list.files("~/Dropbox/Flight/", pattern=".*.csv")
	tys_flights <- data.frame()
	for (i in seq_along(flight_files)) {
		flights <- read.table(paste0("~/Dropbox/Flight/", flight_files[i]), header=TRUE, sep=",")
		tys_flights <- rbind(tys_flights, flights)
	}
	#print(sort(unique(paste0(tys_flights$YEAR, "_",tys_flights$MONTH))))
	tys_flights_only <- rbind(subset(tys_flights, ORIGIN=="TYS"), subset(tys_flights, DEST=="TYS"))
	return(tys_flights_only)
}

# CreateIndividualHospitalData <- function() {
# 	temp = tempfile(fileext = ".csv")
# 	dataURL <- "https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD&api_foundry=true"
# 	download.file(dataURL, destfile=temp, mode='wb')

# 	hhs_capacity <- read.csv(temp, header=TRUE)
# 	]
# 	hhs_capacity_tn_focal[hhs_capacity_tn_focal==-999999] <- NA
# 	hhs_capacity_tn_focal[hhs_capacity_tn_focal=="-999999"] <- NA
# 	hhs_capacity_tn_focal$DATE <- as.Date(hhs_capacity_tn_focal$collection_week)

# 	hhs_capacity_tn_focal$hospital_name <- gsub("TENNOVA HEALTHCARE", "TENNOVA", hhs_capacity_tn_focal$hospital_name )
# 	hhs_capacity_tn_focal$hospital_name <- stringr::str_to_title(hhs_capacity_tn_focal$hospital_name)
# 	hhs_capacity_tn_focal$hospital_name[grepl("University Of Tn", hhs_capacity_tn_focal$hospital_name, ignore.case=TRUE)] <- "University of TN Medical Center"

# 	hhs_capacity_tn_focal$percent_icu_adult_patients_who_have_confirmed_and_suspected_covid <- 100*hhs_capacity_tn_focal$staffed_icu_adult_patients_confirmed_and_suspected_covid_7_day_sum/hhs_capacity_tn_focal$staffed_adult_icu_bed_occupancy_7_day_sum

# 	hhs_capacity_tn_focal$percent_inpatient_adult_patients_who_have_confirmed_and_suspected_covid <- 100*hhs_capacity_tn_focal$total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum/hhs_capacity_tn_focal$all_adult_hospital_inpatient_bed_occupied_7_day_sum


# 	hhs_capacity_tn_focal$number_of_empty_but_staffed_adult_icu_beds <- round((1/7)*(hhs_capacity_tn_focal$total_staffed_adult_icu_beds_7_day_sum-hhs_capacity_tn_focal$staffed_adult_icu_bed_occupancy_7_day_sum),1)

# 	hhs_capacity_tn_focal <- dplyr::select(hhs_capacity_tn_focal, -ends_with("coverage"))	
	
# 	hhs_capacity_tn <- subset(hhs_capacity, state=="TN")

# 	focal_cities <- toupper(c("Oak Ridge", "Knoxville", "Lenoir City", "Maryville", "Sweetwater", "Harriman", "Powell", "Jefferson City", "Athens", "Morristown", "Sevierville", "Tazewell", "La Follette", "Jellico", "Sneedville", "Oneida"))
# 	hhs_capacity_tn_focal <- hhs_capacity_tn[hhs_capacity_tn$city%in%focal_cities,
# }

GetMicrocovid <- function() {
	microcovids <- list.files(path="/Users/bomeara/Dropbox/Microcovid", pattern="*html", full.names=TRUE)
    microcovid_data <- data.frame(names=gsub("/Users/bomeara/Dropbox/Microcovid/", "", microcovids), percent=NA, date=NA, focal_person=NA, others=NA)
	for (i in seq_along(microcovids)) {
		try({
			focal <- readLines(microcovids[i])	
			percent <- as.numeric(str_extract(str_extract(focal, "\\(\\d+.?\\d*\\%\\)</strong> chance of getting COVID from this activity with these people")[1], "\\d+\\.?\\d*")[1])
			microcovid_data$percent[i] <- percent
		})
		microcovid_data$date[i] <- anytime::anytime(strsplit(microcovid_data$names[i], "-")[[1]][1], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+")
		category <- gsub("micro_", "", strsplit(microcovid_data$names[i], "-")[[1]][2])
		microcovid_data$focal_person[i] <- strsplit(category, "_")[[1]][1]
		microcovid_data$others[i] <- strsplit(category, "_")[[1]][2]
	}
	return(microcovid_data)
}

SummarizeMicrocovidData <- function(microcovid_data) {
	focal_categories <- unique(microcovid_data$focal_person)
	other_categories <- unique(microcovid_data$others)	
	results <- matrix(nrow=length(focal_categories), ncol=length(other_categories), dimnames=list(focal_categories, other_categories))
	for (row_index in seq_along(focal_categories)) {
		for (col_index in seq_along(other_categories)) {
			relevant_data <- subset(microcovid_data, focal_person==focal_categories[row_index] & others==other_categories[col_index])
			if(nrow(relevant_data)>0) {
				relevant_data <- relevant_data[order(relevant_data$date, decreasing=TRUE),]
				results[row_index, col_index] <- relevant_data$percent[1]
			}
		}
	}
	
	rownames(results) <- gsub("boostedN95", "Boosted & N95", rownames(results))
	rownames(results) <- gsub("vaxednotboostedCloth", "Vax but no booster & cloth", rownames(results))
	rownames(results) <- gsub("unvaxunmasked", "Not vax, no mask", rownames(results))

	
	results6classes <- round(100*(1-((100-results)/100)^6),1)
	return(list(perclass=ComputeFractionPerCell(round(results,1)), forsixclasses=ComputeFractionPerCell(results6classes)))
}

ComputeFractionPerCell <- function(input_table) {
	for (i in sequence(nrow(input_table))) {
		for (j in sequence(ncol(input_table))) {
		input_table[i,j] <- paste0("1/", round(1/(.01*as.numeric(input_table[i,j])),0), " (", input_table[i,j], "%)")
		}
	}
	return(input_table)
}

GetCountyOnly <- function(x) {
    counties <- rep(NA, length(x))
    x <- gsub("_", ", ", x)
    for (i in seq_along(x)) {
        counties[i] <- strsplit(x[i], ", ")[[1]][1]
    }
    return(counties)
}

GetStateOnly <- function(x) {
    states <- rep(NA, length(x))
    x <- gsub("_", ", ", x)
    for (i in seq_along(x)) {
        states[i] <- strsplit(x[i], ", ")[[1]][2]
    }
    return(states)
}


# Uses NY Times' data
GetCountyCaseAverages <- function() {
    nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties.csv")
    most_recent_dates <- sort(unique(nyt$date), decreasing=TRUE)[1:2]
    nyt_last2weeks <- subset(nyt, date %in% most_recent_dates)
    nyt_last2weeks$County_State <- paste0(nyt_last2weeks$county, "_", nyt_last2weeks$state)
    nyt_2wk_sum <- nyt_last2weeks %>% group_by(County_State) %>% summarise(cases_avg=sum(cases_avg), cases_avg_per_100k=sum(cases_avg_per_100k), deaths_avg=sum(deaths_avg), deaths_avg_per_100k=sum(deaths_avg_per_100k))
    nyt_2wk_sum$cases_per_day_avg <- nyt_2wk_sum$cases_avg/14
    nyt_2wk_sum$cases_per_day_per_100k_avg <- nyt_2wk_sum$cases_avg_per_100k/14
    nyt_2wk_sum$County  <- GetCountyOnly(nyt_2wk_sum$County_State)
    nyt_2wk_sum$State  <- GetStateOnly(nyt_2wk_sum$County_State)
    nyt_2wk_sum$StateAbbreviation <- state.abb[match(toupper(nyt_2wk_sum$State), toupper(state.name))]
    nyt_2wk_sum$County_StateAbbreviation <- paste0(nyt_2wk_sum$County, "_", nyt_2wk_sum$StateAbbreviation)
    return(nyt_2wk_sum)
}

GetCommunityTransmissionReportLastDate <- function() {
	spreadsheet <- read.csv("https://healthdata.gov/api/views/6hii-ae4f/rows.csv?accessType=DOWNLOAD")	
	attachments <- (jsonlite::fromJSON(spreadsheet[nrow(spreadsheet),]$Metadata.Published))$attachments
	attachments <- attachments[grepl("xlsx", attachments$name),]
	attachments <- attachments[order(attachments$name, decreasing=TRUE),]
	url <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/", attachments$assetId[1],"?download=true&filename=", attachments$name[1], "_Public.xlsx")
	report_date <- gsub("Community_Profile_Report_", "", gsub("_Public.xlsx", "", attachments$name[1]))
	return(list(report_url=url, report_date=report_date))
}

# From https://beta.healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9
GetCommunityTransmissionReport <- function(report_url) {
    #url <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/93e07285-d044-4fc4-b93b-e8100db5e7a1?download=true&filename=Community_Profile_Report_", reportdate, "_Public.xlsx")
    #GET(paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/93e07285-d044-4fc4-b93b-e8100db5e7a1?download=true&filename=Community_Profile_Report_", report_date, "_Public.xlsx"), write_disk(path <- tempfile(fileext = ".xlsx")))
    GET(report_url, write_disk(path <- tempfile(fileext = ".xlsx")))
    cdc_list <-
        path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map(~ read_excel(path = path, sheet = .x, skip = 1), .id = "Sheet")
    cdc_county <- cdc_list[["Counties"]]
    cdc_state <- cdc_list[["States"]]
    cdc_county$Population_65_Plus <- cdc_county$`People who are fully vaccinated - ages 65+`/cdc_county$`People who are fully vaccinated as % of population - ages 65+`
    cdc_county$Population_Under_65 <- cdc_county$Population - cdc_county$Population_65_Plus
    cdc_county$Proportion_Fully_Vaccinated_Under_65 <- (cdc_county$`People who are fully vaccinated` - cdc_county$`People who are fully vaccinated - ages 65+`)/ cdc_county$Population_Under_65
	cdc_county$Proportion_Fully_Vaccinated_All <- cdc_county$"People who are fully vaccinated as % of total population"
    cdc_county$`% staffed adult ICU beds occupied_County` <- cdc_county$`% staffed adult ICU beds occupied`
	cdc_county$Proportion_Fully_Vaccinated_12_to_17 <- NA
	if("People who are fully vaccinated as % of population - ages 12-17" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_12_to_17 <- cdc_county$"People who are fully vaccinated as % of population - ages 12-17")
	}
	
	cdc_county$Proportion_Fully_Vaccinated_18_plus <- NA

	if("People who are fully vaccinated as % of population - ages 18+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_18_plus <-  cdc_county$"People who are fully vaccinated as % of population - ages 18+")
	}
	
	cdc_county$Proportion_Fully_Vaccinated_65_plus <- NA
	if("People who are fully vaccinated as % of population - ages 65+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_65_plus <-  cdc_county$"People who are fully vaccinated as % of population - ages 65+")
	}
	
	
	
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_12_to_17 <- NA
	
	if("People initiating vaccination as % of population - last 7 days - ages 12-17" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_12_to_17 <- cdc_county$"People initiating vaccination as % of population - last 7 days - ages 12-17")
	}
	
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_18_plus <- NA
	if("People initiating vaccination as % of population - last 7 days - ages 18+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_18_plus <- cdc_county$"People initiating vaccination as % of population - last 7 days - ages 18+")
	}
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_65_plus <- NA
	if("People initiating vaccination as % of population - last 7 days - ages 65+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_65_plus <-  cdc_county$"People initiating vaccination as % of population - last 7 days - ages 65+")
	}

	cdc_county$Percent_inpatient_beds_occupied_by_covid_patient_County <- 100*cdc_county$"% inpatient beds occupied by COVID-19 patient"
	cdc_county$Confirmed_covid_admissions_per_100K_7_days_County <- cdc_county$"Confirmed COVID-19 admissions - last 7 days"/cdc_county$"Population"*100000
	cdc_county$Confirmed_covid_admissions_7_days_County <- cdc_county$"Confirmed COVID-19 admissions - last 7 days"

	cdc_county$Cases_7_days_County <- cdc_county$"Cases - last 7 days"
	cdc_county$Cases_7_days_per_100K_County <- cdc_county$"Cases per 100k - last 7 days"
	cdc_county$Deaths_7_days_County <- cdc_county$"Deaths - last 7 days"

	
    cdc_state$Population_65_Plus <- cdc_state$`People who are fully vaccinated - ages 65+`/cdc_state$`People who are fully vaccinated as % of population - ages 65+`
    cdc_state$Population_Under_65 <- cdc_state$Population - cdc_state$Population_65_Plus
    cdc_state$Proportion_Fully_Vaccinated_Under_65 <- (cdc_state$`People who are fully vaccinated` - cdc_state$`People who are fully vaccinated - ages 65+`)/ cdc_state$Population_Under_65
    

    # Now deal with counties where there is no vaccination info by county, like all of Texas. Report state estimate as best one
    cdc_county$Vaccination_Data_Is_For_County <- TRUE
    for (county_index in sequence(nrow(cdc_county))) {
        if(is.na(cdc_county$Proportion_Fully_Vaccinated_Under_65[county_index])) {
            cdc_county$Proportion_Fully_Vaccinated_Under_65[county_index] <- cdc_state$Proportion_Fully_Vaccinated_Under_65[which(cdc_state$`State Abbreviation`==cdc_county$`State Abbreviation`[county_index])]
            cdc_county$`People who are fully vaccinated as % of total population`[county_index] <- cdc_state$`People who are fully vaccinated as % of total population`[which(cdc_state$`State Abbreviation`==cdc_county$`State Abbreviation`[county_index])]
            cdc_county$Vaccination_Data_Is_For_County[county_index] <- FALSE
        }
    }

    # Hospital capacity probably is more relevant on a state basis than a county (since hospitals work regionally -- a small rural county might not have many ICU beds but the neighboring city might)
    cdc_state_hospital <- cdc_state %>% select("State Abbreviation", "% staffed adult ICU beds occupied")
    cdc_county_with_hospital <- cdc_county%>% left_join(cdc_state_hospital, by="State Abbreviation", copy=TRUE, suffix=c("_County", "_State"))
	transmission <- rep(NA, nrow(cdc_county_with_hospital))
	try(transmission <- result$`Community Transmission Level`)
	try(transmission <- result$`Community Transmission Level - last 7 days`)
	cdc_county_with_hospital$`Community Transmission Level - last 7 days` <- transmission
    cdc_county_with_hospital$FIPS <- cdc_county_with_hospital$`FIPS code`
    cdc_county_with_hospital <- cdc_county_with_hospital %>% select(County, "FIPS", "State Abbreviation", "Population", "Cases per 100k - last 7 days", "Deaths per 100k - last 7 days", "Community Transmission Level - last 7 days", "% staffed adult ICU beds occupied_State", "% staffed adult ICU beds occupied_County", "People who are fully vaccinated as % of total population", "Proportion_Fully_Vaccinated_Under_65", "Vaccination_Data_Is_For_County", "Area of Concern Category", "Proportion_Fully_Vaccinated_12_to_17" , "Proportion_Fully_Vaccinated_18_plus", "Proportion_Fully_Vaccinated_65_plus", "Proportion_Fully_Vaccinated_All", "Proportion_Initiating_Vaccination_Last_7_days_12_to_17", "Proportion_Initiating_Vaccination_Last_7_days_18_plus", "Proportion_Initiating_Vaccination_Last_7_days_65_plus", "Percent_inpatient_beds_occupied_by_covid_patient_County",  "Confirmed_covid_admissions_per_100K_7_days_County", "Confirmed_covid_admissions_7_days_County", "Cases_7_days_County", "Cases_7_days_per_100K_County", "Deaths_7_days_County")
	
	cdc_county_with_hospital <- GetCommunityLevel(cdc_county_with_hospital)
	
    return(cdc_county_with_hospital)
}

# From https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-Community-Levels-by-County/3nnm-4jni
GetCommunityLevels2022Format <- function() {
	cdc_community_new <- read.csv("https://data.cdc.gov/api/views/3nnm-4jni/rows.csv?accessType=DOWNLOAD")	
	
	# cdc_county_with_hospital <- cdc_county_with_hospital %>% select(County, "FIPS", "State Abbreviation", "Population", "Cases per 100k - last 7 days", "Deaths per 100k - last 7 days", "Community Transmission Level - last 7 days", "% staffed adult ICU beds occupied_State", "% staffed adult ICU beds occupied_County", "People who are fully vaccinated as % of total population", "Proportion_Fully_Vaccinated_Under_65", "Vaccination_Data_Is_For_County", "Area of Concern Category", "Proportion_Fully_Vaccinated_12_to_17" , "Proportion_Fully_Vaccinated_18_plus", "Proportion_Fully_Vaccinated_65_plus", "Proportion_Fully_Vaccinated_All", "Proportion_Initiating_Vaccination_Last_7_days_12_to_17", "Proportion_Initiating_Vaccination_Last_7_days_18_plus", "Proportion_Initiating_Vaccination_Last_7_days_65_plus", "Percent_inpatient_beds_occupied_by_covid_patient_County",  "Confirmed_covid_admissions_per_100K_7_days_County", "Confirmed_covid_admissions_7_days_County", "Cases_7_days_County", "Cases_7_days_per_100K_County", "Deaths_7_days_County")
	cdc_community_new_oldcolnames <- data.frame(County=cdc_community_new$county, FIPS=cdc_community_new$county_fips, `State Abbreviation`=cdc_community_new$state, Population=cdc_community_new$county_population, Percent_inpatient_beds_occupied_by_covid_patient_County=cdc_community_new$covid_inpatient_bed_utilization, Confirmed_covid_admissions_per_100K_7_days_County=cdc_community_new$covid_hospital_admissions_per_100k, Cases_7_days_per_100K_County=cdc_community_new$covid_cases_per_100k, ReportDate=cdc_community_new$date_updated, New_Community_Level=cdc_community_new$covid.19_community_level)
	return(cdc_community_new_oldcolnames)
}


GetCommunityLevel <- function(cdc_county_with_hospital) {
	cdc_county_with_hospital$New_Community_Level <- NA
	for (i in sequence(nrow(cdc_county_with_hospital))) {
		try({
			if(cdc_county_with_hospital$Cases_7_days_per_100K_County[i]<200) {
				cdc_county_with_hospital$New_Community_Level[i] <- "Low"
				if(cdc_county_with_hospital$Confirmed_covid_admissions_per_100K_7_days_County[i]>=10 || cdc_county_with_hospital$Percent_inpatient_beds_occupied_by_covid_patient_County[i]>=10) {
					cdc_county_with_hospital$New_Community_Level[i] <- "Medium"
					if(cdc_county_with_hospital$Confirmed_covid_admissions_per_100K_7_days_County[i]>=20 || cdc_county_with_hospital$Percent_inpatient_beds_occupied_by_covid_patient_County[i]>=15) {
						cdc_county_with_hospital$New_Community_Level[i] <- "High"
					}
				}
			} else {
				cdc_county_with_hospital$New_Community_Level[i] <- "Medium"
				if(cdc_county_with_hospital$Confirmed_covid_admissions_per_100K_7_days_County[i]>=10 || cdc_county_with_hospital$Percent_inpatient_beds_occupied_by_covid_patient_County[i]>=10) {
					cdc_county_with_hospital$New_Community_Level[i] <- "High"
				}
			}
		})
	}
	return(cdc_county_with_hospital)
}

# Min date chosen as the date vaccination data first included
GetAllCommunityTransmissionReportDates <- function(mindate=20210412) {
	spreadsheet <- read.csv("https://healthdata.gov/api/views/6hii-ae4f/rows.csv?accessType=DOWNLOAD")	
	attachments <- (jsonlite::fromJSON(spreadsheet[nrow(spreadsheet),]$Metadata.Published))$attachments
	attachments <- attachments[grepl("xlsx", attachments$name),]
	report_urls <- rep(NA,nrow(attachments)) 
	report_dates <- rep(NA,nrow(attachments)) 
	for (i in seq_along(attachments$assetId)) {
		report_urls[i] <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/", attachments$assetId[i],"?download=true&filename=", attachments$name[i], "_Public.xlsx")
		report_dates[i] <- gsub("Community_Profile_Report_", "", gsub("_Public.xlsx", "", attachments$name[i]))
	}
	if(!is.null(mindate)) {
		report_urls <- report_urls[which(as.numeric(report_dates)>=mindate)]	
		report_dates <- report_dates[which(as.numeric(report_dates)>=mindate)]	

	}
	return(list(report_urls=report_urls, report_dates=report_dates))
}

GetAllCommunityTransmissionReports <- function(urls, dates) {
	all_reports <- data.frame()
	for (i in seq_along(urls)) {
		local_report <- GetCommunityTransmissionReport(urls[i])
		local_report$ReportDate <- dates[i]
		all_reports <- plyr::rbind.fill(all_reports, local_report)
		Sys.sleep(3)	
	}	
	return(as.data.frame(all_reports))
}

CombineOldAndNewCDC <- function(cdc_all_reports_old, cdc_reports_new_2022) {
	return(plyr::rbind.fill(cdc_all_reports_old, cdc_reports_new_2022))	
}

AggregateCountyInformation <- function(hesitancy_by_county, cases_by_county, hospitalization_by_county) {
    step1 <- full_join(hesitancy_by_county, cases_by_county, by="County_StateAbbreviation")
    result <- full_join(step1, hospitalization_by_county, by="County_StateAbbreviation")
    result <- result[!is.na(result$Percent.estimated.hesitant),]
    result <- result[!is.na(result$cases_per_day_avg),]
    result_simplified <- result %>% select(County, County_StateAbbreviation, StateAbbreviation, cases_per_day_avg, cases_per_day_per_100k_avg, Percent_ICU_occupied_7_day_avg, Percent.estimated.hesitant, Percent.estimated.strongly.hesitant, Percent.adults.fully.vaccinated.against.COVID.19, total_icu_beds_7_day_avg)
    result_simplified$County <- GetCountyOnly(result_simplified$County_StateAbbreviation)
    result_simplified$StateAbbreviation <- GetStateOnly(result_simplified$County_StateAbbreviation)
    return(result_simplified)
}

WriteCSVTable <- function(result, file_out="data/aggregation_by_county.csv") {
    write.csv(result, file=file_out, row.names=FALSE)
    return(file_out)
}


MergeAllSitesWithCovidInfo <- function(all_sites, aggregation_by_county) {
    all_sites$County_State <- paste0(all_sites$County, "_", all_sites$State)
    aggregation_by_county$County_State <- paste0(aggregation_by_county$County, "_", aggregation_by_county$State)
    all_info <- left_join(all_sites, aggregation_by_county, by="County_State", suffix=c("_geonames", "_covid"))
    all_info$County <- all_info$County_geonames
    all_info$State <- all_info$State_geonames

    all_info <- all_info[!is.na(all_info$State),]
    all_info <- all_info[!is.na(all_info$Community_Transmission_Level),]

    return(all_info)
}

FormatSitesAndCovid <- function(sites_and_covid) {
    sites_and_covid$Location <- sites_and_covid$name 
    sites_and_covid <- sites_and_covid[!grepl("Fire Department", sites_and_covid$Location, ignore.case=TRUE),]
    sites_and_covid$Latitude <- sites_and_covid$latitude
    sites_and_covid$Longitude <- sites_and_covid$longitude
    sites_and_covid$Type <- stringr::str_to_title(sites_and_covid$Type)

    sites_and_covid <- sites_and_covid[order(sites_and_covid$Percent_Fully_Vaccinated_Under_65, sites_and_covid$Percent_Fully_Vaccinated, sites_and_covid$Location, method="radix", decreasing=c(TRUE, TRUE, FALSE)),]


    sites_and_covid$Percent_Fully_Vaccinated <- round(sites_and_covid$Percent_Fully_Vaccinated,0)
    sites_and_covid$Percent_Fully_Vaccinated_Under_65 <- round(sites_and_covid$Percent_Fully_Vaccinated_Under_65,0)
    sites_and_covid <- sites_and_covid%>%select(Location, Type, County, State, Community_Transmission_Level, Percent_Fully_Vaccinated_Under_65, Percent_Estimated_Vaccination_Hesitant, Percent_State_ICU_Beds_Filled, Percent_County_ICU_Beds_Filled, Percent_Fully_Vaccinated, Percent_Estimated_Vaccination_Strongly_Hesitant, Population, Vaccination_Data_Is_For_County, Latitude, Longitude, Area_of_Concern_Category, FIPS)
    return(sites_and_covid)
}

FormatForDT <- function(site_info_formatted, cities=FALSE, do_cities_filter=TRUE, focal_county=NULL, focal_state=NULL) {
    site_info_formatted <- site_info_formatted[which(nchar(site_info_formatted$Location)>2),]
    site_info_formatted$Percent_Fully_Vaccinated[which(site_info_formatted$Percent_Fully_Vaccinated>99)] <- 99
    site_info_formatted$Percent_Fully_Vaccinated_Under_65[which(site_info_formatted$Percent_Fully_Vaccinated_Under_65>99)] <- 99

    #site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"), "% (", site_info_formatted$Percent_Fully_Vaccinated_Graded_On_Curve, ")")
    site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"))
    #site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"), "%")
   #site_info_formatted$`Fully Vaccinated (<65)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated_Under_65,2,pad="0"), "% (", site_info_formatted$Percent_Fully_Vaccinated_Under_65_Graded_On_Curve, ")")
    site_info_formatted$`Fully Vaccinated (<65)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated_Under_65,2,pad="0"))
    site_info_formatted$`Community Transmission` <- site_info_formatted$Community_Transmission_Level
    site_info_formatted$`Vaccine Hesitant` <- paste0(str_pad(site_info_formatted$Percent_Estimated_Vaccination_Hesitant,2,pad="0"))
    site_info_formatted$`State ICU Beds Filled` <- paste0(str_pad(site_info_formatted$Percent_State_ICU_Beds_Filled,2,pad="0"))
    site_info_formatted$`County ICU Beds Filled` <- paste0(str_pad(site_info_formatted$Percent_County_ICU_Beds_Filled,2,pad="0"))
    site_info_formatted$`Area of Concern` <- site_info_formatted$Area_of_Concern_Category
    site_info_formatted$`County, State` <- paste0(site_info_formatted$County, ", ", site_info_formatted$State)

    site_info_formatted$TransmissionRank <- 0
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="High")] <- 4
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Substantial")] <- 3
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Moderate")] <- 2
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Low")] <- 1
    site_info_formatted <- site_info_formatted[order(site_info_formatted$TransmissionRank, site_info_formatted$Percent_Fully_Vaccinated_Under_65, site_info_formatted$Percent_Fully_Vaccinated, method="radix", decreasing=c(FALSE, TRUE, TRUE)),]
	site_info_summary <- site_info_formatted
    if(do_cities_filter) {
        if(cities) {
            site_info_summary <- site_info_summary[which(site_info_summary$Type=="Populated Place"),]%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", "County, State", "Vaccine Hesitant", "State ICU Beds Filled")
        } else {
            site_info_summary <- site_info_summary[which(site_info_summary$Type!="Populated Place"),]%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", Type, "County, State", "Vaccine Hesitant", "State ICU Beds Filled")
        }
    } else {
        site_info_summary <- subset(site_info_summary, State==focal_state & County==focal_county)%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", Type, "County, State", "Vaccine Hesitant", "State ICU Beds Filled", "County ICU Beds Filled", "Area of Concern", "Latitude", "Longitude", "FIPS")
    }
    return(site_info_summary)
}


ComputeNationwideNumbers <- function(aggregation_by_county) {
    PeoplePerCommunityTransmissionLevel <- aggregation_by_county %>% group_by(Community_Transmission_Level) %>% dplyr::summarise(Population=sum(Population))
    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels <- 0
    
    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]

    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Substantial")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]

    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="High")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Substantial")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]
  
    PeoplePerCommunityTransmissionLevel$PercentPopulationPopulationAtThisTransmission <- 100*PeoplePerCommunityTransmissionLevel$Population/sum(PeoplePerCommunityTransmissionLevel$Population)

    PeoplePerCommunityTransmissionLevel$PercentPopulationAtLowerTransmission <- 100*PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels/sum(PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels)

    return(PeoplePerCommunityTransmissionLevel %>% select(Community_Transmission_Level, PercentPopulationAtLowerTransmission, PercentPopulationPopulationAtThisTransmission))
}

# https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk27/health5_week27.xlsx
# This has hesitancy info broken by demographic groups; I summarize it for just those age 18-64 as working age groups
# Uses tutorial from Isabella Velázquez, https://ivelasq.rbind.io/blog/tidying-census-data/
GetHesitancyWorkingAge <- function(week=27, state_only=TRUE) {
    GET(paste0("https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk", week, "/health5_week", week, ".xlsx"), write_disk(path <- tempfile(fileext = ".xlsx")))
    census_list <-
        path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map(~ read_excel(path = path, sheet = .x, skip = 3), .id = "Sheet")

    column_text <- c("Age", "Total", "Total_Some_Vaccinated", "Receiving_All_Doses", "Receving_Some_But_Not_All_Doses", "Yes_But_Did_Not_Report_Dosing", "Total_Not_Vaccinated", "Will_Definitely_Get", "Will_Probably_Get", "Will_Probably_Not_Get", "Will_Definitely_Not_Get", "No_But_Did_Not_Reply_Why", "No_Response_On_Vaccination")
    census_list <- 
        census_list %>% 
        map(~ slice(.x, -1:-6, -11:-89)) %>% 
        map(., set_names, nm = column_text) %>%
        map(~ mutate_at(.x, column_text[-1], list(~ as.numeric(.))))

    census_df <- data.frame()
    for (i in seq_along(census_list))    {
        local_df <- data.frame(Region=names(census_list)[i])

        local_df <- cbind(local_df, as.data.frame(t(apply(census_list[[i]][,-1], 2, sum, na.rm=TRUE))))
        census_df <- rbind(census_df, local_df)
    }
    census_df$PercentageWorkingAgeHesitant <- 100*(census_df$Will_Probably_Not_Get+census_df$Will_Definitely_Not_Get)/census_df$Total
    census_df$PercentageWorkingAgeVeryHesitant <- 100*(census_df$Will_Definitely_Not_Get)/census_df$Total
    census_df$PercentageWorkingAgeVaccinated <- 100*(census_df$Total_Some_Vaccinated)/census_df$Total
    if(state_only) {
        census_df <- subset(census_df, nchar(Region)==2)
        census_df <- subset(census_df, Region!="US")
    }
    return(census_df)
}

# Gets sewage data from CDC at https://data.cdc.gov/Public-Health-Surveillance/NWSS-Public-SARS-CoV-2-Wastewater-Data/2ew6-ywp6
GetSewageData <- function() {
	sewage <- read.csv("https://data.cdc.gov/api/views/2ew6-ywp6/rows.csv")
	focal_places <- c(496, 536, 405, 271)
	sewage_focal <- subset(sewage, wwtp_id %in% focal_places)
	sewage_focal$date_end <- as.Date(sewage_focal$date_end)
	sewage_focal$Location <- rep(NA)
	sewage_focal$Location[which(sewage_focal$wwtp_id==496)] <- "Bradley, TN"
	sewage_focal$Location[which(sewage_focal$wwtp_id==536)] <- "Eastern Band Cherokee Indians, NC"
	sewage_focal$Location[which(sewage_focal$wwtp_id==405)] <- "Jackson, NC"
	sewage_focal$Location[which(sewage_focal$wwtp_id==271)] <- "Buncombe/Henderson, NC"
	return(sewage_focal)
}

#This gets hesitancy estimated by county by the CDC: https://aspe.hhs.gov/pdf-report/vaccine-hesitancy
GetHesitancyByCounty <- function() {
    #https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv?accessType=DOWNLOAD
    options(timeout=600) # let things download for at least ten minutes
    hesitancy <- read.csv("https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv")
    hesitancy$Percent.adults.fully.vaccinated.against.COVID.19 <- 100*hesitancy$Percent.adults.fully.vaccinated.against.COVID.19
    hesitancy$Percent.estimated.hesitant <- 100*hesitancy$Estimated.hesitant
    hesitancy$Percent.estimated.strongly.hesitant <- 100*hesitancy$Estimated.strongly.hesitant
    hesitancy$FIPS <- hesitancy$`FIPS.Code`
    hesitancy_simplified <- hesitancy %>% select(FIPS, County.Name, State, Percent.estimated.hesitant, Percent.estimated.strongly.hesitant, Percent.adults.fully.vaccinated.against.COVID.19)
    hesitancy_simplified$StateAbbreviation <- state.abb[match(hesitancy_simplified$State, toupper(state.name))]
    hesitancy_simplified$County <- GetCountyOnly(hesitancy_simplified$County.Name)
    hesitancy_simplified$County <- gsub(" County", "", hesitancy_simplified$County)
    hesitancy_simplified$County_StateAbbreviation <- paste0(hesitancy_simplified$County, "_", hesitancy_simplified$StateAbbreviation)
    return(hesitancy_simplified)
}

JoinHesitancyWithCDCWeekly <- function(hesitancy_by_county, cdc_weekly) {
    result <- full_join(cdc_weekly, hesitancy_by_county, by="FIPS", copy=TRUE, suffix=c("_CDC_Name", "_Hesitancy_Name"))
    result$Percent_State_ICU_Beds_Filled <- 100*result$`% staffed adult ICU beds occupied_State`
    result$Percent_County_ICU_Beds_Filled <- 100*result$`% staffed adult ICU beds occupied_County`
    result$Percent_Fully_Vaccinated <- 100 * result$`People who are fully vaccinated as % of total population`
    result$Percent_Fully_Vaccinated_Under_65 <- 100 * result$Proportion_Fully_Vaccinated_Under_65
    result$County <- result$County_Hesitancy_Name
    result$Percent_Estimated_Vaccination_Hesitant <- result$Percent.estimated.hesitant
    result$Percent_Estimated_Vaccination_Strongly_Hesitant <- result$Percent.estimated.strongly.hesitant
	transmission <- rep(NA, nrow(result))
	try(transmission <- result$`Community Transmission Level`)
	try(transmission <- result$`Community Transmission Level - last 7 days`)
	result$`Community Transmission Level - last 7 days` <- transmission
    result$Community_Transmission_Level <- result$`Community Transmission Level - last 7 days`
    result$Area_of_Concern_Category <- result$`Area of Concern Category`
    result$State <- result$StateAbbreviation
    result_simplified <- result %>% select(County, State, Community_Transmission_Level, Percent_Fully_Vaccinated, Percent_Fully_Vaccinated_Under_65, Percent_Estimated_Vaccination_Hesitant, Percent_Estimated_Vaccination_Strongly_Hesitant, Percent_State_ICU_Beds_Filled, Percent_County_ICU_Beds_Filled, Population, FIPS, Vaccination_Data_Is_For_County, Area_of_Concern_Category)
    result_simplified <- result_simplified[!is.na(result_simplified$County),]
    return(result_simplified)
}

GetBiobotSewage <- function() {
	biobot <- read.csv("https://raw.githubusercontent.com/biobotanalytics/covid19-wastewater-data/master/wastewater_by_county.csv")
	biobot <- biobot[, -1] # empty first col
	biobot$date <- as.Date(biobot$sampling_week)
	biobot$effective_concentration_rolling_average <- as.numeric(biobot$effective_concentration_rolling_average)
	return(biobot)
}