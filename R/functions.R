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

    daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",21)))

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
    daily_focal$Active_cases_per_100k <- 100000*(daily_focal$TOTAL_ACTIVE/daily_focal$Population)
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
    try(local_beds <- read.csv(hospital_knox_files[i]), silent=TRUE)
    if(!is.na(local_beds)) {
        local_beds$East.Region.Hospitals <- gsub('All Hospital Beds*', 'All Hospital Beds *', gsub('All Hospital Beds *', 'All Hospital Beds', local_beds$East.Region.Hospitals, fixed=TRUE), fixed=TRUE)
        local_beds$Total.Capacity <- as.numeric(gsub(",",'', local_beds$Total.Capacity))
        local_beds$Current.Census <- as.numeric(gsub(",",'', local_beds$Current.Census))
        local_beds$Current.Utilization <- as.numeric(gsub('%','', local_beds$Current.Utilization))
        local_beds$Available.Capacity <- as.numeric(gsub('%','', local_beds$Available.Capacity))
        local_beds$Date <- anytime::anytime(stringr::str_extract(hospital_knox_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+"))
        if (i==1) {
            hospital_knox <- local_beds
        } else {
            hospital_knox <- rbind(hospital_knox, local_beds)
        }
    }
    }
    hospital_knox <- subset(hospital_knox, East.Region.Hospitals != "Adult Floor Beds/Non-ICU")
    hospital_knox <- hospital_knox[which(nchar(hospital_knox$East.Region.Hospitals)>0),]
    hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)] <- hospital_knox$Current.Utilization[which(hospital_knox$Current.Utilization>100)]/100 #to fix two days of data where Knox County was multiplying these by 100, getting 7935% utilization
    return(hospital_knox)
}

CreateSchoolsOakRidge <- function() {
    oakridge_school_files <- list.files(path="/Users/bomeara/Dropbox/OakRidgeCovid", pattern="*oak_ridge_schools.csv", full.names=TRUE)
    schools_oakridge <- data.frame()
    for (i in seq_along(oakridge_school_files)) {
    school_oakridge_info <- NULL
    try(school_oakridge_info <- read.csv(oakridge_school_files[i]), silent=TRUE)
    if(!is.null(school_oakridge_info)) {
        for (col_index in 3:9) {
            school_oakridge_info[,col_index] <- as.numeric(school_oakridge_info[,col_index])
        }
        school_oakridge_info[is.na(school_oakridge_info)] <- 0
        colnames(school_oakridge_info)[9] <- "student.population"
        local_info <- data.frame(Date=rep(anytime::anytime(stringr::str_extract(oakridge_school_files[i], "\\d+_\\d+_\\d+_\\d+_\\d+_\\d+")), nrow(school_oakridge_info)), School=school_oakridge_info$School, PercentPositiveStudentsYearToDate=100*school_oakridge_info$YTD.Student.Cases/school_oakridge_info$student.population, PercentActiveCovidStudents=100*school_oakridge_info$Current.Student.Cases/school_oakridge_info$student.population)
        if (i==1) {
            schools_oakridge <- local_info
        } else {
            schools_oakridge <- rbind(schools_oakridge, local_info)
        }
    }
    }
    schools_oakridge <- schools_oakridge[is.finite(schools_oakridge$PercentPositiveStudentsYearToDate),]
    return(schools_oakridge)
}

CreateHHSDataTN <- function() {

    hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
    capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility", hhs_sources$dataset$title)


    capacity_by_facility_url <- hhs_sources$dataset$distribution[capacity_by_facility_number][[1]]$downloadURL[1] #often a week behind though
    temp = tempfile(fileext = ".csv")

    utils::download.file(capacity_by_facility_url, temp, method="libcurl")

    hhs_capacity <- read.csv(file=temp)
    hhs_capacity_tn <- subset(hhs_capacity, state=="TN")

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

    hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal

    hhs_capacity_tn_focal_cities <- hhs_capacity_tn_focal %>% group_by(city, DATE) %>% summarize_at(vars(number_unoccupied_adult_hospital_inpatient_beds, number_unoccupied_adult_hospital_ICU_beds), list(sum = ~sum(., na.rm=TRUE)))

    combinations <- expand.grid(DATE = unique(hhs_capacity_tn_focal_cities$DATE), city = unique(hhs_capacity_tn_focal_cities$city))

    hhs_capacity_tn_focal_cities <- full_join(hhs_capacity_tn_focal_cities, combinations, by = c("DATE" = "DATE", "city" = "city")) %>% mutate(number_unoccupied_adult_hospital_inpatient_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_inpatient_beds_sum), 0, number_unoccupied_adult_hospital_inpatient_beds_sum)) %>% mutate(number_unoccupied_adult_hospital_ICU_beds_sum = ifelse(is.na(number_unoccupied_adult_hospital_ICU_beds_sum), 0, number_unoccupied_adult_hospital_ICU_beds_sum))

    hhs_capacity_tn_focal_cities$city <- stringr::str_to_title(hhs_capacity_tn_focal_cities$city)

    hhs_capacity_tn_focal_latest <- subset(hhs_capacity_tn_focal, DATE==max(DATE))
    hhs_capacity_tn_focal_latest <- hhs_capacity_tn_focal_latest[order(hhs_capacity_tn_focal_latest$all_adult_hospital_inpatient_beds_7_day_avg, decreasing=TRUE),]
    return(hhs_capacity_tn_focal_latest)
}

CreateHHSDataFocalCitiesPretty <- function(hhs_capacity_tn_focal_latest) {
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