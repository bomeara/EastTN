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
            colnames(local_beds) <- gsub("\\.$", "", colnames(local_beds))
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
    hospital_knox <- hospital_knox[!grepl('\\*', hospital_knox$East.Region.Hospitals),]
    colnames(hospital_knox) <- gsub("East.Region.Hospitals", "Resource", colnames(hospital_knox))
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

CreateSchoolsKnox <- function() {
    staff_total <-  8000 #"more than" this from knoxschools.org
    student_total <- 60000 #"more than" this from knoxschools.org
    school_knox_info <- read.csv("/Users/bomeara/Dropbox/KnoxSchoolsCovid/KCS COVID Status Dashboard 21-22_Page 1_Bar chart.csv")
    colnames(school_knox_info) <- c("Date", "Active Staff", "Active Student")
    knox_school_files <- list.files(path="/Users/bomeara/Dropbox/KnoxSchoolsCovid", pattern="*knox_schools.html", full.names=TRUE)
    for (i in seq_along(knox_school_files)) {

        try(input_file <- readChar(knox_school_files[i], file.info(knox_school_files[i])$size))
        try(input_file_html <- rvest::read_html(input_file))
        try(tbl <- as.data.frame(rvest::html_table(rvest::html_nodes(input_file_html, "table"))[[1]]))
        try(school_knox_info <- rbind(school_knox_info, tbl))
    }
    school_knox_info <- school_knox_info[!duplicated(school_knox_info$Date),]
    school_knox_info$Date <- as.Date(school_knox_info$Date,"%b %d, %Y")
    colnames(school_knox_info) <- c("Date", "Active_Staff_Count", "Active_Students_Count")
    school_knox_info$Active_Staff_Percent <- 100*school_knox_info$Active_Staff_Count/staff_total
    school_knox_info$Active_Students_Percent <- 100*school_knox_info$Active_Students_Count/student_total
    return(school_knox_info)
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

    daily <- readxl::read_xlsx(temp, sheet =1, col_types=c("date", "text", rep("numeric",8)))
    daily_knox <- subset(daily, COUNTY=="Knox")
    daily_knox$DATE <- as.Date(daily_knox$DATE)
    return(daily_knox)
}

GetTNDeathPredictions <- function() {
  fdat <- covidHubUtils::load_forecasts(
		models = c("COVIDhub-ensemble"),
		dates = as.character(Sys.Date()-1),
		source = "zoltar",
		date_window_size = 6,
		locations = c("47"),
		types = c("quantile", "point"),
		targets = paste(1:4, "wk ahead inc death"),
		verbose = FALSE
	)
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