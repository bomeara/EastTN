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