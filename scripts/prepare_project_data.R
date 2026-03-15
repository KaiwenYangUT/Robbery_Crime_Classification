suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

load_project_data <- function(year = 2020) {
  csv_files <- c(
    "Auto_Theft_Open_Data.csv",
    "Bicycle_Thefts_Open_Data.csv",
    "Break_and_Enter_Open_Data.csv",
    "Homicides_Open_Data_(ASR-RC-TBL-002).csv",
    "Robbery_Open_Data.csv",
    "Shooting_and_Firearm_Discharges_Open_Data.csv",
    "Theft_Over_Open_Data.csv"
  )

  crime <- bind_rows(
    lapply(csv_files, function(file_name) {
      read.csv(file.path("data", file_name), stringsAsFactors = FALSE)
    }),
    .id = "Dataset"
  ) %>%
    mutate(
      Robbery = MCI_CATEGORY == "Robbery",
      Month = match(OCC_MONTH, month.name),
      Occurrence_Time = as.POSIXct(
        paste(OCC_YEAR, Month, OCC_DAY, OCC_HOUR, sep = "-"),
        format = "%Y-%m-%d-%H"
      ),
      Day_Of_Week_Numeric = match(
        weekdays(Occurrence_Time),
        c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
      ),
      Season = case_when(
        Month %in% c(3, 4, 5) ~ "Spring",
        Month %in% c(6, 7, 8) ~ "Summer",
        Month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ "Winter"
      ),
      Darkness = OCC_HOUR >= 18 | OCC_HOUR < 6
    ) %>%
    transmute(
      Crime_Category = MCI_CATEGORY,
      Robbery,
      Occurrence_Time,
      Year = OCC_YEAR,
      Month,
      Day_Of_Week_Numeric,
      Hour = OCC_HOUR,
      Neighbourhood = NEIGHBOURHOOD_158,
      Location_Type = LOCATION_TYPE,
      Premises_Type = PREMISES_TYPE,
      Longitude = LONG_WGS84,
      Latitude = LAT_WGS84,
      Season,
      Darkness
    )

  income <- read_excel(
    file.path("data", "Income_Dataset.xlsx"),
    .name_repair = "minimal"
  )

  income_year <- income[, c(
    "Neighbourhood Name",
    "Median total income in 2020  among recipients ($)",
    "Average total income in 2020 among recipients ($)",
    "Number of persons in private households",
    "One-census-family households without additional persons",
    "Couple-family households",
    "One-parent-family households",
    "Multigenerational households",
    "Multiple-census-family households",
    "One-census-family households with additional persons",
    "Two-or-more-person non-census-family households",
    "Median total income of two-or-more-person households in 2020 ($)",
    "Average total income of two-or-more-person households in 2020 ($)",
    "One-person households",
    "Median total income of one-person households in 2020 ($)",
    "Average total income of one-person households in 2020 ($)",
    "Average household size",
    "Median total income of household in 2020 ($)",
    "Average total income of household in 2020 ($)",
    "Total - Private households by tenure - 25% sample data",
    "Owner",
    "Renter",
    "Total - Labour force aged 15 years and over by class of worker including job permanency - 25% sample data",
    "Permanent position",
    "48-49 Transportation and warehousing",
    "62 Health care and social assistance"
  )]

  colnames(income_year) <- c(
    "Neighbourhood",
    "Individual_Median_Income",
    "Individual_Average_Income",
    "Population",
    "One-census-family households without additional persons",
    "Couple-family households",
    "One-parent-family households",
    "Multigenerational households",
    "Multiple-census-family households",
    "One-census-family households with additional persons",
    "Two-or-more-person non-census-family households",
    "One+_people_Household_Median_Income",
    "One+_people_Household_Average_Income",
    "Number_of_One_person_Households",
    "One_Person_Household_Median_Income",
    "One_Person_Household_Average_Income",
    "Average_Household_Size",
    "Household_Median_Income",
    "Household_Average_Income",
    "Number_of_Private_Household_by_Tenure",
    "Number_of_Owner",
    "Number_of_Rental_Properties",
    "Total_Labour_Force",
    "Permanent_Job_Position",
    "Transportation_Services_Position",
    "Healthcare_Services_Position"
  )

  household_cols <- c(
    "Multiple-census-family households",
    "One-census-family households without additional persons",
    "Couple-family households",
    "One-parent-family households",
    "Multigenerational households",
    "One-census-family households with additional persons",
    "Two-or-more-person non-census-family households"
  )

  income_year$Number_of_One_plus_people_Household_Median_Income <-
    rowSums(income_year[, household_cols], na.rm = TRUE)

  income_year <- income_year %>%
    mutate(
      Permanent_Job_and_Labour_Force_Ratio = Permanent_Job_Position / Total_Labour_Force,
      Transportation_Service_Worker_and_Population_Ratio = Transportation_Services_Position / Population,
      HealthCare_Service_Worker_and_Population_Ratio = Healthcare_Services_Position / Population,
      Difference_in_Individual_Median_Average_Income =
        abs(Individual_Average_Income - Individual_Median_Income)
    ) %>%
    select(-all_of(household_cols))

  income_year$Neighbourhood <- gsub("O`Connor Parkview", "O'Connor-Parkview", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("Danforth-East York", "Danforth East York", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("Taylor Massey", "Taylor-Massey", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("East End Danforth", "East End-Danforth", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("Cabbagetown-South St. James Town", "Cabbagetown-South St.James Town", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("North St. James Town", "North St.James Town", income_year$Neighbourhood)
  income_year$Neighbourhood <- gsub("Yonge-St. Clair", "Yonge-St.Clair", income_year$Neighbourhood)

  crime_year <- crime %>%
    filter(Year == year, Neighbourhood != "NSA") %>%
    filter(!is.na(Longitude), !is.na(Latitude))

  merged <- merge(crime_year, income_year, by = "Neighbourhood", all.x = TRUE) %>%
    na.omit()

  neighbourhood_summary <- merged %>%
    group_by(Neighbourhood) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      median_income = first(Individual_Median_Income),
      average_income = first(Individual_Average_Income),
      rentals = first(Number_of_Rental_Properties),
      population = first(Population),
      permanent_job_ratio = first(Permanent_Job_and_Labour_Force_Ratio),
      avg_longitude = mean(Longitude),
      avg_latitude = mean(Latitude),
      .groups = "drop"
    )

  premises_summary <- merged %>%
    group_by(Premises_Type) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    )

  premises_darkness_summary <- merged %>%
    group_by(Premises_Type, Darkness) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    ) %>%
    mutate(
      light_label = ifelse(Darkness, "Dark hours", "Daylight"),
      rate_pct = robbery_rate * 100
    )

  premise_hour_summary <- merged %>%
    group_by(Premises_Type, Hour) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    ) %>%
    mutate(rate_pct = robbery_rate * 100)

  monthly_summary <- merged %>%
    group_by(Month) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    ) %>%
    mutate(month_label = factor(month.abb[Month], levels = month.abb))

  hourly_summary <- merged %>%
    group_by(Hour) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    )

  daily_summary <- merged %>%
    group_by(Day_Of_Week_Numeric) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    ) %>%
    mutate(day_label = factor(
      c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[Day_Of_Week_Numeric],
      levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    ))

  seasonal_summary <- merged %>%
    group_by(Season) %>%
    summarise(
      total_incidents = n(),
      robbery_count = sum(Robbery),
      robbery_rate = mean(Robbery),
      .groups = "drop"
    ) %>%
    mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")))

  list(
    crime = crime,
    crime_year = crime_year,
    income = income_year,
    merged = merged,
    neighbourhood_summary = neighbourhood_summary,
    premises_summary = premises_summary,
    premises_darkness_summary = premises_darkness_summary,
    premise_hour_summary = premise_hour_summary,
    monthly_summary = monthly_summary,
    hourly_summary = hourly_summary,
    daily_summary = daily_summary,
    seasonal_summary = seasonal_summary
  )
}
