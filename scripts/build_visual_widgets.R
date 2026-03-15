suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(plotly)
  library(htmlwidgets)
})

build_visual_widgets <- function(output_dir = file.path("docs", "widgets")) {
  csv_files <- c(
    "Auto_Theft_Open_Data.csv",
    "Bicycle_Thefts_Open_Data.csv",
    "Break_and_Enter_Open_Data.csv",
    "Homicides_Open_Data_(ASR-RC-TBL-002).csv",
    "Robbery_Open_Data.csv",
    "Shooting_and_Firearm_Discharges_Open_Data.csv",
    "Theft_Over_Open_Data.csv"
  )

  crime_list <- lapply(csv_files, function(file_name) {
    read.csv(file.path("data", file_name), stringsAsFactors = FALSE)
  })
  crime <- bind_rows(crime_list, .id = "Dataset")

  income <- read_excel(file.path("data", "Income_Dataset.xlsx"))

  crime <- crime %>%
    mutate(
      Robbery = MCI_CATEGORY == "Robbery",
      OCC_Month_Numeric = match(OCC_MONTH, month.name),
      Occurrence_Time = as.POSIXct(
        paste(OCC_YEAR, OCC_Month_Numeric, OCC_DAY, OCC_HOUR, sep = "-"),
        format = "%Y-%m-%d-%H"
      ),
      Day_Of_Week_Numeric = match(
        weekdays(Occurrence_Time),
        c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
      )
    ) %>%
    select(
      MCI_CATEGORY,
      Robbery,
      Occurrence_Time,
      OCC_YEAR,
      OCC_Month_Numeric,
      Day_Of_Week_Numeric,
      OCC_HOUR,
      NEIGHBOURHOOD_158,
      LOCATION_TYPE,
      PREMISES_TYPE,
      LONG_WGS84,
      LAT_WGS84
    )

  colnames(crime) <- c(
    "Crime_Category", "Robbery", "Occurrence_Time", "Year", "Month",
    "Day_Of_Week_Numeric", "Hour", "Neighbourhood", "Location_Type",
    "Premises_Type", "Longitude", "Latitude"
  )

  crime <- crime %>%
    mutate(
      Season = case_when(
        Month %in% c(3, 4, 5) ~ "Spring",
        Month %in% c(6, 7, 8) ~ "Summer",
        Month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ "Winter"
      ),
      Darkness = ifelse(Hour >= 18 | Hour < 6, 1, 0)
    )

  crime_2020 <- crime %>%
    filter(Year == 2020, Neighbourhood != "NSA")

  income_2020 <- income[, c(
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

  colnames(income_2020) <- c(
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

  income_2020$Number_of_One_plus_people_Household_Median_Income <-
    rowSums(income_2020[, household_cols], na.rm = TRUE)

  income_2020 <- income_2020 %>%
    mutate(
      Permanent_Job_and_Labour_Force_Ratio = Permanent_Job_Position / Total_Labour_Force,
      Transportation_Service_Worker_and_Population_Ratio = Transportation_Services_Position / Population,
      HealthCare_Service_Worker_and_Population_Ratio = Healthcare_Services_Position / Population,
      Difference_in_Individual_Median_Average_Income =
        abs(Individual_Average_Income - Individual_Median_Income)
    ) %>%
    select(-all_of(household_cols))

  income_2020$Neighbourhood <- gsub("O`Connor Parkview", "O'Connor-Parkview", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("Danforth-East York", "Danforth East York", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("Taylor Massey", "Taylor-Massey", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("East End Danforth", "East End-Danforth", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("Cabbagetown-South St. James Town", "Cabbagetown-South St.James Town", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("North St. James Town", "North St.James Town", income_2020$Neighbourhood)
  income_2020$Neighbourhood <- gsub("Yonge-St. Clair", "Yonge-St.Clair", income_2020$Neighbourhood)

  merged_data <- merge(crime_2020, income_2020, by = "Neighbourhood", all.x = TRUE)
  merged_data <- na.omit(merged_data)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  save_widget <- function(widget, file_name) {
    htmlwidgets::saveWidget(
      widget,
      file = file.path(output_dir, file_name),
      selfcontained = FALSE,
      libdir = "widget_libs"
    )
  }

  figure1_data <- merged_data %>%
    group_by(Neighbourhood) %>%
    summarise(
      Permanent_Job_and_Labour_Force_Ratio = first(Permanent_Job_and_Labour_Force_Ratio),
      Individual_Median_Income = first(Individual_Median_Income),
      robbery_count = sum(Robbery),
      .groups = "drop"
    )

  figure1 <- plot_ly(
    figure1_data,
    x = ~Permanent_Job_and_Labour_Force_Ratio,
    y = ~Individual_Median_Income,
    z = ~robbery_count,
    color = ~robbery_count,
    colors = c("#FDE725", "#5DC863", "#21908C", "#3B528B", "#440154"),
    text = ~paste(
      "Neighbourhood:", Neighbourhood,
      "<br>#P.J. / #L.F.:", round(Permanent_Job_and_Labour_Force_Ratio, 3),
      "<br>Median Income:", Individual_Median_Income,
      "<br>Robbery Count:", robbery_count
    ),
    hoverinfo = "text",
    type = "scatter3d",
    mode = "markers"
  ) %>%
    layout(
      title = "2020 Toronto Neighbourhoods' Robbery Count vs. Socioeconomic Indicators",
      scene = list(
        xaxis = list(title = "Permanent Job and Labour Force Ratio"),
        yaxis = list(title = "Individ. Med. Income"),
        zaxis = list(title = "Robbery Count")
      )
    )
  save_widget(figure1, "figure1.html")

  figure2_data <- merged_data %>%
    group_by(Neighbourhood, Darkness, Premises_Type) %>%
    summarise(robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Darkness = factor(ifelse(Darkness == 1, "Dark", "Light"), levels = c("Light", "Dark")))

  figure2 <- plot_ly(
    figure2_data,
    x = ~Darkness,
    y = ~robbery_count,
    color = ~Premises_Type,
    type = "box",
    text = ~paste("Neighbourhood:", Neighbourhood),
    hoverinfo = "text+y+name"
  ) %>%
    layout(
      title = "Robbery Counts by Premises Type and Darkness",
      xaxis = list(title = "Darkness"),
      yaxis = list(title = "Robbery Counts"),
      boxmode = "group"
    )
  save_widget(figure2, "figure2.html")

  season_order <- c("Spring", "Summer", "Fall", "Winter")

  figure3_season <- merged_data %>%
    group_by(Season, Premises_Type) %>%
    summarise(robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Season = factor(Season, levels = season_order)) %>%
    arrange(Season)

  plot3_season <- plot_ly(
    figure3_season,
    x = ~Season,
    y = ~robbery_count,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Robbery Count by Premises Type and Season",
      xaxis = list(title = "Season"),
      yaxis = list(title = "Robbery Count")
    )
  save_widget(plot3_season, "figure3_season.html")

  figure3_month <- merged_data %>%
    group_by(Month, Premises_Type) %>%
    summarise(robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Month = factor(Month, levels = 1:12, labels = month.abb)) %>%
    arrange(Month)

  plot3_month <- plot_ly(
    figure3_month,
    x = ~Month,
    y = ~robbery_count,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Robbery Count by Premises Type and Month",
      xaxis = list(title = "Month"),
      yaxis = list(title = "Robbery Count")
    )
  save_widget(plot3_month, "figure3_month.html")

  day_levels <- 1:7
  day_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  figure3_day <- merged_data %>%
    group_by(Day_Of_Week_Numeric, Premises_Type) %>%
    summarise(robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Day_Of_Week_Numeric = factor(Day_Of_Week_Numeric, levels = day_levels, labels = day_labels)) %>%
    arrange(Day_Of_Week_Numeric)

  plot3_day <- plot_ly(
    figure3_day,
    x = ~Day_Of_Week_Numeric,
    y = ~robbery_count,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Robbery Count by Premises Type and Day",
      xaxis = list(title = "Day of Week"),
      yaxis = list(title = "Robbery Count")
    )
  save_widget(plot3_day, "figure3_day.html")

  figure3_hour <- merged_data %>%
    group_by(Hour, Premises_Type) %>%
    summarise(robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Hour = factor(Hour, levels = 0:23)) %>%
    arrange(Hour)

  plot3_hour <- plot_ly(
    figure3_hour,
    x = ~Hour,
    y = ~robbery_count,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Robbery Count by Premises Type and Hour of the Day",
      xaxis = list(title = "Hour of the Day"),
      yaxis = list(title = "Robbery Count")
    )
  save_widget(plot3_hour, "figure3_hour.html")

  make_total_vs_robbery_plot <- function(data, x_col, title, x_axis_title) {
    data <- data %>% mutate(robbery_pct = round(100 * robbery_count / total_crime_count, 2))

    plot_ly(data, x = data[[x_col]], y = ~total_crime_count, type = "bar", name = "Total Crime Count") %>%
      add_trace(
        y = ~robbery_count,
        type = "bar",
        name = "Robbery Count",
        opacity = 0.55,
        text = ~paste0(robbery_pct, "%"),
        hovertemplate = paste0(
          x_axis_title, ": %{x}<br>",
          "Robbery Count: %{y}<br>",
          "Robbery / Total: %{text}<extra></extra>"
        )
      ) %>%
      layout(
        title = title,
        xaxis = list(title = x_axis_title),
        yaxis = list(title = "Count"),
        barmode = "overlay"
      )
  }

  figure4_season <- merged_data %>%
    group_by(Season) %>%
    summarise(total_crime_count = n(), robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Season = factor(Season, levels = season_order)) %>%
    arrange(Season)
  save_widget(
    make_total_vs_robbery_plot(
      figure4_season,
      "Season",
      "Total Crime Counts vs. Robbery Counts by Season",
      "Season"
    ),
    "figure4_season.html"
  )

  figure4_month <- merged_data %>%
    group_by(Month) %>%
    summarise(total_crime_count = n(), robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Month = factor(Month, levels = 1:12, labels = month.abb)) %>%
    arrange(Month)
  save_widget(
    make_total_vs_robbery_plot(
      figure4_month,
      "Month",
      "Total Crime Counts vs. Robbery Counts by Month",
      "Month"
    ),
    "figure4_month.html"
  )

  figure4_day <- merged_data %>%
    group_by(Day_Of_Week_Numeric) %>%
    summarise(total_crime_count = n(), robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Day_Of_Week_Numeric = factor(Day_Of_Week_Numeric, levels = day_levels, labels = day_labels)) %>%
    arrange(Day_Of_Week_Numeric)
  save_widget(
    make_total_vs_robbery_plot(
      figure4_day,
      "Day_Of_Week_Numeric",
      "Total Crime Counts vs. Robbery Counts by Day of Week",
      "Day of Week"
    ),
    "figure4_day.html"
  )

  figure4_hour <- merged_data %>%
    group_by(Hour) %>%
    summarise(total_crime_count = n(), robbery_count = sum(Robbery), .groups = "drop") %>%
    mutate(Hour = factor(Hour, levels = 0:23)) %>%
    arrange(Hour)
  save_widget(
    make_total_vs_robbery_plot(
      figure4_hour,
      "Hour",
      "Total Crime Counts vs. Robbery Counts by Hour",
      "Hour"
    ),
    "figure4_hour.html"
  )

  robbery_data <- merged_data %>%
    filter(Robbery == TRUE) %>%
    mutate(
      Latitude_Grid = round(Latitude, 3),
      Longitude_Grid = round(Longitude, 3)
    ) %>%
    group_by(Longitude_Grid, Latitude_Grid, Premises_Type) %>%
    summarise(
      incident_count = n(),
      sample_neighbourhood = first(Neighbourhood),
      .groups = "drop"
    )

  figure5 <- plot_ly(
    robbery_data,
    type = "scattermapbox",
    lon = ~Longitude_Grid,
    lat = ~Latitude_Grid,
    color = ~Premises_Type,
    size = ~incident_count,
    sizes = c(6, 20),
    text = ~paste(
      "Neighbourhood:", sample_neighbourhood, "<br>",
      "Premises Type:", Premises_Type, "<br>",
      "Incident Count (grid):", incident_count
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title = "Map of Toronto Robbery Crimes by Premises Type in 2020",
      mapbox = list(
        style = "open-street-map",
        center = list(lon = -79.4, lat = 43.7),
        zoom = 9.5
      ),
      margin = list(l = 0, r = 0, b = 0, t = 40),
      legend = list(title = list(text = "Premises Type"))
    )
  save_widget(figure5, "figure5_map.html")

  invisible(output_dir)
}
