suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
  library(leaflet)
  library(htmlwidgets)
  library(scales)
})

build_visual_widgets <- function(output_dir = file.path("docs", "widgets")) {
  source(file.path("scripts", "prepare_project_data.R"))
  project_data <- load_project_data(year = 2020)

  merged_data <- project_data$merged
  neighbourhood_summary <- project_data$neighbourhood_summary
  premises_darkness_summary <- project_data$premises_darkness_summary
  premise_hour_summary <- project_data$premise_hour_summary
  monthly_summary <- project_data$monthly_summary
  hourly_summary <- project_data$hourly_summary

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  save_widget <- function(widget, file_name) {
    htmlwidgets::saveWidget(
      widget,
      file = file.path(output_dir, file_name),
      selfcontained = FALSE,
      libdir = "widget_libs"
    )
  }

  figure1_data <- neighbourhood_summary %>%
    mutate(
      label = ifelse(
        robbery_count >= quantile(robbery_count, 0.9),
        Neighbourhood,
        ""
      )
    )

  label_data <- figure1_data %>% filter(label != "")

  figure1 <- plot_ly(
    figure1_data,
    x = ~median_income,
    y = ~robbery_rate * 100,
    type = "scatter",
    mode = "markers",
    color = ~robbery_count,
    colors = c("#f7d9c4", "#c45c2c", "#7d2f16"),
    size = ~rentals,
    sizes = c(12, 42),
    marker = list(opacity = 0.78, line = list(color = "#fff7f0", width = 1.2)),
    customdata = ~cbind(Neighbourhood, robbery_count, rentals),
    hovertemplate = paste(
      "<b>%{customdata[1]}</b><br>",
      "Median income: $%{x:,.0f}<br>",
      "Robbery share: %{y:.1f}%<br>",
      "Robbery count: %{customdata[2]}<br>",
      "Rental properties: %{customdata[3]:,.0f}<extra></extra>"
    )
  ) %>%
    add_text(
      data = label_data,
      x = ~median_income,
      y = ~robbery_rate * 100,
      text = ~label,
      textposition = "top center",
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    layout(
      title = "Neighbourhood concentration: lower-income, rental-heavy areas carry more robbery pressure",
      xaxis = list(title = "Median individual income ($)", zeroline = FALSE),
      yaxis = list(title = "Robbery share of local incidents (%)", zeroline = FALSE),
      legend = list(title = list(text = "Robbery count"))
    )
  save_widget(figure1, "figure1.html")

  daylight <- premises_darkness_summary %>% filter(light_label == "Daylight")
  dark <- premises_darkness_summary %>% filter(light_label == "Dark hours")
  connector_data <- premises_darkness_summary %>%
    group_by(Premises_Type) %>%
    summarise(
      x = min(rate_pct),
      xend = max(rate_pct),
      .groups = "drop"
    )
  category_order <- premises_darkness_summary %>%
    group_by(Premises_Type) %>%
    summarise(max_rate = max(rate_pct), .groups = "drop") %>%
    arrange(max_rate) %>%
    pull(Premises_Type)

  figure2 <- plot_ly() %>%
    add_segments(
      data = connector_data,
      x = ~x,
      xend = ~xend,
      y = ~Premises_Type,
      yend = ~Premises_Type,
      inherit = FALSE,
      line = list(color = "rgba(31,29,26,0.25)", width = 3),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) %>%
    add_markers(
      data = daylight,
      x = ~rate_pct,
      y = ~Premises_Type,
      name = "Daylight",
      marker = list(color = "#1d6f78", size = 11),
      customdata = ~cbind(robbery_count, total_incidents),
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Daylight robbery share: %{x:.1f}%<br>",
        "Robberies: %{customdata[1]}<br>",
        "All incidents: %{customdata[2]}<extra></extra>"
      )
    ) %>%
    add_markers(
      data = dark,
      x = ~rate_pct,
      y = ~Premises_Type,
      name = "Dark hours",
      marker = list(color = "#c45c2c", size = 11),
      customdata = ~cbind(robbery_count, total_incidents),
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Dark-hour robbery share: %{x:.1f}%<br>",
        "Robberies: %{customdata[1]}<br>",
        "All incidents: %{customdata[2]}<extra></extra>"
      )
    ) %>%
    layout(
      title = "Darkness matters less than premise type, and only shifts some settings materially",
      xaxis = list(title = "Robbery share within premise type (%)"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = category_order),
      legend = list(orientation = "h", x = 0.02, y = 1.08)
    )
  save_widget(figure2, "figure2.html")

  season_order <- c("Spring", "Summer", "Fall", "Winter")
  day_levels <- 1:7
  day_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  figure3_season <- merged_data %>%
    group_by(Season, Premises_Type) %>%
    summarise(robbery_rate = mean(Robbery) * 100, .groups = "drop") %>%
    mutate(Season = factor(Season, levels = season_order))

  plot3_season <- plot_ly(
    figure3_season,
    x = ~Season,
    y = ~robbery_rate,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "Season: %{x}<br>Premise: %{fullData.name}<br>Robbery share: %{y:.1f}%<extra></extra>"
  ) %>%
    layout(
      title = "Premise-type risk by season",
      xaxis = list(title = "Season"),
      yaxis = list(title = "Robbery share (%)")
    )
  save_widget(plot3_season, "figure3_season.html")

  figure3_month <- merged_data %>%
    group_by(Month, Premises_Type) %>%
    summarise(robbery_rate = mean(Robbery) * 100, .groups = "drop") %>%
    mutate(Month = factor(Month, levels = 1:12, labels = month.abb))

  plot3_month <- plot_ly(
    figure3_month,
    x = ~Month,
    y = ~robbery_rate,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "Month: %{x}<br>Premise: %{fullData.name}<br>Robbery share: %{y:.1f}%<extra></extra>"
  ) %>%
    layout(
      title = "Premise-type risk by month",
      xaxis = list(title = "Month"),
      yaxis = list(title = "Robbery share (%)")
    )
  save_widget(plot3_month, "figure3_month.html")

  figure3_day <- merged_data %>%
    group_by(Day_Of_Week_Numeric, Premises_Type) %>%
    summarise(robbery_rate = mean(Robbery) * 100, .groups = "drop") %>%
    mutate(Day_Of_Week_Numeric = factor(Day_Of_Week_Numeric, levels = day_levels, labels = day_labels))

  plot3_day <- plot_ly(
    figure3_day,
    x = ~Day_Of_Week_Numeric,
    y = ~robbery_rate,
    color = ~Premises_Type,
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "Day: %{x}<br>Premise: %{fullData.name}<br>Robbery share: %{y:.1f}%<extra></extra>"
  ) %>%
    layout(
      title = "Premise-type risk by day of week",
      xaxis = list(title = "Day of week"),
      yaxis = list(title = "Robbery share (%)")
    )
  save_widget(plot3_day, "figure3_day.html")

  plot3_hour <- plot_ly(
    premise_hour_summary,
    x = ~Hour,
    y = ~Premises_Type,
    z = ~rate_pct,
    type = "heatmap",
    colorscale = list(
      list(0, "#fff6eb"),
      list(0.35, "#f0c7a5"),
      list(0.7, "#c45c2c"),
      list(1, "#7d2f16")
    ),
    customdata = ~cbind(robbery_count, total_incidents),
    hovertemplate = paste(
      "Hour: %{x}:00<br>",
      "Premise: %{y}<br>",
      "Robbery share: %{z:.1f}%<br>",
      "Robberies: %{customdata[1]}<br>",
      "All incidents: %{customdata[2]}<extra></extra>"
    )
  ) %>%
    layout(
      title = "Premise-type risk by hour of day",
      xaxis = list(title = "Hour of day"),
      yaxis = list(title = "")
    )
  save_widget(plot3_hour, "figure3_hour.html")

  make_total_vs_robbery_plot <- function(data, x_col, title, x_axis_title) {
    data <- data %>% mutate(robbery_pct = round(100 * robbery_count / total_incidents, 2))

    plot_ly(
      data,
      x = data[[x_col]],
      y = ~total_incidents,
      type = "bar",
      name = "All incidents",
      marker = list(color = "#e8d7c5")
    ) %>%
      add_trace(
        y = ~robbery_count,
        type = "bar",
        name = "Robbery incidents",
        marker = list(color = "#c45c2c"),
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
        barmode = "overlay",
        legend = list(orientation = "h", x = 0.02, y = 1.08)
      )
  }

  save_widget(
    make_total_vs_robbery_plot(
      project_data$seasonal_summary,
      "Season",
      "All incidents vs. robberies by season",
      "Season"
    ),
    "figure4_season.html"
  )

  save_widget(
    make_total_vs_robbery_plot(
      monthly_summary,
      "month_label",
      "All incidents vs. robberies by month",
      "Month"
    ),
    "figure4_month.html"
  )

  save_widget(
    make_total_vs_robbery_plot(
      project_data$daily_summary,
      "day_label",
      "All incidents vs. robberies by day of week",
      "Day of week"
    ),
    "figure4_day.html"
  )

  save_widget(
    make_total_vs_robbery_plot(
      hourly_summary %>% mutate(hour_label = sprintf("%02d:00", Hour)),
      "hour_label",
      "All incidents vs. robberies by hour",
      "Hour"
    ),
    "figure4_hour.html"
  )

  robbery_map_data <- neighbourhood_summary %>%
    filter(robbery_count > 0) %>%
    mutate(
      radius = rescale(robbery_count, to = c(6, 22)),
      popup = paste0(
        "<strong>", Neighbourhood, "</strong><br>",
        "Robberies: ", robbery_count, "<br>",
        "Robbery share: ", round(robbery_rate * 100, 1), "%<br>",
        "Median income: $", format(median_income, big.mark = ","), "<br>",
        "Rental properties: ", format(rentals, big.mark = ",")
      )
    )
  pal <- colorNumeric(c("#f7d9c4", "#c45c2c", "#7d2f16"), robbery_map_data$robbery_rate)

  map <- leaflet(data = robbery_map_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -79.4, lat = 43.72, zoom = 10) %>%
    addCircleMarkers(
      lng = ~avg_longitude,
      lat = ~avg_latitude,
      radius = ~radius,
      color = "#7d2f16",
      weight = 1,
      fillColor = ~pal(robbery_rate),
      fillOpacity = 0.82,
      popup = ~popup,
      stroke = TRUE
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = robbery_map_data$robbery_rate,
      title = "Robbery share"
    )

  save_widget(map, "figure5_map.html")

  invisible(output_dir)
}
