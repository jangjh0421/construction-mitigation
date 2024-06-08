function(input, output, session) {
  
  ## SELECT INPUT REACTIVITY --------------------------------------------------
  input_choices = reactive({
    if(input$year == "2024"){
      return(c("Q1"))
    } else {
      return(c("Q1", "Q2", "Q3", "Q4"))
    }
  })
  
  observeEvent(input$year, {
    freezeReactiveValue(input, "quarter")
    updateSelectInput(session = session, inputId = "quarter", choices = input_choices())
  })
  
  
  
  
  
  ## CHART REACTIVITY ---------------------------------------------------------
  
  output$selectedBIA <- renderText({
    paste(input$bia)
  })
  
  
  ## VISITOR LEVELS TAB
  
  # Monthly Visits Filter
  monthlyData = reactive({
    req(input$quarter)
    # filter the data based on the input data
    if (input$quarter == "Q1") {
      target_end_date = ymd(paste0(input$year, "03", "31"))
    } else if (input$quarter == "Q2") {
      target_end_date = ymd(paste0(input$year, "06", "30"))
    } else if (input$quarter == "Q3") {
      target_end_date = ymd(paste0(input$year, "09", "30"))
    } else {
      target_end_date = ymd(paste0(input$year, "12", "31"))
    }
    
    # filter the data based on the BIA
    monthlyFiltered = ff_monthly %>%
      filter(Name == str_replace_all(input$bia, " ", "") &
               Date < target_end_date)
  })
  
  # Day of Week Visits Filter
  dayOfWeekData = reactive({
    dayOfWeekFiltered = ff_day_of_week %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year) |
            Year == as.numeric(input$year) - 1
        )
      )
  })
  
  # Time of Day Visits Filter
  timeOfDayData = reactive({
    timeOfDayFiltered = ff_time_of_day %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year) |
            Year == as.numeric(input$year) - 1
        )
      )
  })
  
  # Visitor Type Filter
  visitorTypeData = reactive({
    visitorTypeFiltered = ff_type %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year) |
            Year == as.numeric(input$year) - 1
        )
      )
  })
  
  # Visitor Level Summary
  visitorLevelData = reactive({
    visitorLevelFiltered = ff_quarter %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year)
        )
      ) %>%
      select(time_window, Change) %>%
      rename("Period" = time_window, "Percent Change" = Change)
  })
  
  
  ## COMMERCIAL REAL ESTATE TAB
  
  # Vacancy Rate Summary
  vacancyRateData = reactive({
    vacancyRateFiltered = vacancyrate %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year)
        )
      ) %>%
      select(targetvacancy, controlvacancy, yoy_growth) %>%
      rename(
        "Current Year" = targetvacancy,
        "Previous Year" = controlvacancy,
        "Percent Change" = yoy_growth
      )
  })
  
  # Monthly Rent Summary
  monthlyRentData = reactive({
    monthlyRentFiltered = marketrent %>%
      filter((
        Name == str_replace_all(input$bia, " ", "") &
          Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year)
        )
      ) %>%
      select(targetrent, controlrent, yoy_growth) %>%
      rename(
        "Current Year" = targetrent,
        "Previous Year" = controlrent,
        "Percent Change" = yoy_growth
      )
  })
  
  # Retail Map Filter
  biaPolygonData = reactive({
    biaPolygonFiltered = BIAs_shp %>%
      filter(layer == str_replace_all(input$bia, " ", ""))
  })
  
  businessPointData = reactive({
    businessPointData = Business_shp %>%
      filter(layer == str_replace_all(input$bia, " ", ""))
  })
  
  # Office Vacancy Filter
  officeVacancyData = reactive({
    officeVacancyFiltered = cot_office_vacancy %>%
      filter((
        Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year)
        )
      ) %>%
      select(target, control, yoy_growth) %>%
      rename(
        "Current Year" = target,
        "Previous Year" = control,
        "Percent Change" = yoy_growth
      )
  })
  
  # Unemployment Filter
  unemploymentData = reactive({
    unemploymentFiltered = cot_unemployment %>%
      filter((
        Quarter == input$quarter
      ) &
        (
          Year == as.numeric(input$year)
        )
      ) %>%
      select(target, control, yoy_growth) %>%
      rename(
        "Current Year" = target,
        "Previous Year" = control,
        "Percent Change" = yoy_growth
      )
  })
  
  
  
  
  ## CHART VISUALS ------------------------------------------------------------
  
  
  ## Visitor Levels -----------------------------------------------------------
  
  # Monthly Visits Plot
  output$monthlyPlot = renderPlotly({
    # generate the plot
    plotMonthlyVisits = ggplot(monthlyData(), aes(x = Date, y = Percentage)) +
      geom_hline(size = 0.5,
                 yintercept = 100,
                 color = "gray50") +
      geom_line(size = 1, color = "#00AEF3") +
      ylim(0, 175) +
      labs(x = "Month", y = "Percentage (%)") +
      scale_x_date(
        limits = c(
          as.Date("2020-01-01", "%Y-%m-%d"),
          as.Date("2024-04-01", "%Y-%m-%d")
        ),
        date_breaks = "3 month",
        date_labels = "%b %Y"
      ) +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "none"
      )
    
    # convert into a plotly
    ggplotly(plotMonthlyVisits, dynamicTicks = TRUE) %>%
      config(displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE))
  })
  
  # Day of Week Plot
  output$dayOfWeekPlot = renderPlotly({
    # generate the plot
    plotDayofWeek = ggplot(dayOfWeekData(),
                           aes(
                             x = factor(
                               Day,
                               levels = c(
                                 "Sunday",
                                 "Monday",
                                 "Tuesday",
                                 "Wednesday",
                                 "Thursday",
                                 "Friday",
                                 "Saturday"
                               )
                             ),
                             y = Visits,
                             fill = as.character(Year)
                           )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = 0.7) +
      scale_fill_manual(values = c("#002A41", "#00AEF3")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Day of the Week", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)
      )
    
    ggplotly(plotDayofWeek, tooltip = c("y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          xref = "container"
        ),
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE)
      )
    
  })
  
  # Time of Day Plot
  output$timeOfDayPlot = renderPlotly({
    # generate the plot
    plotTimeofDay = ggplot(timeOfDayData(),
                           aes(
                             x = factor(
                               Time,
                               levels = c(
                                 "Early Morning: 12am - 6am",
                                 "Morning: 6am - 12pm",
                                 "Afternoon: 12pm - 6pm",
                                 "Evening: 6pm - 12am"
                               )
                             ),
                             y = Visits,
                             fill = as.character(Year)
                           )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = 0.8) +
      scale_fill_manual(values = c("#002A41", "#00AEF3")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Time of Day", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8)
      )
    
    # convert into a plotly
    ggplotly(plotTimeofDay, tooltip = c("y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          xref = "container"
        ),
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE),
        margin = list(autoexpand = TRUE)
      )
    
  })
  
  # Visitor Type Plot
  output$visitorTypePlot = renderPlotly({
    # generate the plot
    plotVisitorType = ggplot(visitorTypeData(),
                             aes(
                               x = as.character(Year),
                               y = Visits,
                               fill = factor(
                                 Type,
                                 levels = c("Infrequent Visitor", "Recurring Visitor", "Resident")
                               )
                             )) +
      geom_bar(position = "stack",
               stat = "identity",
               width = 0.7) +
      scale_fill_manual(values = c("#CC2936", "#002A41", "#00AEF3")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Year", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8)
      )
    
    # convert to plotly
    ggplotly(plotVisitorType, tooltip = c("y")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        legend = list(
          font = list(size = 12),
          title = "",
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          xaxis = "container"
        ),
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE),
        margin = list(autoexpand = TRUE)
      )
  })
  
  output$visitorLevelsTable = gt::render_gt({
    visitorLevelData() %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c('Percent Change'),
        decimals = 1,
        scale_values = FALSE
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#43b171"),
        locations = gt::cells_body(
          columns = c('Percent Change'),
          rows = visitorLevelData()[['Percent Change']] > 0
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#CC2936"),
        locations = gt::cells_body(
          columns = c('Percent Change'),
          rows = visitorLevelData()[['Percent Change']] < 0
        )
      ) %>%
      gt::tab_options(table.width = gt::pct(95)) %>%
      gt::opt_table_font(font = list("sans-serif"))
  })
  
  
  
  
  ## Commercial Real Estate ---------------------------------------------------
  
  # Vacancy Rate Summary Table
  output$vacancyRateTable = gt::render_gt({
    vacancyRateData() %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c('Percent Change', 'Current Year', 'Previous Year'),
        decimals = 1,
        scale_values = FALSE
      ) %>%
      gt::tab_options(table.width = gt::pct(95)) %>%
      gt::opt_table_font(font = list("sans-serif"))
  })
  
  # Monthly Rent Summary Table
  output$monthlyRentTable = gt::render_gt({
    monthlyRentData() %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c('Percent Change'),
        decimals = 1,
        scale_values = FALSE
      ) %>%
      gt::fmt_currency(columns = c('Current Year', 'Previous Year'),
                       decimals = 1) %>%
      gt::tab_options(table.width = gt::pct(95)) %>%
      gt::opt_table_font(font = list("sans-serif"))
  })
  
  
  # Office Vacancy Summary Table
  output$officeVacancyTable = gt::render_gt({
    officeVacancyData() %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c('Percent Change', 'Current Year', 'Previous Year'),
        decimals = 1,
        scale_values = FALSE
      ) %>%
      gt::tab_options(table.width = gt::pct(95)) %>%
      gt::opt_table_font(font = list("sans-serif"))
  })
  
  
  # Unemployment Summary Table
  output$unemploymentTable = gt::render_gt({
    unemploymentData() %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c('Percent Change', 'Current Year', 'Previous Year'),
        decimals = 1,
        scale_values = FALSE
      ) %>%
      gt::tab_options(table.width = gt::pct(95)) %>%
      gt::opt_table_font(font = list("sans-serif"))
  })  
      
  
  # BIA Retail Business Map
  output$retailMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = mean(biaPolygonData()$Longitude),
        lat = mean(biaPolygonData()$Latitude),
        zoom = 14
      ) %>%
      addPolygons(
        data = biaPolygonData(),
        color = "#002A41",
        weight = 2,
        fillOpacity = 0.5
      ) %>%
      addMarkers(data = businessPointData())
  })
  
  retailMapProxy <- leafletProxy("retailMap")
  
  observe({
    # Update polygons without changing the view
    fdata <- biaPolygonData()
    retailMapProxy %>%
      clearShapes() %>%
      addPolygons(
        data = fdata,
        color = "#002A41",
        weight = 2,
        fillOpacity = 0.5
      ) %>%
      addMarkers(data = businessPointData())
  })
  
}
