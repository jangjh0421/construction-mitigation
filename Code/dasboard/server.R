function(input, output){
  
  # Monthly Visits Filter
  monthlyData = reactive({
    monthlyFiltered = ff_monthly %>%
      filter(Area == str_replace_all(input$bia, " ", ""))
  })
  
  # Day of Week Visits Filter
  dayOfWeekData = reactive({
    dayOfWeekFiltered = ff_day_of_week %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
  })
  
  # Time of Day Visits Filter
  timeOfDayData = reactive({
    timeOfDayFiltered = ff_time_of_day %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
  })
  
  # Visitor Type Filter
  visitorTypeData = reactive({
    visitorTypeFiltered = ff_type %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) 
  })
  
  # Visitor Level Summary
  visitorLevelData = reactive({
    visitorLevelFiltered = ff_quarter %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
               (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) %>%
      select(time_window, Change)
  })
  
  
  # Monthly Visits Plot
  output$MonthlyPlot = renderPlot({
  
    # generate the plot
    ggplot(monthlyData(), aes(x = date, y = Percentage)) +
      geom_hline(yintercept = 100, color = "#000000") +
      geom_line(size = 1.5, color = "#00AEF3") +
      ylim(0, 180) +
      labs(x = "Month", y = "Percentage (%)") +
      scale_x_date(limits = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2024-01-01", "%Y-%m-%d"), date_breaks = "3 month", date_labels = "%b %Y")) +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_text(size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 30),
        axis.text.y = element_text(size = 7),
        legend.position = "none")
    
  })
  
  # Day of Week Plot
  output$dayOfWeekPlot = renderPlot({
    
    # generate the plot
    ggplot(dayOfWeekData(), aes(x = factor(Day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
                               y = Visits, fill = as.character(Year))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.7) +
      scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Day of the Week", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_text(size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(c(0,0,0,0)),
        legend.position = "bottom")
  })
  
  # Time of Day Plot
  output$timeOfDayPlot = renderPlot({
    
    # generate the plot
    ggplot(timeOfDayData(), aes(x = factor(Time, levels = c("Early Morning: 12am - 6am", "Morning: 6am - 12pm", "Afternoon: 12pm - 6pm", "Evening: 6pm - 12am")),
                               y = Visits, fill = as.character(Year))) +
      geom_bar(position = "dodge", stat = "identity", width = 0.9) +
      scale_color_manual(values = c("#002A41", "#00AEF3"), aesthetics = c("fill", "color")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Time of Day", y = "Visits (Thousands)") +
      theme(
        panel.background = element_rect(fill = 'transparent', colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray80'),
        plot.background = element_rect(fill = 'transparent', colour = NA),
        plot.title = element_text(size = 9),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle = 7),
        legend.position = "none")
  })
  
  # Visitor Type Plot
  output$VisitorTypePlot = renderPlot({
    ggplot(visitorTypeData(), aes(x = as.character(Year),
                            y = Count, fill = factor(Type, levels = c("Infrequent Visitor", "Recurring Visitor", "Resident")))) +
      geom_bar(position = "stack",  stat = "identity", width = 0.7) +
      scale_color_manual(values = c("#CC2936", "#002A41", "#00AEF3"), aesthetics = c("colour", "fill")) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
      labs(x = "Year", y = "Visits (Thousands)") +
      theme(panel.background = element_rect(fill = 'transparent', colour = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = 'gray80'),
            plot.background = element_rect(fill = 'transparent', colour = NA),
            plot.title = element_text(size = 9),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 7),
            axis.text = element_text(size = 7),
            legend.text = element_text(size = 7),
            legend.key.size = unit(0.5, "cm"),
            legend.title = element_blank(),
            legend.margin = margin(c(0,0,0,0)),
            legend.position = "bottom")
  })
  
  # Visitor Summary Table
  output$vistorLevelsTable = renderTable({
    visitorLevelData()
  })
  
  # How to Read Visitor Levels
  output$vistordescription = renderText({
    text <- "<p>This is the first paragraph. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.</p>
    <p>This is the second paragraph. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
    <p>This is the third paragraph with bullet points:</p>
    <ul>
      <li>Bullet point 1</li>
      <li>Bullet point 2</li>
      <li>Bullet point 3</li>
    </ul>"
    
    # Return the styled text
    HTML(text)
  })
  
}

