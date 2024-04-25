function(input, output){
  
  # Monthly Visits Filter
  monthlyData = reactive({
    monthlyFiltered = ff_monthly %>%
      filter(Area == str_replace_all(input$bia, " ", ""))
    
    return(monthlyData)
  })
  
  # Day of Week Visits Filter
  dayOfWeekData = reactive({
    dayOfWeekFiltered = ff_day_of_week %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
    
    return(dayOfWeekFiltered)
  })
  
  # Time of Day Visits Filter
  timeOfDayData = reactive({
    timeOfDayFiltered = ff_time_of_day %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1))
    
    return(timeOfDayFiltered)
  })
  
  # Visitor Type Filter
  visitorTypeData = reactive({
    visitorTypeFiltered = ff_type %>%
      filter((Name == str_replace_all(input$bia, " ", "") & Quarter == input$quarter) &
             (Year == as.numeric(input$year) | Year == as.numeric(input$year) - 1)) 
  })
  
  
  # Monthly Visits Plot
  output$MonthlyPlot = renderPlot({
  
    # generate the plot
    plot = ggplot(monthlyData, aes(x = date, y = percentage)) +
      geom_hline(yintercept = 100, color = "#000000") +
      geom_line(size = 1.5, color = "#00AEF3") +
      ylim(0, 180) +
      labs(title = "Visitor Levels (%) Relative to 2019", x = "Month", y = "Percentage (%)") +
      scale_x_date(limits = c("2020-01-01", max(visitsMonthly$date)), date_breaks = "3 month", date_labels = "%b %Y") +
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
    plot
  })
  
  
}