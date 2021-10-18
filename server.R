library(tidyverse)


server <- function(input, output) {
  
  
  dt <- read.csv("https://lter.kbs.msu.edu/datatables/7.csv",
                 sep = ",", comment.char='#', stringsAsFactors = FALSE)
  
  #dt <- read_csv("https://lter.kbs.msu.edu/datatables/7.csv", comment = "#")
  dt$date <- lubridate::ymd(dt$date)
  dt$year <- lubridate::year(base::as.Date(dt$date, format = "%m/%d/%y"))
  dt$month <- lubridate::month(base::as.Date(dt$date, format = "%m/%d/%y"))
  dt$precipitation <- as.numeric(dt$precipitation)
  dt$temperature <- as.numeric(dt$air_temp_mean)
  
  output$allPlot <- renderPlot({
    
    ggplot(dt, aes(date, precipitation)) + geom_point()
    mon <- input$mt
    
    if(input$var == "precipitation") {
      D <- dt %>% 
        dplyr::select(date,precipitation) %>%
        na.omit() %>%
        mutate(year_month = zoo::as.yearmon(format(base::as.Date(date, format = "%m/%d/%y"), "%Y-%m"))) %>%
        group_by(year_month) %>%
        summarize(mean_monthly = sum(as.numeric(precipitation))) %>%
        mutate(month = substring(year_month, 1, 3)) 
      
      cols = c("all months" = "black", "highlighted month" = "red")
      ggplot(data = D, aes(year_month, 
                           mean_monthly)) +
        
        geom_point(size = 1
                   , aes(colour = "all months")
        ) +
        geom_line(aes(year_month, mean_monthly, group = 1)) +
        geom_point(subset(D, month == mon), 
   
                   mapping = aes(year_month, mean_monthly,
                                 colour = "highlighted months")) +
        xlab("time") +
        theme_bw() +
        ylab("monthly precipitation (mm)") +
        ggtitle("KBS monthly precipitation") +
  
        scale_color_manual(
          name = "month(s)",
          values = c("all months" = "black", "highlighted months" = "red")
        ) +
        theme(
          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top")
        ) 
      
    }
    else if(input$var == "temperature") {
      options(dplyr.summarise.inform = FALSE)
      D <- dt %>% 
        dplyr::select(date,temperature) %>%
        na.omit() %>%
        dplyr::mutate(year_month = zoo::as.yearmon(format(base::as.Date(date, format = "%m/%d/%y"), "%Y-%m"))) %>%
        dplyr::group_by(year_month) %>%
        dplyr::summarize(mean_monthly = mean(as.numeric(temperature))) %>%
        dplyr::mutate(month = substring(year_month, 1, 3))
      
      ggplot(D, aes(year_month, 
                    mean_monthly)) +
        geom_point(size = 1, aes(colour = "all months")) +
        geom_line(aes(year_month, mean_monthly, group = 1)) + 
        geom_point(subset(D, month == mon), 
     
                   mapping = aes(year_month, mean_monthly,
                                 colour = "highlighted months")) + 
        xlab("time") +
        theme_bw() +
        ylab("mean monthly temperature") +
        ggtitle("KBS monthly temperature") +
        scale_color_manual(
        name = "month(s)",
          values = c("all months" = "black", "highlighted months" = "red")
        ) +
        theme(
          legend.position = c(0.95, 0.98),
          legend.justification = c("right", "top")
        ) +
        ylim(c(-15, 30))
    }
  }
    )
  
  output$comparePlot <- renderPlot({
  
    yr <- input$year
    options(dplyr.summarise.inform = FALSE)
    
    dt1 <- dt %>%
      dplyr::select(date, year, month, precipitation, temperature) %>%
      na.omit() %>%
      mutate(year_month = zoo::as.yearmon(format(base::as.Date(date, format = "%m/%d/%y"), "%Y-%m"))) %>%
      group_by(year_month) %>%
      summarize(sum_monthly_precipitation_by_year = sum(precipitation),
                mean_monthly_temperature_by_year = mean(temperature)) %>%
      mutate(month = substring(year_month, 1, 3))%>%
      group_by(month) %>%
      summarise(mean_monthly_precipitation = mean(sum_monthly_precipitation_by_year),
                se_monthly_precipitation = sd(sum_monthly_precipitation_by_year)/sqrt(n()),
                mean_monthly_temperature = mean(mean_monthly_temperature_by_year),
                se_monthly_temperature = sd(mean_monthly_temperature_by_year)/sqrt(n())) %>%
      mutate(YEAR = '1988-2021',
             month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 'Jun',
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
      select(YEAR, month, mean_monthly_precipitation, se_monthly_precipitation,
             mean_monthly_temperature, se_monthly_temperature)
    
    dt2 <- dt %>%
       dplyr::filter(year == yr) %>%
       dplyr::select(date, year, month, precipitation, temperature) %>%
      na.omit() %>%
       mutate(year_month = zoo::as.yearmon(format(base::as.Date(date, format = "%m/%d/%y")), "%Y-%m")) %>%
       group_by(year_month) %>%
      summarize(sum_monthly_precipitation_by_year = sum(precipitation),
                mean_monthly_temperature_by_year = mean(temperature)) %>%
      mutate(month = substring(year_month, 1, 3))%>%
      group_by(month) %>%
      summarise(mean_monthly_precipitation = mean(sum_monthly_precipitation_by_year),
                se_monthly_precipitation = sd(sum_monthly_precipitation_by_year)/sqrt(n()),
                mean_monthly_temperature = mean(mean_monthly_temperature_by_year),
                se_monthly_temperature = sd(mean_monthly_temperature_by_year)/sqrt(n())) %>%
      mutate(YEAR = yr,
             month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", 'Jun',
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
      select(YEAR, month, mean_monthly_precipitation, se_monthly_precipitation,
             mean_monthly_temperature, se_monthly_temperature)
    
   big <- rbind(dt1, dt2)
    
    
    if(input$var == "precipitation") {
      big %>% 
     ggplot(aes(month, mean_monthly_precipitation, group = YEAR)) +
       geom_point(size = 2.5, aes(color = YEAR)) +
        geom_line(aes(linetype = YEAR, color = YEAR), size = 2) +
        theme_bw() +
       geom_errorbar(data = subset(big, YEAR == "1988-2021"), 
                      aes(ymin=mean_monthly_precipitation - se_monthly_precipitation,
                          ymax=mean_monthly_precipitation + se_monthly_precipitation,
                          color = YEAR), width=.2,
                      position=position_dodge(.9),
                      size = 0.6) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        xlab("month") +
        ylab("monthly precipitation (mm)") 
      
     } 
   else if(input$var == "temperature") {
      big  %>%
      mutate(month = factor(month,
                              levels = c("Jan", "Feb", "Mar",
                                         "Apr", "May", "Jun",
                                         "Jul", "Aug", "Sep",
                                         "Oct", "Nov", "Dec"))) %>%
        ggplot(aes(month, mean_monthly_temperature, group = YEAR)) +
        geom_point(size = 2.5, aes(color = YEAR)) +
        geom_line(aes(linetype = YEAR, color = YEAR), size = 2) +
        geom_errorbar(data = subset(big, YEAR == "1988-2021"), 
                      aes(ymin=mean_monthly_temperature - se_monthly_temperature,
                          ymax=mean_monthly_temperature + se_monthly_temperature,
                          color = YEAR), width=.2,
                      position=position_dodge(.9),
                      size = 0.3) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        theme_bw() +
        xlab("month") +
        ylab("mean monthly temperature (°C)") 
    }
}
)
  
  output$selected_year <- renderText({
    paste("You have seleccted", input$year)
  })
}