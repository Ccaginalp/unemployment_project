#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)

function(input, output, session) {
    
    library(shiny)
    library(shinyWidgets)
    library(shinydashboard)
    library(dplyr)
    library(rio)
    library(openxlsx)
    library(tidyverse)
    library(lubridate)
    library(zoo)
    library(viridis)
    library(glue)
    library(usmap)
    
    # Define UI for application that draws a histogram
    state_choices <- c("All", state.abb)
    early_unemp_states <- c("AK", "MO", "MS", "IA", "AL", "ID", "IN", "NE",
                            "NH", "ND", "WV", "WY", "AR", "FL", "GA", "MO",
                            "OH", "OK", "SC", "SD", "TX", "UT", "MD", "TN",
                            "AZ", "LA")
    cluster_legend <- c("Benefit end status")
    
    avg_data <- rio::import("https://www.bls.gov/lau/laucnty19.xlsx")
    older_monthly_data <- rio::import("https://raw.githubusercontent.com/Ccaginalp/unemployment_project/master/data/Apr20_May21.xlsx")
    new_monthly_data <- rio::import("https://www.bls.gov/web/metro/laucntycur14.zip")
    old_periods <- unique(older_monthly_data[, 5])
    new_monthly_data <- new_monthly_data %>% 
        filter(!new_monthly_data[, 5] %in% old_periods)
    colnames(new_monthly_data) <- colnames(older_monthly_data)
    monthly_data <- rbind(older_monthly_data, new_monthly_data)
    
    # Cleaning data -----------------------------------------------------------
    
    colnames(monthly_data) <- c("Series", "StateCode", "CountyCode", "CountyState", 
                                "Period", "Force", "Employed", "Unemployed", "Percent")
    monthly_data <- monthly_data %>% 
        tail(nrow(monthly_data) - 5) %>% 
        mutate(Period = as.yearmon(Period, "%b-%y"),
               Unemployed = as.numeric(Unemployed),
               Percent = as.numeric(Percent),
               Force = as.numeric(Force),
               LogForce = log(Force, base = 10))
    
    colnames(avg_data) <- c("Series", "StateCode", "CountyCode", "CountyState", 
                            "Period", "X", "Force", "Employed", "Unemployed", "Percent")
    avg_data <- avg_data %>% 
        tail(nrow(avg_data) - 5) %>% 
        mutate(Period = as.yearmon(Period, "%y"),
               Unemployed = as.numeric(Unemployed),
               Employ19 = as.numeric(Percent),
               Force = as.numeric(Force),
               LogForce = log(Force, base = 10)) %>% 
        select(CountyState, Employ19)
    
    getSize <- monthly_data %>%
        filter(!is.na(LogForce)) %>% 
        group_by(CountyState) %>% 
        summarise(AvgSizeRoundNum = 10 ** floor(mean(LogForce)),
                  AvgSize = mean(LogForce),
                  PeakUnemp = max(Percent)) %>% 
        mutate(AvgSizeRound = format(AvgSizeRoundNum, big.mark = ",")) %>% 
        mutate(AvgSizeRound = as.factor(AvgSizeRound))
    getSize$AvgSizeRound <- factor(getSize$AvgSizeRound, labels = sort(unique(getSize$AvgSizeRound) %>% 
                                                                           gsub(",", "", .) %>% 
                                                                           as.numeric() %>% 
                                                                           format(., big.mark = ",")))
    # The things we do for properly formatted labels!
    
    monthly_data <- monthly_data %>% 
        left_join(getSize, by = "CountyState") %>% 
        left_join(avg_data, by = "CountyState") %>% 
        separate(CountyState, c("County", "State"), sep = ",") %>% 
        mutate(State = trimws(State),
               Recovery = 100 * (PeakUnemp - Percent) / pmax(PeakUnemp - Employ19, 1)) %>% 
        filter(State != "PR")
    
    
    # source("../../clean/1_download_data.R")
    # source("../../clean/2_clean_data.R")
    # source("../../scripts/1_read_data.R")
    # source("../../scripts/2_set_initial_vars.R")
    state_choices <- c(sort(c(state.abb, "DC")))
    
    
    
    select_state <- reactive({
        x <- c(input$checka, input$checkb, input$checkc)
    })
    
    
    filtered_monthly_data <- reactive({
        monthly_data %>% 
            filter(State %in% select_state()) %>% 
            filter(!is.na(AvgSizeRound)) %>% 
            group_by(Period, AvgSizeRound) %>% 
            summarise(AggPercent = sum(100 * Unemployed) / sum(Force),
                      AvgPercent = sum(Force * Employ19) / sum(Force))
    })
    
    state_group_sizes <- reactive({
        county_sizes_tracker <- monthly_data %>% 
            filter(State %in% select_state()) %>%
            filter(!is.na(AvgSizeRound)) %>% 
            mutate(fips = str_c(StateCode, CountyCode))
        county_sizes_tracker$group_name <- county_sizes_tracker$AvgSizeRound
        county_sizes_tracker
    })
    
    
    
    output$state_group_plot <- renderPlot({
        if(length(select_state()) > 10){
            plot_usmap()
        } else{
            plot_usmap(data = state_group_sizes(), 
                       regions = "counties",
                       values = "group_name", 
                       color = "blue",
                       include = unique(select_state())) +
                scale_fill_discrete(name = "County group")
        }
        
    })
    
    output$unemp_plot <- renderPlot({
        filtered_monthly_data() %>% 
            ggplot() +
            geom_line(aes(Period, AggPercent, group = AvgSizeRound, col = as.factor(AvgSizeRound)), size = 1) +
            geom_point(aes(Period, AggPercent, group = AvgSizeRound, col = as.factor(AvgSizeRound)), size = 2) +
            geom_hline(aes(yintercept = AvgPercent, col = as.factor(AvgSizeRound))) +
            scale_color_viridis(discrete=TRUE) +
            labs(x = "Time",
                 y = "Unemployment [%]",
                 color = "Labor force size") +
            scale_x_yearmon(format = "%h'%y",
                            breaks = unique(monthly_data$Period)) +
            scale_y_continuous(breaks = c(seq(2, 20, by = 2)))
    })
    
    observe({
        if(input$selectall == 0) return(NULL)
        else if (input$selectall%%2 == 1)
        {
            updateCheckboxGroupInput(session, "checka", NULL, choices = state_choices[1:17])
            updateCheckboxGroupInput(session, "checkb", NULL, choices = state_choices[18:34])
            updateCheckboxGroupInput(session, "checkc", NULL, choices = state_choices[35:51])
        }
        else
        {
            updateCheckboxGroupInput(session, "checka", NULL, choices = state_choices[1:17],
                                     selected = state_choices[1:17])
            updateCheckboxGroupInput(session, "checkb", NULL, choices = state_choices[18:34],
                                     selected = state_choices[18:34])
            updateCheckboxGroupInput(session, "checkc", NULL, choices = state_choices[35:51],
                                     selected = state_choices[35:51])
        }
    })
    
    #### Second tab
    
    # select_state2 <- reactive({
    #     input$county_state
    # })
    
    filtered_monthly_data2 <- reactive({
        monthly_data %>% 
            filter(State == input$county_state)
    })
    
    output$county_UI <- renderUI({
        selectInput("county", "County",
                    choices = c("Statewide", unique(filtered_monthly_data2()$County)),
                    selected = "NA"
                    )
    })
    
    county_data <- reactive({
        if (input$county == "Statewide"){
            filtered_monthly_data2() %>% 
                group_by(State, Period) %>% 
                summarise(Percent = sum(Force * Percent) / sum(Force))
        } else {
            filtered_monthly_data2() %>% 
                filter(County == input$county)
        }
    })
    
    
    
    county_plot_title <- reactive({
        if (input$county == "Statewide"){
            glue("Unemployment for {input$county_state} (statewide)")
        }
        else {
            glue("Unemployment for {input$county}, {input$county_state}")
        }
    })
    
    
    output$unemp_county_plot <- renderPlot({
        county_data() %>% 
            ggplot() +
            geom_line(aes(Period, Percent), size = 1) +
            geom_point(aes(Period, Percent), size = 2) +
            scale_color_viridis(discrete=TRUE) +
            labs(x = "Time",
                 y = "Unemployment [%]",
                 color = "Log_10 labor force",
                 title = county_plot_title()) +
            scale_x_yearmon(format = "%h'%y",
                            breaks = unique(monthly_data$Period)) +
            # scale_x_date(labels = date_format("%h-%y"),
            #              breaks = seq(from = min(county_data()$Period),
            #                           to = max(county_data()$Period),
            #                           by = "month")) +
            scale_y_continuous(breaks = c(seq(2, 20, by = 2)))
    })
    
    ## Second tab: county recovery
    
    
    filtered_monthly_data3 <- reactive({
        monthly_data %>% 
            filter(State == input$county_state_recov)
    })
    
    output$county_UI_recov <- renderUI({
        selectInput("county_recov", "County",
                    choices = c("Statewide", unique(filtered_monthly_data3()$County)),
                    selected = "Statewide"
        )
    })
    
    county_recov_data <- reactive({
        if (input$county_recov == "Statewide"){
            filtered_monthly_data3() %>% 
                group_by(State, Period) %>% 
                summarise(StatePercent = sum(Force * Percent) / sum(Force),
                          StatePeak = sum(Force * PeakUnemp) / sum(Force),
                          StateEmploy19 = sum(Force * Employ19) / sum(Force)) %>% 
                mutate(Recovery = 100 * (StatePeak - StatePercent) / (StatePeak - StateEmploy19))
        } else {
            filtered_monthly_data3() %>% 
                filter(County == input$county_recov)
        }
    })
    
    recov_plot_title <- reactive({
        if (input$county_recov == "Statewide"){
            glue("Recovery from peak unemployment for {input$county_state_recov} (statewide)")
        }
        else {
            glue("Recovery from peak unemployment for {input$county_recov}, {input$count_state_recov}")
        }
    })
    
    
    output$county_recov_plot <- renderPlot({
        county_recov_data() %>% 
            ggplot() +
            geom_line(aes(Period, Recovery), size = 1) +
            geom_point(aes(Period, Recovery), size = 2) +
            scale_color_viridis(discrete=TRUE) +
            labs(x = "Time",
                 y = "Recovery from Peak Unemployment [%]",
                 color = "Log_10 pop size",
                 title = recov_plot_title()) +
            scale_x_yearmon(format = "%h'%y",
                            breaks = unique(monthly_data$Period)) +
            scale_y_continuous(breaks = c(seq(0, 200, by = 5)))
    })
    

    
    #### Tab 3: 
    
    state_group <- reactive({
        all_states <- monthly_data %>% 
            mutate(fips = str_c(StateCode, CountyCode)) %>% 
            filter(Force < input$county_size_cluster[[2]] %>% gsub(",", "", .) %>% as.numeric() ,
                   Force > input$county_size_cluster[[1]] %>% gsub(",", "", .) %>% as.numeric())
        all_states$group_name <- ifelse(all_states$State %in% early_unemp_states,
                                        "Early",
                                        "Not early")
        all_states
    })
    
    
    
    output$recovery_state_plot <- renderPlot({
        plot_usmap(data = state_group(), 
                   regions = "counties",
                   values = "group_name", 
                   color = "blue") +
            scale_fill_discrete(name = "State group")
    })
    
    grouped_monthly_data <- reactive({
        if (input$cluster_state == "Early benefit end status"){
            grouped_data <- monthly_data %>% 
                filter(Force < input$county_size_cluster[[2]] %>% gsub(",", "", .) %>% as.numeric() ,
                       Force > input$county_size_cluster[[1]] %>% gsub(",", "", .) %>% as.numeric()) %>% 
                mutate(Grouping = ifelse(State %in% early_unemp_states,
                                      "Early",
                                      "Not Early")) %>% 
                group_by(Grouping, Period) %>% 
                summarise(StatePercent = sum(Force * Percent) / sum(Force),
                          StateEmploy19 = sum(Force * Employ19) / sum(Force), na.rm = TRUE) 
            peaks <- grouped_data %>% 
                group_by(Grouping) %>% 
                summarise(StatePeak = max(StatePercent))
            grouped_data %>%
                left_join(peaks, by = "Grouping") %>% 
                mutate(Recovery = 100 * (StatePeak - StatePercent) / max(StatePeak - StateEmploy19, 1))
        }
    })
    
    
    
    output$cluster_recov_plot <- renderPlot({
        grouped_monthly_data() %>% 
            ggplot() +
            geom_line(aes(Period, Recovery, col = Grouping), size = 1) +
            geom_point(aes(Period, Recovery, col = Grouping), size = 2) +
            # geom_smooth(aes(Period, Recovery, col = Grouping), se = FALSE, method = "gam", linetype = "dashed") +
            scale_color_viridis(discrete=TRUE) +
            labs(x = "Time",
                 y = "Recovery from Peak Unemployment [%]",
                 color = cluster_legend[[1]],
                 title = glue("Recovery for states grouped by {}")) +
            scale_x_yearmon(format = "%h'%y",
                            breaks = unique(monthly_data$Period)) +
            scale_y_continuous(breaks = c(seq(0, 200, by = 5)))
    })

        
    output$cluster_recov_table <- renderTable({
        grouped_monthly_data() %>%
            select(Grouping, Period, Recovery) %>%
            mutate(Period = as.character(Period)) %>% 
            pivot_wider(names_from = "Grouping", values_from = "Recovery") %>% 
            mutate(across(where(is.numeric), ~lag(.x, 0) - lag(.x, 1), .names = "Change {.col}"))
    },
    striped = TRUE,
    digits = 1)
}