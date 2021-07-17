#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)

# Define UI for application that draws a histogram
vars <- setdiff(names(iris), "Species")
state_choices <- c("All", state.abb)

dashboardPage(skin = 'blue',
              dashboardHeader(title = "Unemployment Data Exploration", titleWidth = 280),
              dashboardSidebar(width = 280,  
                               sidebarMenu(
                                   menuItem("Unemployment by State/County", tabName = "county", icon = icon("th")),
                                   menuItem("Recovery by State/County", tabName = "county_recovery", icon = icon("th")),
                                   menuItem("County Size Plot", tabName = "stateplot", icon = icon("dashboard"))
                               )),
              dashboardBody(
                  tabItems(
                      # First tab content
                      tabItem(tabName = "stateplot",
                              pageWithSidebar(
                                  headerPanel('Unemployment Plot by County Size'),
                                  sidebarPanel(
                                      strong("State (click on dot to select)"),
                                      dropdownButton(
                                          label = "Select states",
                                          status = "default", width = 450,
                                          tags$label("Choose :"),
                                          fluidRow(
                                              column(
                                                  width = 4,
                                                  checkboxGroupInput(
                                                      inputId = "checka",
                                                      label = NULL,
                                                      choices = state_choices[1:17],
                                                      selected = state_choices[1:17]
                                                  )
                                              ),
                                              
                                              column(
                                                  width = 4,
                                                  checkboxGroupInput(
                                                      inputId = "checkb",
                                                      label = NULL,
                                                      choices = state_choices[18:34],
                                                      selected = state_choices[18:34]
                                                  )
                                              ),
                                              
                                              column(
                                                  width = 4,
                                                  checkboxGroupInput(
                                                      inputId = "checkc",
                                                      label = NULL,
                                                      choices = state_choices[35:51],
                                                      selected = state_choices[35:51]
                                                  )
                                              ),
                                              actionLink("selectall", "Deselect/select All")
                                          ),
                                          tableOutput("table")
                                      ),
                                      br(),
                                      br(),
                                      br(),
                                      h6("This tab shows the average unemployment of counties with a
                                          labor force of a given size and the change over time. All 
                                          counties of the selected size for states checked under the 
                                          state dropdown are included together. For example, the yellow 
                                          line (6) shows all counties with a workforce of at least 10 ** 6 or 
                                          one million people. The next line (5) is all counties with a workforce
                                          between 10 ** 5 and 10 ** 6. The flat lines indicate average unemployment
                                          during 2019 which may be considered a baseline/goal to eventually
                                          reach, signalling a return to normal economic activity."),
                                      width = 3
                                  ),
                                  mainPanel(
                                      plotOutput('unemp_plot')
                                  )
                              )
                      ),
                      # Second tab content
                      tabItem(tabName = "county",
                              pageWithSidebar(
                                  headerPanel("Unemployment plot for individual state/county"),
                                  sidebarPanel(
                                      selectInput("county_state", "State", state.abb, selected = "AL"),
                                      uiOutput("county_UI"),
                                      width = 4),
                                  mainPanel(
                                      plotOutput('unemp_county_plot')
                                  )
                              )
                      ),
                      # Third tab content
                      tabItem(tabName = "county_recovery",
                              pageWithSidebar(
                                  headerPanel("Unemployment plot for individual state/county"),
                                  sidebarPanel(
                                      selectInput("county_state_recov", "State", state.abb, selected = "AL"),
                                      uiOutput("county_UI_recov"),
                                      width = 4),
                                  mainPanel(
                                      plotOutput('county_recov_plot')
                                  )
                              )
                      )
                  )
              )
)


