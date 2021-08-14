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
library(scales)

# Define UI for application that draws a histogram
vars <- setdiff(names(iris), "Species")
state_choices <- c(state.abb)
clusters <- c("Early benefit end status")
options(scipen = 999)


dashboardPage(skin = 'blue',
              dashboardHeader(title = "Unemployment Data Exploration", titleWidth = 280),
              dashboardSidebar(width = 280,  
                               sidebarMenu(
                                   menuItem("Welcome!", tabName = "Welcome", icon = icon("th")),
                                   menuItem("Unemployment by State/County", tabName = "county", icon = icon("th")),
                                   menuItem("Recovery by State/County", tabName = "county_recovery", icon = icon("th")),
                                   menuItem("Recovery by State/County Cluster", tabName = "cluster", icon = icon("th")),
                                   menuItem("County Size Plot", tabName = "stateplot", icon = icon("th"))
                               )),
              dashboardBody(
                  tabItems(
                      # First tab content
                      tabItem(tabName = "Welcome",
                              fluidPage(
                                title = "Welcome to the app!",
                                mainPanel(
                                  p("Welcome to the app! This app was created to analyze civilian unemployment
                                     in the United States at county and state levels. Data is pulled from the 
                                     Local Area Unemployment Statistics area of the U.S. Bureau of Labor 
                                     Statistics. The focus of this app is to provide insight into unemployment
                                     in different areas of the country, which generally spiked in Spring 2020
                                     due to coronavirus-induced lockdowns and strain on the economy. Since 
                                     states have some level of freedom in unemployment policy, it also yields
                                     the possibility of examining correlations between states with similar types
                                     of policy and the rate of recovery."),
                                  br(),
                                  br(),
                                  p("On subsequent tabs you will find several different illustrations of 
                                     unemployment over time, generally for the period from May 2020 
                                     through June 2021. On the Unemployment for individual state/county
                                     page, you can view unemployment on a monthly basis for any county or
                                     state."),
                                  br(),
                                  br(),
                                  p("The Recovery by state/county plot shows a monthly look at the
                                     progress of the recovery of unemployment. This is defined by looking
                                     at two factors. First, the average unemployment in 2019 is calculated
                                     as a baseline. We consider a region to be recovered, for the purpose
                                     of this plot, when unemployment has reached this level. We then calculate
                                     the fraction recovered for each county by seeing how close unemployment is
                                     from the worst point to this baseline level. For example, if a county saw 
                                     16% unemployment in May 2020, had an average of 4% unemployment in 2019, and
                                     is currently at 10%, we would say it is (16 - 10) / (16 - 4) = 50% recovered.
                                     Note that this definition allows percentages over 100."),
                                  br(),
                                  br(),
                                  p("The recovery by state/county cluster plot also illustrates the progress
                                     of the unemployment recovery over time. However, this is an aggregate look
                                     at the whole country divided into two or more clusters of states or counties.
                                     For example, in summer of 2021, many (but not all) states ended additional
                                     payments to unemployment workers to further incentivize people to return to
                                     work. This tool highlights the differences in recovery between these groups.
                                     Furthermore, the tab allows filtering by the size of the county labor force.
                                     This allows one to limit counties considered to ones that fall between 
                                     specified sizes. This can be constructive if one wishes to exclude some large
                                     counties, as they may wash out other effects."),
                                  br(),
                                  br(),
                                  p("The county size plot focuses on the evolution of unemployment with counties
                                     separated into different sizes, on a log scale. For example, we group up
                                     counties with a work force from 1000 - 9999 as 1000. This tool offers a look
                                     at counties from one or more states.")
                                )
                              )),
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
                                                      choices = state_choices[35:50],
                                                      selected = state_choices[35:50]
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
                                      plotOutput('unemp_plot'),
                                      br(),
                                      br(),
                                      p("Counties included in visualization"),
                                      plotOutput('state_group_plot')
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
                      ),
                      # Fourth tab content
                      tabItem(tabName = "cluster",
                              pageWithSidebar(
                                headerPanel("Unemployment plot for individual state/county"),
                                sidebarPanel(
                                  selectInput("cluster_state", "State", clusters, selected = "Early benefit end status"),
                                  sliderTextInput("county_size_cluster", "Filter by county sizes (workforce)",
                                              # min = 10 ** 3, max = 10 ** 7, 
                                              selected = c("1,000", "6,000,000"),
                                              choices = c(seq(1, 10) * 10 ** 3,
                                                          seq(1, 10) * 10 ** 4,
                                                          seq(1, 10) * 10 ** 5,
                                                          seq(1, 6) * 10 ** 6) %>% 
                                                comma(),
                                              grid = TRUE),
                                  width = 4),
                                mainPanel(
                                  plotOutput('cluster_recov_plot'),
                                  br(),
                                  br(),
                                  br(),
                                  p("Table form of unemployment by group and changes within the group from
                                     each previous period."),
                                  tableOutput('cluster_recov_table'),
                                  br(),
                                  br(),
                                  br(),
                                  p("Counties included in visualization"),
                                  plotOutput('recovery_state_plot')
                                )
                              )
                      )
                  )
              )
)


