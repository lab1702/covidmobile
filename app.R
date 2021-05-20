
library(shiny)
library(bslib)
library(thematic)

library(curl)
library(data.table)

library(scales)


us_data <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
states_data <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
counties_data <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),

  titlePanel("COVID-19 Mobile"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state",
        label = "State:",
        choices = sort(unique(states_data$state)),
        selected = "Michigan"
      ),
      selectInput(
        inputId = "county",
        label = "County:",
        choices = "Select state first..."
      )
    ),

    mainPanel(
      wellPanel(
        tableOutput("summary_table")
      ),
      plotOutput("county_deaths"),
      br(),
      plotOutput("county_cases"),
      br(),
      plotOutput("state_deaths"),
      br(),
      plotOutput("state_cases"),
      br(),
      plotOutput("us_deaths"),
      br(),
      plotOutput("us_cases"),
      br(),
      wellPanel(
        HTML("All data presented here was downloaded from <A HREF='https://github.com/nytimes/covid-19-data/' TARGET='_blank'>https://github.com/nytimes/covid-19-data/</A>")
      ),
      wellPanel(
        HTML("The source code for this R/Shiny app is available at <A HREF='https://github.com/lab1702/covidmobile/' TARGET='_blank'>https://github.com/lab1702/covidmobile/</A>")
      )
    )
  )
)


server <- function(input, output, session) {
  thematic_shiny(session = session)
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "county",
      choices = sort(unique(counties_data[state %in% input$state, county]))
    )
  })


  output$summary_table <- renderTable(
    {
      m <- matrix(nrow = 5, ncol = 3)

      colnames(m) <- c(paste(input$county, "County"), input$state, "USA")
      rownames(m) <- c("Updated", "Deaths Change", "Total Deaths", "Cases CHange", "Total Cases")

      m[1, 1] <- format(tail(counties_data[state == input$state & county == input$county, date], 1), "%b %d")
      m[1, 2] <- format(tail(states_data[state == input$state, date], 1), "%b %d")
      m[1, 3] <- format(tail(us_data$date, 1), "%b %d")

      m[2, 1] <- comma(diff(tail(counties_data[state == input$state & county == input$county, deaths], 2)), accuracy = 1)
      m[2, 2] <- comma(diff(tail(states_data[state == input$state, deaths], 2)), accuracy = 1)
      m[2, 3] <- comma(diff(tail(us_data$deaths, 2)), accuracy = 1)

      m[4, 1] <- comma(diff(tail(counties_data[state == input$state & county == input$county, cases], 2)), accuracy = 1)
      m[4, 2] <- comma(diff(tail(states_data[state == input$state, cases], 2)), accuracy = 1)
      m[4, 3] <- comma(diff(tail(us_data$cases, 2)), accuracy = 1)

      m[3, 1] <- comma(tail(counties_data[state == input$state & county == input$county, deaths], 1), accuracy = 1)
      m[3, 2] <- comma(tail(states_data[state == input$state, deaths], 1), accuracy = 1)
      m[3, 3] <- comma(tail(us_data$deaths, 1), accuracy = 1)

      m[5, 1] <- comma(tail(counties_data[state == input$state & county == input$county, cases], 1), accuracy = 1)
      m[5, 2] <- comma(tail(states_data[state == input$state, cases], 1), accuracy = 1)
      m[5, 3] <- comma(tail(us_data$cases, 1), accuracy = 1)

      m
    },
    rownames = TRUE
  )


  output$county_deaths <- renderPlot({
    plot(
      c(head(deaths, 1), diff(deaths)) ~ date,
      data = counties_data[state %in% input$state & county %in% input$county],
      col = 1,
      type = "h",
      xlab = "Date",
      ylab = "Deaths",
      main = paste(input$county, "\nCounty Deaths")
    )

    lines(
      frollmean(x = c(head(deaths, 1), diff(deaths)), n = 7, align = "center") ~ date,
      data = counties_data[state %in% input$state & county %in% input$county]
    )
  })


  output$county_cases <- renderPlot({
    plot(
      c(head(cases, 1), diff(cases)) ~ date,
      data = counties_data[state %in% input$state & county %in% input$county],
      col = 2,
      type = "h",
      xlab = "Date",
      ylab = "Cases",
      main = paste(input$county, "\nCounty Cases")
    )

    lines(
      frollmean(x = c(head(cases, 1), diff(cases)), n = 7, align = "center") ~ date,
      data = counties_data[state %in% input$state & county %in% input$county]
    )
  })


  output$state_deaths <- renderPlot({
    plot(
      c(head(deaths, 1), diff(deaths)) ~ date,
      data = states_data[state %in% input$state],
      col = 1,
      type = "h",
      xlab = "Date",
      ylab = "Deaths",
      main = paste(input$state, "\nDeaths")
    )

    lines(
      frollmean(x = c(head(deaths, 1), diff(deaths)), n = 7, align = "center") ~ date,
      data = states_data[state %in% input$state]
    )
  })


  output$state_cases <- renderPlot({
    plot(
      c(head(cases, 1), diff(cases)) ~ date,
      data = states_data[state %in% input$state],
      col = 2,
      type = "h",
      xlab = "Date",
      ylab = "Cases",
      main = paste(input$state, "\nCases")
    )

    lines(
      frollmean(x = c(head(cases, 1), diff(cases)), n = 7, align = "center") ~ date,
      data = states_data[state %in% input$state]
    )
  })


  output$us_deaths <- renderPlot({
    plot(
      c(head(deaths, 1), diff(deaths)) ~ date,
      data = us_data,
      col = 1,
      type = "h",
      xlab = "Date",
      ylab = "Deaths",
      main = "National\nDeaths"
    )

    lines(
      frollmean(x = c(head(deaths, 1), diff(deaths)), n = 7, align = "center") ~ date,
      data = us_data
    )
  })


  output$us_cases <- renderPlot({
    plot(
      c(head(cases, 1), diff(cases)) ~ date,
      data = us_data,
      col = 2,
      type = "h",
      xlab = "Date",
      ylab = "Cases",
      main = "National\nCases"
    )

    lines(
      frollmean(x = c(head(cases, 1), diff(cases)), n = 7, align = "center") ~ date,
      data = us_data
    )
  })
}


shinyApp(ui = ui, server = server)
