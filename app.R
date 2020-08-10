


#####################
## LOAD LIBRARIES ## 
#####################
library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(coronavirus, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)     
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
#devtools::install_github('cttobin/ggthemr')




################################
## LOAD AND UPDATE DATA SETS ##
###############################
statedata <- read_csv("https://raw.githubusercontent.com/mjfrigaard/storybench/master/drafts/data/jhsph/2020-06-22-JHCovid19States.csv")
update_dataset()


#####################
## SET PLOT THEMES ##
#####################

library(ggthemr)
ggthemr('chalk')
#ggthemr_reset()

#####################
## RELOAD TO SHINYAPP ##
#####################
# rsconnect::deployApp("/Users/noahalex/Documents/Personal Dox/Programming/R/SICapp")


###############################################
## CREATE DATA AND INPUTS TO BE USED IN APP ###
###############################################

csv1 <- read_csv("data/UT_CONJOINT_DF.csv")
csv2 <- read_csv("data/State of Utah Conjoint_data1.csv")
csv3 <- read_csv("data/Top 10 package data.csv")
csv4 <- read_csv("data/Scraper.csv")
csv5 <- read_csv("data/linkedin.csv")

clr <- "black"
input_text1 <- paste("<p> <span style='font-size:20px; color: ", clr," '>Select My Programming Stats</span></p>")
input_text2 <- paste("<p> <span style='font-size:20px; color: ", clr," '>Select a State</span></p>")
input_text3 <- paste("<p> <span style='font-size:20px; color: ", clr," '>Select a Variable</span></p>")
input_text4 <- paste("<p> <span style='font-size:20px; color: ", clr," '>Select a Moving Average</span></p>")


statedata$state <- rev(statedata$state)


stats2 <- data.frame(
  Years = 4,
  Programs = 38,
  Languages = 8,
  Lines = 12000)

conf_df <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()


##############
## UI CODE ##
##############

corner_element <- HTML(paste0('<a id = "title" href=',shQuote("https://apple.com"), '>', 'Noah Alex', '</a>'))

ui <- shinyUI(tagList(includeCSS("customstyle.css"),
                      
                      
                      
                      ## IN-UI CSS ##
                      tags$head(tags$style(
                        type="text/css",
                        "#profile img {max-width: 100%; width: 300px; border-radius: 5px; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#elevation img {max-width: 100%; width: 85%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#uofu img {max-width: 100%; width: 100%; height: auto; display: block; margin-top:25px; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#stateofutah img {max-width: 100%; width: 55%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#warriorrising img {max-width: 100%; width: 55%; height: auto; display: block; mmargin-top:60px; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#shiny img {max-width: 100%; width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#tensorflow img {max-width: 100%; width: 100%; height: auto; display: block; margin-top:5px; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#alphavantage img {max-width: 100%; width: 75%; height: auto; display: block; margin-top:40px; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#alpaca img {max-width: 100%; width: 85%; height: auto; display: block; mmargin-top:60px; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#selenium img {max-width: 100%; width: 50%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#tidyverse img {max-width: 100%; width: 85%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#regression img {max-width: 100%; width: 100%; height: auto}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#performance img {max-width: 100%; width: 100%; height: auto}"
                      )),
                      tags$head(tags$style(
                        type="text/css",
                        "#wordcloud img {max-width: 100%; border-radius: 5px; width: 75%; height: auto; display: block; margin-left: auto; margin-right: auto;}"
                      )),
                      div(style="background-color:black;padding: 1px 0px;height: 0px",
                          titlePanel(
                            title="", windowTitle="Noah Alex Shiny Dash"
                          )),
                      
                      
                      
                      ## UI START ##
                      navbarPage(
                        corner_element, 
                        inverse = TRUE,
                        position = c("static-top"),
                        
                        tabPanel("Why Me?",
                                 fluidRow(
                                   tags$h1("ABOUT ME", 
                                           align = "center"),
                                   column(6,
                                          tags$h5("The Sorenson Impact Center needs someone who is flexible, experienced, adaptive, and able to learn quickly due to the shortness of the training."),
                                          tags$h5("Throughout my professional experiences, I have shown all of these traits. I even custom-made this Shiny dashboard (the very one you're looking at now) specifically for this internship: "),
                                          tags$a(
                                            id="intext",
                                            href="https://github.com/noahxl10/SIC-dash", 
                                            target = "_blank",
                                            "Check It Out Here",
                                            style = "background-color: orange; padding-top: 10px, padding-bottom: 10px; font-size: 16px; color: black; font-weight: 700;"),
                                          tags$h5("This Shiny dashboard is meant to show off my capabilities in R, as well as a few statistics and data projects I have done using other languages in the past."),
                                          tags$h5("Before this project, I started with very little R experience. I've spent 20 hours learning and creating with the R language and its packages, which means SIC's job to train me is now easier. It also exemplifies my learning potential. I was able to learn and build all of this in less than a week, so one can imagine the quality and quantity of work that a year of interning will provide to SIC."),
                                          br(), br()
                                          
                                   ),
                                   column(2),
                                   column(
                                     4,
                                     wellPanel(
                                       uiOutput("profile"))
                                   )
                                 ),
 
                                 fluidRow(
                                   br(),
                                   
                                   column(6, style = "border: 5px solid #d4d4d4; border-radius: 5px;",
                                          inputPanel(
                                            selectInput(inputId = "variable", 
                                                        shiny::HTML(input_text1),
                                                        choices = c("Lines of code written" = "Lines",
                                                                    "Number of programs created" = "Programs",
                                                                    "Number of coding languages known" = "Languages",
                                                                    "Years of coding experience" = "Years"))),
                                          plotOutput("plot")),
                                   column(6,
                                          wellPanel(
                                            wellPanel(
                                              tags$h2("This Project's Statistics: "),
                                              tags$h6("lines_of_code  <-  640 "), 
                                              tags$h6("custom_code   <-   'R & CSS' "), 
                                              tags$h6(" hours   <-   20 ")),
                                            
                                            wellPanel(
                                              downloadButton("downloadCoverletter", label="Cover Letter", class = "butt1", style = "width: 40%;"),
                                              downloadButton("downloadResume", label = "Resume", class = "butt1", style = "width: 40%;"))),
                                          tags$head(tags$style(".butt1{background-color:orange; color: black; font-family: Courier New}"))
                                          
                                   )),
                                 
                                 fluidRow(
                                   tags$h1("ORGANIZATIONS THAT I'VE WORKED WITH", 
                                           align="center")
                                 ),
                                 
                                 fluidRow(
                                   br(), br(),
                                   column(12,
                                          splitLayout(cellWidths = c("25%", "25%", "25%"),
                                                      uiOutput("elevation"),
                                                      uiOutput("warriorrising"),
                                                      uiOutput("uofu"),
                                                      uiOutput("stateofutah")
                                                      )
                                   )),
                                 
                                 fluidRow(
                                   br(),
                                   tags$h1("EXPERIENCE: PACKAGES AND API'S",
                                           align="center")
                                 ),
                                 
                                 fluidRow(
                                   br(), br(),
                                   
                                   splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                               uiOutput("shiny"),
                                               uiOutput("tensorflow"),
                                               uiOutput("alphavantage"))
                                 ),

                                 fluidRow(
                                   br(), br(),
                                   
                                   splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                               uiOutput("alpaca"),
                                               uiOutput("selenium"),
                                               uiOutput("tidyverse")), 
                                   br(), br())
                        ),
                        navbarMenu("R Examples",
                                   tabPanel("COVID-19 Visualization and Data Manipulation",
                                            fluidRow(
                                              tags$h1("VISUALIZATION AND DATA MANIPULATION IN R", 
                                                      align="center"), 
                                              column(7, 
                                                     br(), br(),
                                                     inputPanel(
                                                       selectInput(inputId = "state", 
                                                                   selected = "Utah", 
                                                                   width = "400px",
                                                                   label = shiny::HTML(input_text2),
                                                                   choices = rev(unique(statedata$state))),
                                                       selectInput(inputId = "data_point", 
                                                                   selected = "deaths", 
                                                                   width = "400px",
                                                                   label = shiny::HTML(input_text3),
                                                                   choices = c("Deaths" = "deaths",
                                                                               "Tests" = "people_tested",
                                                                               "Confirmed Cases" = "confirmed",
                                                                               "Recovered" = "recovered")),
                                                       selectInput(inputId = "moving_average", 
                                                                   selected = "three_day", 
                                                                   width = "400px",
                                                                   label = shiny::HTML(input_text4),
                                                                   choices = c("3 day" = "three_day",
                                                                               "5 day" = "five_day",
                                                                               "7 day" = "seven_day",
                                                                               "15 day" = "fifteen_day"))
                                                     ),
                                                     
                                                     plotOutput("covid_plot")
                                              ),
                                              column(1),
                                              column(4,
                                                     br(),br(),
                                                     
                                                     wellPanel(
                                                       tags$h5("This small project displays my ability to clean data, manipulate, sort, and visualize data through the use of R packages DPLYR, TIDYR, GGPLOT2, PLOTLY, and SHINY. "),
                                                       tags$h5("I used inputs and outputs with Reactiv to take user input and produce a plot that adjusts with user preference.")
                                                     ))
                                            ),
                                            
                                            fluidRow(
                                              br(), br(),
                                              column(6,
                                                     style = "border: 3px solid #d4d4d4; border-radius: 5px;",
                                                     br(),
                                                     coronavirus %>% 
                                                       group_by(type, date) %>%
                                                       summarise(total_cases = sum(cases)) %>%
                                                       pivot_wider(names_from = type, values_from = total_cases) %>%
                                                       arrange(date) %>%
                                                       mutate(active = confirmed - death - recovered) %>%
                                                       mutate(active_total = cumsum(active),
                                                              recovered_total = cumsum(recovered),
                                                              death_total = cumsum(death)) %>%
                                                       plot_ly(x = ~ date,
                                                               y = ~ active_total,
                                                               name = 'Active', 
                                                               fillcolor = '#4f86ab',
                                                               type = 'scatter',
                                                               mode = 'none', 
                                                               stackgroup = 'one') %>%
                                                       add_trace(y = ~ death_total,
                                                                 name = "Death",
                                                                 fillcolor = '#a63544') %>%
                                                       add_trace(y = ~recovered_total, 
                                                                 name = 'Recovered', 
                                                                 fillcolor = '#57b59f') %>%
                                                       layout(title = "Global COVID-19 cases",
                                                              legend = list(x = 0.1, y = 0.9),
                                                              yaxis = list(title = "Number of Cases"),
                                                              xaxis = list(title = "Source: Johns Hopkins University")),
                                                     br()
                                              ),
                                              column(6,
                                                     style = "border: 3px solid #d4d4d4; border-radius: 5px;",
                                                     br(),
                                                     plot_ly(data = conf_df,
                                                             type= "treemap",
                                                             values = ~total_cases,
                                                             labels= ~country,
                                                             parents=  ~parents,
                                                             domain = list(column=0),
                                                             name = "Confirmed",
                                                             marker = list(colorscale="Reds"),
                                                             textinfo="label+value+percent parent") %>%
                                                       layout(title = "Total Confirmed Cases by Country"),
                                                     br())
                                            ), ## end row
                                            fluidRow(br(), br(),br())
                                   )),
                        
                        navbarMenu("Coding Portfolio",
                                   
                                   tabPanel("Stock Analysis",
                                            fluidRow(
                                              
                                              tags$h1("PROJECT: STOCK TRADING ALGORITHM", 
                                                      align="center"),
                                              
                                              column(6,
                                                     wellPanel(
                                                       tags$h5("The stock trading algorithm is by far one of my favorite projects. It uses data API AlphaVantage and integration with a trading API called Alpaca to read real-time data and place trades based on a specific stock ticker. "),
                                                       fluidRow(br()),
                                                       tags$h5("Check out the project: "),
                                                       tags$a(
                                                         id="intext",
                                                         href="https://github.com/noahxl10/trade-algo", 
                                                         target = "_blank",
                                                         "Check It Out Here",
                                                         style = "background-color: orange; padding-top: 10px, padding-bottom: 10px; font-size: 16px; color: black; font-weight: 700;"), br(), br())),
                                              
                                              column(
                                                6,
                                                wellPanel(
                                                  tags$h5("The algorithm reads and uses a number of variables, including price momentum, regression analysis, trends, and volume to make trading decisions. One of the most successful variables that I created is a separate function that back-calculates all previous price movements of a certain high or low percentage and calculates the odds of that stock either moving up or down after a current large percentage movement up or down."),
                                                  br(),
                                                  tags$h5(" ")))),
                                            fluidRow(
                                              wellPanel(
                                                tags$h2("Project Statistics: "),
                                                tags$h6(" lines_of_code  <-  546 "), 
                                                tags$h6(" months   <-   2 "), 
                                                tags$h6(" APIs_and_packages <- 12")
                                              )),
                                            fluidRow(
                                              column(6,
                                                     wellPanel(
                                                       tags$h2("Performance Visualization"),
                                                       uiOutput("performance"))),
                                              column(6,
                                                     wellPanel(
                                                       tags$h2("Regression Visualization"),
                                                       uiOutput("regression")
                                                     ))
                                            )),
                                   
                                   tabPanel("Survey Analysis",
                                            fluidRow(
                                              tags$h1("PROJECT: CONJOINT SURVEY ANALYSIS", 
                                                      align="center"),
                                              column(6,
                                                     wellPanel(
                                                               tags$h5("When I was working on a project for the State of Utah’s HR department, I had the opportunity to clean data and create an optimization algorithm."),
                                                                       tags$h5("Our goal was to find out what benefits prospective millennial employees want out of a job to make certain pay worth it to them. We set up a conjoint survey and then I analyzed the data from there."),
                                                                        tags$h5("Using linear algebra and a custom-made optimization algorithm, our team was able to present a set of “top 10 benefits” that millennial workers would desire to make it worth a certain salary benchmark."),
                                                               tags$h5("Check out the project: "),
                                                               tags$a(
                                                                 id="intext",
                                                                 href="https://github.com/noahxl10/trade-algo", 
                                                                 target = "_blank",
                                                                 "Check It Out Here",
                                                                 style = "background-color: orange; padding-top: 10px, padding-bottom: 10px; font-size: 16px; color: black; font-weight: 700;"))),
                                              column(6,
                                                     wellPanel(
                                                               tags$h3("Final Recommendation to the State of Utah"),
                                                               dataTableOutput("packages"))),
                                              column(6,
                                                     wellPanel(
                                                               tags$h3("Before Cleaning"),
                                                               dataTableOutput("uncleaned_data"))),
                                              column(6,
                                                     wellPanel(
                                                               tags$h3("After Cleaning"),
                                                               dataTableOutput("cleaned_data")))
                                            )),
                                   
                                   tabPanel("Glassdoor Data Scraping",
                                            fluidRow(
                                              tags$h1("PROJECT: GLASSDOOR REVIEW DATA SCRAPER", 
                                                      align="center"),
                                              column(6,
                                                     wellPanel(
                                                       tags$h5("This project utilized my knowledge scraping packages, HTML, and CSS in Python to produce scraped data, which I then used for sentiment analysis (based off of a dictionary of positive and negative words) and converted into a word cloud for visualization. "),
                                                       tags$h5("Check out the project: "),
                                                       tags$a(
                                                         id="intext",
                                                         href="https://github.com/noahxl10/trade-algo", 
                                                         target = "_blank",
                                                         "Check It Out Here",
                                                         style = "background-color: orange; padding-top: 10px, padding-bottom: 10px; font-size: 16px; color: black; font-weight: 700;")),
                                                     wellPanel(
                                                       tags$h3("Wordcloud"),
                                                       uiOutput("wordcloud"))
                                                     ),
                                              column(6,
                                                     wellPanel(
                                                       tags$h3("Scraped Data Output"),
                                                       dataTableOutput("glassdoor")))
                                            )),
                                   
                                   tabPanel("LinkedIn Data Scraping",
                                            fluidRow(
                                              tags$h1("PROJECT: LINKEDIN COMPANY DATA SCRAPER", 
                                                      align="center"),
                                              column(6,
                                                     wellPanel(
                                                       tags$h5("One of the companies I have worked for in the past used LinkedIn and Salesforce to find people to contact. Their method of company data was to one-by-one copy and paste company information into Salesforce. I created a program that scraped company data off of LinkedIn and placed it into an easy-to-upload CSV file."),
                                                       tags$h5("Check out the project: "),
                                                       tags$a(
                                                         id="intext",
                                                         href="https://github.com/noahxl10/trade-algo", 
                                                         target = "_blank",
                                                         "Check It Out Here",
                                                         style = "background-color: orange; padding-top: 10px, padding-bottom: 10px; font-size: 16px; color: black; font-weight: 700;"))),
                                              column(6,
                                                     wellPanel(
                                                       tags$h3("Final Data Product"),
                                                       dataTableOutput("linkedin")))
                                            ))
                        )
                      )
                      # hr(), 
                      # tags$footer("Noah Alex 2020", align = "center", style = "
                      #           position:relative;
                      #         bottom:0;
                      #         width:100%;
                      #         height:5px;   /* Height of the footer */
                      #         color: black;
                      #         font-family: courier;
                      #         padding-bottom: 40px;
                      #         background-color: white;
                      #         z-index: 1000;")
                      
))

#################
## SERVER CODE ##
#################


server <- function(input, output) {
  
  output$plot <- renderPlot({
    ggplot(stats2, aes_string(as.factor(1), y=input$variable)) + 
      geom_col(width = .2) + 
      theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
              axis.text=element_text(size=14),
              axis.title=element_text(size=18,face="bold"))},
        width = "auto", height = "auto")
  
  output$downloadResume <- downloadHandler(
    filename <- "Noah Alex Resume.pdf",
    content <- function(file) {
      file.copy("www/resume.pdf", file)})
  
  output$downloadCoverletter <- downloadHandler(
    filename <- "Noah Alex Cover Letter.pdf",
    content <- function(file) {
      file.copy("www/coverletter.pdf", file)})
  
  output$rona <- renderText(paste(input$state, input$data_point, input$moving_average))
  
  output$covid_plot <- renderPlot({
    inp1 <- sym(input$data_point)
    inp2 <- sym(input$moving_average)
    statedata %>%
      arrange(desc(state)) %>% 
      group_by(state) %>% 
      mutate(three_day = rollmean(!!inp1, k = 3, fill = NA),
             five_day = rollmean(!!inp1, k = 5, fill = NA),
             seven_day = rollmean(!!inp1, k = 7, fill = NA),
             fifteen_day = rollmean(!!inp1, k = 15, fill = NA)) %>%
      ungroup() %>%
      filter(state == input$state) %>% 
      pivot_longer(names_to = "rolling_mean_key", 
                   values_to = "rolling_mean", 
                   cols = c(!!inp1, 
                            !!inp2)) %>%
      ggplot(aes(x = date, 
                 y = rolling_mean, 
                 color = rolling_mean_key)) +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=15,face="bold")) +
      geom_line(size=1) + scale_color_manual(values=c("#41c4be", "#f54720")) + 
      theme(title=element_text(size=15,face="bold")) +
      
      labs(title = paste(input$state, "'s moving average of total COVID-19", input$data_point), 
           subtitle = "Between 2020-04-15 and 2020-07-01",
           y = input$data_point, 
           color = "Legend",
           x = "Date")}, width = "auto", height = "auto")
  
  output$cleaned_data <- renderDataTable(csv1, options = list(scrollX = TRUE))
  output$uncleaned_data <- renderDataTable(csv2, options = list(scrollX = TRUE))
  output$packages <- renderDataTable(csv3, options = list(scrollX = TRUE))
  output$glassdoor <- renderDataTable(csv4, options = list(scrollX = TRUE))
  output$linkedin <- renderDataTable(csv5, options = list(scrollX = TRUE))
  
  output$profile <- renderUI({
    tags$img(src = "images/profile.png")})
  
  output$elevation <- renderUI({
    tags$img(src = "images/elevation.png")})
  
  output$uofu <- renderUI({
    tags$img(src = "images/uofu.png")})
  
  output$stateofutah <- renderUI({
    tags$img(src = "images/stateofutah.png")})
  
  output$shiny <- renderUI({
    tags$img(src = "images/shiny.png")})
  
  output$tensorflow <- renderUI({
    tags$img(src = "images/tensorflow.png")})
  
  output$alphavantage <- renderUI({
    tags$img(src = "images/alphavantage.png")})
  
  output$alpaca <- renderUI({
    tags$img(src = "images/alpaca.png")})
  
  output$selenium <- renderUI({
    tags$img(src = "images/selenium.png")})
  
  output$tidyverse <- renderUI({
    tags$img(src = "images/tidyverse.png")})
  
  output$regression <- renderUI({
    tags$img(src = "images/regression.png")})
  
  output$performance <- renderUI({
    tags$img(src = "images/performance.png")})
  
  output$wordcloud <- renderUI({
    tags$img(src = "images/wordcloud.png")})
  
  output$warriorrising <- renderUI({
    tags$img(src = "images/warriorrising.png")})
  
}

shinyApp(ui, server)
