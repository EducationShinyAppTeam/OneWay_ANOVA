# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(shinyDND)
library(boastUtils)
library(ggplot2)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Oneway ANOVA"
APP_DESCP  <<- paste(
  "In this app the goal is to learn about oneway ANOVA"
)
## End App Meta Data------------------------------------------------------------

# Define global constants and functions, load data ----

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Oneway ANOVA",
      titleWidth = 250,
      tags$li(class = "dropdown",
              actionLink(inputId = "info", label = icon("info"))),
      ## TO DO: Add feedback/comment button ----
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview",icon = icon("tachometer-alt")),
        menuItem("Prerequisites",tabName = "pre",icon = icon("book")),
        menuItem("Application",tabName = "app", icon = icon('wpexplorer')),
        menuItem("Matching Game", tabName = "game1", icon = icon("gamepad")),
        menuItem("Fill in the Blank", tabName = "game2", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Oneway ANOVA"),
          p("The goals of this app are to examine how the oneway ANOVA behaves
            under the null and different alternatives. The app uses the F-test
            to examine  the ANOVA for three group means, and analyzes the
            p-values drawn from each simulation to observe whether the null
            hypothesis is rejected or not."),
          h2("Instructions"),
          tags$ol(
            tags$li("Move each slider to observe what happens to the boxplot."),
            tags$li("Increase the number of simulations and observe the
                    distribution of p-values and how they are affected by each
                    slider."),
            tags$li("After you have sufficiently experimented with the different
                    sliders, move on to the game and test your knowledge!")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goover",
              label = "GO!",
              icon = icon("bolt"),
              size = "large")
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by Alex Chen and improved by
            Yiyang Wang and Xuefei Wang. Special thanks to Jinglin Feng, and
            Yuxin Zhang for the format of the Matching games.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 8/06/2020 by XW.")
          )
        ),
        ### Prereq's ----
        tabItem(
          tabName = "pre",
          withMathJax(),
          h2("Prerequisites"),
          tags$ul(
            tags$li("This module demonstrates oneway Analysis of Variance."),
            tags$li("The goal is to test the null hypothesis
                    \\(H_0: \\mu_1=\\mu_2=\\mu_3\\)."),
            tags$li("The hypothesis is tested using the ratio of between group
                    variability to within group variability
                    \\(F=MS_{between}/MS_{within}\\), which is compared to the F
                    distribution with (number of groups – 1) = 2 numerator
                    degrees of freedom and the (sample size – number of groups)
                    = N-3 denominator degrees of freedom.")
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "explore",
              label = "Explore",
              icon = icon("bolt"),
              size = "large")
          )
        ),
        # Explore
        tabItem(
          tabName = "app",
          h2("ANOVA Test Simulator"),
          fluidRow(
            column(
              width = 4,
              sliderInput(
                inputId = 'mu1',
                label = 'Group 1 Mean, \\(\\mu_1\\)',
                min = -5,
                max = 5,
                value = 1,
                step = 0.1
              ),
              bsPopover(
                id = "mu1",
                title = "Group Mean 1",
                content = "True Mean for 1st Group"
              ),
              sliderInput(
                inputId = 'mu2',
                label = 'Group 2 Mean, \\(\\mu_2\\)',
                min = -5,
                max = 5,
                value = 1,
                step = 0.1
              ),
              bsPopover(
                id = "mu2",
                title = "Group Mean 2",
                content = "True Mean for 2nd Group"
              ),
              sliderInput(
                inputId = 'mu3',
                label = 'Group 3 Mean, \\(\\mu_3\\)',
                min = -5,
                max = 5,
                value = 1,
                step = 0.1
              ),
              bsPopover(
                id = "mu3",
                title = "Group Mean 3",
                content = "True Mean for 3rd Group"
              ),
              sliderInput(
                inputId = 'sigma',
                label = 'Standard Deviation, \\(\\sigma\\)',
                min = 0,
                max = 10,
                value = 1,
                step = 0.1
              ),
              bsPopover(
                id = "sigma",
                title = "Standard Deviation",
                content = "Common standard deviation within group"
              ),
              sliderInput(
                inputId = 'n',
                label = 'Sample size, \\(n\\) ',
                min = 0,
                max = 100,
                value = 30,
                step = 1
              ),
              bsPopover(
                id = "n",
                title = "Sample Size",
                content = "Sample size per group"
              ),
              sliderInput(
                inputId = 'sim',
                label = 'Number of replications',
                min = 0,
                max = 1000,
                value = 30,
                step = 1
              ),
              bsPopover(
                id = "sim",
                title = "Replications",
                content = "Number of replications with above conditions"
              ),
              br(),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "gogame",
                  label = "Go to the Games",
                  icon = icon("bolt"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput('aovPlot'),
              bsPopover(
                id = 'aovPlot',
                title = "ANOVA Box Plot",
                content = paste(
                  "Box Plot for 3 Groups of Means with Standard Deviation",
                  "Black bars denote group means."
                )
              ),
              h3('Example ANOVA Table for 1 Simulation'),
              verbatimTextOutput('aovSummary'),
              bsPopover(
                id = "aovSummary",
                title = "ANOVA Summary Table",
                content = "ANOVA Summary Table for last Simulation"
              ),
              plotOutput('pvalueplot'),
              bsPopover(
                id = 'pvalueplot',
                title = "Histogram of p-values",
                content = "Histogram of p-Values through Simulations",
                placement = "top"
              )
            )
          )
        ),
        # Matching Game ----
        tabItem(
          tabName = "game1",
          h2("Matching Game"),
          tabsetPanel(
            id = "games1",
            tabPanel(
              title = "Instructions",
              value = "instruct1",
              p("To play this game,"),
              tags$ol(
                tags$li("Click Go! to start the game"),
                tags$li("Match each picture to the corresponding group means given
                        in each box"),
                tags$li("At the top of each game, there will be parameters being
                        held constant"),
                tags$li("If needed, go back to the app and play around with the
                        variables to try and recreate the images")
              ),
              div(
                style = "text-align: center;",
                bsButton(
                  inputId = "go1",
                  label = "GO!",
                  icon = icon("bolt"),
                  size = "large"
                )
              )
            ),
            #### Matching Level 1 ----
            tabPanel(
              title = "Level 1",
              value = "l1",
              p("Find Matches Assuming: \\(n\\) = 50 and \\(\\sigma\\) = 1"),
              br(),
              #A & B
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("A"),
                      img(src = "oneatfive.JPG",
                          alt = "graph for level 1, A",
                          width = 750)),
                  br(),
                  dragUI("img9", "A",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("B"),
                      img(src = "oneattwo.JPG",
                          alt = "graph for level 1, B",
                          width = 750)),
                  br(),
                  dragUI("img10","B",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              # Choices
              fluidRow(
                column(3,
                       dropUI("drop9",class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1=\\mu_1=\\mu_1\\)"),
                           htmlOutput("answer9")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop10",class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 0\\)"),
                           htmlOutput("answer10")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop11",class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 5\\ , \\mu_3 = 1\\)"),
                           htmlOutput("answer11")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop12", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 1\\)"),
                           htmlOutput("answer12")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
              ),
              # C & D
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("C"),
                      img(src = "splitbyone.JPG",
                          alt = "graph for level 1, C",
                          width = 750)),
                  br(),
                  dragUI("img11", "C",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("D"),
                      img(src = "allone.JPG",
                          alt = "graph for level 1, D",
                          width = 750)),
                  br(),
                  dragUI("img12","D",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")),
              br(),
              conditionalPanel("(input.drop9 != '') & (input.drop10 != '') &
                           (input.drop11 != '') & (input.drop12 != '')",
                               div(style = "text-align: center",
                                   bsButton(inputId = "submit2", label = "Submit!"),
                                   bsButton(inputId = "next1", label = "Next>>", disabled = F))
              )
            ),

            #### Matching Level 2 ----
            tabPanel(
              title = "Level 2",
              value = "l2",
              p("Find Matches Assuming: \\(n\\) = 50 and \\(\\mu_1 = 1\\ ,
             \\mu_2 = 0\\ , \\mu_3 = -1\\)"),
              br(),
              # A & B
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("A"),
                      img(src = "sd1.JPG",
                          alt = "graph for level 2, A",
                          width = 750)),
                  br(),
                  dragUI("img13", "A",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("B"),
                      img(src = "sd0.JPG",
                          alt = "graph for level 2, B",
                          width = 750)),
                  br(),
                  dragUI("img14", "B",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              # choices
              fluidRow(
                column(3,
                       dropUI("drop13", class = "dropelement dropelement2"),
                       div(style = "text-align: center", h5("\\(\\sigma\\) = 1"),
                           htmlOutput("answer13")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop14", class = "dropelement dropelement2"),
                       div(style = "text-align: center", h5("\\(\\sigma\\) = 10"),
                           htmlOutput("answer14")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop15", class = "dropelement dropelement2"),
                       div(style = "text-align: center", h5("\\(\\sigma\\) = 5"),
                           htmlOutput("answer15")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop16", class = "dropelement dropelement2"),
                       div(style = "text-align: center", h5("\\(\\sigma\\) = 0"),
                           htmlOutput("answer16")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
              ),
              # C & D
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("C"),
                      img(src = "sd5.JPG",
                          alt = "graph for level 2, C",
                          width = 750)),
                  br(),
                  dragUI("img15", "C",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("D"),
                      img(src = "sd10.JPG",
                          alt = "graph for level 2, D",
                          class = "picSize")),
                  br(),
                  dragUI("img16", "D",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")),
              br(),
              conditionalPanel("(input.drop13 != '') & (input.drop14 != '') &
                             (input.drop15 != '') & (input.drop16 != '')",
                               div(style = "text-align: center",
                                   bsButton(inputId = "submit3", label = "Submit!"),
                                   bsButton(inputId = "next2", label = "Next>>", disabled = F))
              )
            ),
            #### Matching Level 3 ----
            tabPanel(
              title = "Level 3",
              value = "l3",
              p("Find Matches Assuming: \\(n\\) = 50"),
              br(),
              # A & B
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("A"),
                      img(src = "lastlevel0.JPG",
                          alt = "graph for level 3, A",
                          width = 750)),
                  br(),
                  dragUI("img17", "A",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("B"),
                      img(src = "lastlevel1.JPG",
                          alt = "graph for level 3, B",
                          width = 750)),
                  br(),
                  dragUI("img18", "B",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              # choices
              fluidRow(
                column(3,
                       dropUI("drop17", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = -2\\ , \\mu_3 = 1\\),
                     \\(\\sigma\\) = 2.5"),
                           htmlOutput("answer17")),
                       class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop18", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1\\ , \\mu_3 = 1\\),
                     \\(\\sigma\\) = 1"),
                           htmlOutput("answer18")),
                       class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop19", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = -1\\ , \\mu_2 = 0\\ , \\mu_3 = 1\\),
                     \\(\\sigma\\) = 4"),
                           htmlOutput("answer19")),
                       class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop20", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 5\\ , \\mu_2 = 0\\ , \\mu_3 = -5\\),
                     \\(\\sigma\\) = 10"),
                           htmlOutput("answer20")),
                       class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12")
              ),
              # C & D
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("C"),
                      img(src = "allone.JPG",
                          alt = "graph for level 3, C",
                          width = 750)),
                  br(),
                  dragUI("img19", "C",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align:center", h4("D"),
                      img(src = "lastlevel2.JPG",
                          alt = "graph for level 3, D",
                          width = 750)),
                  br(),
                  dragUI("img20", "D",
                         style = "background-color:orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              br(),
              conditionalPanel("(input.drop17 != '') & (input.drop18 != '') &
                           (input.drop19 != '') & (input.drop20 != '')",
                               div(style = "text-align: center",
                                   bsButton("submit4", "Submit!"),
                                   bsButton("next3", "Next>>", disabled = F))
              )
            ),
            #### Matching Level 4 ----
            tabPanel(
              title = "Level 4",
              value = "l4",
              p("Find Matches Assuming: \\(n\\) = 30 and \\(\\sigma\\) = 1"),
              br(),
              # A & B
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("A"),
                      img(src = "same.JPG",
                          alt = "graph for level 4, A",
                          width = 750)),
                  br(),
                  dragUI("img1", "A",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("B"),
                      img(src = "attwo.JPG",
                          alt = "graph for level 4, B",
                          width = 750)),
                  br(),
                  dragUI("img2", "B",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              # choices
              fluidRow(
                column(3,
                       dropUI("drop1", class = "dropelement dropelement2"),
                       div(style = "text-align: center", h5("\\(\\mu_1=\\mu_2=\\mu_3\\)"),
                           htmlOutput("answer1")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop2", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1.5\\ , \\mu_3 = 0.5\\)"),
                           htmlOutput("answer2")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop3", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 1\\)"),
                           htmlOutput("answer3")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop4",class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1\\ , \\mu_3 = 1.5\\)"),
                           htmlOutput("answer4")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
              ),
              # C & D
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("C"),
                      img(src = "byhalf.jpg",
                          alt = "graph for level 4, C",
                          width = 750)),
                  br(),
                  dragUI("img3", "C",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("D"),
                      img(src = "oneathalf.JPG",
                          alt = "graph for level 4, D",
                          width = 750)),
                  br(),
                  dragUI("img4", "D",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              br(),
              conditionalPanel("(input.drop1 != '') & (input.drop2 != '') &
                           (input.drop3 != '') & (input.drop4 != '')",
                               div(style = "text-align: center",
                                   bsButton("submit", "Submit!", style = "primary"),
                                   bsButton("next4", "Next>>", style = "primary", disabled = F))
              )
            ),
            #### Matching Level 5 ----
            tabPanel(
              title = "Level 5",
              value = "l5",
              p("Find Matches Assuming: \\(\\mu_1 = 1\\ , \\mu_2 = 1.5\\ ,
             \\mu_3 = 0.5\\)"),
              br(),
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("A"),
                      img(src = "n=10.JPG",
                          alt = "graph for level 5, A",
                          width = 750)),
                  br(),
                  dragUI("img5", "A",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("B"),
                      img(src = "n=50.JPG",
                          alt = "graph for level 5, B",
                          width = 750)),
                  br(),
                  dragUI("img6", "B",
                         style = "background-color:orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              # choices
              fluidRow(
                column(3,
                       dropUI("drop5", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(n\\ = 10, \\sigma = 5\\)"),
                           htmlOutput("answer5")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop6", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(n\\ = 10\\ , \\sigma = 1\\)"),
                           htmlOutput("answer6")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop7", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(n\\ = 50\\ , \\sigma = 5\\)"),
                           htmlOutput("answer7")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                column(3,
                       dropUI("drop8", class = "dropelement dropelement2"),
                       div(style = "text-align: center",
                           h5("\\(n\\ = 50\\ , \\sigma = 1\\)"),
                           htmlOutput("answer8")),
                       class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
              ),
              # C & D
              fluidRow(
                wellPanel(
                  div(style = "text-align: center", h4("C"),
                      img(src = "n=10s=5.JPG",
                          alt = "graph for level 5, C",width = 750)),
                  br(),
                  dragUI("img7", "C",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder"),
                wellPanel(
                  div(style = "text-align: center", h4("D"),
                      img(src = "n=50s=5.JPG",
                          alt = "graph for level 5, D",width = 750)),
                  br(),
                  dragUI("img8", "D",
                         style = "background-color: orange; width: 200px; height: 35px"),
                  class = "col-lg-6 col-md-12 wellBorder")
              ),
              br(),
              conditionalPanel("(input.drop5 != '') & (input.drop6 != '') &
                           (input.drop7 != '') & (input.drop8 != '')",
                               div(style = "text-align: center",
                                   bsButton(inputId = "submit1", label = "Submit!"),
                                   bsButton(inputId = "next5", label = "Next Game!", disabled = F))
              )
            )
          )
        ),
        ### Fill in the blank ----
        tabItem(
          tabName = "game2",
          h2("Fill in the Blank Game"),
          p("To play this game:"),
          tags$ol(
            tags$li("Click Go! to start the game"),
            tags$li("Fill in the blanks with each answer to make the statement
                        true"),
            tags$li("Hint: There is more than one answer to each statement")
          ),
          hr(),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = "firstAns",
                  label = "Question 1: First Blank",
                  choices = c('', "equal", "different")
                ),
                selectInput(
                  inputId = "secondAns",
                  label = "Question 1: Second Black",
                  choices = c('',"will", "won't")
                ),
                selectInput(
                  inputId = "thirdAns",
                  label = "Question 2: Third Black",
                  choices = c('', "Level", "Skewed", "Stays the same")
                ),
                selectInput(
                  inputId = "fourthAns",
                  label = "Question 2: Fourth Blank",
                  choices = c('', "Increases", "Decreases", "Stays the same")
                ),
                selectInput(
                  inputId = "fifthAns",
                  label = "Question 3: Fifth Blank",
                  choices = c('', "Increases", "Decreases", "stays the same")
                ),
                selectInput(
                  inputId = "sixthAns",
                  label = "Question 3: Sixth blank",
                  choices = c('', "Right", "Left")
                )
              )
            ),
            column(
              width = 8,
              p(textOutput("first")),
              fluidRow(
                column(
                  width = 3,
                  bsButton(
                    inputId = "checkFirst",
                    label = "Submit",
                    size = "large"
                  )
                ),
                column(
                  width = 5,
                  conditionalPanel(
                    condition = "input.checkFirst != 0",
                    htmlOutput("ans1")
                  )
                )
              ),
              br(),
              br(),
              br(),
              br(),
              p(textOutput("second")),
              fluidRow(
                column(
                  width = 3,
                  bsButton(
                    inputId = "checkSecond",
                    label = "Submit",
                    size = "large"
                  )
                ),
                column(
                  width = 5,
                  conditionalPanel(
                    condition = "input.checkSecond != 0",
                    htmlOutput("ans2")
                  )
                )
              ),
              br(),
              br(),
              br(),
              br(),
              p(textOutput("third")),
              fluidRow(
                column(
                  width = 3,
                  bsButton(
                    inputId = "checkThird",
                    label = "Submit",
                    size = "large"
                  )
                ),
                column(
                  width = 5,
                  conditionalPanel(
                    condition = "input.checkThird != 0",
                    htmlOutput("ans3")
                  )
                )
              )
            )
          )
        ),
        ### Refrence Page ----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny,
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities, R Package.
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hoffer, A. (2016). shinyDND: Shiny Drag-n-Drop, R Package.
            Available from https://CRAN.R-project.org/package=shinyDND"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2020). shinyWidgets:
            Custom Inputs Widgets for Shiny, R Package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            'Wickham, H. (2016). "ggplot2: Elegant graphics for data analysis",
            R Package. Springer-Verlag New York. Available at
            https: // ggplot2.tidyverse.org'
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2020).
            dplyr: A Grammar of Data Manipulation, R Package.
            Available from https://CRAN.R-project.org/package=dplyr"
          )
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## information icon ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Information",
      text = "This app is designed to explore how a one-way ANOVA behaves under
      differing samples sizes and null and alternative hypotheses. The app includes
      a matching game to test student understanding of the issues illustrated by
      the app.",
      type = "info"
    )
  })

  #Go to overview Button ----
  observeEvent(input$goover, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "pre"
    )
  })

  #Explore Button ----
  observeEvent(input$explore, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "app"
    )
  })

  #Go to the Game Button ----
  observeEvent(input$gogame, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game1"
    )
  })

  ## Create reactive values ----
  gen_data <- reactive({
    value = c(input$n,
              input$mu1,
              input$mu2,
              input$mu3,
              input$sigma
    )
  })

  counter <- reactive(input$sim)

  ## Create the Boxplot ----
  ## *Need to make a ggplot of this to make more interactive*
  output$aovPlot <- renderPlot({
    ### Generating Data ----
    value <- gen_data()
    df <- data.frame(
      y = c(rnorm(value[1], value[2], value[5]),
            rnorm(value[1], value[3], value[5]),
            rnorm(value[1], value[4], value[5])
      ),
      group = rep(sprintf('Group %s', 1:3), each = value[1])
    )

    ### Make plot ----
    ggplot(
      data = df,
      mapping = aes(x = group, y = y)
    ) +
      geom_boxplot() +
      geom_point() +
      ggtitle("Example Data by Groups") +
      ylab("Values") +
      xlab("") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 24),
        text = element_text(size = 18)
      )
  })

  ## ANOVA Table ----
  output$aovSummary <- renderPrint({
    ### Making data, again
    value <- gen_data()
    df <- data.frame(
      `y` = c(rnorm(value[1], value[2], value[5]),
              rnorm(value[1], value[3], value[5]),
              rnorm(value[1], value[4], value[5])),
      `Group` = rep(sprintf('mu%s', 1:3), each = value[1])
    )
    summary(aov(y ~ Group, data = df))
  })

  ## Displays the F Crit Value ----
  ### What is this? Where does this go?
  output$Fcrit <- renderPrint({

    ### Making data, again
    value <- gen_data()
    df <- data.frame(
      y = c(rnorm(value[1], value[2], value[5]),
            rnorm(value[1], value[3], value[5]),
            rnorm(value[1], value[4], value[5])),
      group = rep(sprintf('mu%s', 1:3), each = value[1])
    )

    dfsum <- summary(aov(df$y ~ df$group))
    qf(0.95, df1 = dfsum[[1]]$Df[1], dfsum[[1]]$Df[2])
  })

  # Simulated p-value plot ----
  output$pvalueplot = renderPlot({
    simulations <- counter()
    vector <- c()
    value <- gen_data()
    for (i in 1:(simulations)) {
      df <- data.frame(
        y = c(rnorm(value[1], value[2], value[5]),
              rnorm(value[1], value[3], value[5]),
              rnorm(value[1], value[4], value[5])),
        group = rep(sprintf('mu%s', 1:3), each = value[1])
      )

      dfsum <- summary(aov(df$y ~ df$group))
      pvalue <- dfsum[[1]]$`Pr(>F)`[1]
      vector <- c(vector, pvalue)
    }

    ### Make the plot ----
    ggplot(
      data = data.frame(x = vector),
      mapping = aes(x = x)
    ) +
      geom_histogram(
        binwidth = 0.01,
        boundary = 0,
        closed = "left",
        col = "black",
        fill = psuPalette[3],
        na.rm = TRUE
      ) +
      labs(
        title = paste("P-Value Distribution for", input$sim, "Simulations"),
        x = "p-value",
        y = "Frequency"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16))
  })

  ## Matching Game Subsection ----

  ### Timer Section??? ----
  time <- reactiveValues(
    inc = 0,
    timer = reactiveTimer(1000),
    started = F
  ) ## What's the timer for?

  observeEvent(input$go, {
    time$started <- TRUE
  })
  observeEvent(input$submit, {
    time$started <- FALSE
  })

  values <- reactiveValues(
    count = 0
  )

  observe({
    time$timer()
    if (isolate(time$started))
      time$inc <- isolate(time$inc) + 1
  })

  # output$timer <- renderPrint({
  #   cat("You have used:", time$inc, "secs")})

  # output$score = renderText({
  #   values$count
  # })

  ### Answer checking for Level ? ----
  observeEvent(input$submit, {
    output$answer1 <- renderUI({
      if (!is.null(input$drop1)) {
        if (input$drop1 == "A") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # value$count = values$count - 2
        }
      }
    })

    output$answer2 <- renderUI({
      if (!is.null(input$drop2)) {
        if (input$drop2 == "C") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else{
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer3 <- renderUI({
      if (!is.null(input$drop3)) {
        if (input$drop3 == "B") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer4 = renderUI({
      if (!is.null(input$drop4)) {
        if (input$drop4 == "D") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })
  })

  ### Answer checking for Level ?? ----
  observeEvent(input$submit1, {
    output$answer5 <- renderUI({
      if (!is.null(input$drop5)) {
        if (input$drop5 == "C") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # value$count = values$count - 2
        }
      }
    })

    output$answer6 <- renderUI({
      if (!is.null(input$drop6)) {
        if (input$drop6 == "A") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer7 <- renderUI({
      if (!is.null(input$drop7)) {
        if (input$drop7 == "D") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer8 <- renderUI({
      if (!is.null(input$drop8)) {
        if (input$drop8 == "B") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })
  })

  ## Answer checking for Level ??? ----
  observeEvent(input$submit2, {
    output$answer9 <- renderUI({
      if (!is.null(input$drop9)) {
        if (input$drop9 == "D") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # value$count = values$count - 2
        }
      }
    })

    output$answer10 <- renderUI({
      if (!is.null(input$drop10)) {
        if (input$drop10 == "C") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer11 <- renderUI({
      if (!is.null(input$drop11)) {
        if (input$drop11 == "A") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer12 <- renderUI({
      if (!is.null(input$drop12)) {
        if (input$drop12 == "B") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })
  })

  ### Answer checking for Level ???? ----
  observeEvent(input$submit3, {
    output$answer13 <- renderUI({
      if (!is.null(input$drop13)) {
        if (input$drop13 == "A") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # value$count = values$count - 2
        }
      }
    })

    output$answer14 <- renderUI({
      if (!is.null(input$drop14)) {
        if (input$drop14 == "D") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer15 <- renderUI({
      if (!is.null(input$drop15)) {
        if (input$drop15 == "C") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer16 <- renderUI({
      if (!is.null(input$drop16)) {
        if (input$drop16 == "B") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else{
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })
  })

  ### Answer checking for Level ????? -----
  observeEvent(input$submit4, {
    output$answer17 <- renderUI({
      if (!is.null(input$drop17)) {
        if (input$drop17 == "A") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # value$count = values$count - 2
        }
      }
    })

    output$answer18 <- renderUI({
      if (!is.null(input$drop18)) {
        if (input$drop18 == "C") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer19 <- renderUI({
      if (!is.null(input$drop19)) {
        if (input$drop19 == "B") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })

    output$answer20 <- renderUI({
      if (!is.null(input$drop20)) {
        if (input$drop20 == "D") {
          img(src = "check.png", width = 30)
          # values$count = values$count + 5
        } else {
          img(src = "cross.png", width = 30)
          # values$count = values$count - 2
        }
      }
    })
  })

  ## Game movement buttons ----
  observeEvent(input$go1, {
    updateTabItems(
      session = session,
      inputId = "games1",
      selected = "l1"
    )
  })

  observeEvent(input$next1, {
    updateTabItems(
      session = session,
      inputId = "games1",
      selected = "l2"
    )
  })

  observeEvent(input$next2, {
    updateTabItems(
      session = session,
      inputId = "games1",
      selected = "l3"
    )
  })

  observeEvent(input$next3, {
    updateTabItems(
      session = session,
      inputId = "games1",
      selected = "l4"
    )
  })

  observeEvent(input$next4, {
    updateTabItems(
      session = session,
      inputId = "games1",
      selected = "l5"
    )
  })

  observeEvent(input$next5, {
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game2"
    )
  })

  ## Fill in the Blank Subsection ----

  output$first <- renderText({
    if (input$firstAns == '' && input$secondAns == '') {
      paste("1. When the group means are ____1____, having a larger standard",
            "deviation ____2____ have an effect on the distribution of p-values")
    } else if (input$firstAns == '') {
      paste("1. When the group means are ____1____, having a larger standard deviation",
            input$secondAns, "have an effect on the distribution of p-values")
    } else if (input$secondAns == '') {
      paste("1. When the group means are", input$firstAns, ", having a larger",
            "standard deviation ____2____ have effect on the distribution of p-values")
    } else {
      paste("1. When the group means are", input$firstAns,
            ", having a larger standard deviation", input$secondAns ,
            " have effect on the distribution of p-values")
    }
  })

  output$second <- renderText({
    if (input$thirdAns == '' && input$fourthAns == '') {
      paste("2. The Distribution of P-values, under the alternative hypothesis",
            "is more ____3____ as the number of Samples ____4____")
    } else if (input$thirdAns == '') {
      paste("2. The Distribution of P-values, under the alternative hypothesis",
            "is more ____3____ as the number of Samples", input$fourthAns)
    } else if (input$fourthAns == '') {
      paste("2. The Distribution of P-values, under the alternative hypothesis",
            "is more", input$thirdAns, "as the number of Samples ____4____")
    } else {
      paste("2. The Distribution of P-values, under the alternative hypothesis",
            "is more", input$thirdAns, "as the number of Samples", input$fourthAns)
    }
  })

  output$third <- renderText({
    if (input$fifthAns == '' && input$sixthAns == '') {
      paste("3. As the differences between the three means ____5____ the P-Value",
            "histogram becomes more ____6____ skewed")
    } else if (input$fifthAns == '') {
      paste("3. As the differences between the three means ____5____ the P-Value",
            "histogram becomes more", input$sixthAns, "skewed")
    } else if (input$fourthAns == '') {
      paste("3. As the differences between the three means", input$fifthAns,
            "the P-Value histogram becomes more ____6____ skewed")
    } else {
      paste("3. As the differences between the three means", input$fifthAns,
            "the P-Value histogram becomes more", input$sixthAns, "skewed")
    }
  })

  observeEvent(input$checkFirst, {
    if ((input$firstAns == "different" && input$secondAns == "will") ||
       (input$firstAns == "equal" && input$secondAns == "won't")) {
      output$ans1 <- renderIcon(icon = "correct", width = 45)
    } else if ((input$firstAns == "different" && input$secondAns == "won't") ||
               (input$firstAns == "equal" && input$secondAns == "will")) {
      output$ans1 <- renderIcon(icon = "partial", width = 45)
    } else {
      output$ans1 <- renderIcon(icon = "incorrect", width = 45)
    }
  })

  observeEvent(input$checkSecond, {
    if ((input$thirdAns == "Skewed" && input$fourthAns == "Increases") ||
        (input$thirdAns == "Level" && input$fourthAns == "Decreases")) {
      output$ans2 <- renderIcon(icon = "correct", width = 45)
    } else if ((input$thirdAns == "Skewed" && input$fourthAns != "Increases") ||
               (input$thirdAns != "Skewed" && input$fourthAns == "Increases") ||
               (input$thirdAns == "Level" && input$fourthAns != "Decreases") ||
               (input$thirdAns != "Level" && input$fourthAns == "Decreases")) {
      output$ans2 <- renderIcon(icon = "partial", width = 45)
    } else {
      output$ans2 <- renderIcon(icon = "incorrect", width = 45)
    }
  })

  observeEvent(input$checkThird, {
    if ((input$fifthAns == "Increases" && input$sixthAns == "Right") ||
       input$fifthAns == "Decreases" && input$sixthAns == "Left") {
      output$ans3 <- renderIcon(icon = "correct", width = 45)
    } else if ((input$fifthAns == "Increases" && input$sixthAns != "Right") ||
               (input$fifthAns != "Increases" && input$sixthAns == "Right") ||
               (input$fifthAns == "Decreases" && input$sixthAns != "Left") ||
               (input$fifthAns != "Decreases" && input$sixthAns == "Left")) {
      output$ans3 <- renderIcon(icon = "partial", width = 45)
    } else {
      output$ans3 <- renderIcon(icon = "incorrect", width = 45)
    }
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)