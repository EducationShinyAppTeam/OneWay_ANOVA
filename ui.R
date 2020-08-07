library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyDND)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "One-way ANOVA"
APP_DESCP  <<- paste(
  "In this app the goal is to learn about one-way ANOVA"
)
## End App Meta Data------------------------------------------------------------

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "One-Way ANOVA", 
    titleWidth = 250,
    tags$li(class = "dropdown",
            actionLink(inputId = "info", label = icon("info"), class = "myClass")),
    tags$li(class = "dropdown",
            tags$a(href='https://shinyapps.science.psu.edu/', icon("home")))
  ),
  
  ## Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "tabs",
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
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css",
        href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"
    )
  ),
  
  tabItems(
    tabItem(tabName = "overview",
      withMathJax(),
      h1("One-Way ANOVA"),
      p("The goals of this app are to examine how the one way ANOVA behaves under
        the null and different alternatives. The app uses the F-test to examine 
        the ANOVA for three group means, and analyzes the P-values drawn from each
        simulation to observe whether the null hypothesis is rejected or not."),
      h2("Instructions:"),
      tags$ul(
        tags$li("Move each slider to observe what happens to the boxplot."),
        tags$li("Then, increase the number of simulations and observe the 
                distribution of P-values and how they are affected by each slider."),
        tags$li("After you have sufficiently experimented with the different sliders,
                move on to the game and test your knowledge!")),
      div(style = "text-align: center",
          bsButton(
            inputId = "goover",
            label = "GO!", 
            icon = icon("bolt"), 
            size = "large")),
      br(),
      h2("Acknowledgements"),
      p("This app was developed and coded by Alex Chen and improved by Yiyang Wang
        and Xuefei Wang. Special thanks to Jinglin Feng, and Yuxin Zhang for the
        format of the Matching games."),
      br(),
      br(),
      br(),
      div(class = "updated", "Last Update: 8/06/2020 by XW.")
  ),
    tabItem(tabName = "pre",
      withMathJax(),
      h2("Background: One-Way ANOVA"),
      tags$ul(
        tags$li("This module demonstrates one-way Analysis of Variance."),
        tags$li("The goal is to test the null hypothesis
                \\(H_0: \\mu_1=\\mu_2=\\mu_3\\)."),
        tags$li("The hypothesis is tested using the ratio of between group 
                variability to within group variability
                \\(F=MS_{between}/MS_{within}\\), which is compared to the F 
                distribution with (number of groups – 1) = 2 numerator degrees of
                freedom and the (sample size – number of groups) = N-3 denominator
                degrees of freedom.")),
      br(),
      div(style = "text-align: center",
          bsButton(
            inputId = "explore", 
            label = "Explore",
            icon = icon("bolt"), 
            size = "large"))
    ),
    
    tabItem(tabName = "app",
      h2("ANOVA Test Simulator"),
      fluidRow(
        column(4,
          sliderInput(
            inputId = 'mu1', 
            label = 'Group 1 Mean \\(\\mu_1\\)',
            min = -5, max = 5, value = 1, step = .1),
          bsPopover(
            id = "mu1", 
            title = "Group Mean 1", 
            content = "True Mean for 1st Group"),
          sliderInput(
            inputId = 'mu2', 
            label = 'Group 2 Mean \\(\\mu_2\\)', 
            min = -5, max = 5, value = 1, step = .1),
          bsPopover(
            id = "mu2", 
            title = "Group Mean 2", 
            content = "True Mean for 2nd Group"),
          sliderInput(
            inputId = 'mu3', 
            label = 'Group 3 Mean \\(\\mu_3\\)', 
            min = -5, max = 5, value = 1, step = .1),
          bsPopover(
            id = "mu3", 
            title = "Group Mean 3",
            content = "True Mean for 3rd Group"),
          sliderInput(
            inputId = 'sigma',
            label = 'Standard Deviation \\(\\sigma\\)', 
            min = 0, max = 10, value = 1, step = .1),
          bsPopover(
            id = "sigma", 
            title = "Standard Deviation", 
            content = "Common standard deviation within group"),
          sliderInput(
            inputId = 'n',
            label = 'Sample size(\\(n\\)) ',
            min = 0, max = 100, value = 30, step = 1),
          bsPopover(
            id = "n", 
            title = "# of Samples", 
            content = "Sample Size per group"),
          sliderInput(
            inputId = 'sim', 
            label = 'Number of Simulations', 
            min = 0, max = 1000, value = 30, step = 1),
          bsPopover(
            id = "sim", 
            title = "Simulations", 
            content = "Number of Simulations with above conditions"),
                
          br(),
          div(style = "text-align: center",
              bsButton(
                inputId = "gogame", 
                label = "Go to the Games", 
                icon = icon("bolt"),
                size = "large"))
        
      ),
        column(8,
          plotOutput('aovPlot'),
          bsPopover(
            id = 'aovPlot',
            title = "ANOVA Box Plot", 
            content = paste("Box Plot for 3 Groups of Means with Standard Deviation",
                            "Black bars denote group means.")),
          h3('Example ANOVA Table for 1 Simulation'),
          verbatimTextOutput('aovSummary'),
          bsPopover(
            id = "aovSummary", 
            title = "ANOVA Summary Table", 
            content = "ANOVA Summary Table for last Simulation"),
          plotOutput('pvalueplot'),
          bsPopover(
            id = 'pvalueplot', 
            title = "Histogram of p-values", 
            content = "Histogram of P-Values through Simulations", 
            placement = "top")
        )
      )
    ),
    
    # Drag and Drop game
    tabItem(tabName = "game1",
      tabsetPanel(id = "games1",
        tabPanel(title = h2("Matching Game"), value = "instruct1",
          fluidRow(
            column(width = 10,
              h3("Instructions:"),
              tags$ul(
                tags$li("Click Go! to start the game"),
                tags$li("Match each picture to the corresponding group means given
                        in each box"),
                tags$li("At the top of each game, there will be parameters being
                        held constant"),
                tags$li("If needed, go back to the app and play around with the 
                        variables to try and recreate the images")),
              div(style = "text-align: center",
                bsButton(inputId = "go1", label = "GO!", 
                            icon = icon("bolt"), size = "large"))
            )
          )
        ),
        
        #level 1
        tabPanel(title = h3("Level 1"), value = "l1",
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
        
        #level 2
        tabPanel(title = h3("Level 2"), value = "l2",
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
        
        #level 3
        tabPanel(title = h3("Level 3"), value = "l3",
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
              
        #level 4
        tabPanel(title = h3("Level 4"), value = "l4",
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
        
        #level 5
        tabPanel(title = h3("Level 5"), value = "l5",
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
    #Fill in the blank
    tabItem(tabName = "game2",
      tabsetPanel(id = "games2",
        tabPanel(title = h2("Fill in the Blank"), value = "instruct2",
          fluidRow(
            column(width = 10,
              h3("Instructions"),
              tags$ul(
                tags$li("Click Go! to start the game"),
                tags$li("Fill in the blanks with each answer to make the statement
                        true"),
                tags$li("Hint: There is more than one answer to each statement")),
              div(style = "text-align: center",
                  bsButton(
                    inputId = "go2",
                    label = "GO!",
                    icon = icon("bolt"),
                    size = "large"))
            )
          )
        ),
         
        #filling blanks game
        tabPanel(title = h3("Game"), value = "fill1",
          fluidRow(
            column(4,
              wellPanel(
                selectInput(inputId = "firstans",
                            label = "Question 1: First Blank",
                            choices = c('', "equal", "different")),
                selectInput(inputId = "secondans",
                            label = "Question 1: Second Black",
                            choices = c('',"will", "won't")),
                selectInput(inputId = "thirdans",
                            label = "Question 2: Third Black",
                            choices = c('', "Level", "Skewed", "Stays the same")),
                selectInput(inputId = "fourthans",
                            label = "Question 2: Fourth Blank",
                            choices = c('', "Increases", "Decreases", "Stays the same")),
                selectInput(inputId = "fifthans",
                            label = "Question 3: Fifth Blank",
                            choices = c('', "Increases", "Decreases", "stays the same")),
                selectInput(inputId = "sixthans",
                            label = "Question 3: Sixth blank",
                            choices = c('', "Right", "Left"))
              )
            ),
            column(8,
              p(textOutput("first")),
              fluidRow(
                column(3,
                  bsButton(inputId = "checkfirst", label = "Submit", size = "large")),
                column(5,
                  conditionalPanel("input.checkfirst != 0", htmlOutput("ans1")))),
              br(),br(),br(),br(),
              p(textOutput("second")),
              fluidRow(
                column(3,
                  bsButton(inputId = "checksecond", label = "Submit", size = "large")),
                column(5,
                  conditionalPanel("input.checksecond != 0", htmlOutput("ans2")))),
              br(),br(),br(),br(),
              p(textOutput("third")),
              fluidRow(
                column(3,
                  bsButton(inputId = "checkthird", label = "Submit", size = "large")),
                column(5,
                  conditionalPanel("input.checkthird != 0", htmlOutput("ans3"))))
            )
          )
        )
      )
    ),
  
    # Refrence Page
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