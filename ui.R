library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyDND)
library(shiny)
library(shinydashboard)
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(shinyWidgets)
sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min, 
                               "data-from-max" = from_max 
                               )
  x
}

sliderInput3 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step, animate=TRUE)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min, 
                               "data-from-max" = from_max 
  )
  x
}

header = dashboardHeader(title = "One-Way ANOVA",
                             tags$li(class= "dropdown",
                                     tags$a(href='https://shinyapps.science.psu.edu/',
                                            icon("home",lib = "font-awesome"))),
                             tags$li(class="dropdown",
                                        actionLink("info", icon("info"), class = "myClass"))

                         )

sidebar = dashboardSidebar(
  
  sidebarMenu(id = "tabs",
    menuItem("Prerequisites",tabName = "pre",icon = icon("book")),
    menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
    menuItem("Application",icon = icon('wpexplorer'),tabName = "app"),
    menuItem("Games",tabName = "game",icon = icon("gamepad"),
             menuSubItem("Matching Game", tabName = "game1", icon = icon("gamepad")),
             menuSubItem("Fill in the Blank", tabName = "game2", icon = icon("gamepad")))
  )
)

body = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    tabItem(tabName = "pre",
            
            withMathJax(),
            h3(tags$b("Background: One-Way ANOVA")),br(),
            h4(tags$li("This module demonstrates one-way Analysis of Variance.")),
            h4(tags$li("The goal is to test the null hypothesis \\(H_0: \\mu_1=\\mu_2=\\mu_3\\).")),
            h4(tags$li("The hypothesis is tested using the ratio of between group variability to within group variability \\(F=MS_{between}/MS_{within}\\).
                 which is compared to the F distribution with (number of groups – 1) = 2 numerator degrees of freedom and the (sample size – number of groups) = N-3 denominator degrees of freedom.")),
            
            br(),
            div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "large", class="circle grow"))
            ),
    tabItem(tabName = "overview",
            tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
            br(),br(),br(),
            h3(tags$b("About:")),
            h4(p("The goals of this app are to examine how the one way ANOVA behaves under the null and different alternatives. 
                 The app uses the F-test to examine the ANOVA for three group means, and analyzes the P-values drawn from each simulation to observe whether the null hypothesis is rejected or not.")),
            br(),
            h3(tags$b("Instructions:")),
            h4(tags$li("Move each slider to observe what happens to the boxplot.")),
            h4(tags$li("Then, increase the number of simulations and observe the Distribution of P-values and how they are affected by each slider.")),
            h4(tags$li("After you have sufficiently experimented with the different sliders, move on to the game and test your knowledge!")),
            
            div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large", class="circle grow")),
            br(),
            h3(tags$b("Acknowledgements:")),
            h4(p("This app was developed and coded by Alex Chen and improved by Yiyang Wang.  Special thanks to Jinglin Feng for being my partner in this project and Yuxin Zhang for the format of the Matching games."))
            ),
    
    tabItem(tabName = "app",
            # div(style="display: inline-block;vertical-align:top;",
            #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
            # ),
            h3("ANOVA Test Simulator"),
            sidebarLayout(
              sidebarPanel(
                tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #BB8FCE }")),
                tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #BB8FCE }")),
                tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #BB8FCE }")),
                tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #BB8FCE }")),
                tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #BB8FCE }")),
                tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #BB8FCE }")),
                sliderInput('mu1', 'Group 1 Mean \\(\\mu_1\\)', min = -5, max = 5, value = 1, step = .1),
                bsPopover("mu1", "Group Mean 1", "True Mean for 1st Group"),
                sliderInput('mu2', 'Group 2 Mean \\(\\mu_2\\)', min = -5, max = 5, value = 1, step = .1),
                bsPopover("mu2", "Group Mean 2", "True Mean for 2nd Group"),
                sliderInput('mu3', 'Group 3 Mean \\(\\mu_3\\)', min = -5, max = 5, value = 1, step = .1),
                bsPopover("mu3", "Group Mean 3", "True Mean for 3rd Group"),
                sliderInput('sigma', 'Standard Deviation \\(\\sigma\\)', min = 0, max = 10, value = 1, step = .1),
                bsPopover("sigma", "Standard Deviation", "Common standard deviation within group"),
                sliderInput2('n', 'Sample size(\\(n\\)) ', min = 0, max = 100, value = 30, step = 1, from_min = 2, from_max = 100),
                bsPopover("n", "# of Samples", "Sample Size per group"),
                sliderInput3('sim', 'Number of Simulations', min = 0, max = 1000, value = 30, step = 1, from_min = 1,from_max = 1000),
                bsPopover("sim", "Simulations", "Number of Simulations with above conditions"),
                
                br(),
                div(style = "text-align: center",bsButton("gogame", "Go to the Games", icon("bolt"), size = "large", class="circle grow"))
                
              ),
              
              mainPanel(
                plotOutput('aovPlot'),
                bsPopover('aovPlot', "ANOVA Box Plot", "Box Plot for 3 Groups of Means with Standard Deviation.  Black bars denote group means."),
                h3('Example ANOVA Table for 1 Simulation'),
                verbatimTextOutput('aovSummary'),
                bsPopover("aovSummary", "ANOVA Summary Table", "ANOVA Summary Table for last Simulation"),
                plotOutput('pvalueplot'),
                bsPopover('pvalueplot', "Histogram of p-values", "Histogram of P-Values through Simulations", placement = "top")
              )
            )
            ),

    tabItem(tabName = "game1",
            div(style="display: inline-block;vertical-align:top;",
                tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
            ),
            tabsetPanel(id = "games1",
              tabPanel(title = h5("Matching Game"), value = "instruct1",
                       fluidRow(column(width = 10,
                                       h2(tags$b("Instructions:")), br()
                       )
                       ),
                       fluidPage(
                         theme = "style.css",
                         #titlePanel("Instructions:"),
                         h3(p("Click Go! to start the game")),
                         h3(p("Match each picture to the corresponding group means given in each box")),
                         h3(p("At the top of each game, there will be parameters being held constant")),
                         h3(p("If needed, go back to the app and play around with the variables to try and recreate the images")),
                         fluidRow(column(1, offset = 5, actionButton("go1", "G O !", icon("bolt"), size = "large", class="circle grow")))
                       )
              ),
              #level 1
              tabPanel(title = h5("Level 1"), value = "l1",
                       fluidRow(column(12, offset = 4, h3("Find Matches Assuming: \\(n\\) = 50 and \\(\\sigma\\) = 1"))),
                       br(),br(),
                       
                       #A & B
                       fluidRow(wellPanel(div(style = "text-align:center", h3("A")),
                                          img(src = "oneatfive.JPG", class = "picSize"),
                                          dragUI("img9","A", style = "background-color:orange; width: 200px;height:35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("B")),
                                          img(src = "oneattwo.JPG", class = "picSize"),
                                          dragUI("img10","B", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                      # Choices
                       fluidRow(
                         fluidRow(wellPanel(
                           dropUI("drop9",class = "dropelement dropelement2"),
                           div(style = "position: absolute; top:0;left:1em; font-size: 125%",h5("\\(\\mu_1=\\mu_1=\\mu_1\\)")),
                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer9")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop10",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0;left:1.2em; font-size: 75%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 0\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer10")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop11",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 5\\ , \\mu_3 = 1\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer11")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop12",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 1\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer12")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                         )
                       ),br(),br(),
                       
                       #C & D
                       fluidRow(wellPanel(div(style = "text-align:center", h3("C")),
                                          img(src = "splitbyone.JPG", class = "picSize"),
                                          dragUI("img11","C", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("D")),
                                          img(src = "allone.JPG", class = "picSize"),
                                          dragUI("img12","D", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       br(),
                       conditionalPanel("(input.drop9 != '') & (input.drop10 != '') & (input.drop11 != '') & (input.drop12 != '')",
                                        # fluidRow(column(2, offset = 10, bsButton("submit2", "Submit!", style = "primary"))),
                                        # br(),
                                        # fluidRow(column(2, offset = 10, bsButton("next1", "Next>>", style = "primary", disabled = F)))
                                        div(style = "text-align:center",
                                            bsButton("submit2", "Submit!"),
                                            bsButton("next1", "Next>>",disabled = F))
                       )
              ),
              #level 2
              tabPanel(title = h5("Level 2"), value = "l2",
                       fluidRow(column(12, offset = 3, h3("Find Matches Assuming: \\(n\\) = 50 and \\(\\mu_1 = 1\\ , \\mu_2 = 0\\ , \\mu_3 = -1\\)"))),
                       br(),br(),
                       
                       fluidRow(wellPanel(div(style = "text-align:center", h3("A")),
                                          img(src = "sd1.JPG", class = "picSize"),
                                          dragUI("img13","A", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("B")),
                                          img(src = "sd0.JPG", class = "picSize"),
                                          dragUI("img14","B", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       
                       fluidRow(
                         fluidRow(wellPanel(
                           dropUI("drop13",class = "dropelement dropelement2"),
                           div(style = "position: absolute; top:0;left:1em; font-size: 125%",h5("\\(\\sigma\\) = 1")),
                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer13")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop14",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0;left:1.2em; font-size: 75%",h5("\\(\\sigma\\) = 10")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer14")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop15",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\sigma\\) = 5")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer15")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop16",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\sigma\\) = 0")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer16")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                         )
                       ),br(),br(),
                       
                       fluidRow(wellPanel(div(style = "text-align:center", h3("C")),
                                          img(src = "sd5.JPG", class = "picSize"),
                                          dragUI("img15","C", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("D")),
                                          img(src = "sd10.JPG", class = "picSize"),
                                          dragUI("img16","D", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       br(),
                       conditionalPanel("(input.drop13 != '') & (input.drop14 != '') & (input.drop15 != '') & (input.drop16 != '')",
                                        # fluidRow(column(2, offset = 10, bsButton("submit3", "Submit!", style = "primary"))),
                                        # br(),
                                        # fluidRow(column(2, offset = 10, bsButton("next2", "Next>>", style = "primary", disabled = F)))
                                        div(style = "text-align:center",
                                            bsButton("submit3", "Submit!", style = "primary"),
                                            bsButton("next2", "Next>>", style = "primary", disabled = F))
                       )
                       
                
              ),
              #level 3
              tabPanel(title = h5("Level 3"), value = "l3",
                       fluidRow(column(12, offset = 4, h3("Find Matches Assuming: \\(n\\) = 50"))),
                       br(),br(),
                       fluidRow(wellPanel(div(style = "text-align:center", h3("A")),
                                          img(src = "lastlevel0.JPG", class = "picSize"),
                                          dragUI("img17","A", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("B")),
                                          img(src = "lastlevel1.JPG", class = "picSize"),
                                          dragUI("img18","B", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       
                       fluidRow(
                         fluidRow(wellPanel(
                           dropUI("drop17",class = "dropelement dropelement2"),
                           div(style = "position: absolute; top:0;left:1em; font-size: 125%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = -2\\ , \\mu_3 = 1\\), \\(\\sigma\\) = 2.5")),
                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer17")), class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop18",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0;left:1em; font-size: 75%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1\\ , \\mu_3 = 1\\), \\(\\sigma\\) = 1")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer18")), class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop19",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = -1\\ , \\mu_2 = 0\\ , \\mu_3 = 1\\), \\(\\sigma\\) = 4")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer19")), class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop20",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = 5\\ , \\mu_2 = 0\\ , \\mu_3 = -5\\),  \\(\\sigma\\) = 10")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer20")), class = "wellTransparent3 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                         )
                       ),br(),br(),
                       
                       fluidRow(wellPanel(div(style = "text-align:center", h3("C")),
                                          img(src = "allone.JPG", class = "picSize"),
                                          dragUI("img19","C", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("D")),
                                          img(src = "lastlevel2.JPG", class = "picSize"),
                                          dragUI("img20","D", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       br(),
                       conditionalPanel("(input.drop17 != '') & (input.drop18 != '') & (input.drop19 != '') & (input.drop20 != '')",
                                        # fluidRow(column(2, offset = 10, bsButton("submit4", "Submit!", style = "primary"))),
                                        # br(),
                                        # fluidRow(column(2, offset = 10, bsButton("next3", "Next>>", style = "primary", disabled = F)))
                                        div(style = "text-align:center",
                                            bsButton("submit4", "Submit!", style = "primary"),
                                            bsButton("next3", "Next>>", style = "primary", disabled = F))
                       )
              ),
              #level 4
              tabPanel(title = h5("Level 4"), value = "l4",
                       fluidRow(column(12, offset = 4, h3("Find Matches Assuming: \\(n\\) = 30 and \\(\\sigma\\) = 1"))),
                       br(),br(),
                       fluidRow(wellPanel(div(style = "text-align:center", h3("A")),
                                          img(src = "same.JPG", class = "picSize"),
                                          dragUI("img1","A", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("B")),
                                          img(src = "attwo.JPG", class = "picSize"),
                                          dragUI("img2","B", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       
                       fluidRow(
                         fluidRow(wellPanel(
                           dropUI("drop1",class = "dropelement dropelement2"),
                           div(style = "position: absolute; top:0;left:1em; font-size: 125%",h5("\\(\\mu_1=\\mu_2=\\mu_3\\)")),
                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer1")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop2",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0;left:1.2em; font-size: 75%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1.5\\ , \\mu_3 = 0.5\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer2")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop3",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 2\\ , \\mu_3 = 1\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer3")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop4",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(\\mu_1 = 1\\ , \\mu_2 = 1\\ , \\mu_3 = 1.5\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer4")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                         )
                       ),br(),br(),
                       
                       fluidRow(wellPanel(div(style = "text-align:center", h3("C")),
                                          img(src = "byhalf.jpg", class = "picSize"),
                                          dragUI("img3","C", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("D")),
                                          img(src = "oneathalf.JPG", class = "picSize"),
                                          dragUI("img4","D", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       br(),
                       conditionalPanel("(input.drop1 != '') & (input.drop2 != '') & (input.drop3 != '') & (input.drop4 != '')",
                                        # fluidRow(column(2, offset = 10, bsButton("submit", "Submit!", style = "primary"))),
                                        # br(),
                                        # fluidRow(column(2, offset = 10, bsButton("next4", "Next>>", style = "primary", disabled = F)))
                                        div(style = "text-align:center",
                                            bsButton("submit", "Submit!", style = "primary"),
                                            bsButton("next4", "Next>>", style = "primary", disabled = F))
                       )
              ),
              #level 5
              tabPanel(title = h5("Level 5"), value = "l5",
                       fluidRow(column(12, offset = 3, h3("Find Matches Assuming: \\(\\mu_1 = 1\\ , \\mu_2 = 1.5\\ , \\mu_3 = 0.5\\)"))),
                       br(),br(),
                       fluidRow(wellPanel(div(style = "text-align:center", h3("A")),
                                          img(src = "n=10.JPG", class = "picSize"),
                                          dragUI("img5","A", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("B")),
                                          img(src = "n=50.JPG", class = "picSize"),
                                          dragUI("img6","B", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       
                       fluidRow(
                         fluidRow(wellPanel(
                           dropUI("drop5",class = "dropelement dropelement2"),
                           div(style = "position: absolute; top:0;left:1em; font-size: 125%",h5("\\(n\\ = 10,  \\sigma = 5\\)")),
                           div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer5")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop6",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0;left:1.2em; font-size: 75%",h5("\\(n\\ = 10\\ ,  \\sigma = 1\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer6")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop7",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(n\\ = 50\\ ,  \\sigma = 5\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer7")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12"),
                           wellPanel(
                             dropUI("drop8",class = "dropelement dropelement2"),
                             div(style = "position: absolute; top:0; left:1em; font-size: 125%",h5("\\(n\\ = 50\\ ,  \\sigma = 1\\)")),
                             div(style = "position: absolute; top:20%; right:8%;", htmlOutput("answer8")), class = "wellTransparent2 col-lg-3 col-md-6 col-sm-6 col-xs-12")
                         )
                       ),br(),br(),
                       
                       fluidRow(wellPanel(div(style = "text-align:center", h3("C")),
                                          img(src = "n=10s=5.JPG", class = "picSize"),
                                          dragUI("img7","C", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder"),
                                
                                wellPanel(div(style = "text-align:center", h3("D")),
                                          img(src = "n=50s=5.JPG", class = "picSize"),
                                          dragUI("img8","D", style = "background-color:orange; width: 200px; height: 35px")
                                          ,class = "col-lg-6 col-md-12 wellBorder")),
                       
                       br(),
                       conditionalPanel("(input.drop5 != '') & (input.drop6 != '') & (input.drop7 != '') & (input.drop8 != '')",
                                        # fluidRow(column(2, offset = 10, bsButton("submit1", "Submit!", style = "primary"))),
                                        # fluidRow(column(2, offset = 10, bsButton("next5", "Next>>", style = "primary", disabled = F)))
                                        div(style = "text-align:center",
                                            bsButton("submit1", "Submit!", style = "primary"),
                                            bsButton("next5", "Next>>", style = "primary", disabled = F))
                       )
              )
            )
            ),
    #Fill in the blank
    tabItem(tabName = "game2",
            div(style="display: inline-block;vertical-align:top;",
                tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
            ),
      tabsetPanel(id = "games2",
                  tabPanel(title = h5("Fill in the Blank"), value = "instruct2",
                           fluidRow(column(width = 10,
                                           h2(tags$b("Instructions:")), br()
                           )
                           ),
                           fluidPage(
                             theme = "bootstrap.css",
                             #titlePanel("Instructions for Fill in the blank!"),
                             h3(p("Click Go! to start the game")),
                             h3(p("Fill in the blanks with each answer to make the statement true")),
                             h3(p("Hint: There is more than one answer to each statement")),
                             
                             #fluidRow(column(3, offset = 9, textOutput("timer2"))),
                             fluidRow(column(1, offset = 5, bsButton("go2", "G O !", icon("bolt"), size = "large", class="circle grow")))
                             
                           )
                           
                  ),
                  
                  #filling blanks game
                  tabPanel(title = h5("Game"), value = "fill1",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("firstans",
                                    "1: First Answer",
                                    c('', "equal", "different")),
                        selectInput("secondans",
                                    "2: Second Answer",
                                    c('',"will", "won't")),
                        selectInput("thirdans",
                                    "3: Third Answer",
                                    c('', "Level", "Skewed", "Stays the same")),
                        selectInput("fourthans",
                                    "4: Fourth Answer",
                                    c('', "Increases", "Decreases", "Stays the same")),
                        selectInput("fifthans",
                                    "5: Fifth Answer",
                                    c('', "Increases", "Decreases", "stays the same")),
                        selectInput("sixthans",
                                    "6: Sixth Answer",
                                    c('', "Right", "Left"))
                      ),
                      mainPanel(
                        h4(textOutput("first")),
                        bsButton("checkfirst", "Check!"),
                        conditionalPanel("input.checkfirst != 0",
                                         htmlOutput("ans1")),
                        br(),br(),br(),br(),
                        
                        h4(textOutput("second")),
                        bsButton("checksecond", "Check!"),
                        conditionalPanel("input.checksecond != 0",
                                         htmlOutput("ans2")),
                        br(),br(),br(),br(),
                        
                        h4(textOutput("third")),
                        bsButton("checkthird", "Check!"),
                        conditionalPanel("input.checkthird != 0",
                                         htmlOutput("ans3"))
                        
                      )
                    )
                  )
        
      )
    )
    
  )
)

shinyUI(dashboardPage(skin = "black", header, sidebar, body))