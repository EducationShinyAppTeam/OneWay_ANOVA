library(shiny)
library(ggplot2)

# Add in plot interaction with ggplot
# Include better boxplot -- Done
# Other things other than normal data -- Can do once students can draw their own curves
#######  Actual App ###################################################################
shinyServer(function(input, output, session) {
  #Go to overview Button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "overview")
  })
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "app")
  })
  #Go to the Game Button
  observeEvent(input$gogame, {
    updateTabItems(session, "tabs", "game1")
  })
  
  gen_data = reactive({
    value = c(input$n, 
              input$mu1, 
              input$mu2, 
              input$mu3,
              input$sigma
    )
  })
  
  #Creates the BoxPlot *Need to make a ggplot of this to make more interactive*
  output$aovPlot = renderPlot({
    value = gen_data()
    df = data.frame(y = c(rnorm(value[1], value[2], value[5]), 
                          rnorm(value[1], value[3], value[5]), 
                          rnorm(value[1], value[4], value[5])),
                    group = rep(sprintf('Group %s', 1:3), each = value[1]))
    ggplot(data = df, aes(x = df$group, y = df$y)) + 
      geom_boxplot(data = df, aes(x = df$group, y = df$y)) + 
      geom_point(data = df, aes(x = df$group, y = df$y)) +
      ggtitle("Example Data by Groups") + ylab("Values") + xlab("") +
    theme(plot.title = element_text(hjust = 0.5, size = 24),
          text = element_text(size = 18))
      
  })
  
  #Summary of the data
  output$aovSummary = renderPrint({
    value = gen_data()
    df = data.frame(`y` = c(rnorm(value[1], value[2], value[5]), 
                          rnorm(value[1], value[3], value[5]), 
                          rnorm(value[1], value[4], value[5])),
                    `Group` = rep(sprintf('mu%s', 1:3), each = value[1]))
    
    summary(aov(y ~ Group, data = df))
  })
  
  #Displays the F Crit Value
  output$Fcrit = renderPrint({
    value = gen_data()
    df = data.frame(y = c(rnorm(value[1], value[2], value[5]), 
                          rnorm(value[1], value[3], value[5]), 
                          rnorm(value[1], value[4], value[5])),
                    group = rep(sprintf('mu%s', 1:3), each = value[1]))
    
    dfsum = summary(aov(df$y ~ df$group))
    qf(0.95, df1 = dfsum[[1]]$Df[1], dfsum[[1]]$Df[2])
  })
  # output$Fdist = renderPlot({
  #   value = gen_data()
  #   df = data.frame(y = c(rnorm(value[1], value[2], value[5]), 
  #                         rnorm(value[1], value[3], value[5]), 
  #                         rnorm(value[1], value[4], value[5])),
  #                   group = rep(sprintf('mu%s', 1:3), each = value[1]))
  #   dfsum = summary(aov(df$y ~ df$group))
  #   df1 = dfsum[[1]]$Df[1]
  #   df2 = dfsum[[1]]$Df[2]
  #   curve(df(x, df1 = df1, df2 = df2), from = 0, to = 100, xLab = "F-statistic
  #         ")# Add in ability to repeat same sample. P-value distribution changes
  # })
  
  #Makes number of simulations into a reactive variable
  counter = reactive(input$sim)
  #Creates the P-value plot through the number of simulations
  output$pvalueplot = renderPlot({
    simulations = counter()
    vector = c()
    value = gen_data()
    for(i in 1:(simulations)){
      # for(i in 0:input$sim){
      df = data.frame(y = c(rnorm(value[1], value[2], value[5]), 
                            rnorm(value[1], value[3], value[5]), 
                            rnorm(value[1], value[4], value[5])),
                      group = rep(sprintf('mu%s', 1:3), each = value[1]))
      
      dfsum = summary(aov(df$y ~ df$group))
      pvalue = dfsum[[1]]$`Pr(>F)`[1]
      vector = c(vector, pvalue)
    }
    hist(vector, main = paste("P-Value Distribution for", input$sim, "Simulations"), 
         xlab = "P-value", xlim = range(0,1), font.lab = 2, font = 2, col = "lightblue")
    
  })
  # Different types(not just normal) of populations
  # Boxplot from ggplot
  # Be able to select points and see where they are on the box plot
  
  
  ###########################################################################################################
  ######################## Matching #########################################################################
  time<-reactiveValues(inc=0, timer=reactiveTimer(1000), started=F)
  
  observeEvent(input$go, {time$started<-T})
  observeEvent(input$submit, {time$started <- F})
  
  values = reactiveValues(
    count = 0
  )
  
  observe({
    time$timer()
    if(isolate(time$started))
      time$inc<-isolate(time$inc)+1
  })
  
  # output$timer <- renderPrint({
  #   cat("You have used:", time$inc, "secs")})
  
  # output$score = renderText({
  #   values$count
  # })
observeEvent(input$submit, {
  output$answer1 = renderUI({
    if(!is.null(input$drop1)){
      if(input$drop1 == "A"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # value$count = values$count - 2
      }
    }
  })
  
  output$answer2 = renderUI({
    if(!is.null(input$drop2)){
      if(input$drop2 == "C"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer3 = renderUI({
    if(!is.null(input$drop3)){
      if(input$drop3 == "B"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer4 = renderUI({
    if(!is.null(input$drop4)){
      if(input$drop4 == "D"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
})

observeEvent(input$submit1, {
  output$answer5 = renderUI({
    if(!is.null(input$drop5)){
      if(input$drop5 == "C"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # value$count = values$count - 2
      }
    }
  })
  
  output$answer6 = renderUI({
    if(!is.null(input$drop6)){
      if(input$drop6 == "A"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer7 = renderUI({
    if(!is.null(input$drop7)){
      if(input$drop7 == "D"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer8 = renderUI({
    if(!is.null(input$drop8)){
      if(input$drop8 == "B"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
})

observeEvent(input$submit2, {
  output$answer9 = renderUI({
    if(!is.null(input$drop9)){
      if(input$drop9 == "D"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # value$count = values$count - 2
      }
    }
  })
  
  output$answer10 = renderUI({
    if(!is.null(input$drop10)){
      if(input$drop10 == "C"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer11 = renderUI({
    if(!is.null(input$drop11)){
      if(input$drop11 == "A"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer12 = renderUI({
    if(!is.null(input$drop12)){
      if(input$drop12 == "B"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
})

observeEvent(input$submit3, {
  output$answer13 = renderUI({
    if(!is.null(input$drop13)){
      if(input$drop13 == "A"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # value$count = values$count - 2
      }
    }
  })
  
  output$answer14 = renderUI({
    if(!is.null(input$drop14)){
      if(input$drop14 == "D"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer15 = renderUI({
    if(!is.null(input$drop15)){
      if(input$drop15 == "C"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer16 = renderUI({
    if(!is.null(input$drop16)){
      if(input$drop16 == "B"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
})

observeEvent(input$submit4, {
  output$answer17 = renderUI({
    if(!is.null(input$drop17)){
      if(input$drop17 == "A"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # value$count = values$count - 2
      }
    }
  })
  
  output$answer18 = renderUI({
    if(!is.null(input$drop18)){
      if(input$drop18 == "C"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer19 = renderUI({
    if(!is.null(input$drop19)){
      if(input$drop19 == "B"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
  
  output$answer20 = renderUI({
    if(!is.null(input$drop20)){
      if(input$drop20 == "D"){
        img(src = "check.png", width = 30)
        # values$count = values$count + 5
      }
      else{
        img(src = "cross.png", width = 30)
        # values$count = values$count - 2
      }
    }
  })
})

  
observeEvent(input$go1, {
  updateTabItems(session, "games1", selected = "l1")
})

observeEvent(input$go2, {
  updateTabItems(session, "games2", selected = "fill1")
})

observeEvent(input$next1, {
  updateTabItems(session, "games1", selected = "l2")
})

observeEvent(input$next2, {
  updateTabItems(session, "games1", selected = "l3")
})

observeEvent(input$next3, {
  updateTabItems(session, "games1", selected = "l4")
})

observeEvent(input$next4, {
  updateTabItems(session, "games1", selected = "l5")
})

observeEvent(input$next5, {
  updateTabItems(session, "tabs", selected = "game2")
})
  
  ###########################################################################################################
  ######################## Fill in the Blank ################################################################
  
#observeEvent(input$go2, {  
  output$first = renderText({
    if(input$firstans == '' && input$secondans == ''){
      paste("1. When the group means are ____1____, having a larger standard deviation ____2____ have an effect on the distribution of p-values")
    }
    else if(input$firstans == ''){
      paste("1. When the group means are ____1____, having a larger standard deviation", input$secondans, "have an effect on the distribution of p-values")
    }
    
    else if(input$secondans == ''){
      paste("1. When the group means are", input$firstans, ", having a larger standard deviation ____2____ have effect on the distribution of p-values")
    }
    else{
      paste("1. When the group means are", input$firstans, ", having a larger standard deviation", input$secondans ," have effect on the distribution of p-values")
    }
  })
  
  output$second = renderText({
    if(input$thirdans == '' && input$fourthans == ''){
      paste("2. The Distribution of P-values, under the alternative hypothesis is more ____3____ as the number of Samples ____4____")
    }
    else if(input$thirdans == ''){
      paste("2. The Distribution of P-values, under the alternative hypothesis is more ____3____ as the number of Samples", input$fourthans)
    }
    
    else if(input$fourthans == ''){
      paste("2. The Distribution of P-values, under the alternative hypothesis is more", input$thirdans, "as the number of Samples ____4____")
    }
    else{
      paste("2. The Distribution of P-values, under the alternative hypothesis is more", input$thirdans, "as the number of Samples", input$fourthans)
    }
  })
  
  output$third = renderText({
    if(input$fifthans == '' && input$sixthans == ''){
      paste("3. As the differences between the three means ____5____ the P-Value histogram becomes more ____6____ skewed")
    }
    else if(input$fifthans == ''){
      paste("3. As the differences between the three means ____5____ the P-Value histogram becomes more", input$sixthans, "skewed")
    }
    
    else if(input$fourthans == ''){
      paste("3. As the differences between the three means", input$fifthans, "the P-Value histogram becomes more ____6____ skewed")
    }
    else{
      paste("3. As the differences between the three means", input$fifthans, "the P-Value histogram becomes more", input$sixthans, "skewed")
    }
  })
  
  observeEvent(input$checkfirst, {
    if((input$firstans == "different" & input$secondans == "will") || 
       (input$firstans == "equal" && input$secondans == "won't")){
      output$ans1 = renderUI({
        img(src = "check.png", width = 75)
      })
    }
    else
      {
      output$ans1 = renderUI({
        img(src = "cross.png", width = 75)
      })
    }
    
  })
  
  observeEvent(input$checksecond, {
    if((input$thirdans == "Skewed" && input$fourthans == "Increases") ||
       (input$thirdans == "Level" && input$fourthans == "Decreases")){
      output$ans2 = renderUI({
        img(src = "check.png", width = 75)
      })
    }
    else{
      output$ans2 = renderUI({
        img(src = "cross.png", width = 75)
      })
    }
  })
  
  observeEvent(input$checkthird, {
    if((input$fifthans == "Increases" && input$sixthans == "Right") ||
       input$fifthans == "Decreases" && input$sixthans == "Left"){
      output$ans3 = renderUI({
        img(src = "check.png", width = 75)
      })
    }
    else{
      output$ans3 = renderUI({
        img(src = "cross.png", width = 75)
      })
    }
  })
#})
  
})