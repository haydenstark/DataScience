
#install.packages("shinydashboard")
library(lattice)
library(latticeExtra)
library(randomForest)
library(caret)
library(shiny)
library(shinydashboard)

data(iris)
head(iris)
unique(iris$Species)

setosa = subset(iris, subset=Species=='setosa')
versi = subset(iris, subset=Species=='versicolor')     
virg = subset(iris, subset=Species=='virginica')

iris_means = data.frame(
  Sepal.Length=c(
    mean(setosa$Sepal.Length),
    mean(versi$Sepal.Length),
    mean(virg$Sepal.Length)
  ),
  Sepal.Width=c(
    mean(setosa$Sepal.Width),
    mean(versi$Sepal.Width),
    mean(virg$Sepal.Width)
  ),
  Petal.Length=c(
    mean(setosa$Petal.Length),
    mean(versi$Petal.Length),
    mean(virg$Petal.Length)
  ),
  Petal.Width=c(
    mean(setosa$Petal.Width),
    mean(versi$Petal.Width),
    mean(virg$Petal.Width)
  ),
  Species=c('setosa','versicolor','virginica')
)
iris_means

set.seed(42)
sample = sample.int(n = nrow(iris), size=floor(.8*nrow(iris)), replace=F)
train = iris[sample, c(3:5)]
test = iris[-sample, c(3:5)]

train_y = train[, 'Species']
train_x = train[, names(train)!='Species']
rf_model = randomForest(train_x, y=train_y, ntree=500, importance=T)
rf_model$confusion
rf_model$importance

test_y = test[, 'Species']
test_x = test[, names(test)!='Species']

predict = predict(rf_model, test_x)

confusionMatrix(data=predict, reference=test_y)

predictdf = data.frame(Petal.Length=c(7,4,7), Petal.Width=c(7,5,9)) #dummy data
predict(rf_model, newdata=predictdf)


#---------------------------------------------
ui = dashboardPage(
  skin='green',
  dashboardHeader(title = 'A Look into Iris Data'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Model", tabName = "model", icon = icon("chevron-right")),
      menuItem("Petal", tabName = "petals", icon = icon("chevron-right")),
      menuItem("Sepal", tabName = "sepals", icon = icon("chevron-right"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "model",
              box(plotOutput("interactive", height=350), width=12),
              box(h4("Your predicted species is:", align="right"),
                  h4(strong(textOutput("predict")), align="right"), width=6),
              box(sliderInput(inputId="petallength", "Petal Length:", min=0, max=8.0, value=4, step=.2),
                  sliderInput(inputId="petalwidth", "Petal Width:", min=0, max=4.5, value=2, step=.2), width=6)
      ),
      
      tabItem(tabName = "petals",
              h3("Petal Length by Petal Width"),
              box(plotOutput("petal", height=350), width=12),
              h4("Mean Average"),
              box(plotOutput("petal_mean", height=350), width=12),
      ),
      
      tabItem(tabName = "sepals",
              h3("Sepal Length by Sepal Width"),
              box(plotOutput("sepal", height=350), width=12),
              h4("Mean Average"),
              box(plotOutput("sepal_mean", height=350), width=12)
      )
    )
  )
)

server = function(input, output){
  output$predict = renderText({
    user_input = data.frame(
      Petal.Length = input$petallength,
      Petal.Width = input$petalwidth
    )
    
    species = predict(
      object=rf_model,
      newdata = user_input
    )
    
    return(as.character(species))
  })
  
  output$interactive = renderPlot({
    sctplt = xyplot(
      x=Petal.Width~Petal.Length,
      data=iris,
      group=Species,
      xlab='Petal Length (cm)',
      ylab='Petal Width (cm)',
      xlim=c(0,8),
      ylim=c(0,4.5),
      auto.key = list(
        title="Species",
        x = .01,
        y = .98),
      par.settings = list(superpose.symbol = list(col = c('darkorange','orchid','seagreen'), pch = 16))
    )
    point = xyplot(
      x=input$petalwidth~input$petallength,
      col='red',
      pch=13,
      cex=2
    )
    return(sctplt + as.layer(point))
  })
  
  output$petal = renderPlot({
    xyplot(
      x=Petal.Width~Petal.Length,
      data=iris,
      group=Species,
      xlab='Petal Length (cm)',
      ylab='Petal Width (cm)',
      xlim=c(0,8),
      ylim=c(0,4.5),
      auto.key = list(
        title="Species",
        x = .01,
        y = .98),
      par.settings = list(superpose.symbol = list(col = c('darkorange','orchid','seagreen'), pch = 16))
    )
  })
  
  output$petal_mean = renderPlot({
    xyplot(
      x=Petal.Width~Petal.Length,
      data=iris_means,
      group=Species,
      xlab='Petal Length (cm)',
      ylab='Petal Width (cm)',
      xlim=c(0,8),
      ylim=c(0,4.5),
      auto.key = list(
        title="Species",
        x = .01,
        y = .98),
      par.settings = list(superpose.symbol = list(col = c('darkorange','orchid','seagreen'), pch = 16, cex=1.5))
    )
  })
  
  output$sepal = renderPlot({
    xyplot(
      x=Sepal.Width~Sepal.Length,
      data=iris,
      group=Species,
      xlab='Sepal Length (cm)',
      ylab='Sepal Width (cm)',
      xlim=c(0,8),
      ylim=c(0,4.5),
      auto.key = list(
        title="Species",
        x = .01,
        y = .98),
      par.settings = list(superpose.symbol = list(col = c('darkorange','orchid','seagreen'), pch = 16))
    )
  })
  
  output$sepal_mean = renderPlot({
    xyplot(
      x=Sepal.Width~Sepal.Length,
      data=iris_means,
      group=Species,
      xlab='Sepal Length (cm)',
      ylab='Sepal Width (cm)',
      xlim=c(0,8),
      ylim=c(0,4.5),
      auto.key = list(
        title="Species",
        x = .01,
        y = .98),
      par.settings = list(superpose.symbol = list(col = c('darkorange','orchid','seagreen'), pch = 16, cex=1.5))
    )
  })
}

shinyApp(ui, server)
