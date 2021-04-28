# Define server logic to read selected file ----
library(shiny)
library(corrplot)
library(ggfortify)
library(factoextra)
library(BiocManager)
library(ShinyQuickStarter)

options(repos = BiocManager::repositories())
wd <- getwd()
setwd(wd)#dirname(rstudioapi::getSourceEditorContext()$path))


shinyServer(function(input, output, session) {
  currentDataFrame <- data.frame
  
  
  covar <-
    function(X) {
      crossprod(sweep(X, 2L, colMeans(X))) / (nrow(X) - 1L)
    }
  
  observeEvent(input$doPCA, {
    req(input$checkboxes_PCA)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else{
        return(NULL)
      }
      
    }
    
    df <- currentDataFrame
    #accomplish covar of matrix of specified variables
    matrixSelection <- input$checkboxes_matrixop
    covar <-
      function(X) {
        crossprod(sweep(X, 2L, colMeans(X))) / (nrow(X) - 1L)
      }
    
    if (matrixSelection == "correlation Matrix") {
      currentData <- t(as.matrix(covar(df[c(input$checkboxes_PCA)])))
      rm = rowMeans(currentData)
      X = currentData - matrix(rep(rm, dim(currentData)[2]), nrow = dim(currentData)[1])
      # Calculate P
      A = X %*% t(X)
      E = eigen(A, TRUE)
      P = t(E$vectors)
      # Find the new data and standard deviations of the principal components
      newdata = P %*% X
      sdev = sqrt(diag((1 / (dim(
        X
      )[2] - 1) * P %*% A %*% t(P))))
      
      covarariance_matrix <-
        currentData#covar(df[c(input$checkboxes_PCA)])
      s.eigen <- eigen(covar(df[c(input$checkboxes_PCA)]))
      
      #s.eigen$values <- scale(s.eigen$values)
      
      
      
      output$matEigens <- renderPrint({
        s.eigen
      })
      
      output$pcaPlot <- renderPlot({
        par(mfrow = c(1, 2))
        #par(mar=c(0.1,0.1,0.1,0.1))
        cl <- rainbow(nrow(newdata))
        
        
        plot(
          newdata,
          xlab = 'PC1',
          ylab = 'PC2',
          main = 'Principal Component Plot',
          col = "red"
        )
        plot(
          s.eigen$values,
          xlab = 'Eigenvalue Number',
          ylab = 'Eigenvalue Size',
          main = 'Scree Graph',
          col = "blue"
        )
        lines(s.eigen$values, col = "blue")
        
        
        
        
        variances <- c()
        for (s in s.eigen$values) {
          variances <- c(variances, s / sum(s.eigen$values))
        }
        
        output_table <-
          data.frame(Eigenvalue = c(s.eigen$values),
                     Variance = c(variances))
        output$variance <-
          renderTable({
            output_table
          }, caption = "Data Variance per Eigenvalue", caption.placement = "top", hover = TRUE, width =
            "220px", rownames = TRUE, colnames = TRUE, bordered = TRUE)
        
        
      })
    }
    
    
    
    else if (matrixSelection == "Covariance Matrix" &&
             !(is.null(df))) {
      currentData <- t(as.matrix(cor(df[c(input$checkboxes_PCA)])))
      if (is.null(currentData)) {
        modalDialog(
          title = "Error",
          "You must read data before attempting an operation on the data!",
          footer = modalButton("Ok"),
          
        )
        return
      }
      
      rm = rowMeans(currentData)
      X = currentData - matrix(rep(rm, dim(currentData)[2]), nrow = dim(currentData)[1])
      # Calculate P
      A = X %*% t(X)
      E = eigen(A, TRUE)
      P = t(E$vectors)
      # Find the new data and standard deviations of the principal components
      newdata = P %*% X
      sdev = sqrt(diag((1 / (dim(
        X
      )[2] - 1) * P %*% A %*% t(P))))
      
      
      
      
      correlation_matrix <-
        currentData#cor(df[c(input$checkboxes_PCA)])
      r.eigen <- eigen(correlation_matrix)
      
      output$matEigens <- renderPrint({
        r.eigen
      })
      output$pcaPlot <- renderPlot({
        par(mfrow = c(1, 2))
        
        
        plot(
          newdata,
          xlab = 'PC1',
          ylab = 'PC2',
          main = 'Principal Component Plot',
          col = sample(rainbow(length(newdata)))
        )
        plot(
          r.eigen$values,
          xlab = 'Eigenvalue Number',
          ylab = 'Eigenvalue Size',
          main = 'Scree Graph',
          col = "black"#(rainbow(length(r.eigen$values)))
        )
        lines(r.eigen$values, col = "blue")
        
        variances <- c()
        for (r in r.eigen$values) {
          variances <- c(variances, r / sum(r.eigen$values))
        }
        
        output_table <-
          data.frame(Eigenvalue = c(r.eigen$values),
                     Variance = c(variances))
        output$variance <-
          renderTable({
            output_table
          }, caption = "Data Variance per Eigenvalue", caption.placement = "top", hover = TRUE, width =
            "220px", rownames = TRUE, colnames = TRUE, bordered = TRUE)
        
        #pca.plot <- autoplot(correlation_matrix,data=df[c(input$checkboxes_PCA)],color = "Group")
        #pca.plot
        
        
      })
    }
    
    
    
  })
  
  
  observeEvent(input$update, {
    output$corPlot <- renderPlot({
      if (isTruthy(input$infile)) {
        currentDataFrame <- read.csv(
          input$infile$datapath,
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
        
        
      }
      else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
        if (input$dataset == "Red Wine Quality") {
          currentDataFrame <- read.csv(
            "red_wine.csv",
            header = TRUE,
            sep = input$sep,
            quote = input$quote
          )
        }
        
        
        else if (input$dataset == "Adult Data Set") {
          currentDataFrame <- read.csv(
            "Adult.csv",
            header = TRUE,
            sep = input$sep,
            quote = input$quote
          )
        }
        else if (input$dataset == "Framingham Risk Score") {
          currentDataFrame <- read.csv(
            "Framingham_Test.csv",
            header = TRUE,
            sep = input$sep,
            quote = input$quote
          )
        }
        else{
          return(NULL)
        }
        
        
      }
      df <- currentDataFrame
      df <-
        df[, sapply(df, is.numeric)]#data[,-which(sapply(data, class) == "factor")]
      M <- cor(df)
      corrplot(M, method = input$corMethod)
      
    })
    
    
  })
  
  
  
  ##end of built in data sets
  
  
  plotdata <- reactive({
    #req(input$infile)
    
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
    }
    df <- currentDataFrame
    df
  })
  
  
  
  
  output$plot <- renderPlot({
    #req(input$infile)
    
    req(input$vars_PCA)
    #req(input$update)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
    }
    df <- currentDataFrame
    
    hist(
      df[, input$vars_PCA],
      breaks = input$bins,
      xlab = input$vars_PCA,
      labels = TRUE,
      main = paste("Histogram of ", input$vars_PCA),
      col = c("cadetblue")
    )
    
  })
  output$contents <- renderTable({
    # input$infile will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    #For custom selected Data sets ##########
    #req(input$infile)
    #req(input$update)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else{
        return(NULL)
      }
      
    }
    df <- currentDataFrame
    
    if (input$disp == "head") {
      return(head(df))
    }
    else if (input$disp == "tail") {
      return(tail(df))
    }
    else {
      return(df)
    }
    
  })
  output$summary <- renderPrint({
    #req(input$infile)
    #req(input$update)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
    }
    df <- currentDataFrame
    summary(df)
  })
  
  observeEvent(input$update, {
    currentDataFrame <- data.frame
    #req(input$infile)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else{
        return(NULL)
      }
      
    }
    df <- currentDataFrame
    
    #print(df[,sapply(df, is.numeric)] )
    #print(df[,-which(sapply(df, class) == "factor")])
    df <- df[, sapply(df, is.numeric)]
    #df <- df[,-which(sapply(df, class) == "factor")]
    theseChoices <- names(df)
    
    updateSelectInput(
      session,
      inputId = "vars_PCA",
      label = "Target Variable",
      choices = names(df),
      selected = names(df)[1]
    )
  })
  
  
  ##observe update button
  observeEvent(input$update, {
    currentDataFrame <- data.frame
    #req(input$infile)
    #req(input$update)
    
    if (isTruthy(input$infile)) {
      currentDataFrame <- read.csv(
        input$infile$datapath,
        header = TRUE,
        sep = input$sep,
        quote = input$quote
      )
      
      
    }
    else if (isTruthy(input$dataset) & !isTruthy(input$infile)) {
      if (input$dataset == "Red Wine Quality") {
        currentDataFrame <- read.csv(
          "red_wine.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      
      else if (input$dataset == "Adult Data Set") {
        currentDataFrame <- read.csv(
          "Adult.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      
      else if (input$dataset == "Framingham Risk Score") {
        currentDataFrame <- read.csv(
          "Framingham_Test.csv",
          header = TRUE,
          sep = input$sep,
          quote = input$quote
        )
      }
      else{
        return(NULL)
      }
    }
    df <- currentDataFrame
    df <-
      df[, sapply(df, is.numeric)] #df[,-which(sapply(df, class) == "factor")]
    theseChoices <- names(df)
    
    #Delete non-numeric col in drop down menu to avoid errors
    
    updateSelectInput(
      session,
      inputId = "vars_PCA",
      label = "Target Variable",
      choices = names(df),
      selected = names(df)[1]
    )
    
    updateCheckboxGroupInput(
      session,
      inputId = "checkboxes_PCA",
      label = "Select variables to include in PCA",
      choices = names(df),
      selected = names(df)
    )
    
    
  })
  
  plt_img <- function(x) {
    image(x, col = grey(seq(0, 1, length = 256)))
  }
  
  
  observeEvent(input$reset, {
    updateSelectInput(
      session,
      inputId = "dataset",
      "Select a built in dataset:",
      choices = c(
        '\n',
        "Adult Data Set",
        "Red Wine Quality",
        "Framingham Risk Score"
      ),
      selected = ""
    )
  })
  
 
  
  
  
})#end of main func