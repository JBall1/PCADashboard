# Define server logic to read selected file ----
library(shiny)
library(corrplot)
library(ggfortify)
library(factoextra)
library(BiocManager)
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
  
  #Begin eigenfaces
  library(imager)
  library(foreach)
  library(magick)
  library(OpenImageR)
  library(jpeg)
  library(EBImage)
  imagePloter <-
    function(x, res) {
      image(x, col = grey(seq(0, 1, length = res)))
    }
  rotate <- function(x)
    t(apply(x, 2, rev))
  
  
  
  observeEvent(input$faceUpdate, {
    final_face_data <- NULL
    final_eigen_vectors <- NULL
    paths <- NULL
    face_data_csv <- NULL
    faceDATA <- NULL
    if (isTruthy(input$imageDataSet) & !isTruthy(input$inImages)) {
      req(input$imageDataSet)
      
      if (input$imageDataSet == "Nottingham Originals Dataset") {
        paths <- list.files("halfFace", full.names = TRUE)
      }
      else if (input$imageDataSet == "Olivetti Dataset") {
        #Place holder
        face_data_csv <- read.csv("olivetti_y.csv")
      }
      else{
        paths <- NULL
        face_data_csv <- NULL
      }
    }
    
    if (isTruthy(input$inImages)) {
      req(input$inImages)
      paths <- input$inImages$datapath
    }
    
    output$faceAvgPlot <- renderPlot({
      if (is.null(face_data_csv) == TRUE) {
        imageData <- NULL
        withProgress(message = 'Generating Plots', value = 0.1, {
          withProgress(message = 'Reading Images',
                       detail = "Image 0",
                       value = 0,
                       {
                         #look <- NULL
                         
                         for (i in 1:length(paths)) {
                           initPic <- resize(readJPEG(paths[i]), w = 256, h = 256)
                           
                           if (length(dim(initPic)) == 3) {
                             greyPIC <- rgb_2gray(initPic)
                           }
                           else {
                             greyPIC <- initPic
                           }
                           c <-
                             as.numeric((apply(
                               matrix(
                                 as.numeric(greyPIC),
                                 nrow = 256,
                                 byrow = T
                               ), 2, rev
                             )))
                           
                           imageData <- rbind(imageData, c)
                           incProgress(1 / length(paths), detail = paste("Image", i))
                           
                         }
                       })
          
          #print("putting faces into final_face_data")
          
          final_face_data <- imageData
          #print(is.null(final_face_data))
          incProgress(0.5)
          faceDF = as.data.frame(imageData)
          # faceDATA <- faceDF
          par(mfrow = c(1, 2))
          par(mar = c(1, 0.1, 0.1, 0.1))
          #print(head(faceDF))
          average_face <- colMeans(faceDF)
          
          AVF = matrix(average_face, nrow = 1, byrow = T)
          m <- as.matrix(AVF)
          
          mergedImages <- matrix(m[1:ncol(m)], 256, 256, byrow = T)
          image(t(apply(mergedImages, 2, rev)), col = grey(seq(0, 1, length =
                                                                 256)))
          
          
          #AVF=matrix(average_face,nrow=1,byrow=T)
          #aig <- imagePloter(rotate(matrix(average_face,nrow=256,byrow=T)),res = 256)
          #img(look)
          acp <-
            image(rotate((
              matrix(average_face, nrow = 256, byrow = T)
            )), useRaster = T)
          #save.image(aig,"file.jpg")
          incProgress(0.5)
          #print("scaling data")
          D <- t(as.matrix(imageData))
          
          # Step 2: calculate covarariance matrix
          A <- covar(D)
          A_ <- t(D) %*% D / (nrow(D) - 1)
          # Note that the two matrices are the same
          max(A - A_) # Effectively zero
          
          eigs <- eigen(A)
          
          eigenvalues <- eigs$values
          # Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
          eigenvectors <- eigs$vectors
          final_eigen_vectors <- eigenvectors
          par(mfrow = c(1, 1))
          par(mar = c(2.5, 2.5, 2.5, 2.5))
          y = eigenvalues[1:40]
          #print(y)
          # First 40 eigenvalues dominate
          output$faceEPlot <- renderPlot({
            plot(
              1:40,
              y,
              type = "o",
              log = "y",
              main = "Magnitude of the 40 biggest eigenvalues",
              xlab = "Eigenvalue #",
              ylab = "Magnitude"
            )
          })
          setProgress(1)
          
          
          
          
          # First 40 eigenvalues dominate
          #acp
        })
        
      }
      
      #Credit to https://rpubs.com/JanpuHou/469414 for some portions of code here.
      else if (is.null(face_data_csv) == FALSE) {
        withProgress(message = 'Generating Plots', value = 0.1, {
          withProgress(message = 'Reading Data',
                       detail = "Loading CSV..",
                       value = 0,
                       {
                         # X: features
                         X_df <-
                           read.csv("olivetti_X.csv",
                                    header = F) %>% as.matrix()
                         # y: labels
                         y_df <-
                           read.csv("olivetti_y.csv",
                                    header = F) %>% as.matrix()
                         incProgress(0.2)
                         
                         newdata <- NULL
                         # Rotate every image and save to a new file for easy display in R
                         for (i in 1:nrow(X_df))
                         {
                           # Rotated Image 90 degree
                           c <-
                             as.numeric((apply(
                               matrix(
                                 as.numeric(X_df[i,]),
                                 nrow = 64,
                                 byrow = T
                               ), 2, rev
                             )))
                           # Vector containing the image
                           newdata <- rbind(newdata, c)
                         }
                         final_face_data <- newdata
                         df = as.data.frame(newdata)
                         
                         par(mfrow = c(1, 2))
                         par(mar = c(0.1, 0.1, 0.1, 0.1))
                         incProgress(0.3)
                         
                         AV1 = colMeans(data.matrix(df[1:10, ]))
                         plt_img(matrix(AV1, nrow = 64, byrow = T))
                         
                         AV2 = colMeans(data.matrix(df[11:20, ]))
                         #plt_img(matrix(AV2,nrow=64,byrow=T))
                         image(rotate((
                           matrix(AV2, nrow = 64, byrow = T)
                         )), useRaster = T)
                         incProgress(0.5)
                         #faceDATA <- df
                         D <- t(as.matrix(df))
                         
                         # Step 2: calculate covarariance matrix
                         A <- covar(D)
                         A_ <- t(D) %*% D / (nrow(D) - 1)
                         # Note that the two matrices are the same
                         max(A - A_) # Effectively zero
                         
                         eigs <- eigen(A)
                         
                         eigenvalues <- eigs$values
                         # Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
                         eigenvectors <- eigs$vectors
                         final_eigen_vectors <- eigenvectors
                         par(mfrow = c(1, 1))
                         par(mar = c(2.5, 2.5, 2.5, 2.5))
                         y = eigenvalues[1:40]
                         #print(y)
                         # First 40 eigenvalues dominate
                         output$faceEPlot <- renderPlot({
                           plot(
                             1:40,
                             y,
                             type = "o",
                             log = "y",
                             main = "Magnitude of the 40 biggest eigenvalues",
                             xlab = "Eigenvalue #",
                             ylab = "Magnitude"
                           )
                         })
                         setProgress(1)
                       })
          
          
          
          
          
        })
        
        
        
        
        
      }
      
      observeEvent(input$performClass, {
        if (isTruthy(input$target)) {
          req(input$inImages)
          paths <- input$inImages$datapath
        }
        if (is.null(input$inImages)) {
          return(NULL)
        }
        matching_image <- NULL
        greyPIC <- NULL
        
        test_img <- resize(readJPEG(paths), w = 256, h = 256)
        
        if (length(dim(test_img)) == 3) {
          greyPIC <- rgb_2gray(test_img)
        }
        else {
          greyPIC <- test_img
        }
        c <-
          as.numeric((apply(
            matrix(
              as.numeric(test_img),
              nrow = 256,
              byrow = T
            ), 2, rev
          )))
        
        
        #print(as.matrix(c))
        
        PF1 <- data.matrix(scale(c)) %*% final_eigen_vectors[1]
        
        # Transform all the traning photos onto eigen space and get the coefficients
        PFall <-
          data.matrix(scale(final_face_data)) %*% final_eigen_vectors[1]
        
        # Find the simple difference and multiplied by itself to avoid negative value
        test <- matrix(rep(1, 400), nrow = 400, byrow = T)
        test_PF1 <- test %*% PF1
        Diff <- PFall - test_PF1
        y <- (rowSums(Diff) * rowSums(Diff))
        
        # Find the minimum number to match the photo in the files
        x = c(1:400)
        newdf = data.frame(cbind(x, y))
        
        the_number = newdf$x[newdf$y == min(newdf$y)]
        matching_image <- the_number
        
        output$simPlot <- renderPlot({
          par(mfrow = c(1, 1))
          par(mar = c(1, 1, 1, 1))
          plot(y, main = "Similarity Plot: 0 = Most Similar")
        })
        
        output$matchingImg <- renderPrint({
          d <- cat("the minimum number occurs at row = ", matching_image)
          
          d
        })
        
      })
      
      
      #imagePloter(imageData,res = 128)
    })
    
    #Do classification
    
    
  })
  
  
  
})#end of main func