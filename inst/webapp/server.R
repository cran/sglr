library(shiny)
library(sglr)

## Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    ## Return the requested dataset
    glrResult <- reactive(function() {
        glrSearch(p = c(input$p0, input$p1),
                  alpha = input$alpha,
                  beta = input$beta,
                  stepSize = input$stepSize,
                  tol = input$tol,
                  maxIter = input$maxIter,
                  gridIt = TRUE,
                  nGrid = input$nGrid,
                  verbose = FALSE)
    })

    ## Show the alphaTable
    output$alphaTable <- reactiveTable(function() {
        table <- glrResult()$alphaTable
        rownames(table) <- sub("b1", "$b_1$", rownames(table))
        colnames(table) <- sub("b0", "$b_0$", colnames(table))
        table
    })

    ## Show the betaTable
    output$betaTable <- reactiveTable(function() {
        table <- glrResult()$betaTable
        rownames(table) <- sub("b1", "$b_1$", rownames(table))
        colnames(table) <- sub("b0", "$b_0$", colnames(table))
        table
    })

    output$plot <- reactivePlot(function() {
        print(plotBoundary(b0 = input$bb0,
                     b1 = input$bb1,
                     p = c(input$bp0, input$bp1)))
    })

    output$boundary <- reactiveTable(function() {
        boundary <- computeBoundary(b0 = input$bb0,
                                    b1 = input$bb1,
                                    p = c(input$bp0, input$bp1),
                                    tol = input$btol)
        ##print(boundary$estimate)
        ##print(class(boundary$estimate))
        ##
        ## To save screen space, fold the vectors into five columns
        ## Abort the thought....
        ## cols <- 5
        ## n <- length(boundary$lower)
        ## paddedN <- cols * ceiling( n / cols)
        ## ##Pad the vectors with NAs
        ## boundary$lower <- c(boundary$lower, rep(NA, paddedN - n))
        ## boundary$upper <- c(boundary$upper, rep(NA, paddedN - n))
        ## ## Build indices
        ## iSeq <- seq(paddedN)
        ## zSeq <- iSeq - 1
        ## splits <- split(iSeq, (zSeq %/% (paddedN / cols)))
        ## ## split it
        ## l <- lapply(splits, function(x) cbind(iSeq[x], boundary$lower[x], boundary$upper[x]))
        ## ## bind it
        ## d <- do.call(cbind, l)
        ## ## these are integers..
        ## storage.mode(d) <- "integer"
        ## rownames(d) <- NULL
        ## colnames(d) <- rep(c("total", "lower", "upper"), cols)
        ## End of Aborted section
        d <- cbind(total=seq(length(boundary$lower)), lower=boundary$lower, upper=boundary$upper)
        storage.mode(d) <- "integer"
        colnames(d) <- c("Total No. of AEs", "Lower Boundary (Vaccine AEs)", "Upper Boundary (Vaccine AEs)")
        d
    })

    output$estimate <- reactiveText(function() {
        boundary <- computeBoundary(b0 = input$bb0,
                                    b1 = input$bb1,
                                    p = c(input$bp0, input$bp1),
                                    tol = input$btol)

        paste("Type I Error:", format(boundary$estimate[1], digits=4),
              ", Type II Error:", format(boundary$estimate[2], digits=4))
    })

    output$maxAE <- reactiveText(function() {
        boundary <- computeBoundary(b0 = input$bb0,
                                    b1 = input$bb1,
                                    p = c(input$bp0, input$bp1),
                                    tol = input$btol)
        paste("Maximum Total Number of AEs:", length(boundary$lower))
    })

})
