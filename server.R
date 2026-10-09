library(car)
library(shiny)
library(MASS)
library(lme4)
library(vioplot)
library(randomcoloR)
library(lmerTest)

shinyServer(
  function(input, output) {    
    
    dataInput <- reactive({
      N <- input$N
      n <- input$n
      
      mu.1 <- input$mu.1
      mu.2 <- input$mu.2
      
      VG.1 <- input$VG.1
      VG.2 <- input$VG.2
      
      rG.12 <- input$rG.12
      
      VE <- input$VE
 
      # Simulate data
      
      # Individual effect
      covG.12 <- rG.12 * sqrt(VG.1 * VG.2)
      
      G <- matrix(c(VG.1, covG.12, covG.12, VG.2), nrow=2)
      a <- mvrnorm(n=N, mu=c(0,0), Sigma = G, empirical = TRUE)
      a <- as.data.frame(a)
      colnames(a) <- c("a.1", "a.2")
      cor(a)
      
      a <- a[rep(seq_len(nrow(a)), each = n), ]
      
      # Residual term
      # Assume homogeneous residual variances for now, so equal residual variance and 
      # correlation of 0, but can easily be extended to heterogeneous residuals
      
      E <- matrix(c(VE, 0, 0, VE), nrow=2) 
      e <- mvrnorm(n=N*n, mu=c(0,0), Sigma = E, empirical = TRUE)
      e <- as.data.frame(e)
      colnames(e) <- c("e.1", "e.2")
      cor(e)
      
      # Create phenotypes
      y.1 <- mu.1 + a$a.1 + e$e.1
      y.2 <- mu.2 + a$a.2 + e$e.2
      
      # Create individual IDs
      id <- rep(paste0("ID.", seq_len(N)), each=n)
      
      # Environments
      x <- c("1", "2") # Can later add option change this to e.g. Male/Female
      x.factor <- relevel(as.factor(x), ref= x[1])
      
      # Create data frame in wide format
      list(data.frame(id, y.1, y.2), x, x.factor)
    })
    
    # From wide to long format
    dataInput.long <- reactive({
      d.wide <- dataInput()[[1]]
      x <- dataInput()[[3]]
      data.frame(id=c(d.wide$id, d.wide$id), 
                 trait=rep(rep(x, each=input$N*input$n), times=2), 
                 y= c(d.wide$y.1, d.wide$y.2))
    }) 
    
    cols <- reactive({
      ids <- unique(dataInput.long()$id)
      distinctColorPalette(length(ids))
    })
    
    # Create plot of raw data
    output$data.plot <- renderPlot(height=600,{ 
      d.wide <- dataInput()[[1]]
      x <-  dataInput()[[2]]
      x.factor <- dataInput()[[3]]
      d.long <- dataInput.long()
      ids <- unique(dataInput.long()$id)
      
      min <- min(d.long$y)
      max <- max(d.long$y)
      
      par(mfrow=c(1,1), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
      plot(0, type="n", xaxt='n', xlim=c(0.8,2.2), ylim=c(min, max), 
           xlab="environment", ylab = "phenotype")
      axis(1, labels=x.factor, at=c(1,2))

      x.jitter <- as.numeric(d.long$trait) + 
        rnorm(2*input$N*input$n, 0, 0.06)

      for (i in 1:length(ids)) {
        points(x.jitter[d.long$id==ids[i]], 
               d.long$y[d.long$id==ids[i]], 
               pch=19, cex=1.5, col=cols()[i])
        mean.x <- c(mean(x.jitter[d.long$id==ids[i] & d.long$trait==1]),
                    mean(x.jitter[d.long$id==ids[i] & d.long$trait==2]))
        mean.y <- c(mean(d.long$y[d.long$id==ids[i] & d.long$trait==1]),
                    mean(d.long$y[d.long$id==ids[i] & d.long$trait==2]))
        lines(mean.x, mean.y, col=cols()[i])
      }
    })
    
    
    ############################
    # Character-state approach #
    ############################

    # Model
    m.cs <- reactive({
      d.long <- dataInput.long()
      m.cs <- lmer(y ~ trait + (0+trait|id), data=d.long)
      })
    
    output$cs.lmer <- renderPrint({
      d.long <- dataInput.long()
      summary(m.cs())
    })
    
    output$cs.G <- renderPrint({
      as.matrix(VarCorr(m.cs())$id[1:2, 1:2])
    })
    
    # Plots
    
      # Plot of means
      output$cs.plot.means <- renderPlot(height = 400, { 
      m <- m.cs()
      x.factor <- dataInput()[[3]]
      mean.se <- lsmeansLT(m, "trait")

      par(mfrow=c(1,1), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
      plot(c(1, 2), mean.se$Estimate, xlim=c(0.8, 2.2), 
            ylim=c(min(mean.se$Estimate-mean.se$`Std. Error`), 
                   max(mean.se$Estimate+mean.se$`Std. Error`)),
            xlab="environment", ylab="mean phenotype +/- se", 
            pch=19, xaxt='n', cex=1.4)
       arrows(c(1, 2), mean.se$Estimate-mean.se$`Std. Error`, c(1,2), 
              mean.se$Estimate+mean.se$`Std. Error`, length=0.05, angle=90, code=3)
       axis(1, labels=x.factor, at=c(1,2))
      })

      # Variance and correlation plot
      output$cs.plot.var.cor <- renderPlot(height = 400, { 
        m <- m.cs()
        x.factor <- dataInput()[[3]]
        
        blups.wide <- as.data.frame(ranef(m)$id)
        blups.long <- data.frame(blups=c(blups.wide[,1], blups.wide[,2]), 
                                 trait=rep(x.factor, each=input$N))
        
        min <- min(blups.long$blups)
        max <- max(blups.long$blups)
        
        par(mfrow=c(1,2), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
        
        # Violin plot of variances 
        vioplot(blups.long$blups ~ blups.long$trait, xlab="environment", ylab="genotypic value")

       # Scatter plot of correlation
       plot(blups.wide[,2] ~ blups.wide[,1], xlim=c(min,max),ylim=c(min, max),
            xlab="genotypic value environment 1", ylab="genotypic value environment 2",
            pch=19, col=cols(), cex=1.4)
       dataEllipse(blups.wide[,1], blups.wide[,2], center.pch=F, add=T,
                   plot.points=F, levels=c(0.95), lwd=1, fill=TRUE, col = "grey"
                   )
      })

      # Line plot
      output$cs.plot.lines <- renderPlot(height = 400, { 
        m <- m.cs()
        x.factor <- dataInput()[[3]]
        blups.wide <- as.data.frame(ranef(m)$id)
        blups.long <- data.frame(blups=c(blups.wide[,1], blups.wide[,2]), 
                                 trait=rep(x.factor, each=input$N))
        
        min <- min(blups.long$blups)
        max <- max(blups.long$blups)
        
        par(mfrow=c(1,1), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
        
       plot(0, type="n", xaxt='n', xlim=c(0.8,2.2), ylim=c(min, max),
            xlab="environment", ylab = "genotypic value")
       axis(1, labels=x.factor, at=c(1,2))
       for (i in 1:nrow(blups.wide)) {
         points(x.factor, blups.wide[i,1:2], type="b", pch=19, col=cols()[i], cex=1.4)
       }
       
    })
    
    ##########################
    # Reaction norm approach #
    ##########################

    # Model
    m.rn <- reactive({
      d.long <- dataInput.long()

      trait.numeric <- as.numeric(d.long$trait)
      if (input$intercept == 0) trait.numeric.scaled <- trait.numeric
      if (input$intercept == 1) trait.numeric.scaled <- trait.numeric - 1
      if (input$intercept == 2) trait.numeric.scaled <- trait.numeric - 2
      
      m.rn <- lmer(y ~ trait.numeric.scaled + (trait.numeric.scaled|id), data=d.long)
    })

    output$rn.lmer <- renderPrint({
        summary(m.rn())
      })
      
      output$rn.G <- renderPrint({
        as.matrix(VarCorr(m.rn())$id[1:2, 1:2])
      })

      output$cs.G2 <- renderPrint({
        x.factor <- dataInput()[[3]]
        
        m <- m.rn()
        G.rn <- as.matrix(VarCorr(m)$id[1:2, 1:2])
        m <- matrix(c(1,1, as.numeric(x.factor)), nrow = 2)
        m %*% G.rn %*% t(m)
      })
      

    # Plots

# Plot regression
      output$rn.plot.reg <- renderPlot(height=400, {
        m <- m.rn()
        d.long <- dataInput.long()
        
        trait.numeric <- as.numeric(d.long$trait)
        if (input$intercept == 0) trait.numeric.scaled <- trait.numeric
        if (input$intercept == 1) trait.numeric.scaled <- trait.numeric - 1
        if (input$intercept == 2) trait.numeric.scaled <- trait.numeric - 2
        
        x.factor <- dataInput()[[3]]
        
        blups.wide <- as.data.frame(ranef(m)$id)
        blups.long <- data.frame(blups=c(blups.wide[,1], blups.wide[,2]), trait=rep(x.factor, each=input$N))
        
      min <- min(blups.long$blups)
      max <- max(blups.long$blups)

            par(mfrow=c(1,1), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
      
      plot(0, type="n", xlim=c(0, max(trait.numeric.scaled)), ylim=c(min, max), 
           xlab="environment", ylab = "phenotype")
      
      c <- summary(m)$coefficients
      abline(a=c[1,1], b=c[2,1], lwd=2)
      abline(v=trait.numeric.scaled, lty=2, col="grey")
      
      })
      
# Plot variances and correlation
      output$rn.plot.var.cor <- renderPlot(height=400, {
        m <- m.rn()
        blups.wide <- as.data.frame(ranef(m)$id)
        d.long <- dataInput.long()
        
        trait.numeric <- as.numeric(d.long$trait)
        if (input$intercept == 0) trait.numeric.scaled <- trait.numeric
        if (input$intercept == 1) trait.numeric.scaled <- trait.numeric - 1
        if (input$intercept == 2) trait.numeric.scaled <- trait.numeric - 2
        
        x.factor <- dataInput()[[3]]
        
        blups.long <- data.frame(blups=c(blups.wide[,1], blups.wide[,2]), trait=rep(x.factor, each=input$N))
        
        min <- min(blups.long$blups)
        max <- max(blups.long$blups)
        
      par(mfrow=c(1,2), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
      
      par(xaxt = "n")
      vioplot(blups.wide[,1], blups.wide[,2], ylab="estimate")
      par(xaxt = "s")
      axis(1, labels=c("intercept", "slope"), at=c(1,2))
      
      plot(blups.wide[,2] ~ blups.wide[,1], xlim=c(min,max),ylim=c(min,max),xlab="intercept", ylab="slope", 
           pch=19, col=cols(), cex=1.4)
      dataEllipse(blups.wide[,1],blups.wide[,2], center.pch=F, add=T, plot.points=F, levels=c(0.95), lwd=1, fill=TRUE, col = "grey")
      })
      
      # Line plot
      output$rn.plot.lines <- renderPlot(height=400, {
        m <- m.rn()
        blups.wide <- as.data.frame(ranef(m)$id)
        d.long <- dataInput.long()
        
        trait.numeric <- as.numeric(d.long$trait)
        if (input$intercept == 0) trait.numeric.scaled <- trait.numeric
        if (input$intercept == 1) trait.numeric.scaled <- trait.numeric - 1
        if (input$intercept == 2) trait.numeric.scaled <- trait.numeric - 2
        
        x.factor <- dataInput()[[3]]
        
        blups.long <- data.frame(blups=c(blups.wide[,1], blups.wide[,2]), 
                                 trait=rep(x.factor, each=input$N))
        
        min <- min(blups.long$blups)
        max <- max(blups.long$blups)
        
        par(mfrow=c(1,1), font.lab=2, cex=1, cex.lab=1.2, font.lab=2, pty='s')
        
      plot(0, type="n", xlim=c(0, max(trait.numeric.scaled)), ylim=c(min, max), 
           xlab="environment", ylab = "genotypic value")
      for (i in 1:nrow(blups.wide)) {
        abline(a=blups.wide[i,1], b=blups.wide[i,2],col=cols()[i], cex=1.4)
      }
      abline(v=trait.numeric.scaled, lty=3)
    })
      
  })
