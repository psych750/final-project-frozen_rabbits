modelAssumptions = function (Model, Type = "NORMAL", ID = row.names(Model$model), 
                             one.page = TRUE) 
{
  switch(toupper(Type), NORMAL = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    qqPlot(Model, labels = FALSE, sim = TRUE, main = "Quantile-Comparison Plot to Assess Normality", 
           xlab = "t Quantiles", ylab = "Studentized Residuals")
    plot(density(rstudent(Model)), main = "Density Plot to Assess Normality of Residuals", 
         xlab = "Studentized Residual")
    zx <- seq(-4, 4, length.out = 100)
    lines(zx, dnorm(zx, mean = 0, sd = sd(rstudent(Model))), 
          lty = 2, col = "blue")
    cat("Descriptive Statistics for Studentized Residuals\n")
    describe(rstudent(Model))
  }, CONSTANT = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    plot(rstudent(Model) ~ fitted.values(Model), main = "Studentized Residuals vs. Fitted Values", 
         xlab = "Fitted Values", ylab = "Studentized Residuals")
    abline(h = 0, lty = 2, col = "blue")
    print(spreadLevelPlot(Model))
    cat("\n\n")
    print(ncvTest(Model))
  }, LINEAR = {
    dev.new(width = 7, height = 7, record = TRUE)
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    crPlots(Model, ask = TRUE)
  }, {
    print("Valid options for type: normal, constant, linear")
  })
}

modelCaseAnalysis = function (Model, Type = "RESIDUALS", Term = NULL, ID = row.names(Model$model)) 
{
  switch(toupper(Type), UNIVARIATE = {
    d = Model$model
    {
      Vars = names(d)
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      if (is.factor(d[[varname]])) {
        plot(d[varname], xlab = varname, ylab = "Frequency")
      } else {
        hist(d[[varname]], xlab = varname, main = "Red: Mean +- 3SD; Green: Median +- 2.2IQR")
        text(d[[varname]], rep(0, length(d[[varname]])), labels = ID, pos = 3, cex = 0.7)
        abline(v = c(-3, 0, 3) * sd(d[[varname]]) + 
                 mean(d[[varname]]), col = "red", lty = c(1, 
                                                          2, 1))
        abline(v = c(-2.2, 0, 2.2) * IQR(d[[varname]]) + 
                 median(d[[varname]]), col = "green", lty = c(1, 
                                                              2, 1))
      }
    }
  }, HATVALUES = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Small sample cut (green) = 3 * mean(Hat)\nLarge sample cut: 2 * mean(Hat)", 
                     sep = "")
    hist(hatvalues(Model), xlab = "Hat Values", main = TheTitle)
    abline(v = c(2, 3) * mean(hatvalues(Model)), col = c("red", 
                                                         "green"))
    text(hatvalues(Model), rep(0, length(hatvalues(Model))), labels = ID, pos = 3, cex = 0.7)
    points(hatvalues(Model), rep(0, length(hatvalues(Model))), 
           pch = "|", col = "blue")
  }, RESIDUALS = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Bonferroni corrected p < .05 cut-off in red", 
                     sep = "")
    hist(rstudent(Model), xlab = "Studentized Residuals", 
         main = TheTitle)
    abline(v = c(-1, 1) * TCut, col = "red")
    text(rstudent(Model), rep(0, length(rstudent(Model))), labels = ID, pos = 3, cex = 0.7)
    points(rstudent(Model), rep(0, length(rstudent(Model))), 
           pch = "|", col = "blue")
  }, COOKSD = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(cooks.distance(Model))
    k = length(coef(Model)) - 1
    TheTitle = paste("Model: ", Model$call[2], "\n", "4/(N-P) cut-off (red)\nqf(.5,P,N-P) cut-off (green)", 
                     sep = "")
    hist(cooks.distance(Model), xlab = "Cooks d", main = TheTitle)
    abline(v = c((4/(N - k - 1)), qf(0.5, k + 1, N - k - 
                                       1)), col = c("red", "green"))
    text(cooks.distance(Model), rep(0, length(cooks.distance(Model))), labels = ID, pos = 3, cex = 0.7)
    points(cooks.distance(Model), rep(0, length(cooks.distance(Model))), 
           pch = "|", col = "blue")
  }, DFBETAS = {
    if (is.null(Term)) {
      {
        Vars = dimnames(dfbetas(Model))[[2]]
      }
    } else {
      if (!(Term %in% dimnames(dfbetas(Model))[[2]])) {
        stop("Term specified for DFBETAS not valid")
      } else Vars = Term
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      TheTitle = paste("Model: ", Model$call[2], "\n", 
                       "B= ", coef(Model)[varname], sep = "")
      hist(dfbetas(Model)[, varname], xlab = paste("DFBETAS:", 
                                                   varname), main = TheTitle)
      text(dfbetas(Model)[, varname], rep(0, length(dfbetas(Model)[, 
                                                                   varname])), labels = ID, pos = 3, cex = 0.7)
      abline(v = c(-2, 2), col = "red")
      points(dfbetas(Model)[, varname], rep(0, length(dfbetas(Model)[, varname])), 
             pch = "|", col = "blue")
    }
    
  }, INFLUENCEPLOT = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Influence Bubble plot", "\nModel: ", 
                     Model$call[2], sep = "")
    plot(hatvalues(Model), rstudent(Model), type = "n", 
         xlab = "Hat Values", ylab = "Studentized Residuals", 
         main = TheTitle)
    cooksize = 10 * sqrt(cooks.distance(Model))/max(cooks.distance(Model))
    points(hatvalues(Model), rstudent(Model), cex = cooksize)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    abline(h = c(-1, 0, 1) * TCut, col = "red", lty = c(1, 
                                                        2, 1))
    abline(v = c(1, 2, 3) * mean(hatvalues(Model)), col = "red", 
           lty = c(2, 1, 1))
    text(hatvalues(Model),x = hatvalues(Model), y = rstudent(Model),
         rep(0, length(hatvalues(Model))), labels = ID, pos = 3, cex = 0.7)
  }, COVRATIO = {
    N = length(covratio(Model))
    k = length(coef(Model)) - 1
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "abs((3*P)/N)-1 cut-off in red", 
                     sep = "")
    hist(covratio(Model), xlab = "CovRatio", main = TheTitle)
    abline(v = abs((3 * (k + 1)/N) - 1), col = "red")
    text(covratio(Model), rep(0, length(covratio(Model))), labels = ID, pos = 3, cex = 0.7)
  }, {
    print("Valid options for type: hatvalues, residuals, cooksd, dfbetas, influenceplot, covratio, univariate")
  })
  Rownames = row.names(Model$model)
  Cases = list(Rownames = Rownames, Values = ID)
  return(Cases)
}