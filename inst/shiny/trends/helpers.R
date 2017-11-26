## required packages
library(readxl)
library(NADA)

## helper functions
readxl <-
  function (...) {
    xl <- read_excel(...)
    class(xl) <- "data.frame"
    colnames(xl) <- make.names(colnames(xl))
    return(xl)
  }

expit <-
  function(x) {
    exp(x)/(1 + exp(x))
  }

logit <-
  function(x) {
    log(x/(1 - x))
  }

## main wrapper function
fit_all <-
  function(file, level) {
    ## extract sheet names
    sheets <- excel_sheets(file)
    
    ## define years of analysis
    years <- 2008:2016
    
    ## transfer to appropriate function
    if ("Tout Programmation" %in% sheets) {
      fit_discrete(file, years, level)
      
    } else {
      fit_continuous(file, years, level)
    }
  }

## fit models to discrete data
fit_discrete <-
  function(file, years, level) {
    ## parameters
    par <- readxl(file, "Tout Programmation", range = cell_limits(c(NA, 11), c(4, NA)))
    par <- par[, complete.cases(t(par)), drop = FALSE]
    n_par <- ncol(par)
    
    ## data
    x <- readxl(file, "Tout Programmation", range = cell_limits(c(5, 3), c(NA, NA)))
    x <- cbind(x[as.numeric(level)], x[, -c(1:7)])
    x <- head(x, -1)             # strip last row
    x <- x[, seq(1, ncol(x)-4)]  # strip last columns
    
    ## to numeric
    x[, -1] <- sapply(x[, -1], as.numeric)
    
    ## matrices
    mat <- factor(x[[1]])
    n_mat <- length(levels(mat))
    
    ## year
    year <- x[[2]]
    
    ## prepare output table
    out_tab <- matrix(ncol = 6, nrow = n_par * n_mat)
    
    ## prepare output list
    out_list <- vector("list", n_par * n_mat)
    
    ## fit all models
    for (i in seq(n_par)) {
      xy <- as.matrix(x[, (4:3)+(4*(i-1))])
      xy[is.na(xy)] <- 0
      
      for (j in seq(n_mat)) {
        mx <- matrix(0, ncol = 2, nrow = length(years))
        id <- which(mat == levels(mat)[j])
        yr <- match(year[id], years)
        mx[yr, ] <- xy[id, ]
        
        if (sum(mx) > 0) {
          fit <- glm(mx ~ years, family = binomial)
          if (!is.na(coef(fit)[2])) {
            p <- round(coef(summary(fit))[2, 4], 3)
          } else {
            p <- NA
          }
          
        } else {
          p <- NA
        }
        
        ## complete output table
        row <- j + n_mat * (i-1)
        out_tab[row, ] <-
          c(par[3, i],
            levels(mat)[j],
            sum(mx),
            length(unique(yr)),
            round(coef(fit)[2], 3),
            p)
        
        ## complete output list
        out_list[[row]] <-
          list(years = years,
               mx = mx,
               fit = fit,
               p = p,
               PAR = par[3, i],
               MAT = levels(mat)[j])
      }
    }
    
    out_tab <- data.frame(out_tab, stringsAsFactors = FALSE)
    colnames(out_tab) <- c("Parameter", "Matrix", "Samples", "Years", "Coefficient", "P-value")
    out_tab[[3]] <- as.numeric(out_tab[[3]])
    out_tab[[4]] <- as.numeric(out_tab[[4]])
    out_tab[[5]] <- as.numeric(out_tab[[5]])
    out_tab[[6]] <- as.numeric(out_tab[[6]])
    
    out_tab$Interpretation <-
      "No trend analysis possible"
    out_tab$Interpretation[which(out_tab$'P-value' > 0.05)] <-
      "Non-significant trend"
    out_tab$Interpretation[which(out_tab$'P-value' < 0.05 & out_tab$Coefficient < 0)] <-
      "Decreasing trend"
    out_tab$Interpretation[which(out_tab$'P-value' < 0.05 & out_tab$Coefficient > 0)] <-
      "Increasing trend"
    
    return(list(out_tab = out_tab, out_list = out_list, type = "discrete"))
  }

## fit models to continuous data
fit_continuous <-
  function(file, years, level) {
    ## helper functions
    Pval <- function(x) summary(x)@.Data[[9]]["year", "p"]
    
    ## read data
    x <- readxl(file, 1)
    
    ## extract relevant columns
    cols <- c("Ann\xE9e", paste("Mat Niveau", level), "Param\xE8tre", "Ana.Ech: R\xE9sultat")
    cols <- iconv(cols, "latin1", "UTF-8")
    x <- x[, make.names(cols)]
    colnames(x) <- c("year", "matrix", "parameter", "result")
    
    ## remove observations without year
    x <- x[complete.cases(x$year), ]
    
    ## define censored observations
    x$cen <- grepl("<", x$result)
    ## .. extract LOQ/LOD from entry
    x$result <- gsub("\\(.*", "", gsub("<", "", x$result))
    ## .. convert values to numeric
    x$result <- as.numeric(gsub(",", ".", x$result))
    
    ## define unique parameters
    par <- unique(x$parameter)
    n_par <- length(par)
    
    ## define unique matrices
    mat <- unique(x$matrix)
    n_mat <- length(mat)
    
    ## prepare output table
    out_tab <- matrix(ncol = 4, nrow = n_par * n_mat)
    
    ## prepare output list
    out_list <- vector("list", n_par * n_mat)
    
    ## fit models for all parameter-matrix pairs
    for (i in seq(n_par)) {
      xi <- subset(x, parameter == par[i])
      
      for (j in seq(n_mat)) {
        xim <- subset(xi, matrix == mat[j])
        n_years <- length(unique(xim$year))
        
        if (n_years == 1 | length(unique(xim$result)) == 1) {
          P <- COEF <- NA
          
        } else {
          fit <- with(xim, cenreg(Cen(result, cen) ~ year))
          COEF <- coef(fit)
          P <- Pval(fit)
        }
        
        ## complete output table
        row <- j + n_mat * (i-1)
        out_tab[row, ] <- c(par[i], mat[j], exp(COEF[2]), P)
        
        ## complete output list
        out_list[[row]] <- list(x = xim, COEF = COEF, P = P, years = years)
      }
    }
    
    ## remove non-existing combinations
    is_empty <- sapply(out_list, function(x) nrow(x$x) == 0)
    out_tab <- out_tab[!is_empty, ]
    out_list <- out_list[!is_empty]
    
    ## compile and return results
    out_tab <- data.frame(out_tab, stringsAsFactors = FALSE)
    colnames(out_tab) <- c("Parameter", "Matrix", "Coefficient", "P-value")
    out_tab[[3]] <- round(as.numeric(out_tab[[3]]), 3)
    out_tab[[4]] <- round(as.numeric(out_tab[[4]]), 3)
    
    out_tab$Interpretation <-
      "No trend analysis possible"
    out_tab$Interpretation[which(out_tab$'P-value' > 0.05)] <-
      "Non-significant trend"
    out_tab$Interpretation[which(out_tab$'P-value' < 0.05 & out_tab$Coefficient < 0)] <-
      "Decreasing trend"
    out_tab$Interpretation[which(out_tab$'P-value' < 0.05 & out_tab$Coefficient > 0)] <-
      "Increasing trend"
    
    return(list(out_tab = out_tab, out_list = out_list, type = "continuous"))
  }
