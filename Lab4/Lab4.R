linreg <- setRefClass("linreg",
                      fields = list(
                        formula="formula", 
                        data="data.frame",
                        y_label = "character",
                        y = "vector",
                        beta = "vector",
                        X = "matrix",
                        resid = "vector",
                        std_resid = "vector",
                        yhat = "vector",
                        df = "numeric",
                        varbeta = "matrix",
                        tvals = "vector",
                        pvals = "vector"
                      
                      ),
                      methods = list(
                      
                        initialize = function(formula, data){
                          
                          data <<- data
                          formula <<- formula
                          y_label <<- all.vars(formula)[1]
                          y <<- data[[y_label]]
                          X <<- model.matrix(formula, data)
                          QR <- qr(X)
                          Q <- qr.Q(QR)
                          R <- qr.R(QR)
                          
                          beta <<- solve.qr(QR, y)
                          yhat <<- as.vector(Q %*% t(Q) %*% y)
                          resid <<- y - yhat
                          std_resid <<- resid/(sd(resid))
                          
                          df <<- nrow(X) - ncol(X)
                          sigmasq <- (t(resid) %*% resid)/df
                          varbeta <<-  as.numeric(sigmasq) * chol2inv(R)
                          tvals <<- beta/sqrt(diag(varbeta))
                          pvals <<- pt(abs(tvals), df, lower.tail = F)
                          
                        
                      },
                        print = function(){
                          cat("\nCall: \n")
                          cat("linreg(formula = ", paste(Reduce(paste,deparse(formula)),",", sep="" ), "data = ", paste(substitute(data),")", sep="" ))
                        
                          cat("\n\nCoefficients:\n")
                          cat(paste(colnames(X),collapse = "  "),collapse="\n")
                          cat(paste(as.numeric(round(beta,4)), collapse = "  "),collapse="\n")
                      },
                        plot = function(){
                          library(ggplot2)
                          
                          dff <- data.frame(residuals = resid, fitted = yhat, standard_resid = sqrt(std_resid^2))
                          gg1 <- ggplot(data = dff, aes(x = fitted, y = resid)) + 
                            geom_point(color='black') +
                            stat_summary(fun=median,color="red", geom="line") +
                            geom_hline(yintercept=0, linetype="dashed") +
                            xlab("Fitted values")+
                            ylab("Residuals") + 
                            ggtitle("Residual vs Fitted") + 
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          gg2 <- ggplot(data = dff, aes(x = fitted, y = sqrt(standard_resid))) + 
                            geom_point(color='black') +
                            stat_summary(fun=median,color="red", geom="line") +
                            geom_hline(yintercept=0, linetype="dashed") +
                            xlab("Fitted values")+
                            ylab(expression(sqrt("|Standardized residuals|"))) + 
                            ggtitle("Scale-Location") + 
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(list(gg1,gg2))
                          
                      },
                        pred = function(){
                          return(yhat)
                      },
                        coef = function(){
                          coffs <- beta
                          names(coffs) <- colnames(X)
                          return(coffs)   
                      },
                        summary = function(){
                          
                          resid_mat <- matrix(c(min(resid), quantile(resid, probs=0.25), median(resid), quantile(resid, probs=0.75), max(resid)), nrow=1, ncol=5)
                          colnames(resid_mat) <- c("Min","1Q","Median","3Q", "Max")
                          row.names(resid_mat) <- ""
                          
                          m <- cbind(round(beta,4), round(sqrt(diag(varbeta)),4), round(tvals,4), round(pvals,4))
                          row.names(m) <- colnames(X)
                          colnames(m) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                          
                          RSE <- t(resid) %*% resid/df
                          Rsq <- 1 - sum(resid^2)/sum((y - mean(y))^2)
                          AdjRsq <- 1 - (var(resid)/var(y) * (nrow(X) - 1)/(nrow(X) - ncol(X) - 1))
                          
                          Fstat <- (Rsq/(ncol(X) - 1))/((1-Rsq)/(nrow(X) - ncol(X)))
                          Fpval <- pf(Fstat, ncol(X) - 1, nrow(X)-ncol(X), lower.tail = F)
                  
                          cat("\nCall: \n")
                          cat("linreg(formula = ", paste(Reduce(paste,deparse(formula)),",", sep="" ), "data = ", paste(substitute(data),")", sep="" ))
                          cat("\nResiduals: \n")
                          printCoefmat(resid_mat)
                          cat("\nCoefficients: \n")
                          printCoefmat(m)
                          cat("\nResidual standard error:", round(sqrt(RSE),4), "on", df, "degrees of freedom\n")
                          cat("Multiple R-squared:", paste(round(Rsq,4),",", sep=""), "Adjusted R-squared:", round(AdjRsq,4),"\n")
                          cat("F-statistic:", round(Fstat), "on", (ncol(X) - 1), "and", (nrow(X)-ncol(X)), "DF,", "p-value:", Fpval)
                      }
                      
                      )                      
)
lm_obj <- linreg$new(Petal.Length ~ Species + Petal.Width, data=iris)
#lm_obj$print()
#lm_obj$plot()
#lm_obj$pred()
#lm_obj$coef()
lm_obj$summary()

