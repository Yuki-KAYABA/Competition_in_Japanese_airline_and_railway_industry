## Making tables

## Utility analysis
## Estimation Results
coef_logit <- c(logit7.1$coefficients)
coef_IV <- c(IVreg7$coefficients)
coef <- cbind(coef_logit, coef_IV)
coef <- format(round(coef, 4), 3)
rownames(coef) <- c("intercept", "price", "log(frequency)", "acceessibility", "railwaydummy", "seat", "freight", "duration")
colnames(coef) <- c("OLS", "IV")
xx2 <- Hmisc::latex(coef, file = "Estimation_results.tex", 
                    caption = "Estimation Results", 
                    where = "h", 
                    col.just = rep("r", dim(coef)[2]))
