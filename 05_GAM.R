## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Descriptives and GAM ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

library(tidyverse)
library(mgcv)
library(caret)

setwd(dir = "your/wd")
load("predictors_and_DV_v3l.RData")

X <- predictors
X$curiosity <- chapter_descriptives$curious_mean

# short descriptives
psych::describe(X[,c("curiosity","novel_topics",
                     "b0", "b1", "b2",
                     "bottle_dim0", "bottle_dim1", "bottle_dim2",
                     "wasser_dim0", "wasser_dim1", "wasser_dim2")])

# winsorize
X <- X %>%
  mutate(across(everything(), ~ DescTools::Winsorize(.x, val = quantile(.x, probs = c(0.025, 0.975), na.rm = T))))

# standardize
preprocess <- preProcess(X, method = c("center", "scale"))
X <- predict(preprocess, X)
X$chapter <- 1:27


## ## ## ## ## ## ## ## ## ## ## ## #
## ## ## Descriptives ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## #

# Correlations
X_corre <- X[,c("curiosity","chapter","novel_topics",
                "b0_detr", "b1_detr", "b2_detr",
                "bottle_dim0_detr", "bottle_dim1_detr", "bottle_dim2_detr",
                "wasser_dim0_detr", "wasser_dim1_detr", "wasser_dim2_detr")]

colnames(X_corre) <- c("curiosity","chapter","novel topics","b0","b1","b2", "bottledist. dim0", "bottledist. dim1", "bottledist. dim2", "wasserstein dist. dim0", "wasserstein dist. dim1", "wasserstein dist. dim2")
corri <- cor(X_corre, method = "spearman")


# Figure of submission
corrplot::corrplot(corri,
                   method = "circle",       # Use color for correlation strength
                   type = "lower",         # Show only the lower triangle
                   order = "original",        # Order by hierarchical clustering (more informative)
                   addCoef.col = "black",   # Add correlation coefficients in black
                   tl.col = "black",        # Black text labels for variables
                   tl.srt = 45,            # Rotate text labels 45 degrees
                   tl.cex = 0.8,           # Slightly larger text labels (adjust as needed)
                   number.cex = 0.7,        # Slightly larger coefficient text
                   col = corrplot::COL2("RdYlBu", 200),# Use a diverging color palette (Purple-Orange)
                   cl.pos = "b",            # Put color legend at the bottom
                   cl.ratio = 0.15,          # Adjust color legend width
                   diag = FALSE,           # Remove the diagonal (it's always 1)
                   addrect = 2,            # Add rectangles around clusters (if using hclust)
                   rect.col = "blue",      # Color of the rectangles
                   rect.lwd = 2,            # Line width of the rectangles
                   title = "\nCorrelation Matrix"  # Add a title (with newline for spacing)
)


# Plot for submission: curiosity plot
plot(X$curiosity, type = "l"
     , xlab = "Chapter", ylab = "Avg. curiosity"
     , main = "Average Curiosity per Chapter")


## ## ## ## ## ## ## ##
## ## ## GAM ## ## ## #
## ## ## ## ## ## ## ##
# The GAM extends traditional linear models by allowing for non-linear relationships 
# between predictors and the response variable, using smooth functions (splines) 
# for each predictor. It combines these smooth functions additively, providing a 
# flexible framework that can capture complex patterns in the data while maintaining interpretability
# bs="cr": Specifies the use of cubic regression splines for smoothing.
# k=4: Sets the maximum number of knots (or degrees of freedom) for each smooth term to 4.
# select=T: Enables additional shrinkage on the smooth terms, potentially setting some to zero.

# Fit Full GAM model
k=4

model <- gam(curiosity ~ s(chapter, bs = "cs", k=k) +
               s(novel_topics, bs = "cs", k=k) + 
               s(bottle_dim0_detr, bs = "cs", k=k) +
               s(bottle_dim1_detr, bs = "cs", k=k) +
               s(bottle_dim2_detr, bs = "cs", k=k) +
               s(wasser_dim0_detr, bs = "cs", k=k) +
               s(wasser_dim1_detr, bs = "cs", k=k) +
               s(wasser_dim2_detr, bs = "cs", k=k) +
               s(b0_detr, bs = "cs", k=k) +
               s(b1_detr, bs = "cs", k=k) +
               s(b2_detr, bs = "cs", k=k) 
             , data = X
             , method = "REML"
             , select = T
             , gamma = 1)

#cor(X)
#Print model summary
summary(model)

# Diagnostics
plot(model, page = 1, residuals = T, pch = 20)
concurvity(model, full = F)
gam.check(model, pch = 20)


# Plots
vis.gam(model, view = c("bottle_dim0","b0_detr"), theta = 130)
vis.gam(model, view = c("wasser_dim1","b0_detr"), theta = 100, phi = 20)


#== == == == == == == == == == == == == == == == == == == == == == == == == == 
# Fit Null Model
null_model <- gam(curiosity ~ s(chapter, bs = "cs", k=k) +
                    s(novel_topics, bs = "cs", k=k) 
                  , data = X
                  , method = "REML"
                  , select = T
                  , gamma = 1)

summary(null_model)


# ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  ==  == 
# model comparison
model_comparison <- anova(null_model, model, test = "Chisq")
print(model_comparison)


# == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Permutation test
set.seed(1137)
deviance <- numeric(1)
rsquared <- numeric(1)
curiosity <- chapter_descriptives$curious_mean
for(i in 1:1000){
  data_permuted <- X
  curiosity_permuted <- sample(curiosity)
  data_permuted$curiosity <- curiosity_permuted
  
  model_permuted <- gam(curiosity ~ s(chapter, bs = "cs", k=k) +
                          s(novel_topics, bs = "cs", k=k) + 
                          s(bottle_dim0_detr, bs = "cs", k=k) + #set comment for null model
                          s(bottle_dim1_detr, bs = "cs", k=k) + #set comment for null model
                          s(bottle_dim2_detr, bs = "cs", k=k) + #set comment for null model
                          s(wasser_dim0_detr, bs = "cs", k=k) + #set comment for null model
                          s(wasser_dim1_detr, bs = "cs", k=k) + #set comment for null model
                          s(wasser_dim2_detr, bs = "cs", k=k) + #set comment for null model
                          s(b0_detr, bs = "cs", k=k) + #set comment for null model
                          s(b1_detr, bs = "cs", k=k) + #set comment for null model
                          s(b2_detr, bs = "cs", k=k)  #set comment for null model
                        , data = data_permuted
                        , method = "REML"
                        , select = T
                        , gamma = 1)
  
  deviance[i] <- (1 - model_permuted$deviance/model_permuted$null.deviance)*100
  rsquared[i] <- summary(model_permuted)$r.sq
}

# results odf permutation test
hist(deviance, breaks = 40, main = paste("Histogram of explained deviance"), xlab = "Explained Deviance")
modeldeviance <- (1 - model$deviance/model$null.deviance)*100
abline(v = modeldeviance, col = "red", lwd = 2)  # Add the vertical line
ecdf(deviance)((1 - model$deviance/model$null.deviance)*100)

hist(rsquared, breaks = 40, main = paste("Histogram of R²"), xlab = "R²")
modelrsquared <- summary(model)$r.sq
abline(v = modelrsquared, col = "red", lwd = 2)  # Add the vertical line
ecdf(rsquared)(summary(model)$r.sq)

