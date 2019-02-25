# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for ClosePrice

library(ggplot2) # Provides ggplot(), aes(), geom_boxplot(), stat_summary(), xlab(), ggtitle(), theme().

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.
#you can add other variable by the numbers of their col

library(GGally) # Provides ggpairs().
library(ggplot2) # Provides theme(), element_blank().

ggpairs(sep.envdataset[sep.envsample,],
        columns=c(7,12,13,14,15,16,29),
        colour="LVal18AFC",
        diag=list(continuous="densityDiag",
                  discrete="barDiag"),
        upper=list(continuous="cor",
                   combo="box",
                   discrete="ratio"),
        lower=list(continuous="points",
                   combo="denstrip",
                   discrete="facetbar")) +
  theme(panel.grid.major=element_blank())

#============================================================

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for NumberTrade

library(ggplot2) # Provides ggplot(), aes(), geom_boxplot(), stat_summary(), xlab(), ggtitle(), theme().

# Generate a box plot.

p01 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$NumberTrade)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of NumberTrade (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for VolumeTrade

# Generate a box plot.

p02 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$VolumeTrade)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of VolumeTrade (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for ClosePrice

# Generate a box plot.

p03 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$ClosePrice)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of ClosePrice (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for X5.SMA

# Generate a box plot.

p04 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$X5.SMA)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of X5.SMA (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for X20_SMA

# Generate a box plot.

p05 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$X20_SMA)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of X20_SMA (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for X5.WMA

# Generate a box plot.

p06 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$X5.WMA)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of X5.WMA (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for X5.EMA

# Generate a box plot.

p07 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$X5.EMA)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of X5.EMA (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for X26.EMA

# Generate a box plot.

p08 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$X26.EMA)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of X26.EMA (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Use ggplot2 to generate box plot for NVI

# Generate a box plot.

p09 <- sep.env %>%
  with(sep.env$dataset[sample,]) %>%
  ggplot(aes(y=sep.env$sep.envdataset$NVI)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  geom_boxplot(aes(x=sep.envdataset$LVal18AFC, fill=sep.envdataset$LVal18AFC), notch=TRUE) +
  stat_summary(aes(x=sep.envdataset$LVal18AFC), fun.y=mean, geom="point", shape=8) +
  xlab("LVal18AFC\n\n gryphosonix") +
  ggtitle("Distribution of NVI (sample)\nby LVal18AFC") +
  theme(legend.position="none")

# Display the plots. 
#after see plot in rstudio plot 
#u can save them with high resolution 

library(gridExtra) # Provides grid.arrange().

grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09)

#============================================================

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only. PEARSON

sep.envcor <- cor(sep.envdataset[sep.envsample, sep.envnumeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

sep.envord <- order(sep.envcor[1,])
sep.envcor <- sep.envcor[sep.envord, sep.envord]

# Display the actual correlations.

print(sep.envcor)

# Graphically display the correlations.

corrplot(sep.envcor, mar=c(0,0,1,0))
title(main="Correlation ikhodro.csv using Pearson",
      sub=paste("SSIAVASH", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only. KENDALL

sep.envcor <- cor(sep.envdataset[sep.envsample, sep.envnumeric], use="pairwise", method="kendall")

# Order the correlations by their strength.

sep.envord <- order(sep.envcor[1,])
sep.envcor <- sep.envcor[sep.envord, sep.envord]

# Display the actual correlations.

print(sep.envcor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(sep.envcor, mar=c(0,0,1,0))
title(main="Correlation ikhodro.csv using Kendall",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only. SPEARMAN

sep.envcor <- cor(sep.envdataset[sep.envsample, sep.envnumeric], use="pairwise", method="spearman")

# Order the correlations by their strength.

sep.envord <- order(sep.envcor[1,])
sep.envcor <- sep.envcor[sep.envord, sep.envord]

# Display the actual correlations.

print(sep.envcor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(sep.envcor, mar=c(0,0,1,0))
title(main="Correlation ikhodro.csv using Spearman",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

sep.envcor <- cor(sep.envdataset[sep.envsample, sep.envnumeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

sep.envord <- order(sep.envcor[1,])
sep.envcor <- sep.envcor[sep.envord, sep.envord]

# Display the actual correlations.

print(sep.envcor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(sep.envcor, mar=c(0,0,1,0))
title(main="Correlation ikhodro.csv using Pearson",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(sep.env$seed)
sep.envrf <- randomForest::randomForest(ClosePrice ~ .,
                                        data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], 
                                        ntree=300,
                                        mtry=9,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

rn <- round(randomForest::importance(sep.envrf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 11.00 secs

#============================================================
# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model. bag = 200samples

set.seed(sep.env$seed)
sep.envrf <- randomForest::randomForest(ClosePrice ~ .,
                                        data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], 
                                        ntree=300,
                                        mtry=9,
                                        sampsize=c(200),
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

rn <- round(randomForest::importance(sep.envrf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 2.21 secs

#============================================================

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(sep.env$seed)
sep.envrf <- randomForest::randomForest(ClosePrice ~ .,
                                        data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], 
                                        ntree=300,
                                        mtry=9,
                                        sampsize=c(100),
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

rn <- round(randomForest::importance(sep.envrf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 1.14 secs

#============================================================

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(sep.env$seed)
sep.envrf <- randomForest::randomForest(ClosePrice ~ .,
                                        data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], 
                                        ntree=300,
                                        mtry=9,
                                        sampsize=c(500),
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

rn <- round(randomForest::importance(sep.envrf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 3.63 secs

#============================================================

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(sep.env$seed)
sep.envrf <- randomForest::randomForest(ClosePrice ~ .,
                                        data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], 
                                        ntree=300,
                                        mtry=9,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

rn <- round(randomForest::importance(sep.envrf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 10.79 secs

#============================================================

# Plot the relative importance of the variables.

randomForest::varImpPlot(sep.envrf, main="")
title(main="Variable Importance Random Forest ikhodro.csv",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Display tree number 14. you can change 14 for get any tree results

printRandomForests(sep.envrf, 14)

# Plot the error rate against the number of trees.

plot(sep.envrf, main="")
legend("topright", c(""), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest",
      sub=paste("SSIAVASH", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Plot the OOB ROC curve. (for classification)

#library(verification)
#aucc <- verification::roc.area(as.integer(as.factor(sep.envdataset[sep.envsample, sep.envtarget]))-1,
#                 sep.envrf$votes[,2])$A
#verification::roc.plot(as.integer(as.factor(sep.envdataset[sep.envsample, sep.envtarget]))-1,
#         sep.envrf$votes[,2], main="")
#legend("bottomright", bty="n",
#       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
#title(main="OOB ROC Curve Random Forest ikhodro.csv",
#    sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Random Forest 

# The 'party' package provides the 'cforest' function.

library(party, quietly=TRUE)

# Build the Random Forest model.

set.seed(sep.env$seed)
sep.envrf <- party::cforest(ClosePrice ~ .,
                            data=sep.envdataset[sep.envsample,c(sep.envinput, sep.envtarget)], controls=cforest_unbiased(
                              ntree=300,
                              mtry=9))

# Generate textual output of 'Random Forest' model.

sep.envrf

# List the importance of the variables.

data.frame(Importance=sort(varimp(sep.envrf), decreasing=TRUE))

# Time taken: 1.04 mins

#============================================================
# Plot the relative importance of the variables.

# Note that values vary slightly on each call to varimp().

v    <- party::varimp(sep.envrf)
vimp <- data.frame(Variable=as.character(names(v)),
                   Importance=v,
                   row.names=NULL, stringsAsFactors=FALSE)

p <- ggplot2::ggplot(vimp, ggplot2::aes(Variable, Importance)) +
  ggplot2::geom_bar(stat="identity", position="identity") +
  ggplot2::scale_x_discrete(limits=with(vimp, Variable[order(Importance)])) +
  ggplot2::theme(legend.position="none",
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank()) +
  ggplot2::coord_flip()
p

# Plot the OOB ROC curve. for classification

#library(verification)
#aucc <- verification::roc.area(as.integer(as.factor(sep.envdataset[sep.envsample, sep.envtarget]))-1,
#                 sep.envrf$votes[,2])$A
#verification::roc.plot(as.integer(as.factor(sep.envdataset[sep.envsample, sep.envtarget]))-1,
#         sep.envrf$votes[,2], main="")
#legend("bottomright", bty="n",
#       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
#title(main="OOB ROC Curve Random Forest ikhodro.csv",
#    sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================

# Evaluate model performance. 

# for cross validation

# RF: Generate a Predicted v Observed plot for rf model on ikhodro.csv [validate].

sep.envpr <- predict(sep.envrf, newdata=sep.envdataset[sep.envvalidate, c(sep.envinput, sep.envtarget)])

# Obtain the observed output for the dataset.

obs <- subset(sep.envdataset[sep.envvalidate, c(sep.envinput, sep.envtarget)], select=sep.envtarget)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(ClosePrice=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=sep.envpr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="ClosePrice", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
      Random Forest Model
      ikhodro.csv [validate]",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================

# Evaluate model performance. for whole data 

# RF: Generate a Predicted v Observed plot for rf model on ikhodro.csv.

sep.envpr <- predict(sep.envrf, newdata=sep.envdataset)

# Obtain the observed output for the dataset.

obs <- subset(sep.envdataset, select=sep.envtarget)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(ClosePrice=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=sep.envpr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="ClosePrice", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
      Random Forest Model
      ikhodro.csv",
      sub=paste("Gryphosonix", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================

# Score a dataset. 

# Obtain predictions for the Random Forest model on ikhodro.csv.

sep.envpr <- predict(sep.envrf, newdata=sep.envdataset)

# Extract the relevant variables from the dataset.

sdata <- sep.envdataset

# Output the combined data.

write.csv(cbind(sdata, sep.envpr), file="~/reduced data output/irkhodro_score_all.csv", row.names=FALSE)