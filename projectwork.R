##update 2.24.19
#Initial Steps

#Update R
#install.packages('devtools') #assuming it is not already installed
#library(devtools)
#install_github('andreacirilloac/updateR')
#library(updateR)
#updateR(admin_password = 'your password')

#save/load the data
save(Final_Data, file="Final_Data.RData")
load("Final_Data.RData")

#drop unused factor level
droplevels(Final_Data$Census.Region)
str(Final_Data)
#work dataset uses 5 variables
#work = Final_Data[-c(1,3,5,8,9)] 

model.matrix( ~ covariates + covariates, Final_Data)[,-1]

hist(Final_Data$Single.Year.Ages.Code)

#keep a copy of the dataset in .dat file extension
write.table(Final_Data,"dataset.dat",row.names=FALSE,sep="\t", quote = FALSE)

#analysis

work = Final_Data[-c(1,3,5,8,9)]
work$UCD...Drug.Alcohol.Induced.Cause.Code

library(tidyr)
library(ggplot2)
work[1:2500,] %>%
  gather(-Single.Year.Ages.Code, -Census.Region.Code, -UCD...Drug.Alcohol.Induced.Cause.Code,
         key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Single.Year.Ages.Code, 
             color = Census.Region.Code,
             shape = factor(UCD...Drug.Alcohol.Induced.Cause.Code))) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Loess lines

work[1:500,] %>%
  gather(-Single.Year.Ages.Code, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = Single.Year.Ages.Code)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#scatter plot for Causes of Death by Region
ggplot(work, aes(x = UCD...Drug.Alcohol.Induced.Cause.Code, 
                         y = Single.Year.Ages.Code , color = Census.Region.Code)) + 
  geom_point(position = position_dodge(width = 0.4))

#scatter plot with box plots
ggplot(work, aes(x = UCD...Drug.Alcohol.Induced.Cause.Code, 
                 y = Single.Year.Ages.Code,fill=Census.Region.Code)) +
  geom_boxplot(outlier.size=0) +
  geom_jitter(aes(UCD...Drug.Alcohol.Induced.Cause.Code + Single.Year.Ages.Code),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=3,
              show_guide=FALSE) +
  scale_fill_discrete(name="Age Group") +
  xlab("Causes of Death") +
  ylab("Age") +
  theme(axis.title.x = element_text(face='bold',size=16,hjust=0.5),
        axis.title.y = element_text(face='bold',size=16,vjust=1),
        axis.text.x = element_text(face='bold',size=14,color='black'),
        axis.text.y = element_text(face='bold',size=14,color='black'),
        legend.title = element_text(face="bold", color="black", size=14),
        legend.text = element_text(face="bold", color="black", size=12))


#######################################
#######################################
#######################################
####### EXPLORATORY ANALYSIS ##########
#######################################
#######################################
#######################################

#######################################
#Pairwise Plots Using GGally
library(GGally)
#ggpairs - exploratory analysis
ggpairs(work2,aes(colour = Census.Region.Code, alpha=0.4))


#######################################
#2D scatter
ggplot(work2, aes(x=Year, y=Single.Year.Ages.Code)) +
  geom_point(size=2, shape=23)

#3D Scatterplot
#since only two quantitative, change the causes of death to "1:7"
#install.packages("scatterplot3d")
library(scatterplot3d)
work3D = work2
work3D$UCD...Drug.Alcohol.Induced.Cause.Code
z <- c(1,2,3,4,5,6,7)
work3D$UCD...Drug.Alcohol.Induced.Cause.Code <- ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "A1", z[1],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "A9", z[2],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "D1", z[3],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "D2", z[4],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "D4", z[5],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "D9", z[6],
                                                       ifelse(work3D$UCD...Drug.Alcohol.Induced.Cause.Code == "O9", z[7], NA
                                                              )))))))
#work3D$UCD...Drug.Alcohol.Induced.Cause.Code is now numeric...
attach(work3D)
scatterplot3d(Single.Year.Ages.Code,
              Year,
              UCD...Drug.Alcohol.Induced.Cause.Code, main="3D Scatterplot",
              pch = UCD...Drug.Alcohol.Induced.Cause.Code,
              color = UCD...Drug.Alcohol.Induced.Cause.Code)

#rotatable 3D scatterplot
#install.packages("rgl")
library(rgl) #interactive 3D scatter...
library(plot3D)
plot3d(x = Single.Year.Ages.Code, 
       y = Year, 
       z = UCD...Drug.Alcohol.Induced.Cause.Code,
       col = UCD...Drug.Alcohol.Induced.Cause.Code )

#another way
scatter3D(x = Single.Year.Ages.Code, 
          y = Year, 
          z = UCD...Drug.Alcohol.Induced.Cause.Code)


#######################################
#PCA (mixed)
#install.packages("PCAmixdata")
library(PCAmixdata)
work2 = na.omit(work) #get rid of 8 missing values
split = splitmix(work2)
X1 = split$X.quanti
#scale continuous variables (age, year - log transform(?))
X1 = scale(X1)
X2 = split$X.quali
res.pcamix = PCAmix(X.quanti = X1, X.quali = X2, ndim=8, rename.level=TRUE, graph=FALSE)

#pcamix
res.pcamix
summary(res.pcamix)

#sqloadings
res.pcamix$sqload

#variance of the pca & eig
res.pcamix$sqload
res.pcamix$eig

#plotting the pca
?plot.PCAmix
par(mfrow=c(2,2))
plot(res.pcamix,choice="ind",coloring.ind=X2$houses,label=FALSE,
     posleg="bottomright", main="Observations")
plot(res.pcamix,choice="levels",xlim=c(-1.5,2.5), main="Levels")
plot(res.pcamix,choice="cor",main="Numerical variables")
plot(res.pcamix,choice="sqload",coloring.var=T, leg=FALSE,main="All variables")

#predict and plot new observations
par(mfrow=c(1,1))
set.seed(10)
test <- sample(1:nrow(work2),100)
train.pcamix <- PCAmix(X1[-test,],X2[-test,],ndim=2,graph=FALSE)
pred <- predict(train.pcamix,X1[test,],X2[test,])
head(pred)
plot(train.pcamix,axes=c(1,2),label=FALSE,main="Observations map")
points(pred,col=2,pch=16)
legend("bottomleft",legend = c("train","test"),fill=1:2,col=1:2)

#######################################
#PCArot - varimax rotation
#varimax rotation: finds the rotation that maximizes the sample variances of the standardized loadings

res.pcarot <- PCArot(res.pcamix,dim=8,graph=FALSE)
res.pcarot$eig #variance of the rotated PCs
res.pcarot$sqload
#plots comparison
par(mfrow=c(2,2))
plot(res.pcamix,choice="ind",label=FALSE,main="Observations before rotation")
plot(res.pcarot,choice="ind",label=FALSE,main="Observations after rotation")
plot(res.pcamix,choice="sqload", coloring.var=TRUE, leg=FALSE, posleg="topleft", main="Squared Loadings before rotation")
plot(res.pcarot,choice="sqload", coloring.var=TRUE, leg=FALSE, posleg="topright", main="Squared Loadings after rotation")

#correlation circles (before and after rotation)
plot(res.pcamix,choice="cor", coloring.var=TRUE, leg=FALSE, main="Before rotation")
plot(res.pcarot,choice="cor", coloring.var=TRUE, leg=FALSE, main="After rotation")
plot(res.pcamix,choice="levels",  leg=FALSE, main="Before rotation",xlim=c(-1.5,2.5))
plot(res.pcarot,choice="levels", leg=FALSE,main="After rotation",xlim=c(-1.5,2.5))

#######################################
#MCA - multiple correspondence analysis.
# Goal
#1) A group of individuals with similar profile in their answers to the questions
#2) The associations between variable categories

#install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)

work3 = work2[-c(2,4)]
cats = apply(work3, 2, function(x) nlevels(as.factor(x)))

# apply MCA
mca = MCA(work3, graph = FALSE)
mca
mca$eig

# data frame with variable coordinates
mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca_obs_df = data.frame(mca$ind$coord)

# plot of variable categories
# region, gender, and O9 are highly correlated.
par(mfrow=c(1,1))
ggplot(data=mca_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA Plot of Variables")

#Include both observations and the categories
# MCA plot of observations and categories
# this also adds the density curves to identify the zones that are highly concentrated.
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables with Observations") +
  scale_colour_discrete(name = "Variable")

#######################################
#Canonical Correlation Analysis (CCA)
#canonical variable: a weighted sum of the variables
#we have two vectors (sets) of random variables, and there are correlations among the variables
#canonical-correlation finds linear combinations of the two vectors which have maximum correlation with each other.
library(dotCall64)
library(fields)
library(CCA)

str(work2)

#separate into set 1 (age, year) and set 2 (gender, region, cause of death)
X <- work2[c(2,4)]
Y <- work2[-c(2,4)]

