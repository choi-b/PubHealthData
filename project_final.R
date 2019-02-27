# Update 2.27.19

#load data
load("Final_Data.RData")
summary(Final_Data)

#learn data
table(Final_Data$UCD...Drug.Alcohol.Induced.Cause)
table(Final_Data[,9:10]) #
table(Final_Data[,1:2]) #count of deaths in each region
table(Final_Data[,c(2,10)]) #count of deaths across regions and death category
table(Final_Data[,c(5,10)]) #count of deaths in each category by gender
table(Final_Data[,c(1,4)]) #count of deaths by age, region
table(Final_Data[,c(1,7)]) #region/year

#clean clean clean
work2 = Final_Data[-c(1,3,5,8,9)]

#test run with dummy variables
binary_work = model.matrix( ~ Census.Region.Code + Gender.Code + 
                              UCD...Drug.Alcohol.Induced.Cause.Code, work2)[,-1]
binary_work<-binary_work[,-c(4,6,13)]
table(work2$UCD...Drug.Alcohol.Induced.Cause.Code) #A1 gone

numeric = work2[,c(2,4)]

work4 = merge(numeric,binary_work,by = "row.names", all = TRUE)
work4 = work4[,-1]

colnames(work4) = c("Age","Year","R2","R3","R4","GenderM","A9","D1","D2","D4","D9","O9")
#work4

#colnames(work2) = c("Region","Age","Gender","Year","Cause_Death")

#######################################
#######################################
#######################################
####### EXPLORATORY ANALYSIS ##########
#######################################
#######################################
#######################################

#EDA - correlation tables
#install.packages("gclus")
library(gclus)
my.abs = abs(cor(work4[,-1]))
my.colors = dmat.color(my.abs)
my.ordered = order.single(cor(work4[,-1]))
cpairs(work4,my.ordered,panel.colors=my.colors, gap=0.5)

#EDA - side by side bar charts

tbl = table(droplevels(Final_Data[,c(5,10)]))
tbl
tbl = tbl[,1:6]

tbl2 = table(droplevels(Final_Data[,c(1,10)]))
tbl2 = tbl2[,1:6]
tbl2
rownames(tbl2) = c("Northeast","Midwest","South","West")

tbl3 = table(droplevels(Final_Data[,c(1,4)]))

tbl4 = table(droplevels(Final_Data[,c(1,7)]))


library(dplyr)
library(ggplot2)

# Count of Deaths in Each Cause (Except O9) by Gender
tbl %>%
  as.data.frame()  %>%
  ggplot(., aes(x= UCD...Drug.Alcohol.Induced.Cause.Code , y = Freq)) + 
  geom_bar(aes(fill=Gender), position = "dodge", stat = "identity") +
  xlab("Causes of Death") +
  ylab("Count of Deaths") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=13))

#Count of Deaths in Each Cause (Except 09) by Region
tbl2 %>%
  as.data.frame()  %>%
  ggplot(., aes(x= UCD...Drug.Alcohol.Induced.Cause.Code , y = Freq)) + 
  geom_bar(aes(fill=Census.Region), position = "dodge", stat = "identity") +
  xlab("Causes of Death") +
  ylab("Count of Deaths") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=13))

#Count of Deaths in Each Region by Age
tbl3 %>%
  as.data.frame()  %>%
  ggplot(., aes(x= Single.Year.Ages.Code , y = Freq)) + 
  geom_bar(aes(fill=Census.Region), position = "dodge", stat = "identity") +
  xlab("Age") +
  ylab("Count of Deaths") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=13))

tbl4 %>%
  as.data.frame()  %>%
  ggplot(., aes(x= Year , y = Freq)) + 
  geom_bar(aes(fill=Census.Region), position = "dodge", stat = "identity") +
  xlab("Year") +
  ylab("Count of Deaths") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=13))


#######################################
#######################################
#######################################
####### Multivariate ANALYSIS #########
#######################################
#######################################
#######################################


##### PCA #####
cd.pca <- prcomp(work4,center = TRUE,scale. = TRUE) 
print(cd.pca)

#summary
summary(cd.pca)
cd.pca
#eigenvalues
cd.pca$sdev^2

#rotation (PC loadings)
cd.pca$rotation

##screeplot##
library(factoextra)
fviz_eig(cd.pca)

#Graph of variables
fviz_pca_var(cd.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#biplot?
#fviz_pca_biplot(cd.pca, repel = TRUE,
#                col.var = "#2E9FDF", # Variables color
#                col.ind = "#696969"  # Individuals color
#)

#Dotplot PC1
load    <- cd.pca$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotchart(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="blue")

#Dotplot PC2
sorted.loadings2 <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2" 
myXlab  <- "Variable Loadings"
dotchart(sorted.loadings2, main=myTitle, xlab=myXlab, cex=1.5, col="blue")

#Biplot
biplot(cd.pca)

##### VARIMAX #####
#Apply the varimax rotation
#change of coordinates that maximizes the 
#sum of the variances of the squared loadings.
#clean up the rotations
cd.pca.var = varimax(cd.pca$rotation)
cd.pca.var = varimax(cd.pca$rotation[,1:7])
cd.pca.var

#plots before and after rotation

##### MCA #####

#preprocess for MCA
work5 = work4[-c(1,2)]
#make all columns from numeric ==> factors
cols = c("R2", "R3", "R4", "GenderM","A9","D1","D2","D4","D9","O9")
work5[cols] = lapply(work5[cols], factor)


library(FactoMineR)
mca = MCA(work5, graph = FALSE)
summary(mca)
mca
#Correlation between variables and principal dimensions
fviz_mca_var(mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())



cats = apply(work5, 2, function(x) nlevels(as.factor(x)))
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
  geom_text_repel(aes(colour=Variable)) +
  ggtitle("MCA Plot of Variables")

#Include both observations and the categories
# MCA plot of observations and categories
# this also adds the density curves to identify the zones that are highly concentrated.
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text_repel(data = mca_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables with Observations") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=13)) +
  scale_colour_discrete(name = "Variable")


##### CCA #####
#CANONICAL CORRELATION ANALYSIS (CCA)
#canonical variable: a weighted sum of the variables
#we have two vectors (sets) of random variables, and there are correlations among the variables
#canonical-correlation finds linear combinations of the two vectors which have maximum correlation with each other.
library(dotCall64)
library(fields)
library(CCA)

#FIRST TIME 
#separate into set 1 (region variables) and set 2 (rest)
X = work4[,3:5] #region vars
Y = work4[-c(3:5)]

##################
#Scaled vers. (The same - I think)
Xs = scale(X)
Ys = scale(Y)
#heplot (Scaled)
ccaS = cancor(Xs,Ys)
heplot(cca)
abline(h=0,v=0)
##################

correl <- matcor(X,Y)
correl
img.matcor(correl, type = 2)

cca = cc(X,Y)
cca = cancor(X,Y)
cca

#heplot
heplot(cca)
abline(h=0,v=0)

#plot variable
plt.var(cca, d1=1, d2=2, 
        int = 0.5, var.label = TRUE, 
        Xnames = NULL, Ynames = NULL)


#SECOND TIME
#separate into set 1 (cause of death variables) and set 2 (rest)
X2 = work4[,7:12]
Y2 = work4[-c(7:12)]
correl2 <- matcor(X2,Y2)
correl2
img.matcor(correl2, type = 2)

cca2 = cc(X2,Y2)
cca2 = cancor(X2,Y2)
cca2


#heplot
heplot(cca2)
abline(h=0,v=0)
plt.var(cca2, d1=1, d2=2, 
        int = 0.5, var.label = TRUE, 
        Xnames = NULL, Ynames = NULL)



#THIRD TIME
#separate into set 1 (age, year) and set 2 (rest)
X3 = work4[,1:2]
Y3 = work4[-c(1:2)]
correl3 <- matcor(X3,Y3)
correl3
img.matcor(correl3, type = 2)

cca3 = cc(X3,Y3)
cca3$cor

#Plots for variables representation for CCA
plt.var(cca,d1=1,d2=2,var.label = TRUE)
plt.var(cca2,d1=1,d2=2,var.label = TRUE)
plt.var(cca3,d1=1,d2=2,var.label = TRUE)

#Canonical Loadings
cl = comput(X,Y,cca)
cl[3:6]


#Canonical Correlation Analysis
CCorA(X, Y)
CCorA(X2, Y2)
CCorA(X3, Y3)


plt.var(cca3, d1=1, d2=2, 
        int = 0.5, var.label = TRUE, 
        Xnames = NULL, Ynames = NULL)

library(candisc)
cca <- candisc::cancor(d %>% select(O:N), d %>% select(Real:Conv))
cca %>% summary
plt.var(cca, d1=1, d2=2)
heplot(cca)
