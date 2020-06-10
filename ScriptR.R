if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(matrixStats)
install.packages("Rborist")
library(rpart)
################################
# Download source file
################################

url <- "https://raw.githubusercontent.com/dxng-sg/HarvardxPokemon/master/datasets_121_280_Pokemon.csv"
dl <- tempfile()
download.file(url, dl)
dat <- read_csv(dl)
file.remove(dl)

####################################
## METHOD AND ANALYSIS
###################################

##### DATA DESCRIPTION
##To identify the data type of each variables (ie., factor, numeric, or character)
head(dat)

## TO identify all the variables in dataset
names(dat)

## To identify the total number of observations and variables
dim(dat)

## The number of levels within Pokemon Types (our classification)
levels(as.factor(dat$`Type 1`))

# To find the distribution of Type 1 pokemon
# This plot shows that there are only a few Pokemon with Type 1 'Flying', 'Fairy', 'Ice', 'Fighting', and 'Steel'
dat%>%
  group_by(`Type 1`)%>%
  mutate(count=n())%>%
  ungroup()%>%
  ggplot(aes(`Type 1`))+
  geom_bar()+
  geom_label(aes(x=`Type 1`, 
                 y = count+3.5, 
                 label = count))+
  ggtitle("Distribution of Type 1 Pokemon")


# To find the proportion of pokemon with Type 2 characteristics
mean(is.na(dat$`Type 2`) == FALSE)
sum(is.na(dat$`Type 2`) == FALSE)
sum(is.na(dat$`Type 2`) == TRUE)

# To find the distribution of proportion of `Type 1` Pokemons with `Type 2` 
# This plot shows that Bug, Dark, Dragon, Ghost, Ground, Rock, Steel are likely to have a `Type 2` characteristic
# While Fairy, Fighting, Psychi are less likely to have a `Type 2` characteristic
dat%>%
  group_by(`Type 1`)%>%
  mutate(count=n())%>%
  ungroup()%>%
  ggplot(aes(`Type 1`))+
  geom_bar(aes(fill = `Type 2`),colour = "#0E0A09")+
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.2))+
  ggtitle("Distribution of `Type 1` and `Type 2`")

#To find the distribution of Type 2 within each `Type 1`
dat%>%
  filter(is.na(`Type 2`) == FALSE)%>%
  group_by(`Type 1`, `Type 2`)%>%
  mutate(count=n())%>%
  ungroup()%>%
  ggplot(aes(`Type 2`))+
  geom_bar(aes(fill = `Type 2`))+
  facet_wrap(~`Type 1`)+
  geom_text(aes(x=`Type 2`, 
                y = count+2, 
                label = count))+
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.2))+
  ggtitle("Distribution of `Type 2` (within each 'Type 1` elements)")

# To find the overall distribution of Type 2 pokemon
dat%>%
  filter(is.na(`Type 2`) == FALSE) %>%
  group_by(`Type 2`)%>%
  mutate(count = n())%>%
  ungroup%>%
  ggplot(aes(`Type 2`))+
  geom_bar()+
  geom_label(aes(x=`Type 2`, 
                 y = count + 3.5, 
                 label = count))+
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.2))+
  ggtitle("Distribution of `Type 2`")



# Jitterplot reveals several clusters. For example `Type 1` normal with `Type 2` Flying; `Type 1` Grass with `Type 2` Poison; `Type 1` Bug with `Type 2` Poison.
# We also observe that certain Type 1 pokemon (e.g., Bug, ghost, grass, normal) are more likely to have Type 2 characteristic. 
#While some Type 1 pokemon (e.g., Fairy, electric, ice) are less likely to have Type 2 characteristic. 
dat%>%
  ggplot(aes(x = `Type 1`, y = `Type 2`))+
  geom_jitter()

## The summary statistics for the variables
summary(dat[5:13])

# Explore how the various variables are distributed across Type 1 characteristics
dat%>%
  ggplot(aes(y = Total, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = HP, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Attack, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Defense, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = `Sp. Atk`, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = `Sp. Def`, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Speed, 
             x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(x = Generation))+
  geom_bar()+
  facet_wrap(~`Type 1`)+
  ggtitle("Distribution of Generation (within each `Type 1`)")

dat%>%
  filter(`Type 1` != "Flying")%>%
  group_by(Generation, `Type 1`)%>%
  mutate(count=n())%>%
  ungroup()%>%
  ggplot(aes(x = Generation, 
             y = count,
             fill = `Type 1`))+
  geom_bar(stat="identity",
           colour = "#0E0A09",
           width = 0.7,
           position = position_dodge(width = 0.8))+
  geom_text(position=position_dodge(width=0.8), 
            aes(label=`Type 1`),
            hjust=-0.5,
            vjust=0,
            angle=90)+
  ylim(0, 32)+
  ggtitle("Distribution of `Type 1` within each Generation")

# It is more likely to capture a non-legendary pokemon than a legendary pokemon among all `Type 1`
# You have more chance of capturing a legendary pokemon when you capture a Dragon class than the other types
dat%>%
  ggplot(aes(x = Legendary))+
  geom_bar()+
  facet_wrap(~`Type 1`)+
  ggtitle("Distribution of Legendary status (within each `Type 1`)")


#Among Legendary pokemon, there are more psychic and dragon types than the other types
dat%>%
  filter(`Type 1` != "Flying")%>%
  group_by(Legendary, `Type 1`)%>%
  mutate(count=n())%>%
  ungroup()%>%
  ggplot(aes(x = Legendary, 
             y = count,
             fill = `Type 1`))+
  geom_bar(stat="identity",
           colour = "#0E0A09",
           width = 0.7,
           position = position_dodge(width = 0.8))+
  geom_text(position=position_dodge(width=0.8), 
            aes(label=`Type 1`),
            hjust=-0.5,
            vjust=0,
            angle=90)+
  ylim(0, 32)+
  ggtitle("Distribution of `Type 1` within each Legendary class")


## Correlation between the continuous variables 
cor(dat[5:11])

x <- as.matrix(dat[5:13])
y <- factor(dat$`Type 1`)
nearZeroVar(x)
sds<-colSds(x)
qplot(sds)
set.seed(2007, sample.kind="Rounding")

#Knn model
fit_knn <- knn3(y ~ x, data = dat, k = 1)
y_hat_knn<-predict(fit_knn, dat, type = "class")
cm <- confusionMatrix(y_hat_knn,y)
cm$overall["Accuracy"]

#Electric, Fairy, Fire, Flying, Water are hard to detect; 
# incorrectly predicted type is Dark, Electric, Fire, Flying, Grass
cm$byClass[,1:2]

#QDA model
fit_lda <- train(`Type 1` ~ HP + Attack + Defense + `Sp. Atk` + `Sp. Def` + Speed + Generation + Legendary, method = "lda", data = dat)
y_hat_lda <- predict(fit_lda, dat)
cm <- confusionMatrix(y_hat_lda,y)
cm$overall["Accuracy"]
cm$byClass[,1:2]

# Use decision tree
fit_rpart<-train(x,y,
                   method = "rpart",
                   tuneGrid=data.frame(cp=seq(0,0.1, 
                                              len=25)),
                   control = rpart.control(minsplit = 0))

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
confusionMatrix(predict(train_rpart,x),y)$overall["Accuracy"]

#Use Random Forest
grid <- data.frame(mtry = c(1, 3, 5, 7, 9))
control<-trainControl(method = "cv", number = 5)
fit_rf <- train(x, y,
                  method = "rf",
                  ntree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 700)
ggplot(fit_rf)
fit_rf$bestTune
y_hat_rf <- predict(train_rf,dat)
cm <- confusionMatrix(y_hat_rf,y)
cm$overall["Accuracy"]
cm$byClass[,1:2]
varImp(fit_rf)
