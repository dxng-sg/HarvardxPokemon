if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


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

## The summary statistics for the variables
summary(dat[5:13])

## The total number of Pokemon Types (our classification)
levels(as.factor(dat$`Type 1`))

# To find the distribution of Type 1 pokemon
dat%>%ggplot(aes(`Type 1`))+geom_bar()

# To find the proportion of Pokemons having more than 1 type
mean(is.na(dat$`Type 2`) == FALSE)

# To find the distribution of Type 2 pokemon
dat%>%filter(is.na(`Type 2`) == FALSE) %>%ggplot(aes(`Type 2`))+geom_bar()

# Among the pokemon with 'Flying` as their secondary type, most of them have a Bug or NOrmal as their Type 1
dat%>%filter(`Type 2` == "Flying") %>%ggplot(aes(`Type 1`))+geom_bar()

# Jitterplot reveals several clusters. For example certain Type 1 pokemon (e.g., Bug, ghost, grass, normal) are more likely to have Type 2 characteristic. 
#While some Type 1 pokemon (e.g., Fairy, electric, ice) are less likely to have Type 2 characteristic. 
dat%>%ggplot(aes(x = `Type 1`, y = `Type 2`))+geom_jitter()

# Explore how the various variables are distributed across Type 1 characteristics
dat%>%
  ggplot(aes(y = Total, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = HP, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Attack, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Defense, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = `Sp. Atk`, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = `Sp. Def`, x = `Type 1`))+
  geom_boxplot()

dat%>%
  ggplot(aes(y = Speed, x = `Type 1`))+
  geom_boxplot()

dat%>%ggplot(aes(x = Generation))+geom_bar()+facet_wrap(~`Type 1`)
dat%>%ggplot(aes(x = Legendary))+geom_bar()+facet_wrap(~`Type 1`)

## Correlation between the variables
cor(dat[5:13])

set.seed(2007, )
test_index <- createDataPartition((dat$`Type 1`, times = 1, p = 0.5, list=FALSE))