### Daniel Addokwei Tetteh
### OKPolicy Data Analyst Exercise 
### 02/06/2022



# Importing Relevant Libraries
install.packages("inspectdf")
install.packages("mice")
install.packages("moonBook")
install.packages("CRAN")
install.packages("plotrix")
install.packages("mltools")
install.packages("data.table")
install.packages("ClusterR")
install.packages("cluster")



library(tidyverse)
library(corrr)
library(dplyr)
library(ggplot2)
library(naniar)
library(viridis)
library(forcats)
library(knitr)
library(car)        
library(VIM)
library(MASS)
library(caret)
library(AppliedPredictiveModeling)
library(mlbench)
library(Metrics)
library(Amelia)
library(Hmisc)
library(plyr)
library(scales)
library(devtools)
library(inspectdf)
library(dplyr)
library(mice)
library(hrbrthemes)
library(ggridges)
library(corrgram)
library(moonBook)
library(plotrix)
library(mltools)
library(data.table)
library(ClusterR)
library(cluster)
library(factoextra) # clustering and visualization
library(dplyr)
library(readr)
library(factoextra)


# Importing the data after renaming it to Dataset
Cases_data <- read.csv(file = 'Dataset.csv')
glimpse(Cases_data)
str(Cases_data)
Cases_data$file_year=as.character(Cases_data$file_year)

# Find the unique entries in each column
# Counties of courts

unique(Cases_data$court)
unique(Cases_data$casetype)
unique(Cases_data$file_year)
str(unique(Cases_data$file_date))
str(unique(Cases_data$iss_desc))
str(unique(Cases_data$plaintiff))
str(unique(Cases_data$disp_date))
str(unique(Cases_data$disp_case))
str(unique(Cases_data$close_date))

str(Cases_data)

describe(Cases_data)

###################### EXERCISE 1 ##############################
####################### Visualize Data ##########################

#### Missing Data
gg_miss_var(Cases_data)
missmap(Cases_data, legend = TRUE)
view(miss_var_summary(Cases_data))

# Lets view the unique values in the case description 
unique(Cases_data$iss_desc)
str(unique(Cases_data$iss_desc))

# We can see that we have 4290 different unique values in the iss_desc column, i.e. 
# the description of the count listed on the case.
# Also, some of the rows that have FORCIBLE ENTRY & DETAINER have values attached to them. Eg-
# >k. We will Asumme that these are cases involving amounts k in dollars

#Eviction_cases=Cases_data %>%
  #dplyr::select(contains("iss"))... Selecting columns

# Getting all rows that have the string 'FORCIBLE ENTRY & DETAINER'

# 1-1 Subset of only eviction cases
Eviction_cases=Cases_data %>% filter(grepl('FORCIBLE ENTRY & DETAINER', iss_desc))
View(Eviction_cases)


# 1-2 Line Graph for Eviction cases in Tulsa and Oklahoma 

Tulsa_OK = Eviction_cases[Eviction_cases$court %in% c('TULSA','OKLAHOMA'),]
Tulsa_OK$file_dates= as.Date(Tulsa_OK$file_date,"%m/%d/%Y")
str(Tulsa_OK)
Tulsa_OK$file_month=months(Tulsa_OK$file_dates)
View(Tulsa_OK)

# Before the line graph, lets visualize with a bar graph 
ggplot(data = Tulsa_OK, aes(x = file_month, fill = file_year)) + 
  geom_bar()+ ggtitle("Number of Eviction Cases by Month for Tulsa & Oklahoma")

ggplot(data = Tulsa_OK, aes(x = file_month, fill = disp_case)) + 
  geom_bar() +ggtitle("Disposition on All Eviction Cases by Month for Tulsa & Oklahoma")

#########Good#####################
#ggplot(data = Tulsa_OK, aes(x = file_month, fill = court)) + 
  #geom_bar(stat="count",position="fill")+
  #scale_fill_brewer(palette="Dark2")+
  #scale_y_continuous(labels=scales::percent)+
  #labs(y="Proportion of Cases")+
  #ggtitle("Proportion of Eviction Cases for Tulsa & Oklahoma Compared")
#######################################

ggplot(Tulsa_OK, aes(x = file_month, y= length(unique(file_month)),fill = court),) +
  geom_col(position = "fill")+scale_fill_brewer(palette="Dark2")+
  labs(y="Proportion of Cases")+
  ggtitle("Proportion of Eviction Cases for Tulsa & Oklahoma Compared")

#### Preparing Data for Line Graphs ###########
#Oklahoma
Tulsa_OK$OKJan = nrow(Tulsa_OK[Tulsa_OK$file_month=='January'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKFeb = nrow(Tulsa_OK[Tulsa_OK$file_month=='February'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKMarch = nrow(Tulsa_OK[Tulsa_OK$file_month=='March'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKApr = nrow(Tulsa_OK[Tulsa_OK$file_month=='April'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKMay = nrow(Tulsa_OK[Tulsa_OK$file_month=='May'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKJun = nrow(Tulsa_OK[Tulsa_OK$file_month=='June'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKJul = nrow(Tulsa_OK[Tulsa_OK$file_month=='July'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKAug = nrow(Tulsa_OK[Tulsa_OK$file_month=='August'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKSep = nrow(Tulsa_OK[Tulsa_OK$file_month=='September'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKOct = nrow(Tulsa_OK[Tulsa_OK$file_month=='October'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKNov = nrow(Tulsa_OK[Tulsa_OK$file_month=='November'& Tulsa_OK$court=="OKLAHOMA",])
Tulsa_OK$OKDec = nrow(Tulsa_OK[Tulsa_OK$file_month=='December'& Tulsa_OK$court=="OKLAHOMA",])

#Tulsa
Tulsa_OK$TLJan = nrow(Tulsa_OK[Tulsa_OK$file_month=='January'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLFeb = nrow(Tulsa_OK[Tulsa_OK$file_month=='February'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLMar = nrow(Tulsa_OK[Tulsa_OK$file_month=='March'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLApr = nrow(Tulsa_OK[Tulsa_OK$file_month=='April'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLMay = nrow(Tulsa_OK[Tulsa_OK$file_month=='May'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLJun = nrow(Tulsa_OK[Tulsa_OK$file_month=='June'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLJul = nrow(Tulsa_OK[Tulsa_OK$file_month=='July'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLAug = nrow(Tulsa_OK[Tulsa_OK$file_month=='August'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLSep = nrow(Tulsa_OK[Tulsa_OK$file_month=='September'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLOct = nrow(Tulsa_OK[Tulsa_OK$file_month=='October'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLNov = nrow(Tulsa_OK[Tulsa_OK$file_month=='November'& Tulsa_OK$court=="TULSA",])
Tulsa_OK$TLDec = nrow(Tulsa_OK[Tulsa_OK$file_month=='December'& Tulsa_OK$court=="TULSA",])

# Subset
X<- subset(Tulsa_OK, select = c(OKJan,OKFeb, OKMarch, OKApr, OKMay, OKJun, OKJul, OKAug, OKSep,
                                OKOct, OKNov, OKDec,TLJan, TLFeb, TLMar, TLApr, TLMay,TLJun, TLJul,
                                TLAug, TLSep, TLOct, TLNov, TLDec))

View(X)
X_new= X[1,]
X_new=t(X_new)

class(X_new)
str(X_new)

X_df=as.data.frame(X_new)
View(X_df)
names(X_df)[0] <-'Month'
names(X_df)[1] <-('Number_of_Cases')
X_df <- cbind(Months = rownames(X_df), X_df)
rownames(X_df) <- 1:nrow(X_df)

X_df['Month']=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
X_df['Court_County']=c('OK','OK','OK','OK','OK','OK','OK','OK','OK','OK','OK','OK',
                'TL','TL','TL','TL','TL','TL','TL','TL','TL','TL','TL','TL')
x_df_new=X_df[,2:4]

#Switch Columns
x_df_new <- x_df_new[,c("Court_County", "Month", "Number_of_Cases")]

# Now the Line Graph will be plotted
x_df_new %>%ggplot( aes(x=Month, y=Number_of_Cases, group=Court_County, color=Court_County)) +
  geom_line(aes(colour=Court_County),size=0.8) +
  geom_point(aes(colour=Court_County)) +
  scale_color_manual(values=c('#FFFF00',"#663399")) +
  ggtitle("Number of Evictions filed for Tulsa and Oklahoma County") +
  ylab("Number of Eviction Cases")





########################################################################################
###### EXERCISE 2 ####################################
# 2-1 Exploratory Data Analysis
# Format date and month 

# View Cases 
str(Cases_data)

#Missing Data 
gg_miss_var(Cases_data)
missmap(Cases_data, legend = TRUE)
view(miss_var_summary(Cases_data))

# Create a new dataset for EDA
EDA_Data <-Cases_data

str(EDA_Data)
class(EDA_Data)

EDA_Data=EDA_Data[!is.na(EDA_Data$file_date),] # remove missing date values
str(EDA_Data)

#Lets do some EDA without addressing missing values

# Lets do some treatment to the data first
#Creating columns for file date and file months


EDA_Data$file_dates= as.Date(EDA_Data$file_date,"%m/%d/%Y")
str(EDA_Data)
EDA_Data$file_month=months(EDA_Data$file_dates)
View(EDA_Data)


# Count of cases accross counties for all 6 different years

ggplot(data = EDA_Data) + 
  geom_bar(mapping = aes(y = court, fill = file_year),stat='count')+
ggtitle("Number of Cases in All Counties By Year")+
  ylab("Court County")


# Count of cases based on years

ggplot(data = EDA_Data) + 
  geom_bar(mapping = aes(y = file_year, fill = court),stat='count') 

###########################################################################################

#Pie chart
describe(EDA_Data$file_year)
Case_year= data.frame("year" = c("2016","2017","2018","2019","2020","2021"),
                      "share" = c(65283,62934,65651,63558,40946,1453))
lab=  paste0(round(Case_year$share/sum(Case_year$share) * 100, 2), "%")

pie3D(Case_year$share,
      col=c('#FFFF00', "#CC0033", "#663399","#DD9977", "#00EE00", "#DD9908"),
      labels = paste0((Case_year$year),"\n",lab, sep =""),
      labelcex = 1,
      explode=0.1,
      theta = 0.8,
      main="Cases Filed Cross-county based on years")


################################  SKIP    ##########################################################

#dt=EDA_Data%>%
  #dplyr::group_by(court, file_month)%>%
  #dplyr::tally()%>%
  #dplyr::mutate(percent=n/sum(n))

#p=ggplot(data = dt,aes(x = court, y=n, fill = file_month))
  #p+ geom_bar(stat="identity")+
  #ggtitle("Number of Cases in All Counties By Month")+
  #ylab("Court County") +
  #geom_text(aes(label=paste0(sprintf("%1.1f",percent*100),"%")),
                #position=position_stack(vjust=0.5))

#########################################################################################

#### Count of cases across counties for all months across years
ggplot(data = EDA_Data) + 
  geom_bar(mapping = aes(x = court, fill = file_month),)+
  ggtitle("Number of Cases in All Counties By Month")+
  ylab("Court County") +
  
#pie3D(survey,
      #col=c("steelblue4", "steelblue", "steelblue3", "steelblue2", "steelblue1", "skyblue1"),
      #labels = names(EDA_Data$file_year),
      #labelcex = 1,
      #explode=0.1,
      #theta = 0.8,
      #main="3D Pie Chart")



####### Comparing Cases description across all counties

str(unique(Cases_data$iss_desc))
# There are 4290 unique description of top counts for the iss_desc column, 
# we need a more efficient way to see the common descriptions
# Word_Cloud
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("tm")
library(tm)#Create a vector containing only the text
text <- EDA_Data$text# Create a corpus  
corpus <- Corpus(VectorSource(EDA_Data$iss_desc))
glimpse(corpus)
corpus[1][1]

corpus<-tm_map(corpus, content_transformer(tolower))

corpus<-tm_map(corpus, removeNumbers)

corpus<-tm_map(corpus,removeWords, stopwords("english"))

corpus<-tm_map(corpus,removePunctuation)

corpus<-tm_map(corpus,stripWhitespace)


#Create TDM
dtm <- TermDocumentMatrix(corpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

dev.off()
set.seed(1234) # for reproducibility

wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'),scale=c(2,1.5))

wordcloud2(data=df, size=0.8,color='random-dark')

#Notes
#1. From Wordcloud, Forcibly Entry and Detainer, Indebtedness, Promisory Notes
# were the most common descriptions of top count

### Comparing Indebtedness Cases filed across all Counties
Indebtedness=EDA_Data %>% filter(grepl('INDEBTEDNESS', iss_desc))
View(Indebtedness)


####ADAIR
Indebtedness$ADJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="ADAIR",])
Indebtedness$ADFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="ADAIR",])
Indebtedness$ADMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="ADAIR",])
Indebtedness$ADApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="ADAIR",])
Indebtedness$ADMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="ADAIR",])
Indebtedness$ADJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="ADAIR",])
Indebtedness$ADJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="ADAIR",])
Indebtedness$ADAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="ADAIR",])
Indebtedness$ADSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="ADAIR",])
Indebtedness$ADOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="ADAIR",])
Indebtedness$ADNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="ADAIR",])
Indebtedness$ADDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="ADAIR",])

####CANADIAN
Indebtedness$CDJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDAug = nrow(Indebtedness[Indebtedness$file_month=='Aug'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="CANADIAN",])
Indebtedness$CDDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="CANADIAN",])


###CLEVELAND
Indebtedness$CLVJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVApr = nrow(Indebtedness[Indebtedness$file_month=='Apr'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLVDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="CLEVELAND",])

###COMANCHE
Indebtedness$COMJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="COMANCHE",])
Indebtedness$COMDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="COMANCHE",])


###TULSA
Indebtedness$TULJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="TULSA",])
Indebtedness$TULFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="TULSA",])
Indebtedness$TULMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="TULSA",])
Indebtedness$TULApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="TULSA",])
Indebtedness$TULMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="TULSA",])
Indebtedness$TULJun = nrow(Indebtedness[Indebtedness$file_month=='June' & Indebtedness$court=="TULSA",])
Indebtedness$TULJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="TULSA",])
Indebtedness$TULAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="TULSA",])
Indebtedness$TULSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="TULSA",])
Indebtedness$TULOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="TULSA",])
Indebtedness$TULNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="TULSA",])
Indebtedness$TULDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="TULSA",])


###ELLIS
Indebtedness$ELJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="ELLIS",])
Indebtedness$ELFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="ELLIS",])
Indebtedness$ELMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="ELLIS",])
Indebtedness$ELApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="ELLIS",])
Indebtedness$ELMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="ELLIS",])
Indebtedness$ELJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="ELLIS",])
Indebtedness$ELJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="ELLIS",])
Indebtedness$ELAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="ELLIS",])
Indebtedness$ELSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="ELLIS",])
Indebtedness$ELOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="ELLIS",])
Indebtedness$ELNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="ELLIS",])
Indebtedness$ELDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="ELLIS",])

###GARFIELD
Indebtedness$GARJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="GARFIELD",])
Indebtedness$GAROct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="GARFIELD",])
Indebtedness$GARDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="GARFIELD",])

### LOGAN 
Indebtedness$lOGJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="LOGAN",])
Indebtedness$lOGDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="LOGAN",])


###ROGERS
Indebtedness$ROGJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="ROGERS",])
Indebtedness$ROGDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="ROGERS",])


###ROGERMILLS
Indebtedness$RGMJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGMDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="ROGERMILLS",])


###PUSHMATAHA
Indebtedness$PUSJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUSDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="PUSHMATAHA",])



###PAYNE
Indebtedness$PAYJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="PAYNE",])
Indebtedness$PAYDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="PAYNE",])


###OKLAHOMA
Indebtedness$OKLJan = nrow(Indebtedness[Indebtedness$file_month=='January'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLFeb = nrow(Indebtedness[Indebtedness$file_month=='February'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLMar = nrow(Indebtedness[Indebtedness$file_month=='March'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLApr = nrow(Indebtedness[Indebtedness$file_month=='April'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLMay = nrow(Indebtedness[Indebtedness$file_month=='May'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLJun = nrow(Indebtedness[Indebtedness$file_month=='June'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLJul = nrow(Indebtedness[Indebtedness$file_month=='July'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLAug = nrow(Indebtedness[Indebtedness$file_month=='August'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLSep = nrow(Indebtedness[Indebtedness$file_month=='September'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLOct = nrow(Indebtedness[Indebtedness$file_month=='October'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLNov = nrow(Indebtedness[Indebtedness$file_month=='November'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OKLDec = nrow(Indebtedness[Indebtedness$file_month=='December'& Indebtedness$court=="OKLAHOMA",])


# Subset
Y<- subset(Indebtedness, select = c(ADJan,ADFeb, ADMar, ADApr, ADMay, ADJun, ADJul, ADAug, ADSep,
                                    ADOct, ADNov, ADDec,CDJan, CDFeb, CDMar, CDApr, CDMay,CDJun, CDJul,
                                    CDAug, CDSep, CDOct, CDNov, CDDec,CLVJan, CLVFeb, CLVMar, CLVApr, CLVMay,CLVJun, CLVJul,
                                    CLVAug, CLVSep, CLVOct, CLVNov, CLVDec,COMJan, COMFeb, COMMar, COMApr, COMMay,COMJun, COMJul,
                                    COMAug, COMSep, COMOct, COMNov, COMDec,TULJan, TULFeb, TULMar, TULApr, TULMay,TULJun, TULJul,
                                    TULAug, TULSep, TULOct, TULNov, TULDec,ELJan, ELFeb, ELMar, ELApr, ELMay,ELJun, ELJul,
                                    ELAug, ELSep, ELOct, ELNov, ELDec,GARJan, GARFeb, GARMar, GARApr, GARMay,GARJun, GARJul,
                                    GARAug, GARSep, GAROct, GARNov, GARDec,lOGJan, lOGFeb, lOGMar, lOGApr, lOGMay,lOGJun, lOGJul,
                                    lOGAug, lOGSep, lOGOct, lOGNov, lOGDec,ROGJan, ROGFeb, ROGMar, ROGApr, ROGMay,ROGJun, ROGJul,
                                    ROGAug, ROGSep, ROGOct, ROGNov, ROGDec,RGMJan, RGMFeb, RGMMar, RGMApr, RGMMay,RGMJun, RGMJul,
                                    RGMAug, RGMSep, RGMOct, RGMNov, RGMDec,PUSJan, PUSFeb, PUSMar, PUSApr, PUSMay,PUSJun, PUSJul,
                                    PUSAug, PUSSep, PUSOct, PUSNov, PUSDec,PAYJan, PAYFeb, PAYMar, PAYApr, PAYMay,PAYJun, PAYJul,
                                    PAYAug, PAYSep, PAYOct, PAYNov, PAYDec,OKLJan, OKLFeb, OKLMar, OKLApr, OKLMay,OKLJun, OKLJul,
                                    OKLAug, OKLSep, OKLOct, OKLNov, OKLDec))

Y_new= Y[1,]
Y_new=t(Y_new)
View(Y_new)


Y_df=as.data.frame(Y_new)
View(Y_df)
names(Y_df)[0] <-'Month'
names(Y_df)[1] <-('Number_of_Cases')
Y_df <- cbind(Months = rownames(Y_df), Y_df)
rownames(Y_df) <- 1:nrow(Y_df)

Y_df['Month']=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
Y_df['Court_County']=c('ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR',
                       'CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN',
                       'CANADIAN','CANADIAN','CANADIAN','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND',
                       'CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','COMANCHE','COMANCHE','COMANCHE',
                       'COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE',
                       'TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA',
                       'ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS',
                       'GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD',
                       'GARFIELD','GARFIELD','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN',
                       'LOGAN','LOGAN','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS',
                       'ROGERS','ROGERS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS',
                       'ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA',
                       'PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PAYNE',
                       'PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','OKLAHOMA','OKLAHOMA','OKLAHOMA',
                       'OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA')
View(Y_df)
Y_df_new=Y_df[,2:4]

#Switch Columns
Y_df_new <- Y_df_new[,c("Court_County", "Month", "Number_of_Cases")]

view(Y_df_new)


Y_df_new %>%ggplot( aes(x=Month, y=Number_of_Cases, group=Court_County, color=Court_County)) +
  geom_line(aes(colour=Court_County),size=0.8) +
  geom_point(aes(colour=Court_County))+
  scale_color_manual(values=c('#FFFF00',"#663399","#DD9977",
                       "#CC0033","Red","#00EE00",
                       "#FF0000","#AA0068","#000005",
                       "#888888","#DD9908",
                       "#FF9999","#00AA00")) +
  ggtitle("Number of Files Indebtedness Cases filed for All County Courts by Month") +
  ylab("Number of Indebtedness Cases")


#### Years with the highest number of cases
ggplot(EDA_Data = EDA_Data, aes(x = file_year, fill = file_year)) + 
  geom_bar()


 
#### Basics Statistics of Interesting Years
# We will look at the statistics of Indebtedness and Eviction Cases for 2016, 2020 and 2021
# Basically how covid affected people and how they got in debt

######################## INDEBTEDNESS ##########################################

####ADAIR
Indebtedness$AD2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="ADAIR",])
Indebtedness$AD2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="ADAIR",])
Indebtedness$AD2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="ADAIR",])

####CANADIAN
Indebtedness$CAN2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="CANADIAN",])
Indebtedness$CAN2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="CANADIAN",])
Indebtedness$CAN2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="CANADIAN",])

####CLEVELAND
Indebtedness$CLV2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLV2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="CLEVELAND",])
Indebtedness$CLV2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="CLEVELAND",])


####COMANCHE
Indebtedness$COM2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="COMANCHE",])
Indebtedness$COM2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="COMANCHE",])
Indebtedness$COM2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="COMANCHE",])


####ELLIS
Indebtedness$EL2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="ELLIS",])
Indebtedness$EL2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="ELLIS",])
Indebtedness$EL2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="ELLIS",])

####GARFIELD
Indebtedness$GAR2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="GARFIELD",])
Indebtedness$GAR2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="GARFIELD",])
Indebtedness$GAR2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="GARFIELD",])

####LOGAN
Indebtedness$LOG2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="LOGAN",])
Indebtedness$LOG2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="LOGAN",])
Indebtedness$LOG2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="LOGAN",])

####OKLAHOMA
Indebtedness$OK2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OK2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="OKLAHOMA",])
Indebtedness$OK2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="OKLAHOMA",])


####PAYNE
Indebtedness$PAY2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="PAYNE",])
Indebtedness$PAY2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="PAYNE",])
Indebtedness$PAY2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="PAYNE",])

####PUSHMATAHA
Indebtedness$PUS2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUS2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="PUSHMATAHA",])
Indebtedness$PUS2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="PUSHMATAHA",])

####ROGERMILLS
Indebtedness$RGM2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGM2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="ROGERMILLS",])
Indebtedness$RGM2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="ROGERMILLS",])


####ROGERS
Indebtedness$ROG2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="ROGERS",])
Indebtedness$ROG2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="ROGERS",])
Indebtedness$ROG2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="ROGERS",])


####TULSA
Indebtedness$TUL2018 = nrow(Indebtedness[Indebtedness$file_year=='2018'& Indebtedness$court=="TULSA",])
Indebtedness$TUL2020 = nrow(Indebtedness[Indebtedness$file_year=='2020'& Indebtedness$court=="TULSA",])
Indebtedness$TUL2021 = nrow(Indebtedness[Indebtedness$file_year=='2021'& Indebtedness$court=="TULSA",])
View(Indebtedness)

#sUBSET

Y_years<- subset(Indebtedness, select = c(AD2018,AD2020,AD2021,CAN2018,CAN2020,CAN2021,CLV2018,CLV2020,CLV2021,
                                          COM2018, COM2020,COM2021,TUL2018,TUL2020,TUL2021,
                                          EL2018,EL2020,EL2021,GAR2018,GAR2020,GAR2021,LOG2018,LOG2020,LOG2021,
                                          ROG2018,ROG2020,ROG2021,
                                          RGM2018,RGM2020,RGM2021,
                                          PUS2018,PUS2020,PUS2021,PAY2018,PAY2020,PAY2021,OK2018, OK2020,OK2021))

Y_years_new= Y_years[1,]
View(Y_years_new)
Y_years_new=t(Y_years_new)
View(Y_years_new)


Y_years_df=as.data.frame(Y_years_new)
View(Y_years_df)
names(Y_years_df)[0] <-'Years'
names(Y_years_df)[1] <-('Number_of_Cases')
Y_years_df <- cbind(Years = rownames(Y_years_df), Y_years_df)
rownames(Y_years_df) <- 1:nrow(Y_years_df)

View(Y_years_df)

Y_years_df_1=Y_years_df

Y_years_df_1['Years']=c('2018','2020','2021')
View(Y_years_df_1)
Y_years_df_1['Court_County']=c('ADAIR','ADAIR','ADAIR','CANADIAN','CANADIAN','CANADIAN','CLEVELAND','CLEVELAND','CLEVELAND','COMANCHE','COMANCHE','COMANCHE',
                       'TULSA','TULSA','TULSA','ELLIS','ELLIS','ELLIS','GARFIELD','GARFIELD','GARFIELD','LOGAN','LOGAN','LOGAN',
                       'ROGERS','ROGERS','ROGERS','ROGERMILLS','ROGERMILLS','ROGERMILLS','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PAYNE',
                       'PAYNE','PAYNE','OKLAHOMA','OKLAHOMA','OKLAHOMA')


###Statistical Analysis of indebtedness
County_Court <- c("ADAIR", "CANADIAN","CLEVELAND","COMANCHE","TULSA",'ELLIS',"GARFIELD","LOGAN",'ROGERS',
                  "ROGERMILLS","PUSHMATAHA","PAYNE","OKLAHOMA")

Year_2018 <- c(401,1382,3128,1586,3504,0,908,235,791,8,55,938,11340)
Year_2020 <- c(294,754,1786,593,1428,3,418,136,492,1,29,471,8075)
Year_2021 <- c(26,51,83,0,20,0,18,12,0,0,2,16,315)

df_stats <- data.frame(County_Court, Year_2018,Year_2020,Year_2021)

View(df_stats)


####################################################################################


###Violin Plot

ggplot(Y_years_df_1, aes(x = Years, y = Number_of_Cases, fill = Years)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred",width=0.8) + geom_boxplot(width=0.1) +
  stat_summary(fun=median, geom="point", size=2, color="red")+
labs(title="Violin/Box plot",
     subtitle="Distribution of Number of Indebtedness Cases for all Counties",
     caption="Source: OK Justice Data",
     x="Years",
     y="Number of Filed Cases")


#Grouped Bar Barplot

matrix_df=data.matrix(df_stats)
matrix_df_new = matrix_df[,-1]
matrix_df_new

dev.off()
par(mar = c(5, 5, 4, 12))

barplot(matrix_df_new, xlab = "Years",ylab="Number of Cases",
        main = "Filed Indebtedness Cases for Selected Years (Impacts of Covid-19 Pandemic)",
        col = rainbow(13),
        beside = TRUE,
        legend.text = (df_stats$County_Court),
        args.legend = list(title = "Counties", x = "topright",
                           inset = c(-0.20, 0)))
###################################################################################################################3


############################ EVICTIONS ##########################################################

Evictions=EDA_Data %>% filter(grepl('FORCIBLE ENTRY & DETAINER', iss_desc))
View(Evictions)
####### Line Graph ############################################################

####ADAIR
Evictions$ADJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="ADAIR",])
Evictions$ADFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="ADAIR",])
Evictions$ADMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="ADAIR",])
Evictions$ADApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="ADAIR",])
Evictions$ADMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="ADAIR",])
Evictions$ADJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="ADAIR",])
Evictions$ADJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="ADAIR",])
Evictions$ADAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="ADAIR",])
Evictions$ADSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="ADAIR",])
Evictions$ADOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="ADAIR",])
Evictions$ADNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="ADAIR",])
Evictions$ADDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="ADAIR",])

####CANADIAN
Evictions$CDJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="CANADIAN",])
Evictions$CDFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="CANADIAN",])
Evictions$CDMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="CANADIAN",])
Evictions$CDApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="CANADIAN",])
Evictions$CDMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="CANADIAN",])
Evictions$CDJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="CANADIAN",])
Evictions$CDJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="CANADIAN",])
Evictions$CDAug = nrow(Evictions[Evictions$file_month=='Aug'& Evictions$court=="CANADIAN",])
Evictions$CDSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="CANADIAN",])
Evictions$CDOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="CANADIAN",])
Evictions$CDNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="CANADIAN",])
Evictions$CDDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="CANADIAN",])


###CLEVELAND
Evictions$CLVJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="CLEVELAND",])
Evictions$CLVFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="CLEVELAND",])
Evictions$CLVMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="CLEVELAND",])
Evictions$CLVApr = nrow(Evictions[Evictions$file_month=='Apr'& Evictions$court=="CLEVELAND",])
Evictions$CLVMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="CLEVELAND",])
Evictions$CLVJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="CLEVELAND",])
Evictions$CLVJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="CLEVELAND",])
Evictions$CLVAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="CLEVELAND",])
Evictions$CLVSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="CLEVELAND",])
Evictions$CLVOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="CLEVELAND",])
Evictions$CLVNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="CLEVELAND",])
Evictions$CLVDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="CLEVELAND",])

###COMANCHE
Evictions$COMJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="COMANCHE",])
Evictions$COMFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="COMANCHE",])
Evictions$COMMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="COMANCHE",])
Evictions$COMApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="COMANCHE",])
Evictions$COMMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="COMANCHE",])
Evictions$COMJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="COMANCHE",])
Evictions$COMJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="COMANCHE",])
Evictions$COMAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="COMANCHE",])
Evictions$COMSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="COMANCHE",])
Evictions$COMOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="COMANCHE",])
Evictions$COMNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="COMANCHE",])
Evictions$COMDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="COMANCHE",])


###TULSA
Evictions$TULJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="TULSA",])
Evictions$TULFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="TULSA",])
Evictions$TULMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="TULSA",])
Evictions$TULApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="TULSA",])
Evictions$TULMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="TULSA",])
Evictions$TULJun = nrow(Evictions[Evictions$file_month=='June' & Evictions$court=="TULSA",])
Evictions$TULJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="TULSA",])
Evictions$TULAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="TULSA",])
Evictions$TULSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="TULSA",])
Evictions$TULOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="TULSA",])
Evictions$TULNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="TULSA",])
Evictions$TULDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="TULSA",])


###ELLIS
Evictions$ELJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="ELLIS",])
Evictions$ELFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="ELLIS",])
Evictions$ELMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="ELLIS",])
Evictions$ELApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="ELLIS",])
Evictions$ELMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="ELLIS",])
Evictions$ELJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="ELLIS",])
Evictions$ELJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="ELLIS",])
Evictions$ELAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="ELLIS",])
Evictions$ELSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="ELLIS",])
Evictions$ELOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="ELLIS",])
Evictions$ELNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="ELLIS",])
Evictions$ELDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="ELLIS",])

###GARFIELD
Evictions$GARJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="GARFIELD",])
Evictions$GARFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="GARFIELD",])
Evictions$GARMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="GARFIELD",])
Evictions$GARApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="GARFIELD",])
Evictions$GARMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="GARFIELD",])
Evictions$GARJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="GARFIELD",])
Evictions$GARJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="GARFIELD",])
Evictions$GARAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="GARFIELD",])
Evictions$GARSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="GARFIELD",])
Evictions$GAROct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="GARFIELD",])
Evictions$GARNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="GARFIELD",])
Evictions$GARDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="GARFIELD",])

### LOGAN 
Evictions$lOGJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="LOGAN",])
Evictions$lOGFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="LOGAN",])
Evictions$lOGMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="LOGAN",])
Evictions$lOGApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="LOGAN",])
Evictions$lOGMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="LOGAN",])
Evictions$lOGJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="LOGAN",])
Evictions$lOGJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="LOGAN",])
Evictions$lOGAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="LOGAN",])
Evictions$lOGSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="LOGAN",])
Evictions$lOGOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="LOGAN",])
Evictions$lOGNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="LOGAN",])
Evictions$lOGDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="LOGAN",])


###ROGERS
Evictions$ROGJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="ROGERS",])
Evictions$ROGFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="ROGERS",])
Evictions$ROGMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="ROGERS",])
Evictions$ROGApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="ROGERS",])
Evictions$ROGMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="ROGERS",])
Evictions$ROGJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="ROGERS",])
Evictions$ROGJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="ROGERS",])
Evictions$ROGAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="ROGERS",])
Evictions$ROGSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="ROGERS",])
Evictions$ROGOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="ROGERS",])
Evictions$ROGNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="ROGERS",])
Evictions$ROGDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="ROGERS",])


###ROGERMILLS
Evictions$RGMJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="ROGERMILLS",])
Evictions$RGMFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="ROGERMILLS",])
Evictions$RGMMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="ROGERMILLS",])
Evictions$RGMApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="ROGERMILLS",])
Evictions$RGMMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="ROGERMILLS",])
Evictions$RGMJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="ROGERMILLS",])
Evictions$RGMJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="ROGERMILLS",])
Evictions$RGMAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="ROGERMILLS",])
Evictions$RGMSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="ROGERMILLS",])
Evictions$RGMOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="ROGERMILLS",])
Evictions$RGMNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="ROGERMILLS",])
Evictions$RGMDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="ROGERMILLS",])


###PUSHMATAHA
Evictions$PUSJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="PUSHMATAHA",])
Evictions$PUSDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="PUSHMATAHA",])



###PAYNE
Evictions$PAYJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="PAYNE",])
Evictions$PAYFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="PAYNE",])
Evictions$PAYMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="PAYNE",])
Evictions$PAYApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="PAYNE",])
Evictions$PAYMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="PAYNE",])
Evictions$PAYJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="PAYNE",])
Evictions$PAYJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="PAYNE",])
Evictions$PAYAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="PAYNE",])
Evictions$PAYSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="PAYNE",])
Evictions$PAYOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="PAYNE",])
Evictions$PAYNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="PAYNE",])
Evictions$PAYDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="PAYNE",])


###OKLAHOMA
Evictions$OKLJan = nrow(Evictions[Evictions$file_month=='January'& Evictions$court=="OKLAHOMA",])
Evictions$OKLFeb = nrow(Evictions[Evictions$file_month=='February'& Evictions$court=="OKLAHOMA",])
Evictions$OKLMar = nrow(Evictions[Evictions$file_month=='March'& Evictions$court=="OKLAHOMA",])
Evictions$OKLApr = nrow(Evictions[Evictions$file_month=='April'& Evictions$court=="OKLAHOMA",])
Evictions$OKLMay = nrow(Evictions[Evictions$file_month=='May'& Evictions$court=="OKLAHOMA",])
Evictions$OKLJun = nrow(Evictions[Evictions$file_month=='June'& Evictions$court=="OKLAHOMA",])
Evictions$OKLJul = nrow(Evictions[Evictions$file_month=='July'& Evictions$court=="OKLAHOMA",])
Evictions$OKLAug = nrow(Evictions[Evictions$file_month=='August'& Evictions$court=="OKLAHOMA",])
Evictions$OKLSep = nrow(Evictions[Evictions$file_month=='September'& Evictions$court=="OKLAHOMA",])
Evictions$OKLOct = nrow(Evictions[Evictions$file_month=='October'& Evictions$court=="OKLAHOMA",])
Evictions$OKLNov = nrow(Evictions[Evictions$file_month=='November'& Evictions$court=="OKLAHOMA",])
Evictions$OKLDec = nrow(Evictions[Evictions$file_month=='December'& Evictions$court=="OKLAHOMA",])



# Subset
Z<- subset(Evictions, select = c(ADJan,ADFeb, ADMar, ADApr, ADMay, ADJun, ADJul, ADAug, ADSep,
                                    ADOct, ADNov, ADDec,CDJan, CDFeb, CDMar, CDApr, CDMay,CDJun, CDJul,
                                    CDAug, CDSep, CDOct, CDNov, CDDec,CLVJan, CLVFeb, CLVMar, CLVApr, CLVMay,CLVJun, CLVJul,
                                    CLVAug, CLVSep, CLVOct, CLVNov, CLVDec,COMJan, COMFeb, COMMar, COMApr, COMMay,COMJun, COMJul,
                                    COMAug, COMSep, COMOct, COMNov, COMDec,TULJan, TULFeb, TULMar, TULApr, TULMay,TULJun, TULJul,
                                    TULAug, TULSep, TULOct, TULNov, TULDec,ELJan, ELFeb, ELMar, ELApr, ELMay,ELJun, ELJul,
                                    ELAug, ELSep, ELOct, ELNov, ELDec,GARJan, GARFeb, GARMar, GARApr, GARMay,GARJun, GARJul,
                                    GARAug, GARSep, GAROct, GARNov, GARDec,lOGJan, lOGFeb, lOGMar, lOGApr, lOGMay,lOGJun, lOGJul,
                                    lOGAug, lOGSep, lOGOct, lOGNov, lOGDec,ROGJan, ROGFeb, ROGMar, ROGApr, ROGMay,ROGJun, ROGJul,
                                    ROGAug, ROGSep, ROGOct, ROGNov, ROGDec,RGMJan, RGMFeb, RGMMar, RGMApr, RGMMay,RGMJun, RGMJul,
                                    RGMAug, RGMSep, RGMOct, RGMNov, RGMDec,PUSJan, PUSFeb, PUSMar, PUSApr, PUSMay,PUSJun, PUSJul,
                                    PUSAug, PUSSep, PUSOct, PUSNov, PUSDec,PAYJan, PAYFeb, PAYMar, PAYApr, PAYMay,PAYJun, PAYJul,
                                    PAYAug, PAYSep, PAYOct, PAYNov, PAYDec,OKLJan, OKLFeb, OKLMar, OKLApr, OKLMay,OKLJun, OKLJul,
                                    OKLAug, OKLSep, OKLOct, OKLNov, OKLDec))

Z_new= Z[1,]
Z_new=t(Z_new)
View(Z_new)


Z_df=as.data.frame(Z_new)
View(Z_df)
names(Z_df)[0] <-'Month'
names(Z_df)[1] <-('Number_of_Cases')
Z_df <- cbind(Months = rownames(Z_df), Z_df)
rownames(Z_df) <- 1:nrow(Z_df)

Z_df['Month']=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
Z_df['Court_County']=c('ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR','ADAIR',
                       'CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN','CANADIAN',
                       'CANADIAN','CANADIAN','CANADIAN','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND',
                       'CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','CLEVELAND','COMANCHE','COMANCHE','COMANCHE',
                       'COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE','COMANCHE',
                       'TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA','TULSA',
                       'ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS','ELLIS',
                       'GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD','GARFIELD',
                       'GARFIELD','GARFIELD','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN','LOGAN',
                       'LOGAN','LOGAN','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS','ROGERS',
                       'ROGERS','ROGERS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS',
                       'ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','ROGERMILLS','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA',
                       'PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PAYNE',
                       'PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','PAYNE','OKLAHOMA','OKLAHOMA','OKLAHOMA',
                       'OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA','OKLAHOMA')
View(Z_df)
Z_df_new=Z_df[,2:4]

#Switch Columns
Z_df_new <- Z_df_new[,c("Court_County", "Month", "Number_of_Cases")]

view(Z_df_new)


Z_df_new %>%ggplot( aes(x=Month, y=Number_of_Cases, group=Court_County, color=Court_County)) +
  geom_line(aes(colour=Court_County),size=0.8) +
  geom_point(aes(colour=Court_County))+
  scale_color_manual(values=c('#FFFF00',"#663399","#DD9977",
                                       "#CC0033","Red","#00EE00",
                                       "#FF0000","#AA0068","#000005",
                                       "#888888","#DD9908",
                                       "#FF9999","#00AA00")) +
                                         ggtitle("Number of Eviction Cases filed for All County Courts by Month") +
  ylab("Number of Eviction Cases")



############################## STATISTICS ############################################################

Evictions$AD2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="ADAIR",])
Evictions$AD2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="ADAIR",])
Evictions$AD2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="ADAIR",])

####CANADIAN
Evictions$CAN2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="CANADIAN",])
Evictions$CAN2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="CANADIAN",])
Evictions$CAN2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="CANADIAN",])

####CLEVELAND
Evictions$CLV2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="CLEVELAND",])
Evictions$CLV2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="CLEVELAND",])
Evictions$CLV2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="CLEVELAND",])


####COMANCHE
Evictions$COM2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="COMANCHE",])
Evictions$COM2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="COMANCHE",])
Evictions$COM2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="COMANCHE",])


####ELLIS
Evictions$EL2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="ELLIS",])
Evictions$EL2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="ELLIS",])
Evictions$EL2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="ELLIS",])

####GARFIELD
Evictions$GAR2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="GARFIELD",])
Evictions$GAR2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="GARFIELD",])
Evictions$GAR2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="GARFIELD",])

####LOGAN
Evictions$LOG2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="LOGAN",])
Evictions$LOG2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="LOGAN",])
Evictions$LOG2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="LOGAN",])

####OKLAHOMA
Evictions$OK2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="OKLAHOMA",])
Evictions$OK2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="OKLAHOMA",])
Evictions$OK2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="OKLAHOMA",])


####PAYNE
Evictions$PAY2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="PAYNE",])
Evictions$PAY2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="PAYNE",])
Evictions$PAY2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="PAYNE",])

####PUSHMATAHA
Evictions$PUS2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="PUSHMATAHA",])
Evictions$PUS2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="PUSHMATAHA",])
Evictions$PUS2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="PUSHMATAHA",])

####ROGERMILLS
Evictions$RGM2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="ROGERMILLS",])
Evictions$RGM2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="ROGERMILLS",])
Evictions$RGM2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="ROGERMILLS",])


####ROGERS
Evictions$ROG2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="ROGERS",])
Evictions$ROG2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="ROGERS",])
Evictions$ROG2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="ROGERS",])


####TULSA
Evictions$TUL2018 = nrow(Evictions[Evictions$file_year=='2018'& Evictions$court=="TULSA",])
Evictions$TUL2020 = nrow(Evictions[Evictions$file_year=='2020'& Evictions$court=="TULSA",])
Evictions$TUL2021 = nrow(Evictions[Evictions$file_year=='2021'& Evictions$court=="TULSA",])
View(Evictions)

#############################################################################################################

#sUBSET

T_years<- subset(Evictions, select = c(AD2018,AD2020,AD2021,CAN2018,CAN2020,CAN2021,CLV2018,CLV2020,CLV2021,
                                          COM2018, COM2020,COM2021,TUL2018,TUL2020,TUL2021,
                                          EL2018,EL2020,EL2021,GAR2018,GAR2020,GAR2021,LOG2018,LOG2020,LOG2021,
                                          ROG2018,ROG2020,ROG2021,
                                          RGM2018,RGM2020,RGM2021,
                                          PUS2018,PUS2020,PUS2021,PAY2018,PAY2020,PAY2021,OK2018, OK2020,OK2021))

T_years= T_years[1,]
View(T_years)
T_years_new=t(T_years)
View(T_years_new)


T_years_df=as.data.frame(T_years_new)
View(T_years_df)
names(T_years_df)[0] <-'Years'
names(T_years_df)[1] <-('Number_of_Cases')
T_years_df <- cbind(Years = rownames(T_years_df), T_years_df)
rownames(T_years_df) <- 1:nrow(T_years_df)

View(T_years_df)

T_years_df_1=T_years_df

T_years_df_1['Years']=c('2018','2020','2021')
View(T_years_df_1)
T_years_df_1['Court_County']=c('ADAIR','ADAIR','ADAIR','CANADIAN','CANADIAN','CANADIAN','CLEVELAND','CLEVELAND','CLEVELAND','COMANCHE','COMANCHE','COMANCHE',
                               'TULSA','TULSA','TULSA','ELLIS','ELLIS','ELLIS','GARFIELD','GARFIELD','GARFIELD','LOGAN','LOGAN','LOGAN',
                               'ROGERS','ROGERS','ROGERS','ROGERMILLS','ROGERMILLS','ROGERMILLS','PUSHMATAHA','PUSHMATAHA','PUSHMATAHA','PAYNE',
                               'PAYNE','PAYNE','OKLAHOMA','OKLAHOMA','OKLAHOMA')


###Statistical Analysis of Evictions
County_Court <- c("ADAIR", "CANADIAN","CLEVELAND","COMANCHE","TULSA",'ELLIS',"GARFIELD","LOGAN",'ROGERS',
                  "ROGERMILLS","PUSHMATAHA","PAYNE","OKLAHOMA")

Year_2018 <- c(127,695,3530,2150,14808,0,415,228,381,8,29,476,14396)
Year_2020 <- c(54,549,2258,1324,8400,5,305,153,224,2,26,473,10208)
Year_2021 <- c(2,33,86,0,302,0,11,4,0,0,1,34,368)

df_statss <- data.frame(County_Court, Year_2018,Year_2020,Year_2021)

View(df_statss)

####################################################################################

####################################################################################


###Violin Plot

ggplot(T_years_df_1, aes(x = Years, y = Number_of_Cases, fill = Years)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred",width=0.8) + geom_boxplot(width=0.1) +
  stat_summary(fun=median, geom="point", size=2, color="red")+
  labs(title="Violin/Box plot",
       subtitle="Distribution of Number of Evictions Cases for all Counties",
       caption="Source: OK Justice Data",
       x="Years",
       y="Number of Filed Cases")


############Grouped Bar Plot

matrix_dfs=data.matrix(df_statss)
matrix_df_news = matrix_dfs[,-1]
matrix_df_news

dev.off()
par(mar = c(5, 5, 4, 12))

barplot(matrix_df_news, xlab = "Years",ylab="Number of cases",
        main = "Filed Eviction Cases for Selected Years (Impacts of Covid-19 Pandemic)",
        col = rainbow(13),
        beside = TRUE,
        legend.text = (df_stats$County_Court),
        args.legend = list(title = "Counties", x = "topright",
                           inset = c(-0.20, 0)))



######################## DISPOSITION ON FILED CASES ###########################
# Lets take a look at the last dispositions on the cases filed

str(unique(Cases_data$disp_case))
# There are 38 different disposition descriptions reported
# We will use word cloud to look at the most prevalent across all county courts


text <- EDA_Data$text# Create a corpus  
corpus <- Corpus(VectorSource(EDA_Data$disp_case))
glimpse(corpus)
corpus[1][1]

corpus<-tm_map(corpus, content_transformer(tolower))

corpus<-tm_map(corpus, removeNumbers)

corpus<-tm_map(corpus,removeWords, stopwords("english"))

corpus<-tm_map(corpus,removePunctuation)

corpus<-tm_map(corpus,stripWhitespace)


#Create TDM
dtm <- TermDocumentMatrix(corpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

dev.off()
set.seed(1234) # for reproducibility

wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'),scale=c(2,1.5))

wordcloud2(data=df, size=0.8,color='random-dark')

#############################################################################################################
#Notes
#1. From Wordcloud, Forcibly Entry and Detainer, Indebtedness, Promisory Notes
# were the most common descriptions of top count

#"Default judgement" and "dismissed without prejudice" seem prevalent
# Lets take a look at default judgement for all eviction cases for Tulsa and Oklahoma

Default_cases=EDA_Data %>% filter(grepl('DEFAULT', disp_case))
View(Default_cases)

Tulsa_OK_D = Default_cases[Default_cases$court %in% c('TULSA','OKLAHOMA'),]
View(Tulsa_OK_D)


#Oklahoma
Tulsa_OK_D$OKJan = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='January'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKFeb = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='February'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKMarch = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='March'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKApr = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='April'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKMay = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='May'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKJun = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='June'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKJul = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='July'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKAug = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='August'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKSep = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='September'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKOct = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='October'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKNov = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='November'& Tulsa_OK_D$court=="OKLAHOMA",])
Tulsa_OK_D$OKDec = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='December'& Tulsa_OK_D$court=="OKLAHOMA",])

#Tulsa
Tulsa_OK_D$TLJan = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='January'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLFeb = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='February'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLMar = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='March'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLApr = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='April'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLMay = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='May'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLJun = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='June'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLJul = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='July'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLAug = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='August'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLSep = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='September'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLOct = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='October'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLNov = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='November'& Tulsa_OK_D$court=="TULSA",])
Tulsa_OK_D$TLDec = nrow(Tulsa_OK_D[Tulsa_OK_D$file_month=='December'& Tulsa_OK_D$court=="TULSA",])

# Subset
A<- subset(Tulsa_OK_D, select = c(OKJan,OKFeb, OKMarch, OKApr, OKMay, OKJun, OKJul, OKAug, OKSep,
                                OKOct, OKNov, OKDec,TLJan, TLFeb, TLMar, TLApr, TLMay,TLJun, TLJul,
                                TLAug, TLSep, TLOct, TLNov, TLDec))

View(A)
A_new= A[1,]
A_new=t(A_new)

class(A_new)
str(A_new)

A_df=as.data.frame(A_new)
View(A_df)
names(A_df)[0] <-'Month'
names(A_df)[1] <-('Number_of_Cases')
A_df <- cbind(Months = rownames(A_df), A_df)
rownames(A_df) <- 1:nrow(A_df)

A_df['Month']=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
A_df['Court_County']=c('OK','OK','OK','OK','OK','OK','OK','OK','OK','OK','OK','OK',
                       'TL','TL','TL','TL','TL','TL','TL','TL','TL','TL','TL','TL')
A_df_new=A_df[,2:4]

#Switch Columns
A_df_new <- A_df_new[,c("Court_County", "Month", "Number_of_Cases")]

# Now the Line Graph will be plotted
A_df_new %>%ggplot( aes(x=Month, y=Number_of_Cases, group=Court_County, color=Court_County)) +
  geom_line(aes(colour=Court_County), size=0.8) +
  geom_point(aes(colour=Court_County))+
  scale_color_manual(values=c("#00EE00","#DD9977")) +
  ggtitle("Number of Default Disposition Cases for Tulsa and Oklahoma County") +
  ylab("Number of Defaulted Cases")

###########################################################################################
######################### Lets Look at the Plaintiff #################################

EDA_Data_new=EDA_Data[!is.na(EDA_Data$plaintiff),]

EDA_Data_new$plaintiff
str(unique(Cases_data$plaintiff))
# There are 18824 different Plaintiffs
# We will use word cloud to look at the most prevalent across all county courts

# We will look at names of plaintiffs from only Oklahoma County

EDA_Data_new = EDA_Data_new[EDA_Data_new$court %in% c('OKLAHOMA'),]


text <- EDA_Data$text# Create a corpus  
corpus <- Corpus(VectorSource(EDA_Data_new$plaintiff))
glimpse(corpus)
corpus[1][1]
View(corpus)
corpus<-tm_map(corpus, content_transformer(tolower))

corpus<-tm_map(corpus, removeNumbers)

corpus<-tm_map(corpus,removeWords, stopwords("english"))

corpus<-tm_map(corpus,removePunctuation)

corpus<-tm_map(corpus,stripWhitespace)


#Create TDM
dtm <- TermDocumentMatrix(corpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

dev.off()
set.seed(1234) # for reproducibility

wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, 'Dark2'),scale=c(2,1.5))

wordcloud2(data=df, size=0.8,color='random-dark')



#################### STATISTICAL MODELLING #############################
#################################################################

############# Unsupervised Clustering ##############################

df_stats_m=df_stats
View(df_stats_m)
df_stats_m$County_Court=as.factor(df_stats_m$County_Court)

## Encoding the county_courts
df_stats_mm=one_hot(as.data.table(df_stats_m))
View(df_stats_mm)
df_stats_mm_scaled=scale(df_stats_mm)

#### Detecting the optimal number of clusters ####

### SSW ######################################################################

#####KMEANS
set.seed(80)

SSW <- function(data, nc=15){                    
  par(mfrow=c(1,2))
  wss <- NULL  
  pctExp <-NULL
  for (k in 1:nc){
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Sum of squares within")
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  par(mfrow=c(1,1))
}
SSW(df_stats_mm_scaled,nc=10)

k4 <- kmeans(df_stats_mm_scaled, centers = 2, nstart = 25)

fviz_cluster(k4, data = df_stats_mm_scaled)

##############################################################################

# NB- KMeans did not work well even after one-hot encoding


######################################################################################################
########################################## SCRATCH SPACE##############################################
#### KMODES ON EDA_DATA####
EDA_Data_A = na.omit(EDA_Data)

sum(is.na(EDA_Data_A)) # There are no missing data

View(EDA_Data_A)
str(EDA_Data_A) # About 67,000 observations in the new dataset
# We will now select the new categories

library(klaR)
set.seed(1)
(cl <- kmodes(EDA_Data_A, 2))

summary(cl)

plot(jitter(EDA_Data_A), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)













