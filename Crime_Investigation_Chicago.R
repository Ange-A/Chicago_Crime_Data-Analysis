library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(janitor)
library(lubridate)
library(kableExtra)
library(stringr)





crime <- read.csv("Crimes_Present.csv", header = TRUE, sep= ",")




View(crime)
nrow(crime)
colnames(crime)
head(crime, n= 10)
str(crime)




#data cleaning

#column renaming

colnames(crime)[1]= "ID"
colnames(crime)[2]= "OCCURENCE_DATE"
colnames(crime)[5]="PDESCRIPTION"
colnames(crime)[6]="SDESCRIPTION"
colnames(crime)[7]="LOCATION_DESCRIPTION"
colnames(crime)[12]="FBI_CD"
colnames(crime)[13]="X_COORDINATE"
colnames(crime)[14]="Y_COORDINATE"



#column separation

class(crimes)
crimes <- separate(crime, col= OCCURENCE_DATE, into=c('DATE', 'HOUR','AM'), sep=' ')
crimes$HOUR <- paste(crime2$HOUR, crime2$AM)
crimes <- crimes[,-c(4)]



#Change date_char columns to date types

crimes <- crimes %>% mutate(DATE = as_date(DATE, format= "%m/%d/%Y"))



#Transform N to No #Transform Y to Yes



 crimes[, c(9,10)] [crimes[, c(9,10)] == "N"] <- "NO"
 crimes[, c(9,10)] [crimes[, c(9,10)] == "Y"] <- "YES"



#columns: X_COORDINATE,Y_COORDINATE,LATITUDE,LONGITUDE, LOCATION_COORDINATES wont be used in this analysis
#and present multiple missing values that may coerce the analysis.




#check for empty cells and replacing them with NA

 crimes[crimes == ''] <- NA



# Identifying & Removing all NA  & duplicate Values


sum(is.na(crimes)) #10966
sum(duplicated(crimes)) #11


# Identifying & Removing all NA  & duplicate Values
crimes <- na.omit(crimes)
crimes <- unique(crimes)



#capitalize
 

crimes <- mutate_all(crimes, .funs=toupper)



#ORIGINAL DATA RECORDS: 234,807 entries
#CURRENT DATA RECORDS23:  234,131  entries
 

#Analysis


#descriptive stats


#top 5 crimes---------------------------------------------------------------------

criminality <- crimes %>%  
  group_by(PDESCRIPTION) %>% 
  summarize(n = n())


criminality

High_Crimes<- criminality[order(-criminality$n),]

class(High_Crimes)

High_Crimes %>% summarise(Percentage=n()/ncol(.))

High_Crimes$percentage= 100*(High_Crimes$n/sum(High_Crimes$n))



top5crimes <- High_Crimes[1:5, ]




top5crimesplot <-  ggplot(top5crimes, aes(x = PDESCRIPTION, y= n, fill= PDESCRIPTION))+
  geom_col()+
  
  geom_text(aes(label= paste0(round(percentage), "%")),
                       position = position_stack(vjust=0.9))+
  
  labs(title= ' Top 5 most committed crimes in Chicago Communities',  
       caption="Source: https://www.kaggle.com/datasets/etiennelq/french-employment-by-town", x= "crime", 
       y= "total number of crimes") +
  
    theme(panel.background = element_blank(),
           axis.line = element_blank(),
           axis.text = element_blank(),
           axis.ticks= element_blank(),
           axis.title = element_blank(),
           plot.title = element_text(hjust = 0.2, size = 14, face = "bold"),   
           plot.caption = element_text(hjust = 0.3, face = "italic"))

top5crimesplot + scale_fill_brewer(palette = "Spectral")


#highest crime blocks----------


Block_Crime <- (summary <- crimes %>%
                  group_by(BLOCK)%>%
                  summarize(crime = n())) 

High_Crime_Blocks <- Block_Crime[order(-Block_Crime$crime),]


top5 <- High_Crime_Blocks[1:5, ]


top5plot <-  ggplot(top5, aes(x = BLOCK, y= crime, fill= BLOCK))+
  geom_col()+

  
  labs(title= ' Top 5 Crime Blocks in Chicago',  
       caption="Source: https://www.kaggle.com/datasets/etiennelq/french-employment-by-town", x= "Block", 
       y= "Total Number of Crimes") +
  
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.2, size = 14, face = "bold"),   
        plot.caption = element_text(hjust = 2, face = "italic"))+ 
  
  scale_fill_brewer(palette = "Paired")



top5plot




#Arrest vs no Arrests per block------ 

Arrested <- crimes %>%  
  
  group_by(ARREST) %>% 
  
  summarize(counts= n(),
            percentage= n()/nrow(crimes))

Arrested


Arrestedplot <- ggplot(Arrested, aes(x ="", y = percentage, fill= ARREST))+
  geom_col(color= "black")+
  coord_polar("y", start= 0)+
  
  geom_text(aes(label= paste0(round(percentage*100), "%")),
            position = position_stack(vjust=0.5))+
  
  labs(title=' Arrested Crimes vs Non Arrested Crimes', subtitle= 'Chicago Communities',  
       caption="Source: https://www.kaggle.com/datasets/etiennelq/french-employment-by-town") + 
  
  
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks= element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.caption = element_text(hjust = 0.5, face = "italic")) + 
  
  scale_fill_brewer(palette = "BrBG")



Arrestedplot





#blocks with the highest arrests, blocks with the lowest--------

yesno <- crimes %>%
  group_by(LOCATION_DESCRIPTION) %>%
  summarize(Yes = sum(ARREST== "YES"), NO = sum(ARREST== 'NO'))

yesno

yesno <- yesno[with(yesno,order(-Yes)),]

top10yesno <- yesno [1:10, ]


rounded <- round(top10yesno[2:3] /rowSums(top10yesno[2:3]), 2)

percentages <- cbind(top10yesno$LOCATION_DESCRIPTION, rounded)

colnames(percentages)[1]= "location"



yesnopivot <- pivot_longer(percentages,
                        cols= -location,
                        names_to ="yesno",
                        names_prefix = "Crime",
                        values_to= "percentages"
)

yesnopivot


Arrests <- ggplot(yesnopivot, aes(x = yesno, y= percentages, fill= yesno))+
           geom_col() + 
           facet_wrap(~location) +
  
  geom_text(aes(label= paste0(round(percentages*100), "%")),
            position = position_stack(vjust=0.5)) +
  


  labs(title=' Arrested Crimes vs Non Arrested Crimes per location', subtitle= 'Chicago Communities',  
       caption="Source: https://www.kaggle.com/datasets/
       etiennelq/french-employment-by-town") + 
  
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.caption = element_text(hjust = 0.5, face = "italic")) + 
  
  scale_fill_brewer(palette = "Dark2")


Arrests



View(crimes)

#Crime Areas------------------
CrimeAreas <- (summary <- crimes %>%
                  group_by(LOCATION_DESCRIPTION)%>%
                  summarize(crime = n())) 

CrimeAreas <- CrimeAreas[order(-CrimeAreas$crime),]

top10CrimeAreas <- CrimeAreas  [1:10, ]



CrimeAreasplot  <- ggplot(top10CrimeAreas, aes(x = LOCATION_DESCRIPTION , y= crime, fill= LOCATION_DESCRIPTION ))+
  geom_col() + 
 


  labs(title=' Top 10 Crime Areas ', subtitle= 'Chicago Communities',  
       caption="Source: https://www.kaggle.com/datasets/
       etiennelq/french-employment-by-town") + 
  
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.caption = element_text(hjust = 0.5, face = "italic")) 
  


CrimeAreasplot 






#Domestic vs non domestic offenses------------------
domestic <- crimes %>%  
  
  group_by(DOMESTIC) %>% 
  
  summarize(counts= n(),
            percentage= n()/nrow(crimes))

domestic


domesticplot <- ggplot(domestic, aes(x ="", y = percentage, fill= DOMESTIC))+
  geom_col(color= "BLACK")+
  coord_polar("y", start= 0)+
  
  geom_text(aes(label= paste0(round(percentage*100), "%")),
            position = position_stack(vjust=0.5))+
  
  labs(title=' Domestic Crimes vs Non Domestic Crimes',  
  caption="Source: https://www.kaggle.com/datasets/etiennelq/french-employment-by-town") + 

  
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks= element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),   
        plot.caption = element_text(hjust = 0.3, face = "italic")) + 
  
  scale_fill_brewer(palette = "Blues")
  
    

domesticplot 







