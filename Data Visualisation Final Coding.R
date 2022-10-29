getwd()
#===============R coding=============
FP<-read.csv("pd.csv",header=TRUE)
FP<-FP[-c(1), ] #calling data set 
str(FP)#checking the stucture of dataset
library(dplyr)
library(tidyr)
FP <- FP %>% separate_rows(UOF_NUMBER, FORCE_EFFECTIVE) #separate rows of values which have double values
FP <-subset(FP,select=-c(STREET_NAME, STREET_DIRECTION, STREET_TYPE,#delete columes which has more none value or have no use
                             LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION,
                             TYPE_OF_FORCE_USED2, TYPE_OF_FORCE_USED3,
                             TYPE_OF_FORCE_USED4, TYPE_OF_FORCE_USED5,
                             TYPE_OF_FORCE_USED6,TYPE_OF_FORCE_USED7,
                             TYPE_OF_FORCE_USED8, TYPE_OF_FORCE_USED9,
                             TYPE_OF_FORCE_USED10, NUMBER_EC_CYCLES))

FP<-FP %>% filter(INCIDENT_TIME != "NULL",SUBJECT_DESCRIPTION != "NULL",
                 SUBJECT_GENDER!= "NULL",
                OFFICER_ID != 0, SUBJECT_ID != 0,FORCE_EFFECTIVE != "")

library(lubridate)
class(FP$INCIDENT_DATE)
FP$INCIDENT_DATE <- as.Date(FP$INCIDENT_DATE,"%m/%d/%Y")
FP$INCIDENT_DATE <- months(FP$INCIDENT_DATE)
FP$INCIDENT_DATE <- as.factor(FP$INCIDENT_DATE)


## Convert Incident Time
INCIDENT_TIME <- FP$INCIDENT_TIME
INCIDENT_TIME <- parse_date_time(INCIDENT_TIME ,'%I:%M:%S %p')
INCIDENT_TIME <- as.POSIXct(INCIDENT_TIME ,'%Y-%m-%d %H:%M:%S')
FP$INCIDENT_TIME <- hour(INCIDENT_TIME)
FP$INCIDENT_TIME <- as.numeric(FP$INCIDENT_TIME)

FP$INCIDENT_TIME <- ifelse(FP$INCIDENT_TIME < 4 , "NIGHT",
                           ifelse(FP$INCIDENT_TIME < 12 , "MORNING",
                                  ifelse(FP$INCIDENT_TIME < 16 , "AFTERNOON",
                                         ifelse(FP$INCIDENT_TIME < 19 , "EVENING","NIGHT"))))


FP$OFFICER_HIRE_DATE <- as.Date(FP$OFFICER_HIRE_DATE,"%m/%d/%Y")
FP$OFFICER_HIRE_DATE <- year(FP$OFFICER_HIRE_DATE)
#FP$OFFICER_HIRE_DATE <- as.factor(FP$OFFICER_HIRE_DATE)


library(ggplot2)
library(COUNT)
update.packages(rlang)
install.packages(rlang) 
library(rlang)

library(gridExtra)
p <- ggplot(FP, aes(weight))+ylab("")+ylim(0,1500)+theme_bw()
cs <- p+aes(OFFICER_RACE)+geom_bar(fill="lightblue2")
sx <- p+aes(OFFICER_GENDER)+geom_bar(fill="pink2")
ag <- p+aes(OFFICER_YEARS_ON_FORCE)+geom_bar(fill="plum2")
su <- p+aes(OFFICER_INJURY)+geom_bar(fill="greenyellow")
grid.arrange(cs,sx,ag,su)


pp <- ggplot(FP, aes(weight))+ylab("")+ylim(0,1500)+theme_bw()
pp+aes(OFFICER_RACE)+geom_bar(fill="#af8dc3")

ggplot(FP, aes(OFFICER_RACE)) + 
  geom_point() +
  scale_y_continuous(limits=c(0,300)) +  # Change this to limits=c(0,335) and the warning disappars
  geom_smooth(method="lm")


pq <- ggplot(FP, aes(weight))+ylab("")+ylim(0,2250)+theme_bw()
pq+aes(SUBJECT_RACE)+geom_bar(fill="#af8dc3")
library(plotly)
#number ofincident in each month
pl <- ggplot (FP, aes (x = INCIDENT_DATE,y =..count..))+ geom_bar()+labs (x ="MONTH",y="NUMBER OF INCIDENTS", title =
        "NUMBER OF INCIDENTS IN EACH MONTH") + coord_flip()+
theme (axis.title.x = element_text (size = 8), axis.title.y
       = element_text (size= 6),
       plot.title = element_text (size= 8))
ggplotly(pl)

#Number of Incidents in Time
pltime<- ggplot (FP, aes (x = INCIDENT_TIME , y =..count..)) + 
  geom_bar(fill = "Green") +labs(x = "TIME_OF_INCIDENT",y= "NUMBER_OF_INCIDENTS",
       title = "NUMBER OF INCIDENTS WITH RESPECT TO TIME")+theme_bw()
ggplotly(pltime)

library(lubridate)
library(dplyr)

FP$OFFICER_RACE <- as.factor(FP$OFFICER_RACE)
class(FP$OFFICER_RACE)
table(FP$OFFICER_RACE)


FP %>%
  group_by(OFFICER_RACE) %>%
  filter(!is.na(OFFICER_RACE)) %>%
  summarise(Count = n()) %>%
  mutate(TotalCount = nrow(FP)) %>%
  mutate(Percentage = (Count/TotalCount) * 100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(OFFICER_RACE = reorder(OFFICER_RACE,Count)) %>%
ggplot(aes(x = OFFICER_RACE,y = Percentage)) +
  geom_bar(stat='identity',colour="white", fill ="Blue") +
  geom_text(aes(x = OFFICER_RACE, y = 1, label = paste0("(",round(Percentage,2)," % )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name', 
       y = 'Percentage', 
       title = 'Race Percentage of the Officer') +
  coord_flip() + 
  theme_bw()

# histogram officer years on force
FP$OFFICER_YEARS_ON_FORCE<- as.numeric(FP$OFFICER_YEARS_ON_FORCE)
hist(FP$OFFICER_YEARS_ON_FORCE, 
     breaks = 4, ylim = c(0, 1000), col = 2:6,
        main = "Histogram for the officer years on Force")

#checking the longitude and latitude 
library(htmltools)
library(spatial)
library(leaflet)
library(dplyr)
#9

FP %>%
  count() %>%
  ggplot(aes(x = reorder(0, n),y = n)) + 
  geom_col(fill="#756bb1") + 
  labs(x = "SUBJECT_OFFENSE",
       y = "DIVISION",
       title = paste0("Crimes commited in ",FP$DIVISION)) +
  coord_flip() +
  theme_minimal()
#longitude and litutde
FP %>%dplyr::select(LOCATION_LATITUDE,LOCATION_LONGITUDE,SUBJECT_RACE)%>%
  mutate_at (c ("LOCATION_LATITUDE",
                "LOCATION_LONGITUDE") , as.numeric) %>%
  leaflet() %>%
  addTiles() %>% addMarkers (~LOCATION_LONGITUDE , ~LOCATION_LATITUDE,
                              popup=~as.character (SUBJECT_RACE) ,
                              label=~as.character(SUBJECT_RACE))
#cluster in longituted and latitude
library(leaflet)
FP %>%dplyr::select(LOCATION_LATITUDE,LOCATION_LONGITUDE)%>%
  mutate_at (c ("LOCATION_LATITUDE",
                "LOCATION_LONGITUDE") , as.numeric) %>%
leaflet() %>% addTiles() %>% 
  addMarkers(~LOCATION_LONGITUDE , ~LOCATION_LATITUDE,
    clusterOptions= markerClusterOptions()
    )

#comparing subject was male and female or is subject was arrested
subject_MF <- table(FP$SUBJECT_GENDER, FP$SUBJECT_WAS_ARRESTED)
par(mfrow = c(1,2))
barplot(subject_MF, beside = F, ylim = c(0,5000), 
        names.arg = c("Male", "Female"),col=4:6)
barplot(subject_MF, beside = T, ylim = c(0,5000),
        names.arg = c("Male", "Female"),
        legend.text = c("Not-Arrested", "Arrested"),
col = 2:5)



#Crime Description with respect to Division 
library(dplyr)
library(ggplot2)
FP %>% select(SUBJECT_DESCRIPTION,DIVISION) %>% group_by(DIVISION) %>%
  top_n(25) %>% ggplot(aes(x = SUBJECT_DESCRIPTION, y = ..count..,fill = DIVISION)) +
  geom_bar(position = "dodge") + coord_flip()

#reason of force and type of force used
FP %>% select(REASON_FOR_FORCE,TYPE_OF_FORCE_USED1) %>%
  group_by(REASON_FOR_FORCE,TYPE_OF_FORCE_USED1) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% top_n(5) %>%
  ggplot(aes(x = TYPE_OF_FORCE_USED1 , y = Count)) +
  geom_col() + facet_wrap(~REASON_FOR_FORCE) + coord_flip()
remove.packages("dplyr")
install.packages("dplyr")
install.packages("remotes")
library(plotly)
library(dplyr)
#*# NUMBER OF OFFICERS IN EACH RACE WHO ARE HOSPITALIZED DURING AN INCIDENT
hosp<-FP %>% filter (OFFICER_HOSPITALIZATION == "Yes") %>%
  dplyr::select(OFFICER_RACE, OFFICER_ID, OFFICER_GENDER) %>%
  distinct (OFFICER_ID,.keep_all = TRUE) %>% 
  group_by (OFFICER_RACE) %>%
  summarise(Count = n()) %>%
  ggplot (aes (x = OFFICER_RACE, y = count)) + geom_col (fill="rosybrown4")+
  labs (x="OFFICER RACE" ,Y="COUNT", title =
                        "OFFICER HOSPITALISED DURING INCIDENT")
coord_flip() +
  theme_classic()+ 
  theme(axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size=8),plot.title =element_text(size = 8)) 
        
ggplotly(hosp)

FP %>% dplyr::select(SUBJECT_INJURY,OFFICER_INJURY) %>%
  group_by(SUBJECT_INJURY,OFFICER_INJURY) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% top_n(5) %>%
  ggplot(aes(x = OFFICER_INJURY, y = Count)) +
  geom_col() + facet_wrap(~sUBJECT_INJURY) + coord_flip()

  
  

# FIND INCIDENTS IN EACH MONTH WERE OFFICER & SUBJECT BOTH WERE INJURIED
injury.f<-FP %>% filter(OFFICER_INJURY!="NO" & SUBJECT_INJURY !="NO")%>%
  dplyr::select( SUBJECT_INJURY, INCIDENT_DATE) %>%
  group_by(INCIDENT_DATE) %>% summarise(Count = n()) %>%
  arrange(desc(Count))%>% 
  ggplot (aes (x = INCIDENT_DATE, y = count)) + geom_col (fill="rosybrown4")+
  theme_classic()+ labs (x ="INCIDENT_MONTHS",y="NUMBER_OF_INCIDENTS",
                         title = "INCIDENTS WEHRE OFFICER &
SUBJECT BOTH WERE INJURIED IN EACH MONTH")+coord_flip()+theme_bw()+
  theme(axis.title.x =element_text(size = 8),
        axis.title.y =element_text(size=8),
        plot.title = element_text(size=10))
ggplotly(injury.f)

injury.f <- FP %>% filter (OFFICER_INJURY!="NO") %>%
  dplyr::select (OFFICER_INJURY, INCIDENT_DATE) %>%
  distinct (INCIDENT_DATE, .keep_all = TRUE) %>%
  ggplot (aes (x =INCIDENT_DATE, y=..count.., fill = "OFFICER_INJURY"))+
  geom_bar(position="dodge")+
  labs (x="INCIDENT_MONTHS",y="NUMBER_OF_INCIDENTS" ,title = "INCIDENTS WEHRE OFFICER &
SUBJECT BOTH WERE INJURIED IN EACH MONTH") + 
  coord_flip()+theme_bw()+
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text (size=8),
        plot.title = element_text(size=10))
ggplotly(injury.f)


library(dplyr)
library(ggplot2)
library(plotly)


# OFFICER RACE WITH GENDER
pgh<-FP %>% dplyr::select (OFFICER_RACE, OFFICER_GENDER, OFFICER_ID) %>%
  distinct (OFFICER_ID, .keep_all = TRUE)%>%
  ggplot(aes(x =OFFICER_RACE, Y =..count..,fill = OFFICER_GENDER))+
  geom_bar(position="dodge") + labs (x="OFFICER RACE" ,Y="COUNT", title =
                                        "OFFICER RACE WITH GENDER")
ggplotly(pgh)

# subject RACE WITH GENDER
pgG<-FP %>% dplyr::select (SUBJECT_RACE, SUBJECT_GENDER, SUBJECT_ID) %>%
  distinct (SUBJECT_ID, .keep_all = TRUE)%>%
  ggplot(aes(x =SUBJECT_RACE, Y =..count..,fill = SUBJECT_GENDER))+
  geom_bar(position="dodge") + labs (x="SUBJECT RACE" ,Y="COUNT", title =
                                       "SUBJECT RACE WITH GENDER")
ggplotly(pgG)


#TOP 6 SUBJECT INJURY TYPE

table(FP$SUBJECT_INJURY_TYPE)
tt<-FP %>% filter(SUBJECT_INJURY == "Yes") %>%
  dplyr::select(SUBJECT_INJURY_TYPE, SUBJECT_INJURY) %>%
  group_by (SUBJECT_INJURY_TYPE) %>% summarise(count = n())%>%
  geom_col(fi11="cyan2") %>%
  ggplot (aes (x = SUBJECT_INJURY_TYPE, ylabs (x= "MOST_COMMON_INJURIES",
                                               y= "NUMBER_OF_INJURIES",
                  title = "TOP INJURIES IN SUBJECTS")
               theme_bw()+
                 theme(axis.title.x = element_text(size = 8),
                       axis.title.y = element_text (size=8),
                       plot.title = element_text(size=10))
               ggplotly(sub.inj)
             
## SUBJECT WHO HAVE INJURY
sub.inj <- FP %>% filter (SUBJECT_INJURY != "NO") %>%
  dplyr::select (SUBJECT_RACE, SUBJECT_GENDER, SUBJECT_ID, SUBJECT_INJURY) %>%
   distinct (SUBJECT_ID, .keep_all = TRUE) %>%
  ggplot (aes (x =SUBJECT_RACE, y=..count.., fill = SUBJECT_GENDER))+
  geom_bar(position="dodge")+
  labs (x="RACE",y="COUNT" ,title = "SUBJECT INJURY WITH GENDER") + 
coord_flip()+theme_bw()+
theme(axis.title.x = element_text(size = 8),
                    axis.title.y = element_text (size=8),
      plot.title = element_text(size=10))
      ggplotly(sub.inj)
             