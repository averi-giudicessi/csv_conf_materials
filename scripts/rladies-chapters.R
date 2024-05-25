
library(jsonlite)
data <- jsonlite::fromJSON('https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json')

# View(data)

library(tidyverse)
chapters <- data %>%
  select(1,2,3,7,8)%>% #names
  rename(chapter=group_urlname)%>%
  mutate(location=ifelse(location=="Online event","online","inperson"),
         title=sub(".*-- ","",title),
         #title=sub(".*- ","",title),
         title=gsub("\\s*\\([^\\)]+\\)","",title)) %>%#head
  filter(!str_detect(title,regex("canceled|cancelled",ignore_case=T)),
         !chapter%in%c("RLadiesJeddah","muhq_deleted@4633@rladies-ushuaia",
                      "muhq_deleted@9919@notopic@508502","notopic@544550"))%>%
  arrange(desc(date))%>%
  filter(year(date)<2024)%>%
  mutate(year=year(date))

# write.csv(chapters,"data/rladies_chapters.csv",row.names = F)
rladies_chapters <- read_csv("data/rladies_chapters.csv")
rladies_chapters%>%View
  
dat <- rladies_chapters %>% #View
  group_by(chapter,year)%>%
  count(chapter)%>%
  arrange(-year)

dat %>%
  ggplot(aes(x=factor(year),y=n,fill=chapter))+
  geom_col(show.legend = F)+
  ggthemes::theme_fivethirtyeight()+
  labs(title="R-Ladies Chapters",
  subtitle="Events Distribution",
       x="Year",y="n. of events",
       caption="DataSource: Meetup by {meetupr} | Graphics: Federica Gazzelloni")

# ggsave("images/rladies_chapters_distrib.png")

dat%>%
  #ungroup()%>%
  #arrange(-year)%>%
  group_by(chapter)%>%
  summarize(tot=mean(n))%>%
  arrange(-tot)%>%
  slice(1:25)%>%
  ggplot(aes(x=tot,y=fct_reorder(chapter,tot)))+
  #geom_col()
  geom_segment(aes(x=0,xend=tot,yend=chapter),
               color="#580B3A",linewidth=1.5)+
  geom_point(aes(fill=chapter),
             color="#580B3A",
             size=2.5,
             shape=21,stroke=0.5,
             show.legend = F)+
  coord_cartesian(expand = F,clip = "off")+
  labs(title="Top 25 R-Ladies Chapters",
       subtitle = "Average n. of events per Year",
       x="n. of Events in 10 Years",y="",
       caption="DataSource: Meetup by {meetupr} | Graphics: Federica Gazzelloni")+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.y = element_text(size=7))

  

# ggsave("images/top_rladies_chapters.png")


# type of events

rladies_chapters%>% # dim 4268    6
  filter(str_detect(title,regex("workshop|course|tutorial|introduction|Introducción",ignore_case=T)))

(435/4268)*100 # ~ 10%

rladies_chapters%>% # dim 4268    6
  filter(!str_detect(title,regex("workshop|course|tutorial|introduction|Introducción",ignore_case=T)))%>%
  filter(str_detect(title,regex("career",ignore_case=T)))#%>%
  View

  (20/4268)*100

rladies_chapters%>% # dim 4268    6
  #filter(!str_detect(title,regex("workshop|course|tutorial|introduction|Introducción",ignore_case=T)))%>%
  filter(!str_detect(title,regex("career",ignore_case=T)))%>%
  filter(str_detect(title,regex("data Viz|visualization",ignore_case=T)))#%>%
  View
  
  (97/4268)*100 

rladies_chapters%>% # dim 4268    6
  #filter(!str_detect(title,regex("workshop|course|tutorial|introduction|Introducción",ignore_case=T)))%>%
  filter(!str_detect(title,regex("career",ignore_case=T)))%>%
  filter(str_detect(title,regex("health|health care",ignore_case=T)))%>%
  View

# attendees
id <- rladies_chapters$id
id1 <- id[1:25]
library(meetupr)
# https://stackoverflow.com/questions/72577048/unknown-argument-hasnextpage-on-field-pageinfo-graphql
attendees <- function(id) {
  dat<- meetupr::get_event_attendees(id)%>%
   dim()
    dat[1]
}

mylist <- lapply(id1,attendees)

first25 <- mylist%>%unlist()
first25_events <- rladies_chapters[1:25,]%>%
  cbind(first25)

# meetupr::get_event_attendees("296677321")




top25_attendees <- first25_events%>%
  mutate(year=year(date))%>%
  group_by(chapter,year,title)%>%
  summarise(attendees=sum(first25))%>%
  arrange(-attendees)


# write.csv(top25_attendees,"data/top25_attendees.csv")

top25_attendees%>%
  filter(str_detect(title,regex("workshop",ignore_case=T)))


top25_attendees%>%
  filter(str_detect(title,regex("visualization",ignore_case=T)))


library(tidyverse)
top25_attendees <- read.csv("data/top25_attendees.csv")

top25_attendees%>%
  select(2,4,5)%>%
  group_by(chapter)%>%
  summarize(tot_attendees=sum(attendees))%>%
  arrange(-tot_attendees)%>%
  ggplot(aes(tot_attendees,fct_reorder(chapter,tot_attendees)))+
  geom_col(aes(fill=chapter),show.legend = F)+
  labs(title="Top 25 R-Ladies Chapter",
       subtitle="Total n. of attendees in 2023",
x="Attendees",y="",
caption="DataSource: Meetup by {meetupr} | Graphics: Federica Gazzelloni")+
  ggthemes::theme_fivethirtyeight()


ggsave("images/top_attendees.png")



