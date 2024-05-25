#instalar paquetes
install.packages(c("ggplot2", "tidyverse", "jsonlite", "knitr", "dplyr", "kableExtra",
                   "lubridate", "gghighlight"))


#cargar paquetes
library(ggplot2)
library(tidyverse)
library(jsonlite) 
library(knitr) 
library(dplyr)
library(kableExtra)
library(lubridate)
library(gghighlight)

#data
data <- 
  jsonlite::fromJSON('https://raw.githubusercontent.com/rladies/meetup_archive/main/data/events.json')


#ver datos
head(data)
View(data)


#limpiar datos
rladies_chapters <- data |> 
  select(1,2,3,7,8)|>  #names
  rename(chapter=group_urlname)|> 
  mutate(location=ifelse(location=="Online event","online","inperson"),
         title=sub(".*-- ","",title),
         #title=sub(".*- ","",title),
         title=gsub("\\s*\\([^\\)]+\\)","",title)) |> #head
  filter(!str_detect(title,regex("canceled|cancelled",ignore_case=T)),
         !chapter%in%c("RLadiesJeddah","muhq_deleted@4633@rladies-ushuaia",
                       "muhq_deleted@9919@notopic@508502","notopic@544550"))|> 
  arrange(desc(date)) |> 
  filter(year(date)<2024)|> 
  mutate(year=year(date))



#limpiar datos viendo top 50 capitulos de r-ladies puebla
top_50 <- dat |>
  ungroup() |>
  arrange(-year) |>
  group_by(chapter) |>
  summarize(tot=mean(n)) |>
  arrange(-tot) |>
  slice(1:50)

View(top_50)

# limpiar datos tercera: día de la semana y si estan online o en persona
days_week <- rladies_chapters |>
  select(date, location, year) |>
  mutate(date = lubridate::wday(date, label = TRUE, week_start = 1)) |>
  count(date, location, year) |>
  mutate(location = case_when(
    location == "inperson" ~ "In person",
    location == "online" ~ "Online"
  ))

#####primera visualización#########
dat |>
  ggplot(aes(x=factor(year),y=n,fill=chapter))+
  geom_col(show.legend = F)+
  ggthemes::theme_fivethirtyeight()+
  labs(title="R-Ladies Chapters",
       subtitle="Events Distribution",
       x="año",y="n. de eventos",
       caption="DataSource: r-ladies global | Graphics: R-Ladies Puebla con ayuda de Federica Gazzelloni")

#saving image

 ggsave("rladies_chapters_distrib.png")

 #####segundo visualización#########

top25 <- dat|>
    #ungroup()|>
    #arrange(-year)|>
    group_by(chapter)|>
    summarize(tot=mean(n))|>
    arrange(-tot)|>
    slice(1:25)

top25 |> ggplot(aes(x=tot,y=fct_reorder(chapter,tot)))+
  #geom_col()
  geom_segment(aes(x=0,xend=tot,yend=chapter),
               color="#580B3A",linewidth=1.5)+
  geom_point(aes(fill=chapter),
             color="#580B3A",
             size=2.5,
             shape=21,stroke=0.5,
             show.legend = F)+
  coord_cartesian(expand = F,clip = "off")+
  labs(title="Top 25 Capítulos de R-Ladies",
       subtitle = "Promedio (n) eventos por año",
       x="numero de eventos en 10 años",y="",
       caption="DataSource: R-Ladies Global | Graphics: R-ladies Puebla con ayuda de Federica Gazzelloni")+
  ggthemes::theme_fivethirtyeight()+
  theme(axis.text.y = element_text(size=7))

#####tercera visualización#########


# practicar cargando datos otra vez
data_new <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')


#  Celdas explícitas y crear columna para contar

data_new <- data_new |>
  complete(chapter, year, location) |>
  mutate(event = ifelse(is.na(date), 0, 1))


# Echa un vistazo a los capítulos con el mayor número de eventos
dat_agg_chap <- data_new |>
  group_by(chapter) |>
  summarise(total=sum(event))

# Crear un vector de los 10 capítulos principales por número de eventos

topten <- dat_agg_chap |>
  top_n(10) 
top <- unique(topten$chapter)

# Filtrar capítulos y agregar

dat_top <- data_new |>
  filter(chapter %in% top) |>
  filter(year >= 2015) |>
  group_by(chapter, year, location) |>
  summarise(total = sum(event)) 

ggplot(dat_top, aes(year, total, colour=chapter, group=interaction(chapter, location))) +
  geom_line(aes(linetype=location), size=.75, show.legend = F) +
  gghighlight(total >= 0,
              unhighlighted_params = list(linewidth = 0.5, colour = alpha("grey88", 0.4))) +
  facet_wrap(~chapter, ncol=5) +
  theme_minimal() +
  theme(panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(),
        axis.line = element_line(linewidth=1, color="grey"),
        axis.text = element_text(colour="white"),
        strip.text = element_text(colour="white", size=10),
        plot.title = element_text(colour="white", hjust=.5),
        plot.subtitle = element_text(colour = "grey", hjust=.5, size=12),
        plot.caption = element_text(colour="grey90", size=10)) +
  geom_hline(yintercept=0, color="white", linewidth=.5) +
  labs(title = "Cantidad de eventos de R-Ladies para los capítulos con el mayor número de eventos",
       subtitle = "Las líneas continuas representan eventos presenciales, mientras que las líneas discontinuas representan eventos en línea") +
  theme(plot.tag.position = c(.15,.017),
        plot.tag = element_text(colour="grey20", face=2, size=30))

######tablas######

puebla_members_df <- read.csv('rladies_puebla_members.csv')

# Convert the date column to Date type
puebla_members_df$date <- as.Date(puebla_members_df$date)

# Create a summary table of the total and active members per month
summary_table <- puebla_members_df |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(
    total_members = mean(total),
    active_members = mean(active)
  ) |>
  arrange(month)

#table
kable(summary_table, format = "html", col.names = c("Month", "Average Total Members", "Average Active Members"), caption = "Monthly Summary of Total and Active Members in Puebla") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 10) |>
  column_spec(1, bold = T, color = "white", background = "#580B3A") |>
  column_spec(2:3, background = "#f2f2f2")
