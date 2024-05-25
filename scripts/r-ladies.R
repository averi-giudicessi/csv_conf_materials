# tidy tuesday novemeber 21st

# libraries
library(tidyverse)
library(gghighlight)


# load data otra vez
dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

# make absent cells explicit and create col for counts
dat <- dat |>
  complete(chapter, year, location) |>
  mutate(event = ifelse(is.na(date), 0, 1))
  

# have a look at chapters with highest number of events
dat_agg_chap <- dat |>
  group_by(chapter) |>
  summarise(total=sum(event))

# create vector of top 10 chapters by number of events
topten <- dat_agg_chap |>
  top_n(10) 
top <- unique(topten$chapter)

# filter chapters and aggregate
dat_top <- dat |>
  filter(chapter %in% top) |>
  filter(year >= 2015) |>
  group_by(chapter, year, location) |>
  summarise(total = sum(event)) 


#plot
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
  coord_cartesian(clip="off") +
  theme(plot.tag.position = c(.15,.017),
        plot.tag = element_text(colour="grey20", face=2, size=30))

ggsave("r-ladies.jpg", plot = last_plot())

