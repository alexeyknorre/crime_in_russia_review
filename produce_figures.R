library(ggplot2)
library(lemon)
library(dplyr)
library(readxl)
library(scales)
library(extrafont)

#font_import()
loadfonts()

### Figure 2 - administrative prejudicing

dat <- read_excel("data/prejudicing-data.xlsx")

figure2 <- ggplot(data = dat, aes(x=year,y=share_prosecuted)) +
  geom_point()+
  geom_line() +
  expand_limits(y = 0)+
  scale_x_continuous(breaks = seq(2012,2021,1),limits = c(2012,2021),
                     name = "")+
  scale_y_continuous(name = 'Share of prosecutions with administrative prejudice',
                     labels = scales::label_percent(accuracy = 1L)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=10, family="Times New Roman"))

ggsave("figures/fig2_prejudice.pdf",figure2,
       width = 6, height = 4, bg = "white", dpi = 300)

## Fig 3 crime rates in Russia

cr <- read.csv("data/crime_russia_yearly.csv")

cr <- subset(cr, Measure.Names == "Rate")
cr$Measure.Values <- as.numeric(cr$Measure.Values)

cr$Crime <- recode(cr$Crime,
                   burglary = "Burglary",
                   homicide = "Homicide",
                   mvt = "Vehicle theft",
                   robbery = "Robbery",
                   serious_assault = "Aggravated assault",
                   theft = "Theft")

p <- ggplot(data = cr, aes(x = Year, y = Measure.Values,
                           group = Crime)) +
  geom_point() +
  geom_line()+
  facet_rep_wrap(.~Crime,scales = "free_y", repeat.tick.labels = 'bottom')+
  scale_x_continuous(breaks = seq(1990,2020,5),limits = c(1990,2020))+
  scale_y_continuous(name = 'Rate per 100,000 population') +
  #geom_text(aes(label=round(Measure.Values,digits = 2),
  #              y =Measure.Values),
  #          hjust=0.5, vjust=-1)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=10, family="Times New Roman"),
        strip.background =element_blank())+
  expand_limits(y=0)


ggsave("figures/fig3_crime_drop.pdf",p,
       width = 8, height = 6, bg = "white", dpi = 300)

### Figure 4 - prison population rates

dat <- read_excel("data/penal_data.xlsx",sheet = 1)

figure4 <- ggplot(data = dat, aes(x=year,y=prison_population_rate)) +
  geom_point()+
  geom_line() +
  expand_limits(y = 0)+
  scale_x_continuous(breaks = seq(2000,2020,2),
                     #limits = seq(2000,2020,2),
                     name = "")+
  scale_y_continuous(name = 'Prison population rate per 100,000 population') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=10, family="Times New Roman")) 

ggsave("figures/fig4_prison_population_rate.pdf",
       figure4,width = 6, height = 4, bg = "white", dpi = 300)

### Fig 5 early release

dat <- read_excel("data/penal_data.xlsx",sheet = 2)

figure5 <- ggplot(data = dat, aes(x=year,y=share_early_released_adult_prisoners/100)) +
  geom_point()+
  geom_line() +
  expand_limits(y = 0)+
  scale_x_continuous(breaks = seq(2007,2020,1),
                     #limits = seq(2000,2020,2),
                     name = "")+
  scale_y_continuous(name = 'Share of early released among all released inmates\nfrom colonies for adults prisoners',
                     labels = label_percent(accuracy=1)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text=element_text(size=10, family="Times New Roman")) 

ggsave("figures/fig5_early_released.pdf",
       figure5,width = 6, height = 4, bg = "white", dpi = 300)




