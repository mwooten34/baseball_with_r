#Load Packages
library(tidyverse)
library(ggplot2)
library(here)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(ggpubr)

#Read in Data
forty <- read_csv(here("raw", "40_40.csv"))

#Modify Data for graphing HRs
forty_hr <- forty %>%
  select(Player, HR, Game) %>%
#  filter(Game < 5) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR),
         RAJ = case_when(Player == "Ronald Acuna Jr" ~ "Yes",
                           Player != "Ronald Acuna Jr" ~ "No"))

#Create General plot
plotHR <- ggplot(forty_hr, aes(x = Game , y = CHR, group = Player)) +
  geom_line(size = 1, aes(color = RAJ)) 

#Create set of data ends for labeling end point of line
hr_end <- forty_hr %>%
  group_by(Player) %>%
  top_n(1, Game) 

#Add end point labels to graph
plotHR <- plotHR +
  geom_label_repel(aes(label = Player), data = hr_end) +
  theme(legend.position = "none")

#Add titles and axis labels
plotHR <- plotHR + 
  labs(title = "Home Runs") +
  xlab("Games Played") +
  ylab("Cumulative Home Runs") +
  theme(plot.title = element_text(hjust = 0.5))

#Change the color scales
plotHR <- plotHR +
  scale_color_manual(values=c('Black','Red'))

#Final Plot for HR
plotHR

ab_per_hr <- forty %>%
  select(Player, HR, AB) %>%
  group_by(Player) %>%
  summarise(total_HR = sum(HR),
            total_AB = sum(AB)) %>%
  mutate(ABHR = round(total_AB/total_HR, 2))%>%
  arrange(desc(ABHR))

plot_ABHR <- ggplot(ab_per_hr, aes(x=ABHR, y=reorder(Player, -ABHR))) +
  geom_col() +
  xlab("At Bats per Home Run") +
  ylab("Player") +
  ggtitle("Number of At Bats per Home Run in 40/40 Season") +
  geom_text(aes(label=ABHR), hjust=0)
  

#Modify Data for graphing SBs
forty_sb <- forty %>%
  select(Player, SB, Game) %>%
#  filter(Game < 5) %>%
  group_by(Player) %>%
  mutate(CSB = cumsum(SB),
         RAJ = case_when(Player == "Ronald Acuna Jr" ~ "Yes",
                         Player != "Ronald Acuna Jr" ~ "No"))

#Create General plot
plotSB <- ggplot(forty_sb, aes(x = Game , y = CSB, group = Player)) +
  geom_line(size = 1, aes(color = RAJ)) 

#Create set of data ends for labeling end point of line
sb_end <- forty_sb %>%
  group_by(Player) %>%
  top_n(1, Game) 

#Add end point labels to graph
plotSB <- plotSB +
  geom_label_repel(aes(label = Player), data = sb_end) +
  theme(legend.position = "none")

#Add titles and axis labels
plotSB <- plotSB + 
  labs(title = "Stolen Bases",
       caption = "Data source: Baseball Reference") +
  xlab("Games Played") +
  ylab("Cumulative Stolen Bases") +
  theme(plot.title = element_text(hjust = 0.5))

#Change the color scales
plotSB <- plotSB +
  scale_color_manual(values=c('Black','Red'))

#Final Plot for SB
plotSB

pa_per_sb <- forty %>%
  select(Player, SB, PA) %>%
  group_by(Player) %>%
  summarise(total_SB = sum(SB),
            total_PA = sum(PA)) %>%
  mutate(PASB = round(total_PA/total_SB, 2))%>%
  arrange(desc(PASB))

plot_PASB <- ggplot(pa_per_sb, aes(x=PASB, y=reorder(Player, -PASB))) +
  geom_col() +
  xlab("Plate Appearances per Stolen Base") +
  ylab("Player") +
  ggtitle("Number of Plate Appearances per Stolen Base in 40/40 Season") +
  geom_text(aes(label=PASB), hjust=0)

figure <- grid.arrange(plotHR, plotSB, plot_ABHR, plot_PASB, 
             ncol = 2, nrow = 2)

annotate_figure(figure,
                top = text_grob("Ronald Acuna Jr 40/40 Watch",
                                face = "bold",
                                size = 20))


