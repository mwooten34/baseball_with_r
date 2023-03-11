### Chapter 3 - Graphics

#Load in packages
library(Lahman)
library(tidyverse)
library(here)
library(ggplot2)
library(ggrepel)


#Load in data
#Got data file from github
hof <- read_csv(here("raw", "hofbatting.csv"))

#Create the MidCareer variable
hof <- hof %>%
  mutate(MidCareer = (From +To) / 2,
                      Era = cut(MidCareer,
                                breaks = c(1800, 1900, 1919, 1941,
                                           1960, 1976, 1993, 2050),
                                labels = c("19th Century", "Dead Ball",
                                           "Lively Ball", "Integration",
                                           "Expansion", "Free Agency", 
                                           "Long Ball")))

#Summarize the eras

#Output is not matching the book. I'm not seeing anyone from the 19th Century era
#and the numbers in each category don't match. 

#Turns out I had typed some dates incorrectly in the breaks section of the 
#Midcareer variable creation. All fixed now!
hof_eras <- summarize(group_by(hof, Era), N = n())
hof_eras

#Bar Chart of HOF Players by Era
ggplot(hof, aes(x = Era)) +
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers") 
  
#ggsave(here("figs", "hof_bar.png"))

#Dot Plot of HOF Players by Era
ggplot(hof_eras, aes(Era, N)) +
  geom_point() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers") 
  
#ggsave(here("figs", "hof_dot.png"))

#OPS Single Variable Scatterplot
ggplot(hof, aes(x = OPS, y = 1)) +
  geom_jitter(height = 0.6) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_fixed(ratio = 0.03)

#OPS Histogram
ggplot(hof, aes(x = OPS)) +
  geom_histogram(breaks = seq(0.4, 1.2, by = 0.1),
                 color = "blue", fill = "white")

#Scatterplot for OPS vs MidCareer
ggplot(hof, aes(x = MidCareer, y = OPS)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(data = filter (hof, OPS >1.00 | OPS <.5),
                  aes(MidCareer, OPS, label = Player))

#ggsave(here("figs","HOF_OPS_Scatter_Labeled.png"))

#Working through a chart one piece at a time
p <- ggplot(hof, aes(OBP, SLG)) +
  geom_point()

p <- p + 
  xlim(0.25, 0.50) + ylim(0.28, 0.75) +
  xlab("On Base Percentage") +
  ylab("Slugging Percentage")

p <- p + geom_abline(slope = -1,
                     intercept = seq(0.7, 1, by = 0.1))

p <- p + annotate("text",
                  x = rep(.27, 4), y = c(.42, .52, .62, .72),
                  label = paste("OPS = ",
                                c(0.7, 0.8, 0.9, 1.0)))

#Explore Home Run Rate
hof <- mutate(hof, hr_rate = HR/AB)

#Create Parallel Strip Chart 
ggplot(hof, aes(hr_rate, Era)) +
  geom_jitter(height = 0.1)

ggplot(hof, aes(Era, hr_rate)) +
  geom_boxplot() + coord_flip()

#Comparing Ruth, Aaron, Bonds, and A-Rod
#Apparently the "Master" table has been replaced with a "People" table in recent
#versions of the Lahman dataset

data(Batting) #having lots of issues getting this in. str makes it useable data
data(People)  #but data() did not

#Writing the get_birthyear function
get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name, " "))
  People %>%
    filter(nameFirst == Names[1],
           nameLast == Names[2]) %>%
    mutate(birthYear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthYear)
}

#Create a df with info about the players in question
PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez"))

Batting %>%
  inner_join(PlayerInfo, by = 'playerID') %>%
  mutate(Age = yearID - birthYear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> HRdata

#Create line graph for payers HR by age
ggplot(HRdata, aes(x = Age, y = CHR, linetype = Player)) +
  geom_line()
#ggsave(here("figs", "cumulative_hr_line.png"))

