
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(Lahman)
library(here)

# Load Data ---------------------------------------------------------------

data("Pitching")
data(PitchingPost)
data("People")


# Create Function ---------------------------------------------------------
#Function from Ch3 of Baseball w/R

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


# Aggregate Players of Interest -------------------------------------------

PlayerInfo <- bind_rows(get_birthyear("Pedro Martinez"),
                        get_birthyear("Greg Maddux"),
                        get_birthyear("Christy Matthewson"),
                        get_birthyear("Randy Johnson"),
                        get_birthyear("Walter Johnson"),
                        get_birthyear("Sandy Koufax"),
                        get_birthyear("Clayton Kershaw"),
                        get_birthyear("Bob Gibson"),
                        get_birthyear("Tom Seaver"),
                        get_birthyear("Max Scherzer")) %>%
  filter(!playerID %in% c("johnsra03", "johnsra04", "martipez03", "gibsobo01"))

#Pull together stats by player age
Pitching %>%
  inner_join(PlayerInfo, by = 'playerID') %>%
  mutate(Age = yearID - birthYear) %>%
  select(Player, Age, W, SO, BB, ERA, CG, SHO, IPouts) %>%
  mutate(IP = round((IPouts/3), digits = 1)) %>%
  group_by(Player) -> murph_pitchers

write.csv(murph_pitchers, here("output", "murph_pitchers.csv"))

#Pull together postseason stats by player age
PitchingPost %>%
  inner_join(PlayerInfo, by = 'playerID') %>%
  mutate(Age = yearID - birthYear) %>%
  select(Player, Age, W, SO, BB, ERA, CG, SHO, IPouts) %>%
  mutate(IP = round((IPouts/3), digits = 1)) %>%
  group_by(Player) -> murph_pitchers_post

write.csv(murph_pitchers_post, here("output", "murph_pitchers_post.csv"))
