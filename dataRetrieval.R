library(jsonlite)
library(ggplot2)
library(stringr)
library(dplyr)
library(mongolite)


cong <- mongo(
  url = "mongodb://localhost",
  db="wyscoutdutch",
  collection = "games"
)
conp <- mongo(
  url = "mongodb://localhost",
  db="wyscoutdutch",
  collection = "players"
)
conm <- mongo(
  url = "mongodb://localhost",
  db="wyscoutdutch",
  collection = "matches"
)

dutchmatches=conm$find(
  query = '{"competitionId":635}',
  fields = '{"label":1}'
)

# split into home,away,hscore,ascore
#dutchmatches %>% str_split()
t="Zagłębie Lubin - Raków Częstochowa, 1-2"
r=str_split(t,",")
r[[1]][1]
r[[1]][2]
dutchmatches <- dutchmatches %>% rowwise() %>% mutate(match=str_split(label,",")[[1]][1],
                        score=str_split(label,",")[[1]][2])

dutchmatches <- dutchmatches %>% rowwise() %>% mutate(home=str_split(match," - ")[[1]][1],
                        away=str_split(match," - ")[[1]][2])

dutchmatches <- dutchmatches %>% rowwise() %>% mutate(homescore=str_split(score,"-")[[1]][1],
                        awayscore=str_split(score,"-")[[1]][2])
dutchmatches <- dutchmatches %>% mutate(longinfo=paste0(match,"_",`_id`))

dteams <- dutchmatches %>% select(home) %>% unique()

# wrangle passes
allPasses <- allPasses[!is.na(allPasses$matchId), ]
allPasses <- allPasses %>%  group_by(matchId) %>% mutate(
  interval = cut(minute, 
                 breaks = seq(0, max(minute, na.rm = TRUE) + 10, by = 10), 
                 include.lowest = TRUE, 
                 labels = FALSE)
)

# create bins for heatmap
allPasses <- allPasses %>%
  mutate(
    x_bin = cut(location.x, breaks = seq(0, 100, length.out = 7), include.lowest = TRUE, labels = FALSE),
    y_bin = cut(location.y, breaks = seq(0, 68, length.out = 4), include.lowest = TRUE, labels = FALSE),
    bin = paste(x_bin, y_bin, sep = "-")
  )

pass_counts <- allPasses %>%
  group_by(bin) %>%
  mutate(count = n()) %>% ungroup()
# get testmatch
dftarge=pass_counts %>% filter(matchId== 5241741)
ggplot(dftarge, aes(x = x_bin, y = y_bin, fill = count)) +
  geom_tile() + # Use tiles for heatmap
  scale_fill_gradient(low = "white", high = "red") + # Color gradient
  labs(x = "Pitch Length", y = "Pitch Width", fill = "Pass Count") +
  theme_minimal() +
  coord_fixed(ratio = 68 / 105) 

testmatch <- fromJSON("data/evt_5360025.json", flatten = T)
testmatch <- testmatch$events
testMP <- testmatch %>% filter(type.primary == "pass")

allTeams <- conm$find()
allPasses <- readRDS("data/allPMP.rds")

