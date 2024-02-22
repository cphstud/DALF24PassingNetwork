library(jsonlite)
library(dplyr)
# get testmatch

testmatch <- fromJSON("data/evt_5360025.json", flatten = T)
testmatch <- testmatch$events
testMP <- testmatch %>% filter(type.primary == "pass")


