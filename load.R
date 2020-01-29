#load common packages
.First()

# Batch 1
df <- read.qualtrics.csv(here('master data/Pilot Data 1-27-20.csv'))
mturk <- read.mturk.csv(here('master data/Mturk pilot batch results.csv'))


