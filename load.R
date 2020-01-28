#load common packages
.First()

df <- read.qualtrics.csv(here('master data/Pilot Data 1-27-20.csv'))
old.df <- read.qualtrics.csv(here('master data/Pilot Data 1-24-20.csv'))
mturk <- read.mturk.csv(here('master data/Mturk pilot batch results.csv'))

mturk$code %in% df$Random.ID
mturk$code[!mturk$code %in% old.df$Random.ID]

mturk$WorkerId %in% df$workerID
