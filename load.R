#load common packages
.First()

# Batch 1
pilot.df <- read.qualtrics.csv(here('master data/Pilot Data 1-27-20.csv'))
pilot.mturk <- read.mturk.csv(here('master data/Mturk pilot batch results.csv'))

# Batch 2
df <- read.qualtrics.csv(here('master data/Master Data 2-4-20.csv'))
df <- rbind(df,pilot.df)
mturk <- read.mturk.csv(here('master data/Mturk master batch results.csv'))
mturk[mturk$WorkerId == 'A3MJCHFORYK0US','code'] <- 796471
mturk[mturk$WorkerId == 'A1E8PIR82KIJEP','code'] <- 809268
mturk$code <- as.numeric(mturk$code)
