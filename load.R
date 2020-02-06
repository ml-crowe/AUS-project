#load common packages
.First()


# Batch 1
pilot.df <- read.qualtrics.csv(here('master data/Pilot Data 1-27-20.csv'))
pilot.df$StartDate <- mdy_hm(pilot.df$StartDate)
pilot.df$EndDate <- mdy_hm(pilot.df$EndDate)
pilot.df$RecordedDate <- mdy_hm(pilot.df$RecordedDate)

# Batch 2
df <- read.qualtrics.csv(here('master data/Master Data 2-4-20.csv'))
df <- rbind(df,pilot.df)
rm(pilot.df)
mturk <- read.mturk.csv(here('master data/Mturk master batch results.csv'))
# The following participants contacted me by email to provide their survey code
mturk[mturk$WorkerId == 'A3MJCHFORYK0US','code'] <- 796471
mturk[mturk$WorkerId == 'A1E8PIR82KIJEP','code'] <- 809268
mturk$code <- as.numeric(mturk$code)
