#load common packages
.First()


# 1. Batch 1 ------
pilot.df <- read.qualtrics.csv(here('master data/Pilot Data 1-27-20.csv'))
pilot.df$StartDate <- mdy_hm(pilot.df$StartDate)
pilot.df$EndDate <- mdy_hm(pilot.df$EndDate)
pilot.df$RecordedDate <- mdy_hm(pilot.df$RecordedDate)

# 2. Batch 2 ------
df <- read.qualtrics.csv(here('master data/Master Data 2-4-20.csv'))
df <- rbind(df,pilot.df)
rm(pilot.df)
#df$participant <- 1:nrow(df)
#df <- select(df, participant, everything())

#mturk <- read.mturk.csv(here('master data/Mturk master batch results.csv'))
# The following participants contacted me by email to provide their survey code
#mturk[mturk$WorkerId == 'A3MJCHFORYK0US','code'] <- 796471
#mturk[mturk$WorkerId == 'A1E8PIR82KIJEP','code'] <- 809268
#mturk$code <- as.numeric(mturk$code)

# 3. Prescreen ------
# preliminary prescreen data cleaning completed manually in excel 
# any multiracial responses in race question were changed to a value of 6
# changed name of survey code variable to code
# Identified people that should be not be paid and added an explanation to a Reject variable
# Do not include variable indicates people that should not be rejected, but should also not be included
# in the sample for the full survey

#pre.df <- read_csv(here('doc/manually cleaned prescreen data.csv'), 
#                                            col_types = cols(EndDate = col_datetime(format = "%m/%d/%Y %H:%M"), 
#                                                             RecordedDate = col_datetime(format = "%m/%d/%Y %H:%M"), 
#                                                             StartDate = col_datetime(format = "%m/%d/%Y %H:%M")))


#pre.mturk <- read.mturk.csv(here('master data/Mturk prescreen batch results 3-29-20.csv'))
#pre.mturk[pre.mturk$WorkerId == 'A2L7S6RZOZ6NM9','code'] <- 789682
#pre.mturk[pre.mturk$WorkerId == 'A133OFB7WTNKCP','code'] <- 444636
#pre.mturk[pre.mturk$WorkerId == 'A1ZE0MLGKWQLM8','code'] <- 161343
#pre.mturk[pre.mturk$WorkerId == 'A5BBUOX8P86N4','code'] <- 920466
#pre.mturk[pre.mturk$WorkerId == 'A2C2RTM1W48P5B','code'] <- 871225
#pre.mturk$code <- as.numeric(pre.mturk$code)

# ___3.1 Prescreen qualification assignment -------
#all.workers <- read_csv(here("master data/Mturk workers - 4-9-20 - downloaded after prescreen to assign approval for round 2.csv"),
#                        col_types = cols(BlockReason = col_double(), 
#                                         `CURRENT-AUS screen approved` = col_double(),
#                                         `UPDATE BlockStatus` = col_double(), 
#                                         `UPDATE-AUS screen approved` = col_double(),
#                                         `UPDATE-Completed AUS Part 1` = col_double()))

#also used the manually cleaned prescreen data from above

# 4. Batch 3 (Round 2 data) -------
#downloaded to assign approval to portion of sample prior to automatic approval
#round2.df <- read.qualtrics.csv(here('master data/AUS round 2 - 4-12-20.csv'))
round2.df <- read.qualtrics.csv(here('master data/AUS Round 2 - qualtrics - 4-23-20.csv'))

df <- rbind(df,round2.df)
rm(round2.df)
df$participant <- 1:nrow(df)
df <- select(df, participant, everything())

#round2.mturk <- read.mturk.csv(here('master data/AUS batch results 4-12-20.csv'))
#round2.mturk <- read.mturk.csv(here('master data/AUS round 2 - mturk batch results - 4-23-20.csv'))
# The following participants contacted me by email to provide their survey code
#round2.mturk[round2.mturk$WorkerId == 'AAURS65ZO4SXC','code'] <- 423784
#round2.mturk[round2.mturk$WorkerId == 'A3VL1CBZ3BGQK7','code'] <- 657439
#round2.mturk$code[126]
#round2.mturk$code[126] <- 841749
#round2.mturk$code <- as.numeric(round2.mturk$code)

# 5. Retest qualification assignment -------
workers <- read_csv(here("master data/mturk workers file - 4-23-20.csv"))
workers <- workers %>% #do this to remove spaces from names
  data.frame() %>% 
  tbl_df

# 6. Retest data ---------

# 7. Item content ---------
item.content <- read_csv(here("doc/item content.csv"))

# 8. Save/load results files --------
# ___8.1 Clean datasets -----
save(df, aus, item.content,
     file = here("output/clean data.RData"))

load(here("output/clean data.RData"))

# ___8.2 Results -----
save(parallel, map,
     fa1, fa2, fa3, fa4, fa5, fa6, fa7,
     fastructuresdf, faloadingsdf, fascoresdf,
     compiled.fit,
     aus.f1, aus.f1.grm, #f1.gpcm, #these are good 
     aus.f2, aus.f2.grm,  
     aus.f3, aus.f3.grm,  
     aus.f4, aus.f4.grm,
     file = here("output/fa results.RData"))

load(here("output/fa results.RData"))

# save(aus.grm.model, #34 item, 4-factor GRM
#      aus.multi.grm, #34 item, 4 factor GRM all results
#      file = here("output/mirt results.RData"))
# 
# load(here("output/mirt results.RData"))
# 
# save(mokken.aus, #34 item mokken aisp analysis
#      file = here("output/mokken results.RData"))
# 
# load(here("output/mokken results.RData"))