
# 1. Score Validation ----

#### ___1.1 Virtue Scale ####
df[paste('r_virtue_',1:8,sep = '')]<-NA
df$r_virtue_1[df$virtue_1>=4]<-1 #I have never told a lie to anyone
df$r_virtue_2[df$virtue_2>=4]<-1 #I have never been envious of anyone else
df$r_virtue_6[df$virtue_6>=4]<-1 #I have never been angry 

df$r_virtue_1[df$virtue_1<4]<-0
df$r_virtue_2[df$virtue_2<4]<-0
df$r_virtue_6[df$virtue_6<4]<-0

df$r_virtue_3[df$virtue_3<=2]<-1 #I have lied at least once
df$r_virtue_4[df$virtue_4<=2]<-1 #Treated someone unfairly at least once
df$r_virtue_5[df$virtue_5<=2]<-1 #I have laughed at an inappropriate joke
df$r_virtue_7[df$virtue_7<=2]<-1 #I have been impolite
df$r_virtue_8[df$virtue_8<=2]<-1 #I have eaten more than I should have

df$r_virtue_3[df$virtue_3>2]<-0
df$r_virtue_4[df$virtue_4>2]<-0
df$r_virtue_5[df$virtue_5>2]<-0
df$r_virtue_7[df$virtue_7>2]<-0
df$r_virtue_8[df$virtue_8>2]<-0

#### ___1.2. Infrequency ####
df[paste('r_infreq_',1:8,sep = '')]<-NA
df$r_infreq_1[df$infreq_1>=4]<-1 #I frequently forget my middle name
df$r_infreq_3[df$infreq_3>=4]<-1 #I never speak to anyone during the day
df$r_infreq_5[df$infreq_5>=4]<-1 #I get less than 1 hour of sleep a night
df$r_infreq_7[df$infreq_7>=4]<-1 #I have never listened to music
df$r_infreq_8[df$infreq_8>=4]<-1 #Sailed across the ocean in a balloon

df$r_infreq_1[df$infreq_1<4]<-0
df$r_infreq_3[df$infreq_3<4]<-0
df$r_infreq_5[df$infreq_5<4]<-0
df$r_infreq_7[df$infreq_7<4]<-0
df$r_infreq_8[df$infreq_8<4]<-0

df$r_infreq_2[df$infreq_2<=2]<-1 #I try to eat something almost every day
df$r_infreq_4[df$infreq_4<=2]<-1 #better rested after good night sleep
df$r_infreq_6[df$infreq_6<=2]<-1 #Lend things to people who won't take care

df$r_infreq_2[df$infreq_2>2]<-0
df$r_infreq_4[df$infreq_4>2]<-0
df$r_infreq_6[df$infreq_6>2]<-0

# Scoring 
df$virtue<-rowSums(df[paste('r_virtue_',1:8,sep='')]) #invalid if it sums to 3 or more
df$infreq<-rowSums(df[paste('r_infreq_',1:8,sep='')]) #invalid if it sums to 4 or more

### ___1.3 Identify Invalid responses - ignore virtue scale ####
df$invalid<-0
#df$invalid[df$virtue>2]<-1
df$invalid[df$infreq>3]<-1
df$invalid[ #invalid if missing more than 25% of responses
  select(df,WorkerId:virtue_8) %>%
  apply(1, function(z) sum(is.na(z))) %>% 
  is_weakly_greater_than(length(select(df,WorkerId:virtue_8))/4)
] <- 1
#invalid if they took less than 1 second to respond to each item on average
df$invalid[df$duration<=length(select(df,WorkerId:virtue_8))] <- 1

# 2. IGNORE Reject/approve Workers - Round 1 -------
# possible helpful link:
# https://mysite.ku.edu.tr/swithrow/2015/02/13/automating-the-accept-and-reject-process-in-mturk-with-r/

#Reject people that have too many NAs
#length(select(df,WorkerId:virtue_8))/2 = 50% of items
#Full survey only
excess.na <- mturk$WorkerId %in% 
  (select(df, WorkerId:virtue_8) %>% 
     apply(1, function(z) sum(is.na(z))) %>% 
     is_weakly_greater_than(180.5) %>% 
     df$WorkerId[.])
mturk$Reject[excess.na] <- "The survey submitted is missing more than half of all responses and is not considered a completed HIT."

#Identify people whose ID's are not in the survey and reject them
bad.id <- !(mturk$WorkerId %in% intersect(df$WorkerId, mturk$WorkerId))
mturk$Reject[bad.id] <- "Your Worker ID was not found in the survey. Your survey code could not be authenticated."

#Reject people that don't have a matching ID and survey code set.
#was working, stopped working, added dplyr:: and it works again
#don't understand
wrong.code <- !(mturk$WorkerId %in% dplyr::intersect(select(df, WorkerId, code),select(mturk,WorkerId,code))$WorkerId)
mturk$Reject[wrong.code] <- "The survey code entered into the HIT did not match the code provided by the survey or was left blank."

#identify responders who failed attention checks
#attention.check <- mturk$WorkerId %in% 
#  (select(df, c(WorkerId, virtue, infreq)) %>% 
#  filter(virtue>2|infreq>3) %>% 
#  select(WorkerId))$WorkerId
#mturk$Reject[attention.check] <- "The survey submitted contains more than an acceptable minimum number of failed attention checks and is not considered a completed HIT."

#Approve everyone that didn't get rejected
mturk$Approve[is.na(mturk$Reject)] <- "x"

# completed visual inspection of the 10 rejected responses 
#df$WorkerId[which(df$code == 972295)] #clear typo for worker ID
mturk$Reject[mturk$code == 972295] <- NA
mturk$Approve[mturk$code == 972295] <- 'x'

#df$WorkerId[which(df$code == 218539)] #clear typo for worker ID
mturk$Reject[mturk$code == 218539] <- NA
mturk$Approve[mturk$code == 218539] <- 'x'

#df$WorkerId[which(df$code == 270535)] #clear typo for worker ID
mturk$Reject[mturk$code == 270535] <- NA
mturk$Approve[mturk$code == 270535] <- 'x'

#df[which(df$code == 607919),] #syntax says this person is missing too much data, visual inspection looks fine - worker Id is present twice - did not complete most of it the first time - this attempt looks fine
mturk$Reject[mturk$code == 607919] <- NA
mturk$Approve[mturk$code == 607919] <- 'x'

# Following Worker ID is in the data twice, the attempt associated with this code is good
mturk$Reject[mturk$code == 476986] <- NA 
mturk$Approve[mturk$code == 476986] <- 'x'

duplicates <- df$WorkerId[!is.na(df$WorkerId)] %>% 
  duplicated %>% 
  df$WorkerId[!is.na(df$WorkerId)][.]

#mturk$Reject[mturk$WorkerId %in% duplicates] #all other duplicates are accepted

#df$WorkerId[which(df$code == 590032)] #clear typo for worker ID
mturk$Reject[mturk$code == 590032] <- NA
mturk$Approve[mturk$code == 590032] <- 'x'

#following did not provide worker Id, but data is indicated as valid
mturk$Reject[mturk$code == 208683] <- NA
mturk$Approve[mturk$code == 208683] <- 'x'

#write.excel(select(mturk, c(Approve, Reject)), row.names = F, col.names = F)

#___2.1 IGNORE Rejection/Approval for AUS round 2 ----------------
# completed visual inspection of the rejected responses 
#mturk$code[which(mturk$code == 302194)] #code matches one given
#participant didn't copy worker id into box
#visual inspection of data seems acceptable
mturk$Reject[mturk$code == 302194] <- NA
mturk$Approve[mturk$code == 302194] <- 'x'

#df$WorkerId[which(df$code == 256945)] #clear typo for worker ID
mturk$Reject[mturk$code == 256945] <- NA
mturk$Approve[mturk$code == 256945] <- 'x'

#df$WorkerId[which(df$code == 778514)] #clear typo for worker ID
mturk$Reject[mturk$code == 778514] <- NA
mturk$Approve[mturk$code == 778514] <- 'x'

#df$WorkerId[which(df$code == 162028)] #clear typo for code
mturk$Reject[mturk$code == 16028] <- NA
mturk$Approve[mturk$code == 16028] <- 'x'

#df$WorkerId[which(df$code == 841749)] #left code blank in mturk file
#df$invalid[which(df$code == 841749)] #responses seem valid
mturk$Reject[mturk$WorkerId == "ARG392N6HWZCJ"] <- NA
mturk$Approve[mturk$WorkerId == "ARG392N6HWZCJ"] <- 'x'

mturk[which(mturk$WorkerId == "A1B2V27QSGPT6W"),] #worker id not found in df
# also code is wrong
# participant emailed and complained. gave him approval.

# manually approved 16 additional workers on 4/15/20


#### 3. Remove invalid and duplicate responses #####
#Subset to only the valid items and non-redundant worker IDs
df <- df[which(df$invalid == 0),]

duplicates <- df$WorkerId[!is.na(df$WorkerId)] %>% 
  duplicated %>% 
  df$WorkerId[!is.na(df$WorkerId)][.]

df[df$WorkerId%in%duplicates,]

df[which(df$WorkerId == duplicates[1]),] #save first completion
df <- df[-which(df$participant == 280),]

df[which(df$WorkerId == duplicates[2]),] #save first completion
df <- df[-which(df$participant == 222),]

df[which(df$WorkerId == duplicates[3]),] #save first completion
df <- df[-which(df$participant == 538),]

df[which(df$WorkerId == duplicates[4]),] #save first completion
df <- df[-which(df$participant == 202),]

#### 4. IGNORE Prescreen data cleaning and approval ######

pre.bad.id <- !(pre.mturk$WorkerId %in% intersect(pre.df$WorkerId, pre.mturk$WorkerId))
# pre.mturk$WorkerId[pre.bad.id]
# manually modified the following: 
# Random ID 756004 - Worker ID had odd characters at the end of it
# Random ID 680458 - Worker ID was correct, but in lower case
# Random ID 205493 - Worker ID was preceeded by "COPIED"
# Random ID 275813 - Worker ID had a O in place of a 0
# Random ID 660293 - The link to the qualtrics survey was pasted in place of a Worker ID but there was a matching survey code
pre.mturk$Reject[pre.bad.id] <- "Your Worker ID was not found in the survey. Your survey code could not be authenticated."

#pre.mturk[pre.wrong.code, c('WorkerId','code')] 
# Random ID 871225 - missing one number in MTurk dataset, copied it short - adjusted in "load" file
pre.wrong.code <- !(pre.mturk$WorkerId %in% dplyr::intersect(select(pre.df, WorkerId, code),select(pre.mturk,WorkerId,code))$WorkerId)
pre.mturk$Reject[pre.wrong.code] <- "The survey code entered into the HIT did not match the code provided by the survey or was left blank."

# reject people that gave screen responses that don't deserve payment based on manual cleaning
pre.bad.screen.response <- pre.mturk$WorkerId %in% pre.df$WorkerId[!is.na(pre.df$Reject)]
pre.mturk$Reject[pre.bad.screen.response] <- "Missing or incomplete response to screening item."

#Approve everyone that didn't get rejected
pre.mturk$Approve[is.na(pre.mturk$Reject)] <- "x"

#write.excel(select(pre.mturk, c(Approve, Reject)), row.names = F, col.names = F)

#remove duplicate worker IDs
pre.duplicates <- pre.df$WorkerId[!is.na(pre.df$WorkerId)] %>% 
  duplicated %>% 
  pre.df$WorkerId[!is.na(pre.df$WorkerId)][.]
# one duplicate ID

#___4.1 IGNORE Qualification Assignment ----------

approved.workers <- filter(pre.df, is.na(DoNotInclude))
approved.workers$WorkerId[!approved.workers$WorkerId %in% all.workers$`Worker ID`] #6 workers that were approved were not found in list of all past workers from Mturk

all.workers$`UPDATE-AUS screen approved`[all.workers$`Worker ID` %in% approved.workers$WorkerId] <- 1

#copy value to updated file
write.excel(all.workers$`UPDATE-AUS screen approved`,row.names = F, col.names = F)

#___4.2 IGNORE Test Retest Qualification Assignment ----------

#interested in acquiring workers who completed the AUS roughly two weeks prior
#retest hit will be posted on 4/24/20

#converted round2.df to df for the purposes of coding 
#df <- round2.df
#mturk <- round2.mturk
#write.excel(df)

approve.for.retest <- filter(df, ic == 2) %>%
  filter(invalid == 0) %>% 
  select(WorkerId) %>% 
  dplyr::intersect(select(mturk,WorkerId)) #IDs of workers to be approved

#don't know how to finish this pipe with an assignment
workers %>% 
  filter(Worker.ID %in% pull(approve.for.retest)) %>% 
  select(UPDATE.AUS.RT.Approval)

workers[which(workers$Worker.ID %in% pull(approve.for.retest)),"UPDATE.AUS.RT.Approval"] <- 1

#write.excel(workers$UPDATE.AUS.RT.Approval, col.names = F)





#round2.df <- df
#round2.mturk <- mturk


#### 5. Reverse Coding and Scale Scores -------------------

#___5.1 Demographics ----------

df$sex <- factor(df$sex, levels = 1:3, labels = c('male','female','non-binary'))

df$ethnicity <- factor(df$ethnicity, levels = 1:2, labels = c('Hispanic/Latino','Not Hispanic/Latino'))

df$race[df$race>5] <-6
df$race <- factor(df$race, levels = 1:6, labels = c('American Indian or Alaskan Native','Asian','Black or African American','Native Hawaiian or Other Pacific Islander','White','Multiracial'))

df$marital <- factor(df$marital, levels = 1:7, labels = c('Single (never married)','Married (first marriage)', 'Remarried','Separated','Divorced','Widowed','Long-term domestic partner (at least one year)'))

df$ever_psyc[df$ever_psyc == 2] <- 0

df$current_psyc[df$current_psyc == 2] <- 0

df$device <- factor(df$device, levels = 1:4, labels = c('Computer','Tablet','Smartphone','Other'))

#___5.2 NEO ---------------
df[paste('r_neo_',1:60, sep='')] <- df[paste('neo_',1:60, sep='')]

df[paste('r_neo_',
         c(9,18,19,23,24,30,34,36,37,38,39,40,42,43,44,45,46,48,49,50,51,53,54,55,56,58,59,60)
         ,sep = '')]<-6-df[paste('neo_',
                                 c(9,18,19,23,24,30,34,36,37,38,39,40,42,43,44,45,46,48,49,50,51,53,54,55,56,58,59,60)
                                 ,sep = '')]

df$n_anxiety 		<- rowMeans(df[paste('r_neo_',	c(1, 31),sep = '')])
df$n_anger 			<- rowMeans(df[paste('r_neo_',	c(6, 36),sep = '')])
df$n_depression 	<- rowMeans(df[paste('r_neo_',	c(11,41),sep = '')])
df$n_self_con 		<- rowMeans(df[paste('r_neo_',	c(16,46),sep = '')])
df$n_immoderation 	<- rowMeans(df[paste('r_neo_',	c(21,51),sep = '')])
df$n_vulnerability 	<- rowMeans(df[paste('r_neo_',	c(26,56),sep = '')])

df$e_friendliness 	<- rowMeans(df[paste('r_neo_',	c(2,32),sep = '')])
df$e_gregariousness <- rowMeans(df[paste('r_neo_',	c(7,37),sep = '')])
df$e_assertiveness 	<- rowMeans(df[paste('r_neo_',	c(12,42),sep = '')])
df$e_activity 		<- rowMeans(df[paste('r_neo_',	c(17,47),sep = '')])
df$e_excite_seek 	<- rowMeans(df[paste('r_neo_',	c(22,52),sep = '')])
df$e_cheerfulness 	<- rowMeans(df[paste('r_neo_',	c(27,57),sep = '')])

df$o_imagination 		<- rowMeans(df[paste('r_neo_',	c(3,33),sep = '')])
df$o_artistic_int 		<- rowMeans(df[paste('r_neo_',	c(8,38),sep = '')])
df$o_emotionality 		<- rowMeans(df[paste('r_neo_',	c(13,43),sep = '')])
df$o_adventurousness 	<- rowMeans(df[paste('r_neo_',	c(18,48),sep = '')])
df$o_intellect 			<- rowMeans(df[paste('r_neo_',	c(23,53),sep = '')])
df$o_liberalism 		<- rowMeans(df[paste('r_neo_',	c(28,58),sep = '')])

df$a_trust 			<- rowMeans(df[paste('r_neo_',	c(4,34),sep = '')])
df$a_morality 		<- rowMeans(df[paste('r_neo_',	c(9,39),sep = '')])
df$a_altruism 		<- rowMeans(df[paste('r_neo_',	c(14,44),sep = '')])
df$a_cooperation 	<- rowMeans(df[paste('r_neo_',	c(19,49),sep = '')])
df$a_modesty 		<- rowMeans(df[paste('r_neo_',	c(24,54),sep = '')])
df$a_sympathy 		<- rowMeans(df[paste('r_neo_',	c(29,59),sep = '')])

df$c_self_efficacy 	<- rowMeans(df[paste('r_neo_',	c(5,35),sep = '')])
df$c_orderliness 			<- rowMeans(df[paste('r_neo_',	c(10,40),sep = '')])
df$c_dutifulness 			<- rowMeans(df[paste('r_neo_',	c(15,45),sep = '')])
df$c_achieve_striv <- rowMeans(df[paste('r_neo_',	c(20,50),sep = '')])
df$c_self_discipline 	<- rowMeans(df[paste('r_neo_',	c(25,55),sep = '')])
df$c_cautiousness 			<- rowMeans(df[paste('r_neo_',	c(30,60),sep = '')])

df$n <- mean.n(df[,paste('r_neo_',c(1+(5*0:11)), sep = '')], 2)
df$e <- mean.n(df[,paste('r_neo_',c(2+(5*0:11)), sep = '')], 2)
df$o <- mean.n(df[,paste('r_neo_',c(3+(5*0:11)), sep = '')], 2)
df$a <- mean.n(df[,paste('r_neo_',c(4+(5*0:11)), sep = '')], 2)
df$c <- mean.n(df[,paste('r_neo_',c(5+(5*0:11)), sep = '')], 2)

#___5.3 ari - Affective Reactivity Index ---------------
df$ari <- rowMeans(df[paste('ari_',1:7,sep='')])

#___5.4 aus - Anger Upregulation Scale ---------------
# not currently scored
# variable names aus_1:aus_70

#___5.5 amii - Angry Mood Improvement Inventory ---------------
df$amii_control <- rowMeans(df[paste('amii_',	c(1,4,8,11,15,18,20,24),sep = '')])
df$amii_in <- rowMeans(df[paste('amii_',	c(3,	5,	6,	10,	13,	16,	17,	21),sep = '')])
df$amii_out <- rowMeans(df[paste('amii_',	c(2,	7,	9,	12,	14,	19,	22,	23),sep = '')])

#___5.6 bisbas - BIS/BAS ---------------

df[paste('r_bisbas_',1:24, sep='')] <- df[paste('bisbas_',1:24, sep='')]
df[paste('r_bisbas_',c(2,22),sep = '')]<-5-df[paste('bisbas_',c(2,22),sep = '')]

df$bis <- rowMeans(df[paste('r_bisbas_',c(2,8,13,16,19,22,24),sep='')])
df$bas <- rowMeans(df[paste('r_bisbas_',c(3,4,5,7,9,10,12,14,15,18,20,21,23),sep='')])
df$bas_drive <- rowMeans(df[paste('r_bisbas_',c(3,9,12,21),sep='')])
df$bas_funseek <- rowMeans(df[paste('r_bisbas_',c(5,10,15,20),sep='')])
df$bas_reward <- rowMeans(df[paste('r_bisbas_',c(4,7,14,18,23),sep='')])

#___5.7 bam - Brief Agitation Measure ---------------
df$bam <- rowMeans(df[paste('bam_',c(1,2,3),sep='')])

#___5.8 bite - Brief Irritability Test ---------------
df$bite <- rowMeans(df[paste('bite_',c(1,2,3,4,5),sep='')])

#___5.9 cesd - CES-D ---------------
df[paste('r_cesd_',1:20, sep='')] <- (df[paste('cesd_',1:20, sep='')]-1)
df[paste('r_cesd_',c(4,8,12,16),sep = '')]<-3-df[paste('cesd_',c(4,8,12,16),sep = '')]
df$cesd <- rowMeans(df[paste('r_cesd_',1:20,sep='')])

#___5.10 cars - Child Anger Rumination Scale ---------------
df$cars <- rowMeans(df[paste('cars_',1:19,sep='')])

#___5.11 crrs - Child Ruminative Response Scale ---------------
df$crrs <- rowMeans(df[paste('crrs_',1:13,sep='')])

#___5.12 meaq - Experiential Avoidance ---------------
df$meaq_behav <- rowMeans(df[paste('meaq_',c(1,	4,	6,	8,	10,	12,	15,	17,	19,	21,	23),sep='')])
df$meaq_distress <- rowMeans(df[paste('meaq_',c(2,	3,	5,	7,	9,	11,	13,	14,	16,	18,	20,	22,	24),sep='')])

#___5.13 promis.a - PROMIS Anger ---------------
df$promis.a <- rowMeans(df[paste('promis.a_',1:5,sep='')])

#___5.14 rpa - Reactive and Proactive Aggression ---------------
df$rpa.proact <- rowMeans(df[paste('rpa_',c(2,	4,	6,	9,	10,	12,	15,	17,	18,	20,	21,	23),sep='')])

df$rpa.react <- rowMeans(df[paste('rpa_',c(1,	3,	5,	7,	8,	11,	13,	14,	16,	19,	22),sep='')])

#___5.15 r.pos.aff - Responses to Positive Affect ---------------
df$r.pos.aff.efr <- rowMeans(df[paste('r.pos.aff_',1:5,sep='')])
df$r.pos.aff.damp <- rowMeans(df[paste('r.pos.aff_',6:13,sep='')])
df$r.pos.aff.sfr <- rowMeans(df[paste('r.pos.aff_',14:17,sep='')])

#___5.16 supps - Short UPPS ---------------
#to change perseverance and premeditation to lack thereof
df[paste('r_supps_',1:20, sep='')] <- df[paste('supps_',1:20,sep='')]
df[paste('r_supps_',c(1,	2,	4,	5,	7,	11,	12,	19), sep='')] <- 5- df[paste('supps_',c(1,	2,	4,	5,	7,	11,	12,	19), sep='')]

df$supps.lack.persev <- rowMeans(df[paste('r_supps_',c(1,4,7,11),sep='')])
df$supps.lack.premed <- rowMeans(df[paste('r_supps_',c(2,5,12,19),sep='')])
df$supps.neg.urg <- rowMeans(df[paste('r_supps_',c(6,8,13,15),sep='')])
df$supps.pos.urg <- rowMeans(df[paste('r_supps_',c(3,10,17,20),sep='')])
df$supps.sens.seek <- rowMeans(df[paste('r_supps_',c(9,14,16,18),sep='')])
