
# 1. Score Validation items ----

#### Virtue Scale ####
df[paste('r_virtue_',1:8,sep = '')]<-NA
df$r_virtue_1[df$virtue_1>=4]<-1
df$r_virtue_2[df$virtue_2>=4]<-1
df$r_virtue_6[df$virtue_6>=4]<-1

df$r_virtue_1[df$virtue_1<4]<-0
df$r_virtue_2[df$virtue_2<4]<-0
df$r_virtue_6[df$virtue_6<4]<-0

df$r_virtue_3[df$virtue_3<=2]<-1
df$r_virtue_4[df$virtue_4<=2]<-1
df$r_virtue_5[df$virtue_5<=2]<-1
df$r_virtue_7[df$virtue_7<=2]<-1
df$r_virtue_8[df$virtue_8<=2]<-1

df$r_virtue_3[df$virtue_3>2]<-0
df$r_virtue_4[df$virtue_4>2]<-0
df$r_virtue_5[df$virtue_5>2]<-0
df$r_virtue_7[df$virtue_7>2]<-0
df$r_virtue_8[df$virtue_8>2]<-0

#### Infrequency ####
df[paste('r_infreq_',1:8,sep = '')]<-NA
df$r_infreq_1[df$infreq_1>=4]<-1
df$r_infreq_3[df$infreq_3>=4]<-1
df$r_infreq_5[df$infreq_5>=4]<-1
df$r_infreq_7[df$infreq_7>=4]<-1
df$r_infreq_8[df$infreq_8>=4]<-1

df$r_infreq_1[df$infreq_1<4]<-0
df$r_infreq_3[df$infreq_3<4]<-0
df$r_infreq_5[df$infreq_5<4]<-0
df$r_infreq_7[df$infreq_7<4]<-0
df$r_infreq_8[df$infreq_8<4]<-0

df$r_infreq_2[df$infreq_2<=2]<-1
df$r_infreq_4[df$infreq_4<=2]<-1
df$r_infreq_6[df$infreq_6<=2]<-1

df$r_infreq_2[df$infreq_2>2]<-0
df$r_infreq_4[df$infreq_4>2]<-0
df$r_infreq_6[df$infreq_6>2]<-0

# Scoring 
df$virtue<-rowSums(df[paste('r_virtue_',1:8,sep='')]) #invalid if it sums to 3 or more
df$infreq<-rowSums(df[paste('r_infreq_',1:8,sep='')]) #invalid if it sums to 4 or more

### Invalid responses ####
df$invalid<-0
df$invalid[df$virtue>2]<-1
df$invalid[df$infreq>3]<-1
df$invalid[
  select(df,WorkerId:virtue_8) %>%
  apply(1, function(z) sum(is.na(z))) %>% 
  is_weakly_greater_than(length(select(df,WorkerId:virtue_8))/4)
] <- 1
df$invalid[df$duration<=length(select(df,WorkerId:virtue_8))] <- 1

# Identify participants that should be rejected/approved -------
# possible helpful link:
# https://mysite.ku.edu.tr/swithrow/2015/02/13/automating-the-accept-and-reject-process-in-mturk-with-r/

#Reject people that have too many NAs
#length(select(df,WorkerId:virtue_8))/2 = 50% of items
excess.na <- mturk$WorkerId %in% 
  (select(df, WorkerId:virtue_8) %>% 
     apply(1, function(z) sum(is.na(z))) %>% 
     is_weakly_greater_than(180.5) %>% 
     df$WorkerId[.])
mturk$Reject[excess.na] <- "The survey submitted is missing more than half of all responses and is not considered a completed HIT."

#Reject people that don't have a matching ID and survey code set.
wrong.code <- !(mturk$WorkerId %in% intersect(select(df, WorkerId, code),select(mturk,WorkerId,code))$WorkerId)
mturk$Reject[wrong.code] <- "The survey code entered into the HIT did not match the code provided by the survey or was left blank."

#Identify people whose ID's are not in the survey and reject them
bad.id <- !(mturk$WorkerId %in% intersect(df$WorkerId, mturk$WorkerId))
mturk$Reject[bad.id] <- "Your Worker ID was not found in the survey. Your survey code could not be authenticated."

#identify responders who failed attention checks
#attention.check <- mturk$WorkerId %in% 
#  (select(df, c(WorkerId, virtue, infreq)) %>% 
#  filter(virtue>2|infreq>3) %>% 
#  select(WorkerId))$WorkerId
#mturk$Reject[attention.check] <- "The survey submitted contains more than an acceptable minimum number of failed attention checks and is not considered a completed HIT."

#Approve everyone that didn't get rejected
mturk$Approve[is.na(mturk$Reject)] <- "x"

#write.excel(mturk)

# completed visual inspection of the 10 rejected responses 
df$WorkerId[which(df$code == 972295)] #clear typo for worker ID
mturk$Reject[mturk$code == 972295] <- NA
mturk$Approve[mturk$code == 972295] <- 'x'

df$WorkerId[which(df$code == 218539)] #clear typo for worker ID
mturk$Reject[mturk$code == 218539] <- NA
mturk$Approve[mturk$code == 218539] <- 'x'

df$WorkerId[which(df$code == 270535)] #clear typo for worker ID
mturk$Reject[mturk$code == 270535] <- NA
mturk$Approve[mturk$code == 270535] <- 'x'

#df$invalid[which(df$code == 208683)] #did not provide worker Id, but data seems valid

df[which(df$code == 607919),] #syntax says this person is missing too much data, visual inspection looks fine - worker Id is present twice - first participant did not complete most of it - this attempt looks fine
mturk$Reject[mturk$code == 607919] <- NA
mturk$Approve[mturk$code == 607919] <- 'x'

duplicates <- df$WorkerId[!is.na(df$WorkerId)] %>% duplicated()
df$WorkerId[duplicates]

mturk$Reject[which(mturk$WorkerId %in% df$WorkerId[duplicates])]
#all other duplicates are fine

write.excel(mturk)

#### 2. Reverse Coding and Scale Scores -------------------

#___2.1 NEO ---------------
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

#___2.2 ari - Affective Reactivity Index ---------------
df$ari <- rowMeans(df[paste('ari_',1:7,sep='')])

#___2.3 aus - Anger Upregulation Scale ---------------
# not currently scored
# variable names aus_1:aus_60

#___2.4 amii - Angry Mood Improvement Inventory ---------------
df$amii_control <- rowMeans(df[paste('amii_',	c(1,4,8,11,15,18,20,24),sep = '')])
df$amii_in <- rowMeans(df[paste('amii_',	c(3,	5,	6,	10,	13,	16,	17,	21),sep = '')])
df$amii_out <- rowMeans(df[paste('amii_',	c(2,	7,	9,	12,	14,	19,	22,	23),sep = '')])

#___2.5 bisbas - BIS/BAS ---------------

df[paste('r_bisbas_',1:24, sep='')] <- df[paste('bisbas_',1:24, sep='')]
df[paste('r_bisbas_',c(2,22),sep = '')]<-5-df[paste('bisbas_',c(2,22),sep = '')]

df$bis <- rowMeans(df[paste('r_bisbas_',c(2,8,13,16,19,22,24),sep='')])
df$bas <- rowMeans(df[paste('r_bisbas_',c(3,4,5,7,9,10,12,14,15,18,20,21,23),sep='')])
df$bas_drive <- rowMeans(df[paste('r_bisbas_',c(3,9,12,21),sep='')])
df$bas_funseek <- rowMeans(df[paste('r_bisbas_',c(5,10,15,20),sep='')])
df$bas_reward <- rowMeans(df[paste('r_bisbas_',c(4,7,14,18,23),sep='')])

#___2.6 bam - Brief Agitation Measure ---------------
df$bam <- rowMeans(df[paste('bam_',c(1,2,3),sep='')])

#___2.7 bite - Brief Irritability Test ---------------
df$bite <- rowMeans(df[paste('bite_',c(1,2,3,4,5),sep='')])

#___2.8 cesd - CES-D ---------------
df[paste('r_cesd_',1:20, sep='')] <- (df[paste('cesd_',1:20, sep='')]-1)
df[paste('r_cesd_',c(4,8,12,16),sep = '')]<-3-df[paste('r_cesd_',c(4,8,12,16),sep = '')]
df$cesd <- rowMeans(df[paste('r_cesd_',1:20,sep='')])

#___2.9 cars - Child Anger Rumination Scale ---------------
df$cars <- rowMeans(df[paste('cars_',1:19,sep='')])

#___2.10 crrs - Child Ruminative Response Scale ---------------
df$crrs <- rowMeans(df[paste('crrs_',1:13,sep='')])

#___2.11 meaq - Experiential Avoidance ---------------
df$meaq_behav <- rowMeans(df[paste('meaq_',c(1,	4,	6,	8,	10,	12,	15,	17,	19,	21,	23),sep='')])
df$meaq_distress <- rowMeans(df[paste('meaq_',c(2,	3,	5,	7,	9,	11,	13,	14,	16,	18,	20,	22,	24),sep='')])

#___2.12 promis.a - PROMIS Anger ---------------

#___2.13 rpa - Reactive and Proactive Aggression ---------------

#___2.14 r.pos.aff - Responses to Positive Affect ---------------

#___2.15 supps - Short UPPS ---------------


