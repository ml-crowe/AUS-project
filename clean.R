
# Score Validation items ----

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

#Identify people whose ID's are not in the survey and reject them
bad.id <- !(mturk$WorkerId %in% intersect(df$WorkerId, mturk$WorkerId))
mturk$Reject[bad.id] <- "Your Worker ID was not found in the survey. Your survey key code could not be authenticated."

#Reject people that don't have a matching ID and survey code set.
wrong.code <- !(mturk$WorkerId %in% intersect(select(df, WorkerId, code),select(mturk,WorkerId,code))$WorkerId)
mturk$Reject[wrong.code] <- "The survey code entered into the HIT did not match the code provided by the survey or was left blank."

#identify responders who failed attention checks
attention.check <- mturk$WorkerId %in% 
  (select(df, c(WorkerId, virtue, infreq)) %>% 
  filter(virtue>2|infreq>3) %>% 
  select(WorkerId))$WorkerId
mturk$Reject[attention.check] <- "The survey submitted contains more than an acceptable minimum number of failed attention checks and is not considered a completed HIT."

#Reject people that have too many NAs
#length(select(df,WorkerId:virtue_8))/4 = 25% of items
excess.na <- mturk$WorkerId %in% 
  (select(df, WorkerId:virtue_8) %>% 
     apply(1, function(z) sum(is.na(z))) %>% 
     is_weakly_greater_than(90.25) %>% 
     df$WorkerId[.])
mturk$Reject[excess.na] <- "The survey submitted contains more than an acceptable minimum number of missing values and is not considered a completed HIT. Sorry."

#Approve everyone that didn't get rejected
mturk$Approve[is.na(mturk$Reject)] <- "x"
