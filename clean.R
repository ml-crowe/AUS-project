
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
mturk$reason <- NA
#Identify people whose ID's are not in the survey and reject them
bad.id <- !(mturk$WorkerId %in% intersect(df$WorkerId, mturk$WorkerId))
mturk$Reject[bad.id] <- 1
mturk$reason[bad.id] <- "Your Worker ID was not found in the survey. As such, your survey key code could not be authenticated."

#Reject people that don't have a matching survey code.
wrong.code <- !(mturk$WorkerId %in% intersect(select(df, WorkerId, code),select(mturk,WorkerId,code))$WorkerId)
mturk$Reject[wrong.code] <-1
mturk$reason[wrong.code] <- "The survey code entered into the HIT did not match the code provided by the survey or was left blank."







#Reject people that have too many NAs
survey$numNA [acceptablena] <- "The survey submitted contains more than an acceptable minimum number of missing values and is not considered a completed HIT. Sorry."

#Reject people that failed the attention check items.
survey.attncheck <- tbl_df(data.frame(score.multiple.choice(attncheck.correct, survey[attncheck], score = FALSE)))
survey$attntotal [attnfail] <- "The survey submitted contains more than an acceptable minimum number of failed attention checks and is not considered a completed HIT. Sorry."

#Create a smaller dataset containing the rejected people from the survey
survey.reject %
select(Reject, WorkerId)

#Merge the datasets
merged <- merge(mturk, survey.reject, by="WorkerId", all.x = TRUE)

#Merge the reject columns
merged <- within(merged, {
  Reject <- rep(NA, nrow(merged))
  ifelse (is.na(Reject.x), Reject <- Reject.y, Reject <- Reject.x)
})

#Approve everyone that didn't get rejected
merged$Approve[is.na(merged$Reject)] <- "x"

#Drop the extra columns
merged <- select(merged, -(Reject.x), -(Reject.y))

#Rejected Participants
Rejected % select(WorkerId, Reject, Answer.surveycode, AssignmentId)

#Approved Participants
Approved % select(WorkerId, AssignmentId)

#Save as a csv for uploading to mturk
write.csv(merged, file = “Upload_Mturk.csv”, row.names = FALSE, na = “”)

#Save a rejected csv for double checking
write.csv(Rejected, file = “Rejected_Participants.csv”, row.names = FALSE, na = “”)

#Save an approved csv
write.csv(Approved, file = “Approved_Participants.csv”, row.names = FALSE, na = “”)

# Identify individuals in df that don't provide Worker ID matching that from Amazon
# This isn't really necessary for approval concerns
setdiff(select(df, WorkerId, code),select(mturk,WorkerId,code)) 
