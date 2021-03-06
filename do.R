#### 1. Descriptives and Alphas -------------------

#___1.1 Demographics ----------
df$sex %>% table
df$ethnicity %>% table
df$race %>% table
df$marital %>% table
df$ever_psyc %>% table
df$current_psyc %>% table
df$device %>% table

#___1.2 NEO ----------
df[,paste('r_neo_',c(1+(5*0:11)), sep = '')] %>% alpha #N = .86
df[,paste('r_neo_',c(2+(5*0:11)), sep = '')] %>% alpha #E = .85
df[,paste('r_neo_',c(3+(5*0:11)), sep = '')] %>% alpha #O = .65
df[,paste('r_neo_',c(4+(5*0:11)), sep = '')] %>% alpha #A = .76
df[,paste('r_neo_',c(5+(5*0:11)), sep = '')] %>% alpha #C = .87

#___1.3 ari ---------------
df[paste('ari_',1:7,sep='')] %>% alpha # .90

#___1.4 AUS ---------------
# not currently scored
# variable names aus_1:aus_70

#___1.5 amii_control; _in; _out ---------------
df[paste('amii_',	c(1,4,8,11,15,18,20,24),sep = '')] %>% alpha # Control = .85
df[paste('amii_',	c(3,5,6,10,13,16,17,21),sep = '')] %>% alpha # In = .83
df[paste('amii_',	c(2,7,9,12,14,19,22,23),sep = '')] %>% alpha # Out = .83

#___1.6 bis; bas; bas_drive; _funseek; _reward ---------------

df[paste('r_bisbas_',c(2,8,13,16,19,22,24),sep='')] %>% alpha #BIS = .83
df[paste('r_bisbas_',c(3,4,5,7,9,10,12,14,15,18,20,21,23),sep='')] %>% alpha # BAS = .85
df[paste('r_bisbas_',c(3,9,12,21),sep='')] %>% alpha # Drive = .83
df[paste('r_bisbas_',c(5,10,15,20),sep='')] %>% alpha # Fun seek = .73
df[paste('r_bisbas_',c(4,7,14,18,23),sep='')] %>% alpha # Reward = .79

#___1.7 bam ---------------
df[paste('bam_',c(1,2,3),sep='')] %>% alpha # .92

#___1.8 bite ---------------
df[paste('bite_',c(1,2,3,4,5),sep='')] %>% alpha # .92

#___1.9 cesd ---------------
df[paste('r_cesd_',1:20,sep='')] %>% alpha #.94

#___1.10 cars ---------------
df[paste('cars_',1:19,sep='')] %>% alpha # .95

#___1.11 crrs ---------------
df[paste('crrs_',1:13,sep='')] %>% alpha # .93

#___1.12 meaq_behav; meaq_distress ---------------
df[paste('meaq_',c(1,	4,	6,	8,	10,	12,	15,	17,	19,	21,	23),sep='')] %>% alpha # Behavior = .93
df[paste('meaq_',c(2,	3,	5,	7,	9,	11,	13,	14,	16,	18,	20,	22,	24),sep='')] %>% alpha # Distress = .92

#___1.13 promis.a ---------------
df[paste('promis.a_',1:5,sep='')] %>% alpha # .93

#___1.14 rpa.proact; rpa.react ---------------
df[paste('rpa_',c(2,	4,	6,	9,	10,	12,	15,	17,	18,	20,	21,	23),sep='')] %>% alpha # proactive = .94

df[paste('rpa_',c(1,	3,	5,	7,	8,	11,	13,	14,	16,	19,	22),sep='')] %>% alpha # reactive = .86

#___1.15 r.pos.aff.efr; .damp; .sfr ---------------
df[paste('r.pos.aff_',1:5,sep='')] %>% alpha #efr = .86
df[paste('r.pos.aff_',6:13,sep='')] %>% alpha #damp = .87
df[paste('r.pos.aff_',14:17,sep='')] %>% alpha #sfr = .88

#___1.16 supps.lack.persev; .lack.premed; .neg.urg; .pos.urg; .sens.seek------
df[paste('r_supps_',c(1,4,7,11),sep='')] %>% alpha #lack perseverence = .77
df[paste('r_supps_',c(2,5,12,19),sep='')] %>% alpha #lack premed = .80
df[paste('r_supps_',c(6,8,13,15),sep='')] %>% alpha #negative urgency = .85 
df[paste('r_supps_',c(3,10,17,20),sep='')] %>% alpha #positive urgency = .83
df[paste('r_supps_',c(9,14,16,18),sep='')] %>% alpha #sensation seeking = .76

#### 2. AUS analyses -------------------
aus <- select(df, aus_1:aus_70)

#### ___2.1 Item level analyses ------------
response.dist <- sapply(1:length(aus), function(x){
  table(aus[,x])
}) %>% t()
#look at count for endorse (4+5) and deny (1+2)

#### ______2.1.1 Round 1 removal ------------
make.nar_cors(aus, .65) %>% 
  rownames_to_column(var = 'var') %>% 
  filter_at(vars(starts_with('aus')),any_vars(. > .65))

# items 1, 6, and 52 correlate at greater than .65

aus <- select(aus, -c(aus_6, aus_52))
item.content <- filter(item.content, !number %in% c(6, 52))


fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)

#remove items with loadings <.3 on the first factor?
remove <- which(fa1$loadings[,1] <= .40) #removed 22 items
aus <- aus[-c(remove)]
item.content <- item.content[-c(remove),]
aus <- select(aus, -aus_67)#also removing #67 because it is worded in a confusing mannner
# and it loads in a direction opposite of the way it was intended
item.content <- filter(item.content, number != 67)

fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)
which(fa1$loadings[,1] <= .40) #no more items to remove

#### ______2.1.2 Round 2 removal: to 34 items ------------
#remove additional items based on initial factor analysis
remove.2 <- c(33, 53, 12, 35, 58, 68, 9, 22, 55, 37, 24) 

aus <- select(aus, -paste('aus_', remove.2, sep =''))
item.content <- item.content[-which(item.content$number %in% remove.2),]

fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)
which(fa1$loadings[,1] <= .40) #no additional items to remove

#### _________i. Mokken analysis  -----------------------
# Indicates one scale
mokken.aus <- aus[complete.cases(aus),] %>% data.frame %>% 
  aisp()

#### ______2.1.3 Round 3 removal: to 24 items ------------
#remove.3 <- c(3, 28, 27, 29, 31, 64, 41, 46, 21, 62)
aus <- select(aus, paste('aus_', c(2, 18, 1, 4, 5, 11,
              65,34,39,29,31,36,
              61,50,59,51,60,62,
              17,8,10,7,63,66), sep =''))


#aus <- select(aus, -paste('aus_', remove.3, sep =''))
#item.content <- item.content[which(item.content$number %in% remove.3),]

item.content <- sapply(c(2, 18, 1, 4, 5, 11,
         65,34,39,29,31,36,
         61,50,59,51,60,62,
         17,8,10,7,63,66),
       function(x){
         which(item.content$number == x)
       }) %>% 
  slice(item.content,.)

which(fa1$loadings[,1] <= .40) #no more items to remove

#### ___2.2 Parallel analysis ------------
parallel <- fa.parallel(aus, fm = 'pa', fa = 'both', n.iter = 1000)
#Parallel analysis suggests that the number of factors =  4  and the number of components =  3

data.frame('Factors' = (1:15),
           'Eigenvalues' = parallel$pc.values[1:15],
           'Sim_avg' = parallel$pc.sim[1:15],
           'Sim_95' = (parallel$values %>% 
             data.frame %>% 
             select(CSim1:CSim15) %>% 
             sapply(function(x){
               quantile(x, .95)
             })),
           'one' = rep(1,15))  %>% write.excel(row.names = F, col.names = F)

#### ___2.3 MAP analysis ------------
map <- vss(aus, n = 10, rotate = 'promax', fm = 'pa')
# MAP = 3 factors; BIC = 4 factors

#### ___2.4 Factor Analyses ------------
#these all use regression to calculate factor scores
fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05) 
fa2<-fa(aus,nfactors=2,rotate = 'promax',fm='pa', alpha = .05)
fa3<-fa(aus,nfactors=3,rotate = 'promax',fm='pa', alpha = .05)
fa4<-fa(aus,nfactors=4,rotate = 'promax',fm='pa', alpha = .05)
fa5<-fa(aus,nfactors=5,rotate = 'promax',fm='pa', alpha = .05)
fa6<-fa(aus,nfactors=6,rotate = 'promax',fm='pa', alpha = .05)
fa7<-fa(aus,nfactors=7,rotate = 'promax',fm='pa', alpha = .05)


#### ______2.4.1 Fit and Variance Accounted for ------------
compiled.fit <- data.frame(
  'VarAccounted' = rbind(
    fa1$Vaccounted['Proportion Var',],
    fa2$Vaccounted['Cumulative Var',2],
    fa3$Vaccounted['Cumulative Var',3],
    fa4$Vaccounted['Cumulative Var',4],
    fa5$Vaccounted['Cumulative Var',5],
    fa6$Vaccounted['Cumulative Var',6],
    fa7$Vaccounted['Cumulative Var',7]
    ),
  rbind(
    fa1$RMSEA[1:3],
    fa2$RMSEA[1:3],
    fa3$RMSEA[1:3],
    fa4$RMSEA[1:3],
    fa5$RMSEA[1:3],
    fa6$RMSEA[1:3],
    fa7$RMSEA[1:3]
  ),
  'MAP' = map$map[1:7],
  'BIC' = map$vss.stats$BIC[1:7]
)

compiled.fit$VarDiff <- c(NA,
      sapply(2:length(compiled.fit$VarAccounted), function(x){
              compiled.fit$VarAccounted[x] - compiled.fit$VarAccounted[x-1]
            }))

compiled.fit <- select(compiled.fit, VarAccounted, VarDiff, everything())
compiled.fit %>% write.excel(row.names = F, col.names = F)
#compiled.fit %>% write.excel()


#### ______2.4.2 Factor Loadings (Structure Matrix) ------------
# Structure loading matrix = pattern matrix X factor intercorrelation matrix)
fastructuresdf<-data.frame(fa1$Structure[,1],
                           fa2$Structure[,1:2],
                           fa3$Structure[,1:3],
                           fa4$Structure[,1:4],
                           fa5$Structure[,1:5],
                           fa6$Structure[,1:6],
                           fa7$Structure[,1:7])

names(fastructuresdf)<-c('fa1',
                         paste('fa2_',colnames(fa2$loadings),sep=''),
                         paste('fa3_',colnames(fa3$loadings),sep=''),
                         paste('fa4_',colnames(fa4$loadings),sep=''),
                         paste('fa5_',colnames(fa5$loadings),sep=''),
                         paste('fa6_',colnames(fa6$loadings),sep=''),
                         paste('fa7_',colnames(fa7$loadings),sep=''))



#fastructuresdf<-fastructuresdf[,c(order(names(fastructuresdf)))]
#didn't want to reorder because it could result in confusion down the line
#when comparing findings to those from R output
names(fastructuresdf)<-paste('F',c(1,2,2,3,3,3,rep(4,4),rep(5,5),rep(6,6),rep(7,7)),'.',c(1,1:2,1:3,1:4,1:5,1:6,1:7),sep = "")

write.excel(data.frame(item.content, fastructuresdf), row.names = F, col.names = F)

#### ______2.4.3 Factor Loadings (Pattern matrix) ------------
faloadingsdf<-data.frame(fa1$loadings[,1],
                           fa2$loadings[,1:2],
                           fa3$loadings[,1:3],
                           fa4$loadings[,1:4],
                           fa5$loadings[,1:5],
                           fa6$loadings[,1:6],
                           fa7$loadings[,1:7])

names(faloadingsdf)<-c('fa1',
                         paste('fa2_',colnames(fa2$loadings),sep=''),
                         paste('fa3_',colnames(fa3$loadings),sep=''),
                         paste('fa4_',colnames(fa4$loadings),sep=''),
                         paste('fa5_',colnames(fa5$loadings),sep=''),
                         paste('fa6_',colnames(fa6$loadings),sep=''),
                         paste('fa7_',colnames(fa7$loadings),sep=''))



#fastructuresdf<-fastructuresdf[,c(order(names(fastructuresdf)))]
#didn't want to reorder because it could result in confusion down the line
#when comparing findings to those from R output
names(faloadingsdf)<-paste('F',c(1,2,2,3,3,3,rep(4,4),rep(5,5),rep(6,6),rep(7,7)),'.',c(1,1:2,1:3,1:4,1:5,1:6,1:7),sep = "")

write.excel(data.frame(item.content, faloadingsdf), row.names = F, col.names = F)

#### ______2.4.4 Factor Correlations ------------

#### avoided reordering and just changed the name to ensure no issues


fa2$Phi %>% write.excel(row.names = F, col.names = F)
fa3$Phi %>% write.excel(row.names = F, col.names = F)
fa4$Phi %>% write.excel(row.names = F, col.names = F)
fa5$Phi %>% write.excel(row.names = F, col.names = F)
fa6$Phi %>% write.excel(row.names = F, col.names = F)
fa7$Phi %>% write.excel(row.names = F, col.names = F)

#### 3. Correlating factors with Criterions -------------------
#### ___3.1 Factor scores ------------
fascoresdf<-data.frame(fa1$scores,
                       fa2$scores,
                       fa3$scores,
                       fa4$scores,
                       fa5$scores,
                       fa6$scores,
                       fa7$scores)


names(fascoresdf)<-c('fa1',
                     paste('fa2_',colnames(fa2$scores),sep=''),
                     paste('fa3_',colnames(fa3$scores),sep=''),
                     paste('fa4_',colnames(fa4$scores),sep=''),
                     paste('fa5_',colnames(fa5$scores),sep=''),
                     paste('fa6_',colnames(fa6$scores),sep=''),
                     paste('fa7_',colnames(fa7$scores),sep=''))

### Save and name factor scores
#fascoresdf<-fascoresdf[,c(order(names(fascoresdf)))]
names(fascoresdf)<-paste('F',c(1,2,2,3,3,3,rep(4,4),rep(5,5),rep(6,6),rep(7,7)),'.',c(1,1:2,1:3,1:4,1:5,1:6,1:7),sep = "")

#### ___3.2 Correlations ------------

#### ______3.2.1 NEO  ------------
data.frame(df[c('n',varlist(df,pattern = '^n_'),
                'e',varlist(df,pattern = '^e_'),
                'o',varlist(df,pattern = '^o_'),
                'a',varlist(df,pattern = '^a_'),
                'c',varlist(df,pattern = '^c_'))],
           fascoresdf) %>% 
  r_table(., with = c(names(fascoresdf))) %>% .$cors %>% write.excel(.,row.names = F, col.names = F)

#### ______3.2.2 Other criteria  ------------
data.frame(
  select(df, ari, 
         amii_control, amii_in, amii_out,
         bis, bas, starts_with('bas_'),
         bam, bite, cesd, cars, crrs, 
         meaq_behav, meaq_distress, promis.a, 
         rpa.proact, rpa.react, 
         starts_with('r.pos.aff.'), 
         starts_with('supps.')),
  fascoresdf
) %>% 
  r_table(., with = c(names(fascoresdf))) %>% .$cors %>% write.excel(.,row.names = F, col.names = F)

#### 4. GRM analyses -------------------
#aus.f1 <- select(aus, paste('aus_', c(2, 3, 18, 1, 4, 5, 28, 11, 27), sep = ''))
aus.f1 <- select(aus, paste('aus_', c(2, 18, 1, 4, 5, 11), sep = ''))

#aus.f2 <- select(aus, paste('aus_', c(65,34,39,29,31,64,41,56,36,38), sep = ''))
#aus.f2 <- select(aus, paste('aus_', c(65,34,39,29,31,64,41,36,38), sep = ''))
# item 56 shows misfit
#aus.f2 <- select(aus, paste('aus_', c(65,34,39,29,31,64,41,36), sep = ''))
# item 38 shows misfit
aus.f2 <- select(aus, paste('aus_', c(65,34,39,29,31,36), sep = ''))

# below combo is no good.
#aus.f2 <- select(aus, paste('aus_', c(65,34,39,56,36,38), sep = ''))

#aus.f3 <- select(aus, paste('aus_', c(61,50,46,59,23,51,21,60,62), sep = ''))
#
# Items 23 and 21 seem to be a different construct - something like comfort with the anger of others
#aus.f3 <- select(aus, paste('aus_', c(61,50,46,59,51,60,62), sep = ''))
# remove 46 due to low information
aus.f3 <- select(aus, paste('aus_', c(61,50,59,51,60,62), sep = ''))

#below is initial one - don't use
#aus.f3 <- select(aus, paste('aus_', c(61,50,59,23,51,60), sep = ''))

aus.f4 <- select(aus, paste('aus_', c(17,8,10,7,63,66), sep = ''))

#aus.grm <- irt(aus, 'graded', 4)
#### ___4.1 Factor 1 ------------
aus.f1.grm.model<-mirt(aus.f1,1,'graded', technical = list(removeEmptyRows = TRUE))
aus.f1.m2 <-m2.stats(aus.f1.grm.model,1,'graded')
aus.f1.coefs<-coef(aus.f1.grm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
aus.f1.items<-itemfit(aus.f1.grm.model,simplify=TRUE) #save item fit statistics
aus.f1.grm <- list('model' = aus.f1.grm.model,
                   'm2' = "too few degrees of freedom",
                   'coefs' = aus.f1.coefs,
                   'item.fit' = aus.f1.items)
rm(aus.f1.grm.model,aus.f1.coefs,aus.f1.items)

aus.f1.grm$model %>% summary()
aus.f1.grm$m2
aus.f1.grm$coefs
aus.f1.grm$item.fit #no problems with item fit

plot(aus.f1.grm$model,type='info',theta_lim=c(-4,4))

plot(aus.f1.grm$model,type='infotrace', theta_lim = c(-4,4), main = 'Factor 1 Item Info')

# item 2
itemplot(aus.f1.grm$model,1, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[1])

# item 18
itemplot(aus.f1.grm$model,2, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[2])

# item 1
itemplot(aus.f1.grm$model,3, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[3])

# item 4
itemplot(aus.f1.grm$model,4, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[4])

# item 5
itemplot(aus.f1.grm$model,5, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[5])

# item 11
itemplot(aus.f1.grm$model,6, 'info', theta_lim = c(-4,4), ylim = c(0,2.5), main = names(aus.f1)[6])

#identifying items with the broadest coverage
data.frame(aus.f1.grm$coefs$items[,c('a','b1','b4')]) %>% 
  mutate(width = b4-b1) %>%
  mutate(info = (sapply(1:length(aus.f1), function(x){
    areainfo(aus.f1.grm$model,c(-4,4), which.items=x)
  }, simplify = T) %>% .['Info',] %>% unlist())) %>% 
  mutate(names = names(aus.f1)) %>% 
  mutate(item = 1:length(aus.f1)) %>%
  select(item, names, everything()) %>% 
  arrange(desc(info))
write.excel(row.names = F)

# extract item information at various levels of theta
extract.item(aus.f1.grm$model,1) %>% 
  iteminfo(seq(-4,4,by=.1))

#### ___4.2 Factor 2 ------------
aus.f2.grm.model<-mirt(aus.f2,1,'graded', technical = list(removeEmptyRows = TRUE))
aus.f2.m2 <-m2.stats(aus.f2.grm.model,1,'graded')
aus.f2.coefs<-coef(aus.f2.grm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
aus.f2.items<-itemfit(aus.f2.grm.model,simplify=TRUE, na.rm = T) #save item fit statistics
aus.f2.grm <- list('model' = aus.f2.grm.model,
                   'm2' = "too few degrees of freedom",
                   'coefs' = aus.f2.coefs,
                   'item.fit' = aus.f2.items)
rm(aus.f2.grm.model,aus.f2.m2,aus.f2.coefs,aus.f2.items)

aus.f2.grm$model %>% summary()
aus.f2.grm$m2 # too few degrees of freedom
aus.f2.grm$coefs
aus.f2.grm$item.fit #acceptable item level fit
#1) item 56 is misfit
#2) item 38 is misfit
#3) item fit is acceptable
#4) drop 64 and 41 because they have the least information

itemfit(aus.f2.grm$model,simplify=TRUE, na.rm = T, empirical.plot = 8)
itemfit(aus.f2.grm$model,simplify=TRUE, na.rm = T, fit_stats = 'PV_Q1')

plot(aus.f2.grm$model,type='infotrace', theta_lim = c(-4,4), main = 'Factor 2 Item Info')

data.frame(aus.f2.grm$coefs$items[,c('a','b1','b4')]) %>% 
  mutate(width = b4-b1) %>%
  mutate(info = (sapply(1:length(aus.f2), function(x){
    areainfo(aus.f2.grm$model,c(-4,4), which.items=x)
  }, simplify = T) %>% .['Info',] %>% unlist())) %>% 
  mutate(names = names(aus.f2)) %>% 
  mutate(item = 1:length(aus.f2)) %>%
  select(item, names, everything()) %>% 
  arrange(desc(info))
  write.excel(row.names = F)

#### ___4.3 Factor 3 ------------
aus.f3.grm.model<-mirt(aus.f3,1,'graded', technical = list(removeEmptyRows = TRUE))
aus.f3.m2 <-m2.stats(aus.f3.grm.model,1,'graded')
aus.f3.coefs<-coef(aus.f3.grm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
aus.f3.items<-itemfit(aus.f3.grm.model,simplify=TRUE, na.rm = T) #save item fit statistics
aus.f3.grm <- list('model' = aus.f3.grm.model,
                   'm2' = 'too frew degrees of freedom',
                   'coefs' = aus.f3.coefs,
                   'item.fit' = aus.f3.items)
rm(aus.f3.grm.model,aus.f3.coefs,aus.f3.items)

aus.f3.grm$model %>% summary()
aus.f3.grm$m2
aus.f3.grm$coefs
aus.f3.grm$item.fit #acceptable item fit

#w/ 6 items:
#item fit problems for item 23 and item 60

itemfit(aus.f3.grm$model,simplify=TRUE, na.rm = T, fit_stats = 'PV_Q1')
itemfit(aus.f3.grm$model,simplify=TRUE, na.rm = T, empirical.plot = 4)
itemfit(aus.f3.grm$model,simplify=TRUE, na.rm = T, empirical.plot = 6)


plot(aus.f3.grm$model,type='infotrace', theta_lim = c(-4,4), main = 'Factor 3 Item Info')

data.frame(aus.f3.grm$coefs$items[,c('a','b1','b4')]) %>% 
  mutate(width = b4-b1) %>%
  mutate(info = (sapply(1:length(aus.f3), function(x){
    areainfo(aus.f3.grm$model,c(-4,4), which.items=x)
  }, simplify = T) %>% .['Info',] %>% unlist())) %>% 
  mutate(names = names(aus.f3)) %>% 
  mutate(item = 1:length(aus.f3)) %>%
  select(item, names, everything()) %>% 
  arrange(desc(info))
  write.excel(row.names = F)

#### ___4.4 Factor 4 ------------
aus.f4.grm.model<-mirt(aus.f4,1,'graded', technical = list(removeEmptyRows = TRUE))
aus.f4.m2 <-m2.stats(aus.f4.grm.model,1,'graded')
aus.f4.coefs<-coef(aus.f4.grm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
aus.f4.items<-itemfit(aus.f4.grm.model,simplify=TRUE, na.rm = T) #save item fit statistics
aus.f4.grm <- list('model' = aus.f4.grm.model,
                   'm2' = 'too frew degrees of freedom',
                   'coefs' = aus.f4.coefs,
                   'item.fit' = aus.f4.items)
rm(aus.f4.grm.model,aus.f4.coefs,aus.f4.items)

aus.f4.grm$model
aus.f4.grm$m2
aus.f4.grm$coefs
aus.f4.grm$item.fit

plot(aus.f4.grm$model,type='infotrace', theta_lim = c(-4,4), main = 'Factor 4 Item Info')

data.frame(aus.f4.grm$coefs$items[,c('a','b1','b4')]) %>% 
  mutate(width = b4-b1) %>%
  mutate(info = (sapply(1:length(aus.f4), function(x){
    areainfo(aus.f4.grm$model,c(-4,4), which.items=x)
  }, simplify = T) %>% .['Info',] %>% unlist())) %>% 
  mutate(names = names(aus.f4)) %>% 
  mutate(item = 1:length(aus.f4)) %>%
  select(item, names, everything()) %>% 
  write.excel(row.names = F)

#### ___4.5 Absolute Model fit - need to update factor 1 -----
paste.modfit(aus.f1,aus.f1.grm$model) #good model data fit
paste.modfit(aus.f2,aus.f2.grm$model) #good model data fit
paste.modfit(aus.f3,aus.f3.grm$model) #good fit
paste.modfit(aus.f4,aus.f4.grm$model) #good fit

#### ___4.6 Test reliability functions -----
plot(aus.f1.grm$model, type = 'rxx', theta = c(-3,3))

testinfo(aus.f1.grm$model, seq(-3,3,.1)) %>% write.excel(row.names = F, col.names = F)
testinfo(aus.f2.grm$model, seq(-3,3,.1)) %>% write.excel(row.names = F, col.names = F)
testinfo(aus.f3.grm$model, seq(-3,3,.1)) %>% write.excel(row.names = F, col.names = F)
testinfo(aus.f4.grm$model, seq(-3,3,.1)) %>% write.excel(row.names = F, col.names = F)

#### 5. GPCM analyses -------------------
#### ___A. Factor 1 ------------
f1.gpcm.model<-mirt(aus.f1,1,'gpcmIRT', technical = list(removeEmptyRows = TRUE))
f1.gpcm.m2 <-m2.stats(f1.gpcm.model,1,'gpcmIRT')
f1.gpcm.coefs<-coef(f1.gpcm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
f1.gpcm.items<-itemfit(f1.gpcm.model,simplify=TRUE) #save item fit statistics
f1.gpcm <- list('model' = f1.gpcm.model,
                   'm2' = "too few degrees of freedom",
                   'coefs' = f1.gpcm.coefs,
                   'item.fit' = f1.gpcm.items)
rm(f1.gpcm.model,f1.gpcm.coefs,f1.gpcm.items)

f1.gpcm$model %>% summary()
f1.gpcm$m2
f1.gpcm$coefs
f1.gpcm$item.fit #no problems with item fit

f1.gpcm$model@Fit$BIC
aus.f1.grm$model@Fit$BIC #GRM is better

f1.gpcm$model@Fit$AIC
aus.f1.grm$model@Fit$AIC #GRM is better

#### ___B. Factor 2 ------------
f2.gpcm.model<-mirt(aus.f2,1,'gpcmIRT', technical = list(removeEmptyRows = TRUE))
f2.gpcm.m2 <-m2.stats(f2.gpcm.model,1,'gpcmIRT')
f2.gpcm.coefs<-coef(f2.gpcm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
f2.gpcm.items<-itemfit(f2.gpcm.model,simplify=TRUE, na.rm = T) #save item fit statistics
f2.gpcm <- list('model' = f2.gpcm.model,
                'm2' = f2.gpcm.m2,
                'coefs' = f2.gpcm.coefs,
                'item.fit' = f2.gpcm.items)
rm(f2.gpcm.model,f2.gpcm.coefs,f2.gpcm.items)

f2.gpcm$model %>% summary()
f2.gpcm$m2
f2.gpcm$coefs
f2.gpcm$item.fit
#1) item 56 needs to be dropped
#2) item 38 needs to be dropped
#3) item fit is acceptable

f2.gpcm$model@Fit$BIC
aus.f2.grm$model@Fit$BIC #GRM is better

f2.gpcm$model@Fit$AIC
aus.f2.grm$model@Fit$AIC #GRM is better

#### ___C. Factor 3 ------------
f3.gpcm.model<-mirt(aus.f3,1,'gpcmIRT', technical = list(removeEmptyRows = TRUE))
f3.gpcm.m2 <-m2.stats(f3.gpcm.model,1,'gpcmIRT')
f3.gpcm.coefs<-coef(f3.gpcm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
f3.gpcm.items<-itemfit(f3.gpcm.model,simplify=TRUE, na.rm = T) #save item fit statistics
f3.gpcm <- list('model' = f3.gpcm.model,
                'm2' = "too few degrees of freedom",
                'coefs' = f3.gpcm.coefs,
                'item.fit' = f3.gpcm.items)
rm(f3.gpcm.model,f3.gpcm.coefs,f3.gpcm.items)

f3.gpcm$model %>% summary()
f3.gpcm$m2
f3.gpcm$coefs
f3.gpcm$item.fit

f3.gpcm$model@Fit$BIC
aus.f3.grm$model@Fit$BIC #GRM is better

f3.gpcm$model@Fit$AIC
aus.f3.grm$model@Fit$AIC #GRM is better

#### ___D. Factor 4 ------------
f4.gpcm.model<-mirt(aus.f4,1,'gpcmIRT', technical = list(removeEmptyRows = TRUE))
f4.gpcm.m2 <-m2.stats(f4.gpcm.model,1,'gpcmIRT')
f4.gpcm.coefs<-coef(f4.gpcm.model, IRTpars=TRUE, simplify=TRUE) #save item parameters
f4.gpcm.items<-itemfit(f4.gpcm.model,simplify=TRUE, na.rm = T) #save item fit statistics
f4.gpcm <- list('model' = f4.gpcm.model,
                'm2' = "too few degrees of freedom",
                'coefs' = f4.gpcm.coefs,
                'item.fit' = f4.gpcm.items)
rm(f4.gpcm.model,f4.gpcm.coefs,f4.gpcm.items)

f4.gpcm$model %>% summary()
f4.gpcm$m2
f4.gpcm$coefs
f4.gpcm$item.fit

f4.gpcm$model@Fit$BIC
aus.f4.grm$model@Fit$BIC #GRM is better

f4.gpcm$model@Fit$AIC
aus.f4.grm$model@Fit$AIC #GRM is better

#### 6. EFA - 24 item set -------------------


#### 7. Multidimensional IRT Attempt (GRM) -------------------
comp_aus <- aus[complete.cases(aus),]
aus.grm.model<-mirt(comp_aus,4,'graded', ncpus = detectCores(), technical = list(removeEmptyRows = TRUE))
aus.grm.m2 <-m2.stats(aus.grm.model,1,'graded', QMC = T)
aus.coefs<-coef(aus.grm.model, simplify=TRUE) #save item parameters
aus.items<-itemfit(aus.grm.model,simplify=TRUE, QMC = T) #save item fit statistics

summary(aus.grm.model) #factor loadings are nonsense.

aus.multi.grm <- list('model' = aus.grm.model,
                   'm2' = aus.grm.m2,
                   'coefs' = aus.coefs,
                   'item.fit' = aus.items)


