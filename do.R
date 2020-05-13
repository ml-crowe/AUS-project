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

fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)

#remove items with loadings <.3 on the first factor?
remove <- which(fa1$loadings[,1] <= .40) #removed 22 items
aus <- aus[-c(remove,67)] #also removing #67 because it is worded in a confusing mannner
# and it loads in a direction opposite of the way it was intended
item.content <- item.content[-c(remove,67),]

fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)
which(fa1$loadings[,1] <= .40) #no more items to remove

#remove additional items based on initial factor analysis
remove.2 <- c(9, 53, 12, 24, 22, 35, 58, 68, 55, 37)
aus <- select(aus, -paste('aus_', remove.2, sep =''))
item.content <- item.content[-which(item.content$number %in% remove.2),]

fa1<-fa(aus,nfactors=1,rotate = 'none',fm='pa', alpha = .05)
which(fa1$loadings[,1] <= .40) #no additional items to remove

#### ___2.2 Parallel analysis ------------
parallel <- fa.parallel(aus, fm = 'pa', fa = 'both', n.iter = 1000)
#Parallel analysis suggests that the number of factors =  6  and the number of components =  4

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
# MAP = 4 factors; BIC = 4 factors

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