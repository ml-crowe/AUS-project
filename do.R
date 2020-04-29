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

#___1.3 ARI ---------------
df[paste('ari_',1:7,sep='')] %>% alpha # .90

#___1.4 AUS ---------------
# not currently scored
# variable names aus_1:aus_70

#___1.5 AMII ---------------
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

#___1.12 meaq ---------------
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

#### 2. Factor Analyses -------------------
aus <- select(df, aus_1:aus_70)
parallel <- fa.parallel(aus, fm = 'pa', fa = 'both', n.iter = 100)
#Parallel analysis suggests that the number of factors =  6  and the number of components =  4 
