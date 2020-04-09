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
df[,paste('r_neo_',c(1+(5*0:11)), sep = '')] %>% alpha #N = .85
df[,paste('r_neo_',c(2+(5*0:11)), sep = '')] %>% alpha #E = .85
df[,paste('r_neo_',c(3+(5*0:11)), sep = '')] %>% alpha #O = .63
df[,paste('r_neo_',c(4+(5*0:11)), sep = '')] %>% alpha #A = .73
df[,paste('r_neo_',c(5+(5*0:11)), sep = '')] %>% alpha #C = .87

#### 2. Factor Analyses -------------------
aus <- select(df, aus_1:aus_70)
parallel <- fa.parallel(aus, fm = 'pa', fa = 'both', n.iter = 100)
#Parallel analysis suggests that the number of factors =  6  and the number of components =  4 
