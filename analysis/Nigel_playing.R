wide <- readRDS("~/W&C Simulation Dropbox/Nigel Bean/TSH_2019/data/20191207-01_cleaning-data.rds")

N_YY <- nrow(wide %>% filter(FT4_Sig=="Y",TSH_Sig=="Y"))
N_YN <- nrow(wide %>% filter(FT4_Sig=="Y",TSH_Sig=="N"))
N_NN <- nrow(wide %>% filter(FT4_Sig=="N",TSH_Sig=="N"))
N_NY <- nrow(wide %>% filter(FT4_Sig=="N",TSH_Sig=="Y"))

N = matrix(  c(N_YY, N_YN, N_NY, N_NN),  nrow=2,  ncol=2,  byrow = TRUE)  
mcnemar.test(N, correct=FALSE) 
# Seems to be the right test given there are paired underlying results see https://rcompanion.org/rcompanion/b_07.html

N_YY <- nrow(wide %>% filter(FT4_Sig=="Y",T3_Sig=="Y"))
N_YN <- nrow(wide %>% filter(FT4_Sig=="Y",T3_Sig=="N"))
N_NN <- nrow(wide %>% filter(FT4_Sig=="N",T3_Sig=="N"))
N_NY <- nrow(wide %>% filter(FT4_Sig=="N",T3_Sig=="Y"))

N = matrix(  c(N_YY, N_YN, N_NY, N_NN),  nrow=2,  ncol=2,  byrow = TRUE)  
mcnemar.test(N, correct=FALSE) 
# Seems to be the right test given there are paired underlying results see https://rcompanion.org/rcompanion/b_07.html

