
####################################################

# ____________ # Probabilities  # ________________ #

# ___________ Author: Leonardo Damiani ___________ # 

####################################################

# Packages
library(measures)
library(mlr3measures)

# Probs

##################################################################
#_______ Junta Rodadas _______##_______ Probs por Modelos _______#
##################################################################

rodadas = data.frame()
probs_I_semx = data.frame()
probs_I_semx_cop = data.frame()
probs_I = data.frame()
probs_I_cop = data.frame()
probs_log_semx = data.frame()
probs_log_semx_cop = data.frame()
probs_log = data.frame()
probs_log_cop = data.frame()
probs_mix = data.frame()
probs_mix_cop = data.frame()

wks = c(24, 25, 26, 27, 28, 29, 
        30, 31, 32, 33, 34, 35, 36, 37, 38)

for (i in 1:length(probs_rodada)){
  
  probs <- probs_rodada[[i]]
  
  # Rodada
  aux_rodada = probs$`Identidade Sem Cov`[[1]][, 1:4]
  aux_rodada['Wk'] = wks[i]
  
  rodadas = rbind(rodadas, aux_rodada)
  
  # Probs
  probs_I_semx = rbind(probs_I_semx, 
                       probs$`Identidade Sem Cov`[[1]][c('pVM', 'pE', 'pVV')])
  probs_I_semx_cop = rbind(probs_I_semx_cop, 
                           probs$`Identidade Sem Cov`[[2]][c('pVM', 'pE', 'pVV')])
  probs_I = rbind(probs_I, probs$Identidade[[1]][c('pVM', 'pE', 'pVV')])
  probs_I_cop = rbind(probs_I_cop, probs$Identidade[[2]][c('pVM', 'pE', 'pVV')])
  probs_log_semx = rbind(probs_log_semx, probs$`Log Sem Cov`[[1]][c('pVM', 'pE', 'pVV')])
  probs_log_semx_cop = rbind(probs_log_semx_cop, 
                             probs$`Log Sem Cov`[[2]][c('pVM', 'pE', 'pVV')])
  probs_log = rbind(probs_log, probs$Log[[1]][c('pVM', 'pE', 'pVV')])
  probs_log_cop = rbind(probs_log_cop, probs$Log[[2]][c('pVM', 'pE', 'pVV')])
  probs_mix = rbind(probs_mix, probs$Mix[[1]][c('pVM', 'pE', 'pVV')])
  probs_mix_cop = rbind(probs_mix_cop, probs$Mix[[2]][c('pVM', 'pE', 'pVV')])
  
}



# Resultados das Rodadas
resultado = ifelse(rodadas$GM > rodadas$GV, 'pVM',
            ifelse(rodadas$GM == rodadas$GV, 'pE', 'pVV'))

truth = factor(resultado, levels = c('pVM', 'pE', 'pVV'))
truth

table(truth)

#__________ Log Loss _____________#

loss <- c(
  Logloss(probs_I_semx, truth),
  Logloss(probs_I_semx_cop, truth),
  Logloss(probs_I, truth),
  Logloss(probs_I_cop, truth),
  Logloss(probs_log_semx, truth),
  Logloss(probs_log_semx_cop, truth),
  Logloss(probs_log, truth),
  Logloss(probs_log_cop, truth),
  Logloss(probs_mix, truth),
  Logloss(probs_mix_cop, truth)
)

#__________ Brier Score _____________#

BS <- c(
  mbrier(truth, as.matrix(probs_I_semx)),
  mbrier(truth, as.matrix(probs_I_semx_cop)),
  mbrier(truth, as.matrix(probs_I)),
  mbrier(truth, as.matrix(probs_I_cop)),
  mbrier(truth, as.matrix(probs_log_semx)),
  mbrier(truth, as.matrix(probs_log_semx_cop)),
  mbrier(truth, as.matrix(probs_log)),
  mbrier(truth, as.matrix(probs_log_cop)),
  mbrier(truth, as.matrix(probs_mix)),
  mbrier(truth, as.matrix(probs_mix_cop))
)

#__________ Match Result Score _____________#

probs_I_semx['rslt_est'] = ifelse((probs_I_semx$pVM > probs_I_semx$pE) & (probs_I_semx$pVM > probs_I_semx$pVV), 'pVM',
                           ifelse((probs_I_semx$pE > probs_I_semx$pVM) & (probs_I_semx$pE > probs_I_semx$pVV), 'pE', 'pVV'))

probs_I_semx_cop['rslt_est'] = ifelse((probs_I_semx_cop$pVM > probs_I_semx_cop$pE) & (probs_I_semx_cop$pVM > probs_I_semx_cop$pVV), 'pVM',
                               ifelse((probs_I_semx_cop$pE > probs_I_semx_cop$pVM) & (probs_I_semx_cop$pE > probs_I_semx_cop$pVV), 'pE', 'pVV'))

probs_I['rslt_est'] = ifelse((probs_I$pVM > probs_I$pE) & (probs_I$pVM > probs_I$pVV), 'pVM',
                      ifelse((probs_I$pE > probs_I$pVM) & (probs_I$pE > probs_I$pVV), 'pE', 'pVV'))

probs_I_cop['rslt_est'] = ifelse((probs_I_cop$pVM > probs_I_cop$pE) & (probs_I_cop$pVM > probs_I_cop$pVV), 'pVM',
                          ifelse((probs_I_cop$pE > probs_I_cop$pVM) & (probs_I_cop$pE > probs_I_cop$pVV), 'pE', 'pVV'))

probs_log_semx['rslt_est'] = ifelse((probs_log_semx$pVM > probs_log_semx$pE) & (probs_log_semx$pVM > probs_log_semx$pVV), 'pVM',
                                      ifelse((probs_log_semx$pE > probs_log_semx$pVM) & (probs_log_semx$pE > probs_log_semx$pVV), 'pE', 'pVV'))

probs_log_semx_cop['rslt_est'] = ifelse((probs_log_semx_cop$pVM > probs_log_semx_cop$pE) & (probs_log_semx_cop$pVM > probs_log_semx_cop$pVV), 'pVM',
                                      ifelse((probs_log_semx_cop$pE > probs_log_semx_cop$pVM) & (probs_log_semx_cop$pE > probs_log_semx_cop$pVV), 'pE', 'pVV'))

probs_log['rslt_est'] = ifelse((probs_log$pVM > probs_log$pE) & (probs_log$pVM > probs_log$pVV), 'pVM',
                                      ifelse((probs_log$pE > probs_log$pVM) & (probs_log$pE > probs_log$pVV), 'pE', 'pVV'))

probs_log_cop['rslt_est'] = ifelse((probs_log_cop$pVM > probs_log_cop$pE) & (probs_log_cop$pVM > probs_log_cop$pVV), 'pVM',
                                      ifelse((probs_log_cop$pE > probs_log_cop$pVM) & (probs_log_cop$pE > probs_log_cop$pVV), 'pE', 'pVV'))

probs_mix['rslt_est'] = ifelse((probs_mix$pVM > probs_mix$pE) & (probs_mix$pVM > probs_mix$pVV), 'pVM',
                                   ifelse((probs_mix$pE > probs_mix$pVM) & (probs_mix$pE > probs_mix$pVV), 'pE', 'pVV'))

probs_mix_cop['rslt_est'] = ifelse((probs_mix_cop$pVM > probs_mix_cop$pE) & (probs_mix_cop$pVM > probs_mix_cop$pVV), 'pVM',
                               ifelse((probs_mix_cop$pE > probs_mix_cop$pVM) & (probs_mix_cop$pE > probs_mix_cop$pVV), 'pE', 'pVV'))


# Loop MRS
n = nrow(probs_I)

MRS = c(
  sum(probs_I_semx['rslt_est'] == resultado)/n,
  sum(probs_I_semx_cop['rslt_est'] == resultado)/n,
  sum(probs_I['rslt_est'] == resultado)/n,
  sum(probs_I_cop['rslt_est'] == resultado)/n,
  sum(probs_log_semx['rslt_est'] == resultado)/n,
  sum(probs_log_semx_cop['rslt_est'] == resultado)/n,
  sum(probs_log['rslt_est'] == resultado)/n,
  sum(probs_log_cop['rslt_est'] == resultado)/n,
  sum(probs_mix['rslt_est'] == resultado)/n,
  sum(probs_mix_cop['rslt_est'] == resultado)/n
)


#__________ Data Frame da Perfomance _____________#

desempenho <-  data.frame(Modelos = c('Identidade Sem X', 'Identidade Sem X Cop',
                                      'Identidade', 'Identidade Cop',
                                      'Log Sem X', 'Log Sem X Cop',
                                      'Log', 'Log Cop',
                                      'MIX', 'MIX Cop'),
                          LogLoss = loss,
                          BS = BS,
                          MRS = MRS)



