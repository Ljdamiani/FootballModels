
####################################################

# ____________ # Probabilities  # ________________ #

# ___________ Author: Leonardo Damiani ___________ # 

####################################################

# Packages
library(qs2)
library(purrr)
library(measures)
library(mlr3measures)

###############################
#_______     MODELS    _______#
###############################

# ENG
matches_ENG <- qs_read(paste0("models//ENG_matches.qs2"))
results_ENG_dc <- qs_read(paste0("models//ENG_dc.qs2"))
# results_ENG_bivpois <- qs_read(paste0("models//ENG_bivpois.qs2"))

models_ENG_no_var <- qs_read(paste0("models//ENG_no_var.qs2"))
models_ENG_ma_score <- qs_read(paste0("models//ENG_ma_score.qs2"))
models_ENG_xg <- qs_read(paste0("models//ENG_xg.qs2"))
models_ENG_shots <- qs_read(paste0("models//ENG_shots.qs2"))

# ESP
models_ESP_no_var <- qs_read(paste0("models//ESP_no_var.qs2"))
models_ESP_ma_score <- qs_read(paste0("models//ESP_ma_score.qs2"))

# ITA


# models_ESP_ma_score2 <- qs_read(paste0("models//ESP_ma_score2.qs2"))
# teste <- c(models_ESP_ma_score2, models_ESP_ma_score[40:length(models_ESP_ma_score)])
# qs_save(teste, paste0("models//ESP_ma_score.qs2"))


################################
#_______   JOIN MODELS  _______#
################################

cols = c("Date", "Home", "Away")

# ENG
matches_ENG_models <- matches_ENG %>%
  left_join(results_ENG_dc %>% rename_with(~ paste0(.x, "_dc"), -all_of(cols)), by = cols) %>%
  
  # No X
  left_join(map_dfr(models_ENG_no_var, ~ .x$prob_ind) %>%
              mutate(copula = map_vec(models_ENG_no_var, ~ .x$copula)) %>%
              rename_with(~ paste0(.x, "_novar_ind"), -all_of(cols)), by = cols) %>%
  
  left_join(map_dfr(models_ENG_no_var, ~ .x$prob_cop) %>% 
              rename_with(~ paste0(.x, "_novar_cop"), -all_of(cols)), by = cols)



###############################
#_______   PARAMETERS  _______#
###############################



###############################
#_______     COPULA    _______#
###############################



###############################
#_______     PROBS     _______#
###############################

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



