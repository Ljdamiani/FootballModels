


library(dplyr)
library(readr)
library(lubridate)
odds <- read_csv("dados\\odds.csv")

# Filtro
odds <- odds %>%
  select(Date, HomeTeam, AwayTeam, 
         B365H, B365D, B365A, # Bet365
         BWH, BWD, BWA, # Bet&Win
         IWH, IWD, IWA, # Interwetten
         PSH, PSD, PSA, # Pinnacle
         WHH, WHD, WHA, # William Hill
         VCH, VCD, VCA, # VC Bet
         #MaxH, MaxD, MaxA, # Market Maximus 
         AvgH, AvgD, AvgA # Market Average
  )

# Renomeia alguns nomes dos times
odds$HomeTeam = ifelse(odds$HomeTeam == 'Leeds', 'Leeds United',
                       ifelse(odds$HomeTeam == 'Newcastle', 'Newcastle Utd',
                              ifelse(odds$HomeTeam == 'Leicester', 'Leicester City',
                                     ifelse(odds$HomeTeam == 'Man United', 'Manchester Utd', 
                                            ifelse(odds$HomeTeam == 'Man City', 'Manchester City',
                                                   ifelse(odds$HomeTeam == "Nott'm Forest", "Nott'ham Forest", odds$HomeTeam))))))

odds$AwayTeam = ifelse(odds$AwayTeam == 'Leeds', 'Leeds United',
                       ifelse(odds$AwayTeam == 'Newcastle', 'Newcastle Utd',
                              ifelse(odds$AwayTeam == 'Leicester', 'Leicester City',
                                     ifelse(odds$AwayTeam == 'Man United', 'Manchester Utd', 
                                            ifelse(odds$AwayTeam == 'Man City', 'Manchester City',
                                                   ifelse(odds$AwayTeam == "Nott'm Forest", "Nott'ham Forest", odds$AwayTeam))))))


# Médias e Probs Implicita
odds <- odds %>%
  mutate(OddH = rowMeans(select(., starts_with("B365H"), starts_with("BWH"), starts_with("IWH"), starts_with("PSH"), starts_with("WHH"), starts_with("VCH"))),
         OddE = rowMeans(select(., starts_with("B365D"), starts_with("BWD"), starts_with("IWD"), starts_with("PSD"), starts_with("WDD"), starts_with("VCD"))),
         OddA = rowMeans(select(., starts_with("B365A"), starts_with("BWA"), starts_with("IWA"), starts_with("PSA"), starts_with("WAA"), starts_with("VCA")))
  ) %>%
  mutate(
    ProbH = OddH^(-1),
    ProbE = OddE^(-1),
    ProbA = OddA^(-1)
  ) %>%
  select(Date, HomeTeam, AwayTeam, OddH, OddE, OddA, ProbH, ProbE, ProbA)

  
# Probs
probs_mix_cop = data.frame()

wks = c(24, 25, 26, 27, 28, 29, 
        30, 31, 32, 33, 34, 35, 36, 37, 38)

for (i in 1:length(probs_rodada)){
  
  probs <- probs_rodada[[i]]
  
  # Probs
  probs_mix_cop = rbind(probs_mix_cop, probs$Mix[[2]][c('M', 'GM', 'GV', 'V', 'pVM', 'pE', 'pVV')])
  
}

probs_mix_cop


# --------------------------------------#
#         MERGE         #
# --------------------------------------#

bet <- merge(probs_mix_cop, odds, by.x = c('M', 'V'), by.y = c('HomeTeam', 'AwayTeam'))
bet <- bet %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
  arrange(Date)

bet$Maior_Prob = NA
bet$Predito = NA
bet$aposta = NA
bet$odd = NA
threshold = 0

for (i in 1:nrow(bet)){
  
  aux = bet[i, ]
  probs = c('pVM' = aux$pVM, 'pE' = aux$pE, 'pVV' = aux$pVV)
  
  # Passo 1 - Resultado de Maior Prob
  prob = max(probs)
  bet$Maior_Prob[i] = prob
  
  # Passo 2 - Verifica se Vale a pena apostar
  nome = names(which(probs == prob))
  bet$Predito[i] = nome
  
  aposta = 0
  if (nome == 'pVM'){
    
    odd = aux$OddH
    aux2 = prob/aux$ProbH - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  } else if (nome == 'pE'){
    
    odd = aux$OddE
    aux2 = prob/aux$ProbE - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  } else{
    
    odd = aux$OddA
    aux2 = prob/aux$ProbA - 1
    if (aux2 > threshold){
      aposta = 1
    }
    
  }
  
  bet$aposta[i] = aposta
  bet$odd[i] = odd
  
}

bet$resultado = ifelse(bet$GM > bet$GV, 'pVM',
                ifelse(bet$GM == bet$GV, 'pE', 'pVV'))

table(bet$aposta)

# Somente os que devem ser apostados
bet1 = bet[bet$aposta == 1, ]
# sum(diag(table(bet1$Predito, bet1$resultado))) # acertos

nrow(bet1) # Número de apostas

qtd_apostada = 2 # Libra
qtd_apostada * nrow(bet1)

aux_bet = bet1 %>%
  mutate(odd_posicao = odd * qtd_apostada) %>%
  filter(Predito == resultado)

# Profit
sum(aux_bet$odd_posicao) - qtd_apostada * nrow(bet1)

# Return
(sum(aux_bet$odd_posicao) - (qtd_apostada * nrow(bet1)))/(qtd_apostada * nrow(bet1)) * 100


