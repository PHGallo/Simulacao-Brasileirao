# Carregar bibliotecas
library(tidyverse)
library(ggplot2)
library(progress)
library(rvest)

# Raspagem de dados
dado_cbf = read_html("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2023")

# Seleção dos elementos da página
resultados = dado_cbf %>% 
  html_nodes(".aside-content .clearfix")

# Extração das equipes em casa
casa = resultados %>%
  html_nodes(".pull-left img") %>%
  html_attr("title")

# Extração das equipes visitantes
fora_casa = resultados %>%
  html_nodes(".pull-right img") %>%
  html_attr("title")

# Extração dos placares
placar = resultados %>% 
  html_nodes(".partida-horario") %>% 
  html_text() %>% 
  str_extract("[0-9]{1}\ x\ [0-9]{1}")

# Organização dos dados em um dataframe
dados_partidas = data.frame(home = casa, 
                             score = placar,
                             away = fora_casa) %>%
  separate(col = score, into = c("home_score","away_score"), sep = "x")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Convertendo as colunas "home_score" e "away_score" para o tipo numérico
dados_partidas$home_score = as.numeric(dados_partidas$home_score)
dados_partidas$away_score = as.numeric(dados_partidas$away_score)

# Criando um novo data.frame com as linhas que não têm valores NA nas colunas relevantes
dados_partidas_sem_na = dados_partidas %>%
  filter(!is.na(home_score) & !is.na(away_score))

# Criando um novo data.frame com as linhas que têm valores NA nas colunas relevantes
dados_partidas_com_na = dados_partidas %>%
  filter(is.na(home_score) & is.na(away_score))

# Criando uma tabela auxiliar para unir os dados de ambos os times (casa e fora)
tabela_times_casa = dados_partidas_sem_na %>%
  select(home, home_score, away_score) %>%
  rename(time = home, gols_marcados = home_score, gols_sofridos = away_score)

tabela_times_fora = dados_partidas_sem_na %>%
  select(away, away_score, home_score) %>%
  rename(time = away, gols_marcados = away_score, gols_sofridos = home_score)

# Unindo os dados de ambos os times (casa e fora)
dados_times = bind_rows(tabela_times_casa, tabela_times_fora)

# Processando os dados para os times (casa e fora)
dados_classificacao = dados_times %>%
  mutate(resultado = case_when(gols_marcados > gols_sofridos ~ "vitoria",
                               gols_marcados < gols_sofridos ~ "derrota",
                               TRUE ~ "empate"),
         pontos = case_when(resultado == "vitoria" ~ 3,
                            resultado == "empate" ~ 1,
                            TRUE ~ 0)) %>%
  group_by(time) %>%
  summarise(pontos = sum(pontos),
            jogos = n(),
            vitorias = sum(resultado == "vitoria"),
            empates = sum(resultado == "empate"),
            derrotas = sum(resultado == "derrota")) %>%
  arrange(desc(pontos))

# Exibindo a tabela de classificação
print(dados_classificacao)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Calculando a probabilidade de cada time vencer, empatar e perder em casa
probabilidades_casa = dados_partidas_sem_na %>%
  group_by(home) %>%
  summarise(vitorias_casa = sum(home_score > away_score),
            empates_casa = sum(home_score == away_score),
            derrotas_casa = sum(home_score < away_score),
            total_jogos_casa = n()) %>%
  mutate(prob_vitoria_casa = vitorias_casa / total_jogos_casa,
         prob_empate_casa = empates_casa / total_jogos_casa,
         prob_derrota_casa = derrotas_casa / total_jogos_casa)

# Calculando a probabilidade de cada time vencer, empatar e perder fora de casa
probabilidades_fora = dados_partidas_sem_na %>%
  group_by(away) %>%
  summarise(vitorias_fora = sum(away_score > home_score),
            empates_fora = sum(away_score == home_score),
            derrotas_fora = sum(away_score < home_score),
            total_jogos_fora = n()) %>%
  mutate(prob_vitoria_fora = vitorias_fora / total_jogos_fora,
         prob_empate_fora = empates_fora / total_jogos_fora,
         prob_derrota_fora = derrotas_fora / total_jogos_fora)

# Renomeando as colunas dos data.frames para melhorar a legibilidade
colnames(probabilidades_casa) = c("time", "vitorias_casa", "empates_casa", "derrotas_casa", "total_jogos_casa", "prob_vitoria_casa", "prob_empate_casa", "prob_derrota_casa")
colnames(probabilidades_fora) = c("time", "vitorias_fora", "empates_fora", "derrotas_fora", "total_jogos_fora", "prob_vitoria_fora", "prob_empate_fora", "prob_derrota_fora")

# Exibindo as probabilidades para cada time em casa
print(probabilidades_casa)

# Exibindo as probabilidades para cada time fora de casa
print(probabilidades_fora)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Realizar o left join das tabelas com base na coluna "time"
dados_completos = dados_classificacao %>%
  left_join(probabilidades_casa, by = "time") %>%
  left_join(probabilidades_fora, by = "time")

# Exibir a tabela completa
print(dados_completos)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Estatíticas

# Função para calcular as sequências e armazenar em outra tabela
calcular_sequencias = function(dataframe) {
  # Inicializar a tabela para armazenar as sequências
  sequencias_table = data.frame(
    team = character(0),
    sequencia_vitorias = integer(0),
    sequencia_derrotas = integer(0),
    sequencia_invictas = integer(0),
    sequencia_sem_vitorias = integer(0)
  )
  
  # Obter a lista de times únicos
  times_unicos = unique(c(dataframe$home, dataframe$away))
  
  # Loop através dos times únicos
  for (team in times_unicos) {
    # Filtrar os jogos do time
    team_games = subset(dataframe, home == team | away == team)
    
    # Calcular as sequências para o time
    sequencia_vitorias = 0
    sequencia_derrotas = 0
    sequencia_invictas = 0
    sequencia_sem_vitorias = 0
    
    for (i in 1:nrow(team_games)) {
      if (team_games[i, "home"] == team) {
        if (team_games[i, "home_score"] > team_games[i, "away_score"]) {
          sequencia_vitorias = sequencia_vitorias + 1
          sequencia_derrotas = 0
          sequencia_invictas = sequencia_invictas + 1
          sequencia_sem_vitorias = 0
        } else if (team_games[i, "home_score"] < team_games[i, "away_score"]) {
          sequencia_vitorias = 0
          sequencia_derrotas = sequencia_derrotas + 1
          sequencia_invictas = 0
          sequencia_sem_vitorias = sequencia_sem_vitorias + 1
        } else {
          sequencia_vitorias = 0
          sequencia_derrotas = 0
          sequencia_invictas = sequencia_invictas + 1
          sequencia_sem_vitorias = sequencia_sem_vitorias + 1
        }
      } else {
        if (team_games[i, "away_score"] > team_games[i, "home_score"]) {
          sequencia_vitorias = sequencia_vitorias + 1
          sequencia_derrotas = 0
          sequencia_invictas = sequencia_invictas + 1
          sequencia_sem_vitorias = 0
        } else if (team_games[i, "away_score"] < team_games[i, "home_score"]) {
          sequencia_vitorias = 0
          sequencia_derrotas = sequencia_derrotas + 1
          sequencia_invictas = 0
          sequencia_sem_vitorias = sequencia_sem_vitorias + 1
        } else {
          sequencia_vitorias = 0
          sequencia_derrotas = 0
          sequencia_invictas = sequencia_invictas + 1
          sequencia_sem_vitorias = sequencia_sem_vitorias + 1
        }
      }
      
      # Armazenar as sequências na tabela
      sequencias_table = rbind(sequencias_table, data.frame(
        team = team,
        sequencia_vitorias = sequencia_vitorias,
        sequencia_derrotas = sequencia_derrotas,
        sequencia_invictas = sequencia_invictas,
        sequencia_sem_vitorias = sequencia_sem_vitorias
      ))
    }
  }
  
  return(sequencias_table)
}


# Calcular as sequências
sequencias = calcular_sequencias(dados_partidas_sem_na)

# Agrupar e selecionar a última linha de cada grupo
sequencias %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(desc(sequencia_vitorias))

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Função para calcular as probabilidades do jogo
calcular_probabilidades_jogo = function(time_home, time_away, dados_completos) {
  pva = dados_completos$prob_vitoria_casa[dados_completos$time == time_home]
  pea = dados_completos$prob_empate_casa[dados_completos$time == time_home]
  pda = dados_completos$prob_derrota_casa[dados_completos$time == time_home]
  
  pvb = dados_completos$prob_vitoria_fora[dados_completos$time == time_away]
  peb = dados_completos$prob_empate_fora[dados_completos$time == time_away]
  pdb = dados_completos$prob_derrota_fora[dados_completos$time == time_away]
  
  prob_vitoria = (pva + pdb) / 2
  prob_empate = (pea + peb) / 2
  prob_derrota = (pda + pvb) / 2
  
  return(c(prob_vitoria, prob_empate, prob_derrota))
}

# Função para definir o resultado da partida
definir_resultado_partida = function(probabilidades_jogo) {
  num_aleatorio = runif(1) # Gera um número aleatório entre 0 e 1
  
  limite_vitoria = probabilidades_jogo[1]
  limite_empate = limite_vitoria + probabilidades_jogo[2]
  
  if (num_aleatorio < limite_vitoria) {
    return("Vitória do time home")
  } else if (num_aleatorio < limite_empate) {
    return("Empate")
  } else {
    return("Vitória do time away")
  }
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Função para realizar a simulação de um campeonato
simular_campeonato = function(dados_completos, dados_partidas_com_na) {
  # Loop para definir os resultados de todas as partidas
  resultados = c()
  for (i in 1:nrow(dados_partidas_com_na)) {
    time_home = dados_partidas_com_na$home[i]
    time_away = dados_partidas_com_na$away[i]
    
    probabilidades_jogo = calcular_probabilidades_jogo(time_home, time_away, dados_completos)
    resultado_partida = definir_resultado_partida(probabilidades_jogo)
    resultados = c(resultados, resultado_partida)
  }
  
  # Resultados das partidas
  resultado_das_partidas = data.frame(
    home = dados_partidas_com_na$home,
    away = dados_partidas_com_na$away,
    resultado = resultados
  )
  
  # Atualizar a tabela de classificação com base nos resultados das partidas
  for (i in 1:nrow(resultado_das_partidas)) {
    time_home = resultado_das_partidas$home[i]
    time_away = resultado_das_partidas$away[i]
    resultado = resultado_das_partidas$resultado[i]
    
    # Atualizar o time home
    index_home = which(dados_completos$time == time_home)
    if (resultado == "Vitória do time home") {
      # Atualizar os pontos
      dados_completos$pontos[index_home] = dados_completos$pontos[index_home] + 3
      # Atualizar os jogos
      dados_completos$jogos[index_home] = dados_completos$jogos[index_home] + 1
      dados_completos$total_jogos_casa[index_home] = dados_completos$total_jogos_casa[index_home] + 1
      # Atualizar as vitórias
      dados_completos$vitorias[index_home] = dados_completos$vitorias[index_home] + 1
      dados_completos$vitorias_casa[index_home] = dados_completos$vitorias_casa[index_home] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_casa[index_home] = dados_completos$vitorias_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_empate_casa[index_home] = dados_completos$empates_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_derrota_casa[index_home] = dados_completos$derrotas_casa[index_home]/dados_completos$total_jogos_casa[index_home]
    } else if (resultado == "Empate") {
      # Atualizar os pontos
      dados_completos$pontos[index_home] = dados_completos$pontos[index_home] + 1
      # Atualizar os jogos
      dados_completos$jogos[index_home] = dados_completos$jogos[index_home] + 1
      dados_completos$total_jogos_casa[index_home] = dados_completos$total_jogos_casa[index_home] + 1
      # Atualizar os empates
      dados_completos$empates[index_home] = dados_completos$empates[index_home] + 1
      dados_completos$empates_casa[index_home] = dados_completos$empates_casa[index_home] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_casa[index_home] = dados_completos$vitorias_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_empate_casa[index_home] = dados_completos$empates_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_derrota_casa[index_home] = dados_completos$derrotas_casa[index_home]/dados_completos$total_jogos_casa[index_home] 
    } else if (resultado == "Vitória do time away") {
      # Atualizar os jogos
      dados_completos$jogos[index_home] = dados_completos$jogos[index_home] + 1
      dados_completos$total_jogos_casa[index_home] = dados_completos$total_jogos_casa[index_home] + 1
      # Atualizar as derrotas
      dados_completos$derrotas[index_home] = dados_completos$empates[index_home] + 1
      dados_completos$derrotas_casa[index_home] = dados_completos$derrotas_casa[index_home] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_casa[index_home] = dados_completos$vitorias_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_empate_casa[index_home] = dados_completos$empates_casa[index_home]/dados_completos$total_jogos_casa[index_home]
      dados_completos$prob_derrota_casa[index_home] = dados_completos$derrotas_casa[index_home]/dados_completos$total_jogos_casa[index_home] 
    }
    
    # Atualizar o time away
    index_away = which(dados_completos$time == time_away)
    if (resultado == "Vitória do time away") {
      # Atualizar os pontos
      dados_completos$pontos[index_away] = dados_completos$pontos[index_away] + 3
      # Atualizar os jogos
      dados_completos$jogos[index_away] = dados_completos$jogos[index_away] + 1
      dados_completos$total_jogos_fora[index_away] = dados_completos$total_jogos_fora[index_away] + 1
      # Atualizar as vitórias
      dados_completos$vitorias[index_away] = dados_completos$vitorias[index_away] + 1
      dados_completos$vitorias_fora[index_away] = dados_completos$vitorias_fora[index_away] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_fora[index_away] = dados_completos$vitorias_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_empate_fora[index_away] = dados_completos$empates_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_derrota_fora[index_away] = dados_completos$derrotas_fora[index_away]/dados_completos$total_jogos_fora[index_away]
    } else if (resultado == "Empate") {
      # Atualizar os pontos
      dados_completos$pontos[index_away] = dados_completos$pontos[index_away] + 1
      # Atualizar os jogos
      dados_completos$jogos[index_away] = dados_completos$jogos[index_away] + 1
      dados_completos$total_jogos_fora[index_away] = dados_completos$total_jogos_fora[index_away] + 1
      # Atualizar os empates
      dados_completos$empates[index_away] = dados_completos$empates[index_away] + 1
      dados_completos$empates_fora[index_away] = dados_completos$empates_fora[index_away] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_fora[index_away] = dados_completos$vitorias_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_empate_fora[index_away] = dados_completos$empates_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_derrota_fora[index_away] = dados_completos$derrotas_fora[index_away]/dados_completos$total_jogos_fora[index_away]
    } else if (resultado == "Vitória do time home") {
      # Atualizar os jogos
      dados_completos$jogos[index_away] = dados_completos$jogos[index_away] + 1
      dados_completos$total_jogos_fora[index_away] = dados_completos$total_jogos_fora[index_away] + 1
      # Atualizar as derrotas
      dados_completos$derrotas[index_away] = dados_completos$derrotas[index_away] + 1
      dados_completos$derrotas_fora[index_away] = dados_completos$derrotas_fora[index_away] + 1
      # Atualizar as probabilidades
      dados_completos$prob_vitoria_fora[index_away] = dados_completos$vitorias_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_empate_fora[index_away] = dados_completos$empates_fora[index_away]/dados_completos$total_jogos_fora[index_away]
      dados_completos$prob_derrota_fora[index_away] = dados_completos$derrotas_fora[index_away]/dados_completos$total_jogos_fora[index_away]
    }
  }
  
  # Ordenar a tabela de classificação por pontos e, em caso de empate, por vitórias
  dados_completos = dados_completos[order(-dados_completos$pontos, -dados_completos$vitorias), ]
  
  return(dados_completos)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Definir número de simulações
num_simulacoes = 10^4

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

pb = progress_bar$new(format = "(:spin) [:bar] :percent [Tempo decorrido: :elapsedfull || Tempo restante estimado: :eta]",
                       total = num_simulacoes,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Inicializar um data frame para armazenar os resultados das simulações
resultado_simulacoes = data.frame(
  time = unique(c(dados_partidas_com_na$home, dados_partidas_com_na$away)),
  probabilidade_campeao = rep(0, length(unique(c(dados_partidas_com_na$home, dados_partidas_com_na$away)))),
  probabilidade_libertadores = rep(0, length(unique(c(dados_partidas_com_na$home, dados_partidas_com_na$away)))),
  probabilidade_sulamericana = rep(0, length(unique(c(dados_partidas_com_na$home, dados_partidas_com_na$away)))),
  probabilidade_rebaixamento = rep(0, length(unique(c(dados_partidas_com_na$home, dados_partidas_com_na$away))))
)

# Inicializar um data frame para armazenar as pontuações das simulações
pontos = data.frame(
  pontos_para_campeao = numeric(0),
  pontos_para_evitar_rebaixamento = numeric(0)
)

# Loop para realizar as simulações
for (i in 1:num_simulacoes) {
  pb$tick()
  
  tabela_classificacao_simulacao = simular_campeonato(dados_completos, dados_partidas_com_na)
  
  # Incrementar a probabilidade do time campeão
  tabela_classificacao_simulacao$campeao = row_number(desc(tabela_classificacao_simulacao$pontos)) <= 1
  resultado_simulacoes$probabilidade_campeao[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] = 
    resultado_simulacoes$probabilidade_campeao[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] + as.numeric(tabela_classificacao_simulacao$campeao)
  
  # Incrementar a probabilidade do time classificar para a Libertadores (6 primeiros colocados)
  tabela_classificacao_simulacao$libertadores = row_number(desc(tabela_classificacao_simulacao$pontos)) <= 6
  resultado_simulacoes$probabilidade_libertadores[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] = 
    resultado_simulacoes$probabilidade_libertadores[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] + as.numeric(tabela_classificacao_simulacao$libertadores)
  
  # Incrementar a probabilidade do time classificar para a Sul-Americana (7º ao 12º colocado)
  tabela_classificacao_simulacao$sulamericana = row_number(desc(tabela_classificacao_simulacao$pontos)) >= 7 & row_number(desc(tabela_classificacao_simulacao$pontos)) <= 12
  resultado_simulacoes$probabilidade_sulamericana[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] = 
    resultado_simulacoes$probabilidade_sulamericana[match(tabela_classificacao_simulacao$time, resultado_simulacoes$time)] + as.numeric(tabela_classificacao_simulacao$sulamericana)
  
  # Incrementar a probabilidade do time ser rebaixado
  rebaixados = tail(tabela_classificacao_simulacao$time, 4)
  resultado_simulacoes$probabilidade_rebaixamento[match(rebaixados, resultado_simulacoes$time)] = 
    resultado_simulacoes$probabilidade_rebaixamento[match(rebaixados, resultado_simulacoes$time)] + 1
  
  # Calcular o número de pontos necessário para ser campeão (pontuação do segundo colocado + 1 ponto)
  segundo_colocado_pontos = tabela_classificacao_simulacao$pontos[2]
  pontos_para_ser_campeao = segundo_colocado_pontos + 1
  
  # Calcular o número de pontos necessário para evitar o rebaixamento (pontuação do décimo sétimo colocado + 1 ponto)
  decimo_setimo_colocado_pontos = tabela_classificacao_simulacao$pontos[17]
  pontos_para_evitar_rebaixamento = decimo_setimo_colocado_pontos + 1
  
  # Armazenar os valores calculados nas probabilidades
  pontos = rbind(pontos, c(pontos_para_ser_campeao, pontos_para_evitar_rebaixamento))
}

# Calcular as probabilidades dividindo pelo número de simulações
resultado_simulacoes$probabilidade_campeao = resultado_simulacoes$probabilidade_campeao / num_simulacoes
resultado_simulacoes$probabilidade_libertadores = resultado_simulacoes$probabilidade_libertadores / num_simulacoes
resultado_simulacoes$probabilidade_sulamericana = resultado_simulacoes$probabilidade_sulamericana / num_simulacoes
resultado_simulacoes$probabilidade_rebaixamento = resultado_simulacoes$probabilidade_rebaixamento / num_simulacoes

# Ordenar o resultado das simulações pela probabilidade de ser campeão e pela probabilidade de classificação para a Libertadores
resultado_simulacoes = resultado_simulacoes[order(-resultado_simulacoes$probabilidade_campeao, -resultado_simulacoes$probabilidade_libertadores, -resultado_simulacoes$probabilidade_sulamericana, resultado_simulacoes$probabilidade_rebaixamento), ]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Exibir a tabela com as probabilidades
print(resultado_simulacoes)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Pontuação
colnames(pontos) = c("pontos_para_ser_campeao", "pontos_para_evitar_rebaixamento")

# Ordene o data.frame com base na coluna 'pontos_para_ser_campeao'
pontos_ordenados = pontos %>%
  arrange(pontos_para_ser_campeao)

# Use a função count para contar as ocorrências de cada valor em 'pontos_para_ser_campeao'
agregado = pontos_ordenados %>%
  count(pontos_para_ser_campeao)

# Calcule a probabilidade acumulada
agregado$Probabilidade_Acumulada = cumsum(agregado$n) / sum(agregado$n)

# Renomeie as colunas
colnames(agregado) = c("Pontuacao", "Frequencia", "Probabilidade_Acumulada")

# Exiba o data.frame resultante
print(agregado)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Visualiza o histograma de pontuações para ser campeão brasileiro
ggplot(pontos, aes(x = pontos_para_ser_campeao)) +
  geom_histogram(binwidth = 2, fill = "#f3c52e", color = "black") +
  labs(
    title = "Distribuição de pontuações para ser campeão brasileiro",
    subtitle = "Temporada 2023",
    x = "Pontuação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Ordene o data.frame com base na coluna 'pontos_para_evitar_rebaixamento', da pontuação máxima para a mínima
pontos_rebaixamento_ordenados <- pontos %>%
  arrange(desc(pontos_para_evitar_rebaixamento))

# Use a função count para contar as ocorrências de cada valor em 'pontos_para_evitar_rebaixamento'
agregado_rebaixamento <- pontos_rebaixamento_ordenados %>%
  count(pontos_para_evitar_rebaixamento) %>%
  arrange(desc(pontos_para_evitar_rebaixamento))

# Calcule a probabilidade acumulada
agregado_rebaixamento$Probabilidade_Acumulada <- cumsum(agregado_rebaixamento$n) / sum(agregado_rebaixamento$n)

# Renomeie as colunas
colnames(agregado_rebaixamento) <- c("Pontuacao", "Frequencia", "Probabilidade_Acumulada")

# Exiba o data.frame resultante
print(agregado_rebaixamento)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Visualiza o histograma de pontuações para evitar o rebaixamento
ggplot(pontos, aes(x = pontos_para_evitar_rebaixamento)) +
  geom_histogram(binwidth = 2, fill = "#f3c52e", color = "black") +
  labs(
    title = "Distribuição de pontuações para evitar o rebaixamento",
    subtitle = "Temporada 2023",
    x = "Pontuação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
