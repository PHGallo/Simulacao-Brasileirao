# Modelagem Probabilística e Simulação do Campeonato Brasileiro 2023

Este projeto utiliza uma abordagem para fazer a previsão de resultados em competições de futebol por meio da modelagem estatística e simulação probabilística. A utilização de dados históricos e métricas de desempenho proporciona uma análise das probabilidades de vitória, empate e derrota para cada equipe envolvida em partidas futuras.  Vou explicar as principais etapas do código:

1. **Carregamento de Bibliotecas:**
   As bibliotecas necessárias são carregadas, incluindo "tidyverse", "ggplot2", "progress" e "rvest" para manipulação e visualização de dados.

2. **Raspagem de Dados:**
   A página da web é carregada usando a função `read_html` do pacote `rvest`.

3. **Seleção de Elementos da Página:**
   O código HTML da página é analisado para extrair os elementos desejados, neste caso, as informações sobre os jogos do Campeonato Brasileiro.

4. **Extração de Informações:**
   As informações sobre as equipes em casa, equipes visitantes e placares são extraídas dos elementos HTML selecionados.

5. **Organização dos Dados:**
   As informações extraídas são organizadas em um dataframe chamado `dados_partidas`, separando os placares em colunas separadas para as equipes da casa e visitantes.

6. **Conversão de Colunas para Numérico:**
   As colunas "home_score" e "away_score" são convertidas para o tipo numérico.

7. **Filtragem de Linhas com NA:**
   Duas novas dataframes são criadas, uma contendo linhas sem valores NA nas colunas relevantes (`dados_partidas_sem_na`) e outra contendo as linhas com valores NA (`dados_partidas_com_na`).

8. **Criação de Tabelas Auxiliares:**
   Tabelas auxiliares são criadas para os times da casa e visitantes, renomeando as colunas relevantes.

9. **União de Dados de Times:**
   Os dados dos times da casa e visitantes são unidos em um único dataframe chamado `dados_times`.

10. **Análise de Classificação:**
    As estatísticas da classificação são calculadas, incluindo pontos, jogos, vitórias, empates e derrotas para cada time.

11. **Probabilidades de Jogos em Casa e Fora:**
    As probabilidades de vitória, empate e derrota para cada time em casa e fora de casa são calculadas.

12. **Junção de Tabelas:**
    As tabelas com informações de times, probabilidades de jogos em casa e fora são mescladas em um único dataframe chamado `dados_completos`.

13. **Cálculo de Sequências de Resultados:**
    Uma função chamada `calcular_sequencias` é definida para calcular as sequências de vitórias, derrotas, invencibilidade e jogos sem vitórias para cada time.

14. **Simulação de Campeonato:**
    Uma função chamada `simular_campeonato` é definida para simular o desempenho dos times em várias iterações.

15. **Simulação de Pontuações:**
    O código simula o número de pontos necessário para ser campeão ou evitar o rebaixamento em várias iterações e calcula as probabilidades correspondentes.

16. **Visualização de Resultados:**
    Os resultados das simulações são exibidos, incluindo as probabilidades de ser campeão, classificar para a Libertadores ou Sul-Americana e ser rebaixado. Também são exibidos histogramas das pontuações necessárias para ser campeão ou evitar o rebaixamento.

O código realiza uma análise do desempenho das equipes no Campeonato Brasileiro de 2023 e fornece estimativas de probabilidade para vários cenários, incluindo a classificação final dos times, probabilidades de ser campeão e evitar o rebaixamento. Também inclui simulações estatísticas para avaliar o desempenho potencial das equipes. Assim, este projeto se destaca ao oferecer uma análise, auxiliando na compreensão das dinâmicas da competição e na antecipação de diferentes cenários que podem se desenrolar ao longo do campeonato.

## Referências
[1] LIMA, B.N.B.; BROCHERO, F.; COSTA, G.N.; MARTINS, R.V (2008). Futebol: uma caixinha... de sorteios. Ciência Hoje, v. 254, p. 24-29. 

[2] MARTINS, R.V.; LIMA, B.N.B.; BROCHERO, F.; COSTA, G.N.; ZEFERINO, G.M (2010). Probabilidades no futebol. Matemática Universitária, v. 48/49, p. 15-31.
