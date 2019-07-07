
library(pdftools)
library(tidyverse)

options(OutDec = ",")

### lê o texto e transforma em uma base de dados
texto_bruto <- pdf_text("Ed 8 PAS Subprograma 2018 1a etapa Res final tipo D e redação_revisada.pdf") %>% 
  strsplit("\n") %>% 
  unlist() %>% 
  enframe(name = NULL, value = "Linha")

### limpeza dos dados

texto_limpo <- texto_bruto %>% 
  tail(-15) %>%    # retira as primeiras linhas
  head(-10) %>%    # retira as últimas linhas
  filter(!str_detect(Linha, "^\\s+[0-9]+\\r$")) %>%   # retira número da página
  filter(!str_detect(Linha, "^1\\.1\\.1.+")) %>%    # retira frases no fim do doc
  filter(!str_detect(Linha, "^candidatos")) %>% 
  filter(!str_detect(Linha, "^escore")) %>% 
  filter(!str_detect(Linha, "^nos")) %>% 
  mutate(Linha = str_replace_all(Linha, "18117113", "/ 18117113"))   # insere a barra para ser utilizada como delimitador posteriormente

## transforma em uma linha só

texto_final_bruto <- tibble(Candidatos = stringi::stri_paste(texto_limpo$Linha, collapse = ""))

## quebra por aluno
texto_final_limpo <- texto_final_bruto %>% 
  separate_rows(Candidatos, sep = "/") %>%   # quebra uma coluna em diversas linhas
  mutate(Candidatos = trimws(str_replace_all(Candidatos, "\r", " ")))   # retira os espaços em branco desnecessários

## encontra o candidato 18117113

temp <- texto_final_limpo %>%
  filter(str_detect(Candidatos, "18117113"))

print(temp)

## separa em variáveis

texto_final_limpo <- texto_final_limpo %>% 
  separate(Candidatos, 
           c("Inscrição", "Nome", "EscoreBruto1", "EscoreBruto2", "SomaEB", "NotaD", "NotaRedação"), 
           sep = ",")

# ajustes finais

options(OutDec = ",")

notasPAS <- texto_final_limpo %>% 
  mutate(NotaRedação = str_remove(NotaRedação, "\\.$"),
         EscoreBruto1 = as.double(EscoreBruto1),
         EscoreBruto2 = as.double(EscoreBruto2),
         SomaEB = as.double(SomaEB),
         NotaD = as.double(NotaD),
         NotaRedação = as.double(NotaRedação),
         Nome = trimws(Nome))

notasPAS <- notasPAS %>% 
  mutate(NotaTotal = rowSums(notasPAS[5:7]))

rio::export(notasPAS, "NotasPAS2018.xlsx")
save(notasPAS, file = "notasPAS.RData")
rm(list = ls())

### boxplot
notasPAS <- rio::import("NotasPAS2018.xlsx")
load("notasPAS.RData")

notasPAS %>% 
  select("EscoreBruto2") %>% 
  gather(Nota, Valor) %>% 
  ggplot(aes(Nota, Valor)) +
  geom_boxplot() +
  theme_classic()

### gráfico para o aplicativo

# gráficos iniciais

ggplot(data = notasPAS, 
       aes(x = NotaTotal)) +
  geom_histogram()

ggplot(data = notasPAS, 
       aes(x = NotaTotal)) +
  geom_histogram(bins = 10,   # número de barras
                 boundary = 0,   # força a barra a começar em zero
                 color = "royalblue",   # cor interna
                 fill = "lightblue",) +   # cor da borda
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +   # mostra a legenda dos números de 10 em 10
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +   # formata para inserir o ponto de milhar
  labs(x = "Nota Total",   # legenda do eixo x
       y = "Número de estudantes") +   # legenda do eixo y
  theme_classic()   # tema limpo, sem distrações

# número de inscrição aleatório
inscricao <- sample(notasPAS$Inscrição, 1)

# monta o texto explicativo
# utilizando a função paste para juntar as partes
# e a função ecdf para encontrar a posição do candidato
texto_nota <- paste("Sua nota foi melhor que", 
                    scales::percent(round(ecdf(notasPAS$NotaTotal)(notasPAS$NotaTotal)[notasPAS$Inscrição == inscricao],2)),
                    "dos participantes")

# cria gráfico juntando tudo
ggplot(data = notasPAS, 
       aes(x = NotaTotal)) +
  geom_histogram(bins = 10,
                 boundary = 0,
                 color = "royalblue",
                 fill = "lightblue",) +
  geom_vline(xintercept = notasPAS$NotaTotal[notasPAS$Inscrição == inscricao],   # insere uma linha vertical
             color = "red",                                                      # de cor vermelha
             size = 2) +                                                         # e tamanho 2
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
  labs(x = "Nota Total",
       y = "Número de estudantes",
       title = texto_nota) +   # insere o texto explicativo como título do gráfico
  theme_classic()






