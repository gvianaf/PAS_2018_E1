
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

load("notasPAS.RData")

notasPAS %>% 
  select("EscoreBruto2") %>% 
  gather(Nota, Valor) %>% 
  ggplot(aes(Nota, Valor)) +
  geom_boxplot() +
  theme_classic()

### gráfico para o aplicativo

### fase 1

ggplot(data = notasPAS, 
       aes(x = EscoreBruto1)) +
  geom_histogram(bins = 10,
                 boundary = 0) +
  scale_x_continuous(breaks = c(seq(0:10))) +
  theme_classic()

### testa n bin

graf1 <- ggplot(data = notasPAS, 
                aes(x = SomaEB)) +
  geom_histogram(bins = 10,
                 boundary = 0) +
  # scale_x_continuous(breaks = c(seq(0:10))) +
  labs(title = "10 barras") +
  theme_classic()

graf2 <- ggplot(data = notasPAS, 
                aes(x = SomaEB)) +
  geom_histogram(bins = 15,
                 boundary = 0) +
  # scale_x_continuous(breaks = c(seq(0:10))) +
  labs(title = "15 barras") +
  theme_classic()

graf3 <- ggplot(data = notasPAS, 
                aes(x = SomaEB)) +
  geom_histogram(bins = 20,
                 boundary = 0) +
  # scale_x_continuous(breaks = c(seq(0:10))) +
  labs(title = "20 barras") +
  theme_classic()

graf4 <- ggplot(data = notasPAS, 
                aes(x = SomaEB)) +
  geom_histogram(bins = 25,
                 boundary = 0) +
  # scale_x_continuous(breaks = c(seq(0:10))) +
  labs(title = "25 barras") +
  theme_classic()

gridExtra::grid.arrange(graf1, graf2, graf3, graf4, nrow = 2)

ggplot(data = notasPAS, 
       aes(x = EscoreBruto1)) +
  geom_histogram(bins = unique(bin),
                 boundary = 0) +
  scale_x_continuous(breaks = c(seq(0:10))) +
  facet_wrap(~bin) +
  theme_classic()


inscricao <- sample(notasPAS$Inscrição, 1)

texto_nota <- paste("Sua nota foi melhor que", 
                    scales::percent(round(ecdf(notasPAS$EscoreBruto1)(notasPAS$EscoreBruto1)[notasPAS$Inscrição == inscricao],2)),
                    "dos participantes")

ggplot(data = notasPAS, aes(EscoreBruto1)) +
  geom_histogram(bins = 10,
                 color = "royalblue",
                 fill = "lightblue",
                 boundary = 0) +
  geom_vline(xintercept = notasPAS$EscoreBruto1[notasPAS$Inscrição == inscricao],
             color = "red",
             size = 2) +
  labs(x = "Escore Bruto 1",
       y = "Número de estudantes",
       title = texto_nota) +
  scale_x_continuous(breaks = c(seq(0:10)),
  position = "right") +
  theme_classic()




