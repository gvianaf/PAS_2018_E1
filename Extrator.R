
library(pdftools)
library(tidyverse)

options(OutDec = ",")

### lê o texto
texto <- pdf_text("Ed 8 PAS Subprograma 2018 1a etapa Res final tipo D e redação_revisada.pdf") %>% 
  strsplit("\n") %>% 
  unlist()

### limpa o texto

## transforma em base de dados
texto_t <- tibble(texto)

## limpeza dos dados
texto_t <- texto_t %>% 
  filter(!str_detect(texto, "^\\s+[0-9]+\\r$")) %>%   # retira número da página
  tail(-15) %>%    # retira as primeiras linhas
  head(-9) %>%    # retira as últimas linhas
  filter(!str_detect(texto, "^1\\.1\\.1.+")) %>%    # retira frases no fim do doc
  filter(!str_detect(texto, "^candidatos")) %>% 
  filter(!str_detect(texto, "^escore")) %>% 
  filter(!str_detect(texto, "^nos")) %>% 
  mutate(texto = str_replace_all(texto, "18117113", "/ 18117113"))   # insere a barra para ser utilizada como delimitador posteriormente

## transforma em uma linha só
t <- tibble(Alunos = stringi::stri_paste(texto_t$texto, collapse = ""))

## quebra por aluno
t2 <- t %>% 
  separate_rows(Alunos, sep = "/") %>% 
  mutate(Alunos = trimws(str_replace_all(Alunos, "\r", " ")))

## separa em variáveis

t3 <- t2 %>% 
  separate(Alunos, c("Inscrição", "Nome", "EscoreBruto1", "EscoreBruto2", "SomaEB", "NotaD", "NotaRedação"), sep = ",")

# ajuste final
t3 <- t3 %>% 
  mutate(NotaRedação = str_remove(NotaRedação, "\\.$"))

t3 <- t3 %>% 
  mutate(EscoreBruto1 = as.numeric(EscoreBruto1),
         EscoreBruto2 = as.numeric(EscoreBruto2),
         SomaEB = as.numeric(SomaEB),
         NotaD = as.numeric(NotaD),
         NotaRedação = as.numeric(NotaRedação),
         Nome = trimws(Nome))

rio::export(t3, "NotasPAS2019.xlsx")

inscricao <- sample(PAS2019$Inscrição, 1)
texto_nota <- paste("Sua nota foi melhor que", 
                    scales::percent(round(ecdf(PAS2019$EscoreBruto1)(PAS2019$EscoreBruto1)[PAS2019$Inscrição == inscricao],2)),
                    "dos participantes")

ggplot(data = PAS2019, aes(EscoreBruto1)) +
  geom_histogram(binwidth = 1,
                 color = "royalblue",
                 fill = "lightblue",
                 boundary = 0) +
  geom_vline(xintercept = PAS2019$EscoreBruto1[PAS2019$Inscrição == inscricao],
             color = "red",
             size = 2) +
  labs(x = "Escore Bruto 1",
       y = "Número de estudantes",
       title = texto_nota) +
  scale_x_continuous(breaks = c(seq(0:10)),
                     position = "right") +
  theme_classic()




