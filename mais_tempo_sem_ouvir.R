# https://benjaminbenben.com/lastfm-to-csv/

# pacotes necessários

library(tidyverse)
theme_set(theme_bw())
library(lubridate)
library(stringi)
library(plotly)
library(knitr)
library(kableExtra)

# leitura dos dados

#last.fm <- as_data_frame(read.csv(file = "scrobbles-grandeabobora-1513705894.csv"))
last.fm <- as_tibble(read.csv(file = "grandeabobora_2021.csv", header = FALSE))

# colocar nomes nas colunas

names(last.fm) <- c("artista", "album", "musica", "data")

# converter as datas para um formato reconhecivel pelo R

last.fm$data <- dmy_hm(last.fm$data)
last.fm$data <- date(last.fm$data) # exclui as horas

# nomes em minusculas

last.fm$artista <- sapply(as.character(last.fm$artista), tolower)
last.fm$album   <- sapply(as.character(last.fm$album), tolower)
last.fm$musica  <- sapply(as.character(last.fm$musica), tolower)

# retirar caracteres especiais dos nomes

last.fm$artista <- stri_trans_general(last.fm$artista, "latin-ascii")
last.fm$album   <- stri_trans_general(last.fm$album, "latin-ascii")
last.fm$musica  <- stri_trans_general(last.fm$musica, "latin-ascii")

last.fm$artista <- gsub("&", "and", last.fm$artista)
last.fm$album   <- gsub("&", "and", last.fm$album)
last.fm$musica  <- gsub("&", "and", last.fm$musica)

last.fm$artista <- gsub("jupiter maca", "jupiter apple", last.fm$artista)

# artistas mais ouvidos

last.fm %>% 
  group_by(artista) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 20) %>%
  kable()

# musicas mais ouvidas

last.fm %>% 
  group_by(musica, artista) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 20) %>%
  kable()

# albuns mais ouvidos

last.fm %>% 
  group_by(album, artista) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 20) %>%
  kable()

#####################################
# objeto com os artistas mais ouvidos

top_artistas <- last.fm %>% 
  group_by(artista) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15) %>% # quantidade de artistas
  select(artista)

top_artistas <- apply(top_artistas, 1, as.character)

# audicoes cumulativas dos top 15 artistas

grafico <- last.fm %>% 
  filter(artista %in% top_artistas) %>%
  count(data, artista) %>% 
  complete(data, artista, fill = list(n = 0)) %>% 
  group_by(artista) %>% 
  mutate(n = cumsum(n)) %>%
  ggplot(., aes(x = data, y = n, colour = artista, text = artista)) +
  geom_line() +
  scale_colour_viridis_d() +
  labs(x = "Ano", y = "Número de Execuções", colour = "Artista")

ggplotly(grafico, tooltip = "text")

# objeto com as musicas mais ouvidas

top_musicas <- last.fm %>% 
  group_by(artista, musica) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(15) %>% # quantidade de musicas
  select(musica, artista)

top_musicas <- apply(top_musicas, 1, as.character)

# audicoes cumulativas das top 10 musicas

grafico <- last.fm %>% 
  filter(musica %in% top_musicas) %>%
  count(data, musica) %>% 
  complete(data, musica, fill = list(n = 0)) %>% 
  group_by(musica) %>% 
  mutate(n = cumsum(n)) %>%
  ggplot(., aes(x = data, y = n, colour = musica, text = musica)) +
  geom_line() +
  scale_colour_viridis_d() +
  labs(x = "Ano", y = "Número de Execuções", colour = "Música")

ggplotly(grafico, tooltip = "text")

# artista mais e menos tempo sem ouvir

artistas <- last.fm %>%
  group_by(artista) %>%
  count() %>%
  filter(n >= 2)

last.fm %>%
  filter(artista %in% artistas$artista) %>%
  group_by(artista) %>%
  summarise(diferenca = abs(diff(data))) %>%
  arrange(desc(diferenca))

# cancao mais e menos tempo sem ouvir

musicas <- last.fm %>%
  group_by(musica) %>%
  count() %>%
  filter(n >= 2)

last.fm %>%
  filter(musica %in% musicas$musica) %>%
  group_by(artista, musica) %>%
  summarise(diferenca = abs(diff(data))) %>%
  arrange(desc(diferenca))

# mais tempo sem ouvir de um artista especifico

last.fm %>%
  filter(artista == "the strokes") %>%
  filter(musica %in% musicas$musica) %>%
  group_by(musica) %>%
  summarise(diferenca = abs(diff(data))) %>%
  arrange(desc(diferenca))


