# ---------------------------------------------------------------------------- #
# Curso Online: Dos dados ao mapa: Modelando a distribuição                    #
# das espécies                                                                 #
#                                                                              #
# Criado por: Dr. Carlos de Oliveira                                           #
# Data: 24-01-2026                                                             #
# Contato: carlos.prof.bio@gmail.com                                           #
#                                                                              #
# Descrição: o script representa o processo geral de implementação de          #
# uma rotina de modelagem da adequabilidade ambiental, no contexto da          #
# Modelagem de Nicho Ecológico. Todos os procedimentos podem ser modificados   #
# conforme o escopo e necessidade da pesquisa.                                 #
#                                                                              #
# Notas:                                                                       #
# - eventuais erros podem surgir no script devido atualizações dos             #
# pacotes utilizados pelo script.                                              #
# - as pastas de origem e destino dos arquivos devem ser atualizadas           #
# conforme o computador onde serão realizados os processos de modelagem.       #
# ---------------------------------------------------------------------------- #

options(scipen = 999) # remover notação científica dos dados

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

install.packages("spThin")
install.packages("raster")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("dismo")

library(spThin)    # Realiza o "thinning" espacial, reduzindo a autocorrelação espacial em dados de ocorrência
library(raster)    # Manipulação, análise e visualização de dados espaciais no formato raster
library(tidyverse) # Conjunto de pacotes para manipulação, visualização e análise de dados (ggplot2, dplyr, tidyr, etc.)
library(dplyr)     # Manipulação de dados (selecionar colunas, filtrar linhas, criar variáveis, agrupar, sumarizar)

pal1 <- c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred") # paleta de cores

# ---------------------------------------------------------------------------- #

# 01. Obter dados presença e processar dados ambientais e bióticos -----

## Download ou carregamento das ocorrências -----
sp_guara <- dismo::gbif(
  genus = "Chrysocyon",         # Define o gênero da espécie a buscar no GBIF
  species = "brachyurus",         # Define a espécie
  geo = TRUE,                     # Filtra apenas registros com coordenadas (lat/long)
  removeZeros = TRUE,             # Remove registros com coordenadas inválidas
  download = TRUE                 # Faz o download diretamente do GBIF
)

# ---------------------------------------------------------------------------- #

# sp_guara <- read.csv("nome do arquivo.csv")             # Alternativa: ler ocorrências de um arquivo CSV
# sp_guara <- readxl::read_excel("nome do arquivo.xlsx")  # Alternativa: ler ocorrências de um Excel

# ---------------------------------------------------------------------------- #

names(sp_guara)    # Mostra os nomes das colunas do objeto 'sp'
sp_guara$country   # Exibe todos os dados baixados
nrow(sp_guara)     # Conta o número de linhas (registros) no dataframe

# ---------------------------------------------------------------------------- #

### Tratamento dos dados -----
#sp_guara_br <- sp_guara %>%
#  dplyr::filter(country == "Brazil") %>%  # Mantém apenas ocorrências no Brasil
#  dplyr::select(species, lon, lat)       # Mantém apenas as colunas de interesse

### Tratamento dos dados -----
sp_guara_al <- sp_guara %>%
  dplyr::filter(country %in% c("Brazil", "Argentina", "Paraguay", "Bolivia", "Uruguay")) %>%  # Mantém ocorrências nos países escolhidos
  dplyr::select(species, lon, lat)                         # Mantém apenas colunas de interesse

nrow(sp_guara_al)  # Número de registros após o filtro

# ---------------------------------------------------------------------------- #

sp_guara_al <- sp_guara_al %>%
  distinct() %>%  # Remove registros duplicados
  drop_na()       # Remove registros com valores faltantes

nrow(sp_guara_al)      # Conta registros após limpeza

# ---------------------------------------------------------------------------- #

# Carrega pacotes para visualização
library(ggplot2)  # Pacote para gráficos
library(maps)     # Pacote para mapas simples

#br_map <- map_data("world", region = "Brazil")  # Obtém coordenadas do mapa do Brasil

#ggplot() +
#  geom_polygon(data = br_map, aes(x = long, y = lat, group = group),
#               fill = "gray95", color = "gray60") +   # Desenha o mapa base
#  geom_point(data = sp_br, aes(x = lon, y = lat),
#             color = "red", size = 2) +               # Plota pontos de ocorrência
#  coord_fixed(1.3) +                                  # Mantém proporção correta
#  labs(
#    title = "Ocorrências de Chrysocyon brachyurus no Brasil", # Título
#    x = "Longitude", y = "Latitude"                             # Rótulos dos eixos
#  ) +
#  theme_minimal()                                     # Tema visual simples

# ---------------------------------------------------------------------------- #

# Lista de países da América Latina que você quer exibir
latam_countries_guara_al <- c("Brazil", "Argentina", "Bolivia", "Chile", "Colombia",
                              "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela",
                              "Guyana", "Suriname", "French Guiana")

# Obtém dados do mapa mundial e filtra apenas a América Latina
latam_map_guara_al <- map_data("world") %>%
  filter(region %in% latam_countries_guara_al)

# Plota o mapa da América Latina com os pontos
g1_guara <- ggplot() +
  geom_polygon(data = latam_map_guara_al, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray60") +
  geom_point(data = sp_guara_al, aes(x = lon, y = lat),
             color = "red", size = 2) +
  coord_fixed(1.3) +
  labs(
    title = "Ocorrências de Chrysocyon brachyurus na América Latina",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

g1_guara

# ---------------------------------------------------------------------------- #

# Remove os dois pontos indesejados
sp_guara_al <- sp_guara_al %>%
  filter(!(lon == -46.7837246 & lat == -30.5171954))

# ---------------------------------------------------------------------------- #

### Espacialização geográfica -----
sp_thin_guara <- thin(
  loc.data = sp_guara_al,                       # Dataframe de ocorrências filtrado
  lat.col = "lat",                              # Coluna com latitude
  long.col = "lon",                             # Coluna com longitude
  spec.col = "species",                         # Coluna com o nome da espécie
  thin.par = 100,                               # Distância mínima (km) entre pontos
  reps = 100,                                   # Quantas vezes repetir o processo
  locs.thinned.list.return = TRUE,              # Retorna lista com resultados de cada repetição
  write.files = FALSE,                          # Não salva arquivos automaticamente
  write.log.file = FALSE                        # Não cria arquivo de log
)

n_locs <- sapply(sp_thin_guara, nrow)                           # Número de pontos em cada repetição
n_locs

sp_thin_guara <- sp_thin_guara[[which.max(n_locs)]]             # Repetição com maior número de ocorrências
sp_thin_guara

nrow(sp_thin_guara)                                             # Mostra número de registros

colnames(sp_thin_guara) <- c("longitude", "latitude")           # Renomeia colunas

# ---------------------------------------------------------------------------- #

library(patchwork)

# Plota o mapa da América Latina com os pontos
g2_guara <- ggplot() +
  geom_polygon(data = latam_map_guara_al, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray60") +
  geom_point(data = sp_thin_guara, aes(x = longitude, y = latitude),
             color = "red", size = 2) +
  coord_fixed(1.3) +
  labs(
    title = "Ocorrências de Chrysocyon brachyurus na América Latina (Espacializado)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

# Combina os gráficos lado a lado
g1_guara + g2_guara

# ---------------------------------------------------------------------------- #

### Exportar planilha final -----
#write.csv(sp_thin_guara, "Chrysocyon brachyurus.csv", row.names = FALSE) # Salva CSV no diretório atual

# ---------------------------------------------------------------------------- #

## Download ou carregamento das variáveis ambientais -----
bio_guara <- geodata::worldclim_global(
  var = "bio",     # Variáveis bioclimáticas (BIO1 a BIO19)
  res = 5,         # Resolução espacial (5 minutos de arco)
  path = "C:/Cursos/Modelagem/Vídeo-aula/Modulo_I_Dos_dados_ao_mapa_Modelando_a_distribuicao_das_especies" # Onde salvar
)

#https://www.worldclim.org/data/bioclim.html

# ---------------------------------------------------------------------------- #

bio_guara <- raster::stack(
  list.files(
    path = "C:/Cursos/Modelagem/Vídeo-aula/Modulo_I_Dos_dados_ao_mapa_Modelando_a_distribuicao_das_especies/climate/wc2.1_5m",
    pattern = ".tif", full.names = TRUE
  )
)

bio_guara <- raster::subset(bio_guara, paste0("wc2.1_5m_bio_", 1:19))        # Ordena camadas de 1 a 19
names(bio_guara)                                                             # Lista nomes das variáveis

# ---------------------------------------------------------------------------- #

#br <- geobr::read_country(year = 2020)     # Baixa shapefile do Brasil (SIRGAS 2000)
#bio <- projectRaster(bio, crs = crs(br))   # Reprojeta para o mesmo CRS do shapefile
#bio <- crop(bio_guara, br)                 # Recorta para área do Brasil
#bio <- mask(bio, br)                       # Remove valores fora do Brasil
#plot(bio[[1]])                             # Mostra mapa de uma variável

# ---------------------------------------------------------------------------- #

# Instalar pacotes se ainda não tiver
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("sf")  # necessário para manipular shapefiles

# Carregar pacotes
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Baixar shapefile da América Latina (países da América do Sul)
latam_guara <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")

# Dividir a tela em 1 colunas e 1 linha
par(mfrow = c(1, 1))

# Visualizar a geometria
plot(st_geometry(latam_guara))

# Reprojeta as variáveis ambientais para o mesmo CRS do shapefile
bio_guara <- projectRaster(bio_guara, crs = crs(latam_guara))

# Recorta e aplica máscara para a América Latina
bio_guara <- crop(bio_guara, latam_guara)
bio <- mask(bio_guara, latam_guara)

# Visualiza a primeira variável
plot(bio_guara)
plot(bio_guara[[1]])
plot(bio_guara[[12]])

# ---------------------------------------------------------------------------- #

### Tratamento das variáveis ambientais -----
guara <- raster::extract(bio_guara, sp_thin_guara)           # Extrai valores ambientais para cada ocorrência
guara_vif <- usdm::vifstep(guara, th = 10)                   # Verifica multicolinearidade (VIF)
guara_vif

bio_guara <- usdm::exclude(bio_guara, guara_vif)             # Remove variáveis correlacionadas
plot(bio_guara)                                              # Plota variáveis selecionadas

plot(bio_guara[[1]])                                         # Mostra uma das variáveis filtradas
points(sp_thin_guara, col = "black")                         # Sobrepõe pontos de ocorrência

# ---------------------------------------------------------------------------- #

bio_guara <- raster::stack(bio_guara) # Converte para RasterStack

# ---------------------------------------------------------------------------- #

# Ajustar e treinar modelos de adequabilidade -----
sp_thin_guara <- sp_thin_guara %>% mutate(guara = 1)                 # Adiciona coluna binária de presença
coordinates(sp_thin_guara) <- c("longitude", "latitude")             # Converte para objeto espacial

# Cria um SpatialPointsDataFrame
sp_thin_guara <- SpatialPointsDataFrame(sp_thin_guara, 
                                        data = data.frame(guara = rep(1, nrow(sp_thin_guara))))

sp_thin_guara

# ---------------------------------------------------------------------------- #

library(sdm)

mdata_guara <- sdmData(
  formula = guara ~ .,       # Modelo: presença ~ variáveis ambientais
  train = sp_thin_guara,     # Dados de treino
  predictors = bio_guara,    # Variáveis ambientais
  bg = list(
    n = 135,                 # Número de pontos de background (pseudo ausência)
    method = "gRandom",      # Distribuição aleatória
    remove = TRUE            # Remove pontos de fundo sobrepostos a presenças
  )
)

# ---------------------------------------------------------------------------- #

install.packages(c("gbm", "kernlab", "randomForest", "dismo", "mgcv"))

getMethodNames()

# Rodando múltiplos algoritmos
modelo_multi_guara <- sdm(
  formula = guara ~ .,
  data = mdata_guara,
  methods=c('glm','gam','gbm','svm','rf', "maxent"),
  replication = "cv",   # validação cruzada
  cv.folds = 5,         # número de folds
  test.percent = 30,
  parallelSettings = list(ncore = 2, method = "parallel")
)

# ---------------------------------------------------------------------------- #

modelo_multi_guara              # Mostra sumário do modelo
roc(modelo_multi_guara)         # Calcula curva ROC/AUC
getVarImp(modelo_multi_guara)   # Importância das variáveis

# Curvas de resposta para duas variáveis específicas
rcurve(modelo_multi_guara, gg = TRUE)

# ---------------------------------------------------------------------------- #

## Projetar mapas de adequabilidade -----
proj_multi_guara <- raster::predict(
  bio_guara,
  modelo_multi_guara,           # Modelo treinado
  filename = "C:/Users/carlosoliveira/Documents/Backup/Cursos/Modelagem Preditiva/Modelagem/2° Turma/proj_guara_multi.grd",
  overwrite = TRUE  # Sobrescreve se existir
)

plot(proj_multi_guara, zlim = c(0, 1), col=pal1)
points(sp_thin_guara, col = "black")

# ---------------------------------------------------------------------------- #

## Ensemble -----
ens_multi_guara <- ensemble(
  modelo_multi_guara,
  newdata = bio_guara,
  setting = list(
    method = "weighted",
    stat = "AUC",
    opt = 2
  )
)

# ---------------------------------------------------------------------------- #

# Dividir a tela em 2 colunas e 1 linha
par(mfrow = c(1, 2))

# 1º gráfico: projeção
plot(proj_multi_guara, zlim = c(0, 1), col=pal1)
#points(sp_thin_guara, col = "black")

# 2º gráfico: ensemble
plot(ens_multi_guara, zlim = c(0, 1), col=pal1)
#points(sp_thin_guara, col = "black")

# ---------------------------------------------------------------------------- #

dir.create("C:/Users/carlosoliveira/Documents/Backup/Cursos/Modelagem Preditiva/Modelagem/2° Turma/variaveis_fut_guara", recursive = TRUE)

bio_future_guara <- geodata::cmip6_world(
  model = "MPI-ESM1-2-HR",
  ssp   = "585",             # SSP5-8.5
  time  = "2081-2100",
  var   = "bioc",
  res   = 5,
  path  = "C:/Users/carlosoliveira/Documents/Backup/Cursos/Modelagem Preditiva/Modelagem/2° Turma/variaveis_fut_guara"
)

# ---------------------------------------------------------------------------- #

# 1. Reprojetar e recortar bio_future para América Latina

bio_future_guara <- raster::stack(bio_future_guara)
bio_future_guara <- raster::projectRaster(bio_future_guara, crs = crs(latam_guara))
bio_future_guara <- crop(bio_future_guara, latam_guara)
bio_future_guara <- mask(bio_future_guara, latam_guara)

# ---------------------------------------------------------------------------- #

# Selecionar no FUTURO as mesmas variáveis do PRESENTE e renomear

# 1) extrai os IDs a partir de "wc2.1_5m_bio_X"
ids_guara <- as.integer(sub(".*_bio_", "", names(bio_guara)))
ids_guara

# 2) monta os nomes equivalentes no futuro: "bio02", "bio03", ...
target_future_guara <- paste0("bio", sprintf("%02d", ids_guara))
target_future_guara

# 4) subset no MESMO ORDENAMENTO do presente
bio_future_sel_guara <- subset(bio_future_guara, target_future_guara)
bio_future_sel_guara

# 5) renomeia para ficar idêntico ao presente
names(bio_future_sel_guara) <- names(bio_guara)

# Conferir
names(bio_guara)
names(bio_future_sel_guara)

# ---------------------------------------------------------------------------- #

# Ensemble no futuro (ponderado por AUC)
ens_future_guara <- ensemble(
  modelo_multi_guara,
  newdata = bio_future_sel_guara,
  setting = list(
    method = "weighted",
    stat   = "AUC",
    opt    = 2
  )
)

# Dividir a tela em 2 colunas e 1 linha
par(mfrow = c(1, 3))

# gráfico: projetado sem ensemble
plot(proj_multi_guara, zlim = c(0, 1), col = pal1,
     main = "Projetado sem ensemble")
#points(sp_thin, col = "red")

# gráfico: projetado com ensemble
plot(ens_multi_guara, zlim = c(0, 1), col = pal1,
     main = "Projetado com ensemble")
#points(sp_thin, col = "red")

# gráfico: projetado no futuro
plot(ens_future_guara, zlim = c(0, 1), col = pal1,
     main = "Projetado no futuro\n(RCP8.5 - 2080/2100)")
#points(sp_thin, col = "red")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
