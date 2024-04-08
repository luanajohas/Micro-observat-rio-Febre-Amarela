# Bibliotecas ====

# Carregar o pacote com as informações geográficas do Brasil(estados, municipios e bairros)
library(geobr)
# Carregar o pacote tidyverse, que inclui uma coleção de pacotes para manipulação e visualização de dados.
library(tidyverse)
# Carregar o pacote readxl para leitura de arquivos do Excel.
library(readxl)
# Carregar o pacote paletteer para paletas de cores.
library(paletteer)
# Carregar o pacote ggplot2 para criar gráficos e visualizações avançados.
library(ggplot2)
# Carregar o pacote dplyr para tarefas de manipulação de dados.
library(dplyr)
# Carregar o pacote classInt para funções de classificação de dados.
library(classInt)
# Carregar o pacote raster para trabalhar com dados raster.
library(raster)
# Carregar o pacote maps para exibir mapas.
library(maps)
# Carregar o pacote sp para classes e métodos de dados espaciais.
library(sp)
# Carregar o pacote colorspace para manipulação de cores.
library(colorspace)
# Carregar o pacote cowplot para criar ggplots complexos e arranjar gráficos.
library(cowplot)
# Carregar o pacote sf para features simples e manipulação de dados espaciais.
library(sf)
# Carregar o pacote ggnewscale para adicionar múltiplas escalas de cores em um único gráfico ggplot.
library(ggnewscale)
#simplifica tarefas como instalação de pacotes diretamente do GitHub e desenvolvimento de pacotes R
library(devtools)
devtools::install_github("yutannihilation/ggsflabel")
#Permite a adição e personalização de etiquetas em gráficos criados com o ggplot2.
library(ggsflabel)
#permite combinar vários gráficos em uma única visualização
library(patchwork)

# Objetos ====
# Dados de Febre Amarela (casos humanos e óbitos de PNH) retirados do OpenDataSUS 
FAcasoshumanos <- read.csv("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_casoshumanos_1994-2021.csv", header = TRUE, sep = ";")

FAepizootias <- read.csv("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_epizpnh_1999-2021.csv", header = TRUE, sep = ";")



#Dados espaciais do Brasil do pacote "geobr"
estados <- geobr::read_state()

bairros <- geobr::read_neighborhood()

municipios <- geobr::read_municipality()

#Arquivo para compatibilizar os códigos municipais de 6 dígitos do DataSUS com os 
#códigos municipais de 7 dígitos do pacote "geobr"

dicmunicipios <- read.csv("https://github.com/luanajohas/jictac/raw/main/dicionario_municipios.csv")


# Epizootias====
#Converte a coluna COD_MUN_OCOR, da base FAepizootias, para o tipo de dado caractere
FAepizootias$COD_MUN_OCOR <- as.character(FAepizootias$COD_MUN_OCOR)

#Converte a coluna D_MUNICIP, da base dicmunicipios, para o tipo de dado caractere
dicmunicipios$ID_MUNICIP <- as.character(dicmunicipios$ID_MUNICIP)

#Agrupa os dados por COD_MUN_OCOR (código do município de ocorrência) e ANO_OCOR (ano de ocorrência) e, em seguida, 
#resume os dados, contando o número de ocorrências em cada grupo, armazenando o resultado na nova coluna chamada contzoo.
#As colunas restantes são atribuidas à variável FAepicont
FAepicont <- FAepizootias |>
  group_by(COD_MUN_OCOR,ANO_OCOR)|>
  summarise(contzoo=n())

#Realiza uma junção à esquerda entre dois dataframes, FAepicont e dicmunicipios, 
#utilizando a correspondência entre as colunas COD_MUN_OCOR do dataframe FAepicont e ID_MUNICIP do dataframe dicmunicipios.
FAepicont <-left_join(FAepicont, dicmunicipios,
                        by = c('COD_MUN_OCOR'= 'ID_MUNICIP'))

#Converte a coluna code_muni, da base FAepicont, para o tipo de dado caracter
FAepicont$code_muni <- as.character(FAepicont$code_muni)

#Converte a coluna code_muni, da base municipios, para o tipo de dado caracter
municipios$code_muni <- as.character(municipios$code_muni)

#Realiza uma junção à direita entre dois dataframes, FAepicont e municipios, utilizando a correspondência 
#entre as colunas code_muni do dataframe FAepicont e code_muni do dataframe municipios
FAepicont <- right_join(FAepicont, municipios,
                         by= c('code_muni'='code_muni'))

#Cria uma nova coluna chamada "classe" no dataframe FAepicont. Se o ano de ocorrência (ANO_OCOR) for maior ou igual 
#a 2017, a classe é "Recentes"; caso contrário, é definida como "Antigos".
FAepicont <- FAepicont|>
  mutate(classe = case_when(
    ANO_OCOR >= 2017 ~ "Recentes",
    ANO_OCOR < 2017 ~ "Antigos"
  ))

#Desenvolve um mapa de epizootias de febre amarela no Brasil utilizando o pacote ggplot2 para visualização.
#As epizootias recentes (ocorridas desde 2017) são representadas em azul escuro, enquanto as epizootias mais antigas 
#(anteriores a 2017) são representadas em tons de laranja. O mapa também inclui linhas de contorno dos estados do Brasil.
#A legenda do mapa mostra o número de casos de epizootias para cada período de tempo.

mapaepi1 <- FAepicont|>
  ggplot() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepicont, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00",
          alpha = 0.6)+
  scale_fill_gradientn(colours = c("skyblue", "navyblue"),
                       guide="none",na.value="white") +
  new_scale_fill() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepicont, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#f9dc5c88","#ffa400bb")    ,
                       guide="none",na.value="white") +
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1) +
  labs(title = 'Epizootias de Febre Amarela') +
  theme_void() +
  theme(text = element_text(size = 25))
mapaepi1

#Desenvolve o mesmo mapa de epizootias, com um filtro de acordo com as coordenadas 
mapaepi2 <- FAepicont|>
  ggplot() +
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepicont, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00", 
          alpha = 0.6)+
  scale_fill_gradientn(colours = c("skyblue", "navyblue"),
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos \n 2017-2021' ) +
  new_scale_fill()+
  geom_sf(aes(fill=contzoo,geometry=geom),
          filter(FAepicont, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#f9dc5c88","#ffa400bb")    ,
                       guide="colorbar",na.value="white", name = 'Contagem de Óbitos\n 1999-2016') +
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1)+
  coord_sf(xlim = c(-55, -35), ylim = c(-35, -10)) +
  labs(title=NULL,
       x='longitude',
       y='latitude')+ 
  theme_classic()+
  theme(text = element_text(size = 25))
mapaepi2


#realiza a junção de mapaepi1 e mapaepi2 
mapaepi1 + mapaepi2 +
  labs(caption = 'https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_epizpnh_1999-2021.csv') +
  theme(plot.caption = element_text(size = 10, hjust = 0.8))

#Casos Humanos ====
#Converte a coluna COD_MUN_LPI, da base FAcasoshumanos, para o tipo de dado caracter
FAcasoshumanos$COD_MUN_LPI<-as.character(FAcasoshumanos$COD_MUN_LPI)

#Agrupa os dados por código do município, ano e ocorrência de óbito, e em seguida, calcula a contagem total de casos humanos 
#e a contagem de casos fatais
FAhumcont <- FAcasoshumanos |>
  group_by(COD_MUN_LPI, ANO_IS, OBITO) |>  
  summarise(conthum=n(), 
            contobitos = sum(OBITO == "SIM")) 

#Realiza uma junção à esquerda entre dois dataframes, FAhumcont e dicmunicipios, 
#utilizando a correspondência entre as colunas COD_MUN_LPI do dataframe FAhumcont e ID_MUNICIP do dataframe dicmunicipios.
FAhumcont <- left_join(FAhumcont, dicmunicipios,
                           by = c('COD_MUN_LPI'='ID_MUNICIP'))

#Converte a coluna code_muni, da base FAhumcont, para o tipo de dado caracter
FAhumcont$code_muni <- as.character(FAhumcont$code_muni)

#Realiza uma junção à direita entre dois dataframes, FAhumcont e municipios, utilizando a correspondência 
#entre as colunas code_muni do dataframe FAhumcont e code_muni do dataframe municipios
FAhumcont <- right_join(FAhumcont,municipios,
                            by= c('code_muni'='code_muni'))

#Cria uma nova coluna chamada "classe" no dataframe  FAhumcont. Se o ano de ocorrência (ANO_IS) for maior ou igual 
#a 2017, a classe é "Recentes"; caso contrário, é definida como "Antigos".
FAhumcont <- FAhumcont|>
  mutate(classe = case_when(
    ANO_IS >= 2017 ~ "Recentes",
    ANO_IS < 2017 ~ "Antigos"
  ))


# Carrega os dados dos assentamentos municipais e converte o código do município para caractere
muni_seat <- geobr::read_municipal_seat()|>
  mutate(code_muni = as.character(code_muni))

# Cria um resumo dos dados de FAhumcont, onde os óbitos foram agrupados por código de município e classe, 
#e somente os municípios com pelo menos um óbito serão mantidos
FAcasoshumanosbr_municipal_seat <- FAhumcont|>
  group_by(code_muni, classe)|>
  summarise(obitos_municipal_seat = sum(contobitos))|>
  filter(obitos_municipal_seat > 0)

# Adiciona informações sobre os assentamentos municipais ao dataframe FAcasoshumanosbr_municipal_seat, combinando 
#as linhas com base no código do município
FAcasoshumanosbr_municipal_seat <- left_join(FAcasoshumanosbr_municipal_seat,
                                       muni_seat,
                                       by = c("code_muni"))

#Adiciona três novas colunas ao dataframe FAcasoshumanosbr_municipal_seat. Coluna "label": se a classe for "Recentes", 
#o label será uma combinação do nome do município, o número de óbitos municipais e o intervalo de tempo (2017-2021), e se 
#a classe for "Antigos", o label será uma combinação semelhante, mas com o intervalo de tempo (1994-2016). Colunas "x_coord" e "y_coord", extraem a longitude e a latitude da coluna de geometria ("geom") 

FAcasoshumanosbr_municipal_seat <- FAcasoshumanosbr_municipal_seat|>
  mutate(
    label = case_when(
      classe == "Recentes" ~ paste0(name_muni,
                                    ",\n",
                                    obitos_municipal_seat,
                                    " (2017-2021)"),
      classe == "Antigos" ~ paste0(name_muni,
                                   ",\n",
                                   obitos_municipal_seat,
                                   " (1994-2016)")),
    x_coord = st_coordinates(geom)[,1],
    y_coord = st_coordinates(geom)[,2])

#Gera um mapa que representa a distribuição geográfica de casos humanos de febre amarela no Brasil.
#Os casos são diferenciados entre recentes e antigos, sendo representados por diferentes cores de preenchimento 
#nos polígonos dos municípios. Além disso, o tamanho dos pontos é proporcional ao número de óbitos por município. 
#O código também adiciona rótulos aos pontos para destacar municípios com um número significativo de óbitos.
mapahum1 <- FAhumcont|>
  ggplot() +
  geom_sf(aes(fill=conthum, geometry=geom),
          filter(FAhumcont, classe == "Recentes"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#d8b9d577", "#6a0dad"),
                       guide="none",na.value="white") +
  new_scale_fill()+
  geom_sf(aes(fill=conthum,geometry=geom),
          filter(FAhumcont, classe == "Antigos"),
          linewidth=0.00000001,
          color="#adb5bd00")+
  scale_fill_gradientn(colours = c("#95d5b277","#1b4332bb")    ,
                       guide="none",na.value="white") +
  geom_sf(aes(geometry = geom, size = obitos_municipal_seat, color = classe),
          data = FAcasoshumanosbr_municipal_seat,
          alpha = 0.5)+
  scale_color_manual(values = c("#1b4332bb","#6a0dadbb"))+
  geom_sf(data=estados,
          fill='white',
          alpha= 0,
          linewidth=0.01,
          color='#000000',
          linetype=1) +
  geom_sf_label_repel(# https://ggrepel.slowkow.com/articles/examples
                      # https://github.com/yutannihilation/ggsflabel
    data = FAcasoshumanosbr_municipal_seat|>
      filter(obitos_municipal_seat>8),
    aes(x = x_coord,
        y = y_coord,
        geometry = geom,
        label = label),
    fill = '#ffffff',
    max.overlaps = 20,
    force = 1200,
    segment.inflect = FALSE,
    segment.square = TRUE,
    segment.curvature = 1,
    segment.shape = 0)+
  
labs(title='Mapa de casos humanos de Febre Amarela')+ 
  theme_void() +
  theme(text = element_text(size = 25), 
        legend.position = "none")  
mapahum1


#Desenvolve o mesmo mapa de casos humanos com um filtro de acordo com as coordenadas 
mapahum2 <- FAhumcont|>
    ggplot() +
    geom_sf(aes(fill=conthum, geometry=geom),
            filter(FAhumcont, classe == "Recentes"),
            linewidth=0.00000001,
            color="#adb5bd00")+
    scale_fill_gradientn(colours = c("#d8b9d577", "#6a0dad"),
                         guide="colorbar",na.value="white", name = 'Número de casos \n 2017-2021') +
    new_scale_fill()+
    geom_sf(aes(fill=conthum,geometry=geom),
            filter(FAhumcont, classe == "Antigos"),
            linewidth=0.00000001,
            color="#adb5bd00")+
    scale_fill_gradientn(colours = c("#95d5b277","#1b4332bb")    ,
                         guide="colorbar",na.value="white", name = 'Número de casos \n 1994-2017') +
    geom_sf(aes(geometry = geom, size = obitos_municipal_seat, color = classe),
            data = FAcasoshumanosbr_municipal_seat,
            alpha = 0.5)+
    scale_color_manual(values = c("#1b4332bb","#6a0dadbb"))+
    geom_sf(data=estados,
            fill='white',
            alpha= 0,
            linewidth=0.01,
            color='#000000',
            linetype=1)+
    coord_sf(xlim = c(-55, -35), ylim = c(-35, -10)) +
    labs(title=NULL,
         size = "Óbitos por município",
         color = "Antes/depois de 2017",
         x = "longitude",
         y = "latitude") + 
    theme_classic()
  theme(text = element_text(size = 25))  
mapahum2  

#junta mapahum1 e mapahum2 
mapahum1 + mapahum2 + 
  labs(caption = 'https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/Febre+Amarela/fa_casoshumanos_1994-2021.csv') +
  theme(plot.caption = element_text(size = 10, hjust = 0.8))
  
  
  
  
  
  


