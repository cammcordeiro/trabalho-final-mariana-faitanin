##################################################
#     Atividade Final                             # 
# Disciplina: Ciência colaborativa                #
# Aluna: Mariana Alves Faitanin                   #
# Ano: 2022/1                                     # 
##################################################

#ATIVIDADE 1


#Cada aluno recebeu um conjunto de dados para que pudesse padronizar e no 
#final teríamos que compartilhar um com os outros para que pudessemos criar 
#uma única tabela.
#Para isso, criei um dataset com cada planilha enviada e utilizei a função "rbind" 
#para juntar todas as planilhas em uma só. 

ana<- read.csv("ana-clara.csv")
let<- read.csv("leticia.csv")
luiza<-read.csv("luiza.csv")
marina<-read.csv("marina.csv")
mari<-read.csv("mariana.csv")
victor<-read.csv("victor.csv")
natalia<-read.csv("natalia.csv")

dados1<-rbind(ana,let,luiza,marina,mari,victor,natalia)
dados1
View(dados1)

#Antes de iniciar a atividade 2, precisei converter o dataset "dados1" em uma
#tabela .xlsx e porteriormente em ".csv". Para isso, utilizei o pacote writexl.

library(writexl)
write_xlsx(dados1,"C:\\ciencia-colab\\Atividade-final\\dadosfinal6.xlsx")

#ATIVIDADE 2
#Nesta atividade, continuamos a utilizar a base de dados da atividade 1. Mas agora
#teremos que checar todos os dados do dataset "dadosT3"

dadosT3<-read.csv("dadosfinal3.csv", header = T, sep=";", dec=".")
dadosT3
lapply(dadosT3, unique)

#com a função lapply, podemos conferir a quantidade total de amostra, o nome das
#espécies, áreas e dados de latitude e longitude
#

library(ggplot2)
library(vegan)
library(dplyr)
library(tidyr)
library(validate)
library(taxize)
library(lumberjack)

dadosT3 %>% select(-species, -area, -date) %>% 
  mutate_if(is.character,as.numeric)
bind_cols(dadosT3 %>% select(species, area, date))

#### 
dadosT3 %>% 
  select(species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(y = valores, fill = species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_y') +
  theme_classic()

#Com a função "validator" podemos checar as coordenadas geográficas, áreas,
#datas das ocorrências e o nome completo das espécies.
#atenção: não esquecer de rodar o pacote "validate"  

rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.character(area),
                   is.numeric(date),
                   all_complete(dadosT3))

out   <- confront(dadosT3, rules)
summary(out)
plot(out)

#A função validator nos fornece um resultado gráfico com as variáveis que forma 
#validadas. Note que as variáveis V3 e V5 estão OK e a variável V4 apresentou uma falha.
#Provavelmente esse erro vou devido ao formato da data inserido da planilha. As 
#variáveis V1 e V2 (coordenada geográfica) não foi encontrada.

#Após checar os dados de ocorrências, precisamos checar os dados dos taxons e se
# todos são válidos. Para isso, vamos utilizaremos a função "get_tsn" do pacote "taxize".
# ATENÇÃO: Não esquecer de rodar o pacote "taxize" antes.

species <- dadosT3 %>% 
  distinct(species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(dadosT3 %>% 
              distinct(species))

#No resultado dessa função, aparecerá o número total de espécie da sua planilha 
#e também se os nomes estão corretos, devido a comparação que  será feitoa no 
#Integrated Taxonomic Information System 



#Após toda validação, agora vamos renomear as variáveis da planilha conforme o
#Darwin Core (DwC)


dadosT3_1 <- dadosT3 %>% 
  dplyr::mutate(eventID = paste(area, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(area, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = long, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

#Através das funções eventCore e occurrences, podemos selecionar os dados
#relacionadas as características gerais  das amostras e dos dados das ocorrências.
 
## create eventCore
eventCore <- dadosT3_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, area,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

## create occurrence
occurrences <- dadosT3_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 


#Na planilha de atributos, podemos associar os dados morfométricos das flores
#com os dados de ocorrências 

##create measurementsOrFacts
eMOF <- dadosT3_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))


#Por fim, precisamos fazer um controle de qualidade de todos os dados da planilha
#para vermos se todas as planilhas tem os mesmos valores de eventID.

# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)

# check NA values
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))


#ULTIMO PASSO! Remover todos os arquivos intermediários e salvar os arquivos
# ".csv" no compitador.

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}
