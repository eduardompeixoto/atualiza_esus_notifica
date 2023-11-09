esus<-function(){
# Install the necessary packages if not already installed
required_packages <- c("httr", "jsonlite", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Define the base URL for the Elasticsearch API
url <- "https://elasticsearch-saps.saude.gov.br/desc-esus-notifica-estado-rj/_search"

# Define credentials
user <- "user-public-notificacoes"
senha <- "Za4qNXdyQNSa9YaA"

# Encode credentials in base64
credenciais_base64 <- enc2utf8("dXNlci1wdWJsaWMtbm90aWZpY2Fjb2VzOlphNHFOWGR5UU5TYTlZYUE=")

# Define headers
headers <- add_headers(
  Authorization = paste("Basic", credenciais_base64),
  "Content-Type" = "application/json",
  Cookie = "ELASTIC-PROD=1635856782.811.5320.80284"
)

# Function to make API requests and extract data
fetch_data <- function(query, url, headers) {
  response <- httr::POST(url, body = query, config = headers)
  stop_for_status(response)  # Stop execution if response status code is not 200
  json_data <- content(response)
  data_frame <- json_data$hits$hits %>%
    purrr::map_df(function(hit) as.data.frame(t(hit$`_source`)))
  data_frame <- data_frame %>% mutate(testes_list = list(testes))
  return(data_frame)
}

# Loop to fetch data for different date ranges
data_frames <- lapply(1:28, function(days_back) {
  query <- sprintf('{
    "size": 10000,
    "query": {
      "match": {
        "dataNotificacao": "now-%dd/d"
      }
    }
  }', days_back)
  
  fetch_data(query, url, headers)
})

a<-bind_rows(data_frames)


a$classificacaoFinal<-NULL

a<-distinct(a)
a[a=="NA"]<-NA
a$resultado[a$resultado=="Inconclusivo"]<-NA

a$racaCor[a$racaCor=="Amarela"]<-3
a$racaCor[a$racaCor=="Branca"]<-1
a$racaCor[a$racaCor=="Branco"]<-1
a$racaCor[a$racaCor=="Ignorado"]<-NA
a$racaCor[a$racaCor=="Parda"]<-4
a$racaCor[a$racaCor=="Preta"]<-2
a$racaCor[str_detect(a$racaCor,"Ind")]<-5
a$racaCor<-as.character(a$racaCor)

a$confirmado<-'0'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="ANTÍGENO"]<-'1'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="PCR"]<-'1'
a$confirmado[a$resultado=="positivo"&a$tipoTeste=="RT-LAMP"]<-'1'
a<-arrange(a,desc(a$resultado))
a<-arrange(a,desc(a$confirmado))
a<-distinct(a,a$id,.keep_all = T)
a$confirmado<-as.numeric(a$confirmado)
a$idade<-as.numeric(a$idade)
a$semvacina<-a$recebeuVacina
a$semvacina[a$recebeuVacina>=1]<-'0'
a$semvacina[a$recebeuVacina<1]<-'1'

a
                  }
  
