pkgload::load_all()
# pak::pkg_install("WorldHealthOrganization/godataR")

# df <- atualiza:::update_godata()
# Install the necessary packages if not already installed
required_packages <- c("httr", "jsonlite", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)

# Define the base URL for the Elasticsearch API
url <- "https://elasticsearch-saps.saude.gov.br/desc-esus-notifica-estado-rj/_search"

# Define credentials
user <- "user-public-notificacoes"
senha <- "Za4qNXdyQNSa9YaA"

# Encode credentials in base64
credenciais_base64 <- paste0(user, ":", senha)
credenciais_base64 <- base64enc::base64encode(charToRaw(credenciais_base64))

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
    purrr::map_df(function(hit) as.data.frame(t(hit$`_source`))) %>%
    unnest(cols = testes) %>%
    mutate(testes_list = list(testes))
  return(data_frame)
}

# Initialize an empty data frame to store the results
combined_data <- data.frame()

# Loop to fetch data for different date ranges and combine them using bind_rows
for (days_back in 1:29) {
  query <- sprintf('{
    "size": 10000,
    "query": {
      "match": {
        "dataNotificacao": "now-%dd/d"
      }
    }
  }', days_back)
  
  fetched_data <- fetch_data(query, url, headers)
  combined_data <- bind_rows(combined_data, fetched_data)
}

writexl::write_xlsx(df, "inst/planilha.xlsx")
# write.csv(df, "inst/tabela_teste.csv")

# escrever a mensagem de commit
commit_message <-
  paste0("", Sys.time())

# salvar a mensagem de commit
writeLines(commit_message, "mensagem-comit.txt")

