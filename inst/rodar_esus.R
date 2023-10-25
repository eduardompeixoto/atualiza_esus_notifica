pkgload::load_all()

df_esus <- esus()

write.csv2(df_esus, "inst/planilha_esus.csv")

commit_message <- paste0("", Sys.time())

writeLines(commit_message, "mensagem-comit.txt")
