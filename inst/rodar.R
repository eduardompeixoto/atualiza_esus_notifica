pkgload::load_all()
# pak::pkg_install("WorldHealthOrganization/godataR")

# df <- atualiza:::update_godata()
atualiza:::enviar_alerta()
# writexl::write_xlsx(df, "inst/planilha.xlsx")
# write.csv(df, "inst/tabela_teste.csv")

# escrever a mensagem de commit
commit_message <-
  paste0("", Sys.time())

# salvar a mensagem de commit
writeLines(commit_message, "mensagem-comit.txt")

