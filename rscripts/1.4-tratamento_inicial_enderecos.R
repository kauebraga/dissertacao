
ID = read_delim("data-raw/Linkage Table - BU ID and Card ID_V1.csv", col_names = T, delim=";")

ID_v2 <- ID %>% 
  select(id = CODIGO_USUARIO, endereco = ENDERECO, bairro = BAIRRO) %>%
  mutate(endereco = iconv(endereco, to='WINDOWS-1252')) %>%
  mutate(endereco = tolower(endereco)) %>% #tudo para minusculo
  mutate(primeira_palavra = str_extract(endereco, '\\w*')) %>%
  filter(primeira_palavra != "")

# quais as primeiras palavras mais usadas?

ID_v2 %>%
  count(primeira_palavra) %>%
  arrange(desc(n)) %>%
  View()

primeiras_palavras = c("rua", "r", "r.", "ru",
                       "av", "avenida", "av.",
                       "travessa", "tr", "tv",
                       "Vila", "vl", 
                       "al", "alameda")

ID_v3 <-  ID_v2 %>%
  filter(primeira_palavra %in% primeiras_palavras) %>%
  # corrigir todas as primeiras palavras
  mutate(endereco = sub("r[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("r.[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("ru[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("av[[:blank:]]", "avenida ", endereco)) %>%
  mutate(endereco = sub("av.[[:blank:]]", "avenida ", endereco)) %>%
  mutate(endereco = sub("tr[[:blank:]]", "travessa ", endereco)) %>%
  mutate(endereco = sub("tv[[:blank:]]", "travessa ", endereco)) %>%
  mutate(endereco = sub("vl[[:blank:]]", "vila ", endereco)) %>%
  mutate(endereco = sub("al[[:blank:]]", "alameda ", endereco)) %>%
  # tirar espaços em branco
  mutate(endereco = trimws(endereco, which = "both")) %>%
  # trocar pontuacoes por espaço
  mutate(endereco = gsub("[[:punct:]]", " ", endereco)) %>%
  # manter somente o logradouro
  mutate(endereco_v1 = gsub("^.*? ", "", endereco))

# deletar aqueles que comecam com numeros e so uma letra
ID_v4 = ID_v3 %>% 
  filter(grepl("^\\D{1} ", endereco_v1)) 
filter(grepl("^\\d+", endereco_v1))


