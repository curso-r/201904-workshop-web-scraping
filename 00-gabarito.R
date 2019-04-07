library(tidyverse)
library(rvest)

# Exercício 1 ------------------------------------------------------------------

# O site http://example.webscraping.com/ contém uma série de links que 
# possuem informações sobre países. 

# Construa um `data.frame` com as colunas `pais` e `link` dos 
# dez primeiros países que aparecem na primeira página.

url <- "http://example.webscraping.com"

nos_paises_1 <- url %>% 
  # httr::GET() %>% 
  xml2::read_html() %>%
  # pega todos os <a> dentro da <table> que está
  # dentro da tag que tem como como id results
  xml2::xml_find_all("//*[@id='results']/table//a")

paises <- tibble::tibble(
  paises = xml2::xml_attr(nos_paises_1, "href"), # pega o atributo href
  links = xml2::xml_text(nos_paises_1)           # pega o texto de dentro da <a>
) %>% 
  # junta a url com a url base, já que a url vem incompleta
  mutate(url_base = url) %>% 
  unite(url_total, url_base, links, sep = "", remove = FALSE)

# Exercício 2 ------------------------------------------------------------------

# A partir do objeto `paises` gerado no exercício 1 crie uma coluna `img_src` 
# que guarde o atributo `src` das tags `<img>` 
# (ele é local onde a imagem da bandeira está disponível).

imgs <- nos_paises_1 %>% 
  # pega os filhos dos <a>, que sao os <img>
  xml2::xml_children() %>% 
  # pega o atributo src
  xml2::xml_attr("src")

paises <- paises %>% 
  mutate(img = imgs)

# Exercício 3 ------------------------------------------------------------------

# No navegador, inspecione o http://example.webscraping.com/ e identifique 
# uma tabela no corpo do site. 

# Em seguida, utilize a função `html_table()` do pacote `rvest` e 
# compare o resultado com o observado no inspetor. Qual conteúdo a função 
# devolveu: tag, texto ou atributos?

url %>% 
  xml2::read_html() %>% 
  rvest::html_table()

# apenas texto. usar html_table é prático mas algumas informações são perdidas

# Exercício 4 ------------------------------------------------------------------

# Faça uma requisição que baixa a página de Andorra.
# Extraia os dados de andorra numa tabela tidy.

httr::GET("http://example.webscraping.com/places/default/view/Andorra-6",
          httr::write_disk("andorra.html"))

h <- xml2::read_html("andorra.html")
img <- h %>% 
  xml2::xml_find_first("//img") %>% 
  xml2::xml_attr("src")

andorra <- h %>% 
  # pega todas as tabelas
  rvest::html_table() %>%
  # apenas primeira tabela
  dplyr::first() %>% 
  # coloca o link da imagem no lugar que estava vazio
  dplyr::mutate(X2 = ifelse(X2 == "", img, X2)) %>% 
  # tira coluna vazia
  dplyr::select(-X3) %>% 
  # deixa a base tidy
  tidyr::spread(X1, X2) %>% 
  # arruma os nomes
  janitor::clean_names() %>% 
  # transforma em tibble para imprimir mais bonito
  tibble::as_tibble()


# Exercício 5 ------------------------------------------------------------------

# Baixe as páginas de todos os países

# cria a pasta paises
fs::dir_create("paises")

parser <- function(id) {
  # pagina a ser buscada
  url <- paste0("http://example.webscraping.com/places/default/index/", id)
  
  # nome do arquivo a ser salvo
  arquivo <- sprintf("paises/%02d.html", id)
  
  # se o arquivo nao existe, baixa. se existe, nao faz nada
  if (!file.exists(arquivo)) {
    wd <- httr::write_disk(arquivo, overwrite = TRUE)
    r <- httr::GET(url, wd)
  }
  
  # le o arquivo baixado
  h <- xml2::read_html(arquivo)
  
  # verifica se tem um link como atributo do <a> que tem "Next >" no texto
  acabou <- h %>% 
    xml2::xml_find_first("//a[contains(text(), 'Next >')]") %>% 
    xml2::xml_attr("href") %>% 
    is.na()
  
  # mesma coisa do exercício 1
  nos_paises_1 <- h %>% 
    xml2::xml_find_all("//table//a")
  paises <- tibble::tibble(
    links = xml2::xml_attr(nos_paises_1, "href"),
    nomes = xml2::xml_text(nos_paises_1)
  ) %>% 
    mutate(url_base = "http://example.webscraping.com") %>% 
    unite(url_total, url_base, links, sep = "", remove = FALSE)
  
  # salva a tabela de paises numa list column e a condição de ter
  # próxima pagina para baixar ou não
  tibble::tibble(paises = list(paises), acabou)
}

# baixa a primeira pagina
res <- parser(0)
i <- 1
# enquanto tem o link em Next >, rodar
while(!dplyr::last(res$acabou)) {
  message(i)
  Sys.sleep(2) # dorme 2s para não bloquear IP
  # atualiza a base com a nova pagina
  res <- dplyr::bind_rows(res, parser(i))
  i <- i + 1
}

# explode a list column
paises_lista <- res %>% 
  tidyr::unnest()

# PASSO 2: Baixar os países
baixa_pais <- function(link_pais) {
  # ETAPA DOWNLOAD
  arquivo_pais <- basename(link_pais)
  arquivo_pais <- sprintf("%s/%s.html", "paises", arquivo_pais)
  # tratamento de arquivos existentes
  if (!file.exists(arquivo_pais)) {
    Sys.sleep(1)
    r <- GET(link, write_disk(arquivo_pais))
  } else {
    r <- arquivo_pais
  }
  # ETAPA PARSE
  tab <- r %>% 
    read_html() %>% 
    xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    dplyr::select(-X3) %>% 
    tidyr::spread(X1, X2) %>% 
    janitor::clean_names()
  tab
}

# ITERAR
resultado_final <- 
  abjutils::pvec(todos_paises$link, 
                 baixa_pais, .cores = 1)

# UNNEST no iterar
res <- tidyr::unnest(resultado_final)
cria_tabela <- function(link) {
  link %>% 
    httr::GET() %>% 
    xml2::read_html() %>%
    xml2::xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    dplyr::select(-X3) %>% 
    tibble::as_tibble() %>% 
    tidyr::spread(X1, X2) %>% 
    janitor::clean_names()
}

url_base <- "http://example.webscraping.com"
links_completos <- paste0(url_base, da$link)

d_completa <- links_completos %>% 
  abjutils::pvec(cria_tabela) %>% 
  tidyr::unnest()

View(d_completa)


# Exercício 6 ------------------------------------------------------------------

# Crie uma conta manualmente e depois construa uma função para se logar.
# Dica: use `abjutils::chrome_to_body()` para copiar o form do 
# Chrome para o computador.

url_login <- "http://example.webscraping.com/places/default/user/login?_next=/places/default/index"

# pegar a página inicial para acessar o _formkey de validação
r_get <- httr::GET(url_login)

pegar_token <- function(r_get) {
  # aqui vai uma funcao que pega o _formkey e retorna o value dela
  r_get %>% 
    xml2::read_html() %>% 
    xml2::xml_find_first("//input[@name='_formkey']") %>% 
    xml2::xml_attr("value")
}

# construir o formulário de login
form <- list(
  "email" = "jubs@teste.com",
  "password" = "jubs",
  "_next" = "/places/default/index",
  "_formkey" = pegar_token(r_get),
  "_formname" = "login"
)

# realizar o login
r_post <- httr::POST(url_login, body = form)

# devtools::install_github("jtrecenti/scrapr")
# verificar se está ok
scrapr::html_view(r_post)


# Exercício extra --------------------------------------------------------------

# Baixe o Chance de Gol de todos os anos!

baixar_ano <- function(ano) {
  # contruindo a URL
  ano_link <- ano - 2000
  url <- sprintf("http://www.chancedegol.com.br/br%02d.htm", ano_link)
  # Baixa URL
  r <- httr::GET(url)
  # Pega os vermelhos
  vermelhos <- r %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all("//font[@color='#FF0000']") %>% 
    xml2::xml_text()
  # Monta a tabela
  tab <- r %>% 
    xml2::read_html() %>% 
    xml2::xml_find_first("//table") %>% 
    rvest::html_table() %>% 
    purrr::set_names(.[1,]) %>% 
    janitor::clean_names() %>% 
    dplyr::slice(-1) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(prob_ganhou = vermelhos) %>% 
    tidyr::separate(x, c("gols_mandante", "gols_visitante"), sep = "x") %>% 
    dplyr::mutate(quem_ganhou = dplyr::case_when(
      gols_mandante > gols_visitante ~ "mandante",
      gols_mandante < gols_visitante ~ "visitante",
      TRUE ~ "empate"
    ))
  tab
}
baixa_anos <- function(anos) {
  res <- abjutils::pvec(purrr::set_names(anos), baixar_ano)
  tidyr::unnest(res)
}
cdg <- baixa_anos(2001:2018)

# o CDG está bem calibrado?
cdg %>% 
  mutate(
    vitoria = coalesce(vitoria_do_mandante, vit_ria_do_mandante),
    vitoria = parse_number(str_replace_all(vitoria, ",", ".")),
    prob_cat = as.numeric(cut(vitoria/100, 0:10/10))/10
  ) %>% 
  filter(!is.na(prob_cat)) %>% 
  group_by(prob_cat) %>% 
  summarise(n = n(), prop = mean(quem_ganhou == "mandante")) %>% 
  ggplot(aes(x = prob_cat, prop)) +
  geom_errorbar(aes(ymin = prop - prop*(1-prop)/sqrt(n),
                    ymax = prop + prop*(1-prop)/sqrt(n)), width = .02) +
  geom_point(colour = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, breaks = 0:10/5) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, breaks = 0:10/5) +
  theme_minimal(14) +
  coord_equal() +
  labs(x = "Probabilidade estimada", y = "Proporção observada") +
  ggtitle("Calibração do Chance de Gol", "Anos 2001 a 2018")

