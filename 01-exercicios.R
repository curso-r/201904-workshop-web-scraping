library(tidyverse)
library(rvest)

# Exercício 1 ------------------------------------------------------------------

# O site http://example.webscraping.com/ contém uma série de links que 
# possuem informações sobre países. 

# Construa um `data.frame` com as colunas `pais` e `link` dos 
# dez primeiros países que aparecem na primeira página.

# Exercício 2 ------------------------------------------------------------------

# A partir do objeto `paises` gerado no exercício 1 crie uma coluna `img_src` 
# que guarde o atributo `src` das tags `<img>` 
# (ele é local onde a imagem da bandeira está disponível).

# Exercício 3 ------------------------------------------------------------------

# No navegador, inspecione o http://example.webscraping.com/ e identifique 
# uma tabela no corpo do site. 

# Em seguida, utilize a função `html_table()` do pacote `rvest` e 
# compare o resultado com o observado no inspetor. Qual conteúdo a função 
# devolveu: tag, texto ou atributos?

# Exercício 4 ------------------------------------------------------------------
  
# Baixe as páginas de todos os países

# Exercício 5 ------------------------------------------------------------------

# Crie uma conta manualmente e depois construa uma função para se logar.
# Dica: use `abjutils::chrome_to_body()` para copiar o form do 
# Chrome para o computador.

# Exercício extra --------------------------------------------------------------

# Baixe o Chance de Gol de todos os anos!
