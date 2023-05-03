---
title: "O Apagão do Setor Elétrico"
author: "David Gun e Victor Alves"
subtitle: Problemas em Economia
output:
  html_document: 
    toc: true
    toc_depth: 2
    keep_md: yes
  pdf_document:
    toc: true
    toc_depth: 2
    keep_md: yes
editor_options: 
  markdown: 
    wrap: 72
---


```r
# Carrega os pacotes necessários para a análise de dados
library(tidyverse)    # Pacote que inclui uma coleção de pacotes para manipulação, 
#visualização e modelagem de dados.
library(magrittr)     # Pacote que permite escrever códigos mais legíveis e 
#organizados utilizando o operador %>%
library(lubridate)    # Pacote para trabalhar com datas
library(zoo)          # Pacote para trabalhar com séries temporais
library(janitor)      # Pacote para limpeza e transformação de dados
library(readxl)       # Pacote para importar dados de arquivos do Excel
library(stargazer)    # Pacote para criar tabelas de resultados de modelos
library(ggthemes)     # Pacote para personalização de gráficos criados com ggplot2
library(viridis)      # Pacote para gerar paletas de cores para gráficos
library(GGally)       # Pacote para criação de matriz de dispersão
library(gridExtra)    # Pacote para combinar vários gráficos em uma única imagem
library(grid)         # Pacote para trabalhar com layout e alinhamento de gráficos
library(gridtext)     # Pacote para adicionar texto em gráficos gerados com grid
library(plm)          # Pacote para modelagem de dados em painel
library(tempdisagg)   # Pacote para desagregação temporal de séries de tempo
library(showtext)    # Pacote de fontes
```

# Base de dados

Inicialmente será carregada a base das tarifas de uso e energia para as
distribuidoras. O interesse é trabalhar com tarifas B1, que correspondem
às tarifas residenciais.\


```r
# Lê o arquivo CSV com informações de tarifas
tarifas <- read_csv2('Dados/base_tarifas.csv',
                      locale = readr::locale(encoding = "latin1")) %>% 
  # Filtra para as tarifas de aplicação residencial, subgrupo B1
  filter(DscBaseTarifaria == 'Tarifa de Aplicação', 
         DscSubGrupo == 'B1',
         DscSubClasse == 'Residencial') %>% 
  # Renomeia as colunas e seleciona as relevantes
  rename(data_inicio = DatInicioVigencia,
         data_fim = DatFimVigencia,
         cnpj = NumCNPJDistribuidora,
         tarifa_uso = VlrTUSD,
         tarifa_energia = VlrTE,
         empresa = SigAgente) %>% 
  select(data_inicio, data_fim, empresa, cnpj, tarifa_energia, tarifa_uso) %>%
  # Cria a coluna "data" com um vetor de meses
  mutate(data = map2(data_inicio, data_fim, seq, by = "month")) %>%
  unnest(data) %>%
  # Agrupa as tarifas por distribuidora e mês
  group_by(cnpj, data) %>%
  summarize(tarifa_energia = first(tarifa_energia),
            tarifa_uso = first(tarifa_uso))

# Arredonda as datas para o mês mais próximo
tarifas$data <- floor_date(tarifas$data, unit = "month")

# Adiciona as colunas "mes" e "ano" às tarifas
tarifas %<>% 
  mutate(mes = month(data),
         ano = year(data))
```

Como variável de interesse, a base de satisfação IASC da ANEEL computa a
média anual da satisfação dos consumidores das distribuidoras do
Brasil. Contudo, como tal variável é anual, foi utilizada uma
interpolação para obter valores mensais assumindo que o valor anual é o
último valor mensal daquele ano.\


```r
# Ler arquivo CSV e selecionar colunas
base_satisfacao <- read_csv2('Dados/base_satisfacao.csv',
                             locale = readr::locale(encoding = "latin1")) %>% 
  select(-c(DatGeracaoConjuntoDados, MdaSatisfacaoR2:MdaConfiancaR2, 
            MdaMediaQualidadeInformacaoV15:MdaDsvPadConfianca)) %>% 
  # Renomear colunas
  rename(cnpj = NumCNPJ,
         ano = NumAno,
         classificacao_empresa = DscClassificacao,
         empresa = SigAgente,
         qualidade_percebida = MdaIndicadorQualidade,
         satisfacao = MdaIndicadorSatisfacao,
         fidelidade = MdaIndicadorFidelidade,
         valor_percebido = MdaIndicadorValor,
         confianca = MdaIndicadorConfianca) %>% 
  # Selecionar colunas
  select(ano, classificacao_empresa, DescricaoCategoria, cnpj, satisfacao, valor_percebido) %>% 
  # Criar colunas
  mutate(mes = 12,
         data = as.Date(paste0(ano, "-", mes, "-01"), format = "%Y-%m-%d"))

# Definir a data mínima e a data máxima
min_data <- min(base_satisfacao$data)
max_data <- max(base_satisfacao$data)

# Gerar todas as datas a partir da mínima até a máxima
todas_datas <- seq(min_data, max_data, by = "month")

# Selecionar todas as empresas únicas
todas_empresas <- unique(base_satisfacao$cnpj)

# Criar um dataframe com todas as combinações de empresas e datas
df_all <- expand.grid(cnpj = todas_empresas, data = todas_datas)

# Fazer um merge com os dados de satisfação mensal
base_satisfacao_mensal <- merge(df_all, base_satisfacao, by = c("cnpj", "data"), all.x = TRUE) %>% 
  # Interpolar valores faltantes
  mutate(satisfacao = na.spline(satisfacao),
         valor_percebido = na.spline(valor_percebido),
         # Criar colunas de mês e ano
         mes = month(data),
         ano = year(data)) %>% 
  # Preencher os valores de classificação de empresas e categoria
  fill(classificacao_empresa, .direction = "down") %>% 
  fill(DescricaoCategoria, .direction = "down")
```

A base da DEC (medida de qualidade do serviço) vem na frequência mensal
separado em três bases. Para isso, é necessário unir as bases.


```r
# Leitura dos dados da base_decfec1.csv, filtrando apenas as linhas com SigIndicador igual a 'DEC'
base_decfec1 <- read_csv2('Dados/base_decfec1.csv',
                          locale = readr::locale(encoding = "latin1")) %>% 
    filter(SigIndicador == 'DEC')

# Leitura dos dados da base_decfec2.csv, filtrando apenas as linhas com SigIndicador igual a 'DEC'
base_decfec2 <- read_csv2('Dados/base_decfec2.csv',
                          locale = readr::locale(encoding = "latin1")) %>% 
    filter(SigIndicador == 'DEC')

# Leitura dos dados da base_decfec3.csv, filtrando apenas as linhas com SigIndicador igual a 'DEC'
base_decfec3 <- read_csv2('Dados/base_decfec3.csv',
                          locale = readr::locale(encoding = "latin1")) %>% 
    filter(SigIndicador == 'DEC')

# União das três bases em uma única tabela e remoção das colunas DatGeracaoConjuntoDados e SigIndicador
dec <- rbind(base_decfec1, base_decfec2, base_decfec3) %>% 
  select(-c(DatGeracaoConjuntoDados, SigIndicador)) %>% 
  # Renomeação das colunas
  rename(cnpj = NumCNPJ,
         ano = AnoIndice,
         mes = NumPeriodoIndice,         dec = VlrIndiceEnviado,
         empresa = SigAgente) %>% 
  # Agrupamento por ano, mês, empresa e CNPJ e cálculo da média do dec
  group_by(ano, mes, empresa, cnpj) %>% 
  summarise(dec = mean(dec, na.rm = T))
```

A base do custo no mercado regulado (ACR), é divulgado pelos leilões
realizados. Nele é divulgada a unidade federativa que correspondente
aquele leilão. Deste modo, foi adotada a média mensal dos preços nos
leilões para cada unidade federativa do Brasil.


```r
# Lê o arquivo CSV contendo os dados de leilões de geração de energia e renomeia as colunas de interesse
acr <- read_csv2('Dados/leiloes_geracao.csv',
                             locale = readr::locale(encoding = "latin1")) %>% 
  rename(custo_acr = VlrPrecoLeilao,
         data = DatLeilao,
         uf = SigUFPrincipal) %>% 
  # Extrai informações de mês e ano a partir da data e converte a data para formato adequado
  mutate(mes = month(data),
         ano = year(data),
         data = as.Date(paste0(ano, "-", mes, "-01"), format = "%Y-%m-%d")) %>% 
  # Ordena o dataframe por estado e data
  arrange(uf, data)

# Define a menor e a maior data presente no dataframe e cria uma sequência de datas mensais
min_data <- min(acr$data)
max_data <- max(acr$data)
todas_datas <- seq(min_data, max_data, by = "month")

# Cria um dataframe contendo todas as combinações possíveis de estados e datas mensais
todas_uf <- unique(acr$uf)
df_all <- expand.grid(uf = todas_uf, data = todas_datas)

# Realiza um join do dataframe acima com os dados de leilão de geração, preenchendo os valores faltantes
acr_mensal <- merge(df_all, acr, by = c("uf", "data"), all.x = TRUE) %>% 
  # Extrai informações de mês e ano a partir da data e preenche os valores faltantes de estado e custo
  mutate(mes = month(data),
         ano = year(data)) %>% 
  fill(uf, .direction = "down") %>% 
  fill(custo_acr, .direction = "down") %>% 
  # Agrupa os dados por data e estado e seleciona as colunas de interesse
  group_by(data, uf) %>% 
  select(data, uf, custo_acr)
```

A mesma coisa foi feita para o mercado ACL (mercado livre), mas para as
regiões do Brasil.


```r
acl <- read_excel('Dados/custoACL.xlsx') %>% 
  pivot_longer(cols = SUDESTE:NORTE, # transforma as colunas de SUDESTE até NORTE em linhas
               names_to = 'regiao', # nomeia a nova coluna criada com os valores anteriormente nas colunas
               values_to = 'custo_acl') %>% # nomeia a nova coluna criada com os valores da coluna anterior
  mutate(ano = year(MES), # cria coluna "ano" a partir da coluna "MES"
         mes = month(MES)) %>% # cria coluna "mes" a partir da coluna "MES"
  rename(data = MES) # renomeia a coluna "MES" para "data"
```

Os dados do ENA mostram o potencial de geração elétrica em cada bacia
hidrelétrica do Brasil. Foi criada também uma variável para mostrar em
qual região do Brasil aquela bacia está ligada. Essa medida será
extremamente valiosa, uma vez que pode capturar a relação direta entre produção
energética e volume dos reservatórios de água do país\


```r
# Importar os arquivos da pasta "ENA BACIA", ler e combinar em uma lista de data frames
ena_bacia <- list.files(path = "Dados/ENA BACIA/", pattern = "*.csv") %>% 
  readr::read_csv2(file = paste0("Dados/ENA BACIA/", .), id = "file_name") %>% # Ler cada arquivo CSV e adicionar uma coluna "file_name" para identificá-los
  row_to_names(row_number = 1) %>% # Transformar a primeira linha em nomes de colunas
  filter(nom_bacia != 'nom_bacia') %>% # Remover linhas que contenham "nom_bacia"
  select(-`Dados/ENA BACIA/2000.csv`) %>% # Remover a coluna 2000.csv
  select(ena_bruta_bacia_mwmed, ena_data, nom_bacia) %>% # Selecionar as colunas necessárias
  mutate(mes = month(as.Date(ena_data, format = "%Y-%m-%d")), # Criar coluna para mês
         ano = year(as.Date(ena_data, format = "%Y-%m-%d")), # Criar coluna para ano
         data = as.Date(paste0(ano, "-", mes, "-01"), format = "%Y-%m-%d"), # Criar coluna para data, com o primeiro dia do mês
         ena = as.numeric(ena_bruta_bacia_mwmed), # Converter "ena_bruta_bacia_mwmed" para numérico
         nom_bacia = as_factor(nom_bacia)) %>% # Converter "nom_bacia" para fator
  mutate(regiao = case_when( # Criar coluna de região com base em "nom_bacia"
    nom_bacia %in% c('AMAZONAS', 'TOCANTINS') ~ 'NORTE',
    nom_bacia %in% c('ARAGUARI', 'CAPIVARI', 'DOCE', 'GRANDE', 'IGUACU', 
                     'ITABAPOANA', 'JEQUITINHONHA', 'PARAIBA DO SUL', 
                     'PARANA', 'PARANAIBA', 'PARANAPANEMA', 'TIETE', 
                     'OUTRAS- SUDESTE') ~ 'SUDESTE',
    nom_bacia %in% c('ITAJAI', 'JACUI', 'URUGUAI', 'OUTRAS - SUL') ~ 'SUL',
    nom_bacia %in% c('MUCURI', 'PARAGUACU', 'PARNAIBA', 'SAO FRANCISCO') ~ 'NORDESTE',
    nom_bacia %in% c('PARAGUAI') ~ 'CENTRO-OESTE'
  ),
  ano = year(data), # Repetir a coluna de ano
  mes = month(data)) %>% # Repetir a coluna de mês
  group_by(data, regiao) %>% # Agrupar por data e região
  summarise(ena = mean(ena, na.rm = T)) # Resumir por média da coluna "ena"
```

Foi adicionada uma base com os estados que cada distribuidora atua.


```r
# Importa o arquivo em formato xlsx contendo informações sobre a área de atuação das distribuidoras
area_atuacao_distribuidoras <- read_xlsx('Dados/area_atuacao_distribuidoras.xlsx') %>% 
  # Limpa os nomes das colunas
  janitor::clean_names() %>% 
  # Remove caracteres especiais do CNPJ
  mutate(cnpj = str_remove_all(cnpj, "[./-]")) %>% 
  # Seleciona apenas as colunas "cnpj", "uf", "regiao" e "populacao_atendida"
  select(cnpj, uf, regiao, populacao_atendida)
```

Foi também adicionada uma base do índice de commodities energéticas
(Global price of Energy index) convertido de dólares americanos para reais. Foi
utilizado como proxy para capturar a variação no custo de produção enérgetica por
parte das geradoras termoelétricas. 


```r
# Importa o arquivo em formato xls contendo informações sobre commodities de energia 
comodities <- read_xls('Dados/energy_comodities_brl.xls') %>% 
  # Limpa os nomes das colunas
  janitor::clean_names() %>% 
  # Seleciona apenas as colunas "data" e "index_brl"
  select(data, index_brl)
```

Para criar a base final, foi assumido que as distribuidoras tomam os
custos da sua região/unidade federativa e também se deparam com o
potencial hidroelétrico da região em que atuam.


```r
# Faz o join das tabelas dec e tarifas pelos campos mês, ano e cnpj
# usando o operador full_join para manter todas as linhas em ambas as tabelas.
base <- full_join(dec, tarifas, by = c('mes', 'ano' ,'cnpj')) %>% 
  # Faz o join da tabela resultante com a tabela de áreas de atuação das distribuidoras
  # pelo campo cnpj, usando o operador full_join.
  full_join(., area_atuacao_distribuidoras, by = c('cnpj')) %>% 
  # Faz o join da tabela resultante com a tabela de ACR mensal pelo campo data e uf,
  # usando o operador full_join.
  full_join(., acr_mensal, by = c('data', 'uf')) %>% 
  # Faz o join da tabela resultante com a tabela de ACL pelo campo mes, ano e região,
  # usando o operador full_join.
  full_join(., acl, by = c('mes','ano', 'regiao')) %>% 
  # Faz o join da tabela resultante com a tabela de satisfação mensal pelo campo mes, ano e cnpj,
  # usando o operador full_join.
  full_join(., base_satisfacao_mensal, by = c('mes','ano', 'cnpj')) %>% 
  # Faz o join da tabela resultante com a tabela de commodities pelo campo data,
  # usando o operador full_join.
  full_join(., comodities, by = 'data') %>% 
  # Faz o join da tabela resultante com a tabela de ENA por bacia e região
  # usando o operador full_join e os campos região e data.
  full_join(., ena_bacia, by = c('regiao', 'data')) %>% 
  # Renomeia a coluna data.y para data e cria colunas fator para algumas variáveis.
  mutate(data = data.y,
         sigla = as_factor(empresa),
         d_ano = as_factor(ano),
         cnpj = as_factor(cnpj),
         uf = as_factor(uf),
         mes_ano = as_factor(format_ISO8601(data, precision = "ym"))) %>% 
  # Seleciona as colunas desejadas e agrupa a tabela pelo campo data e cnpj.
  select(-c(data.x, data.y)) %>%
  group_by(data, cnpj) %>% 
  # Remove a atribuição de grupo, ordena a tabela pelo campo cnpj e data e 
  # atribui a tabela resultante para a variável "base".
  ungroup() %>% 
  arrange(cnpj, data)
```

Como a base criada possui informações incompletas e informações não
interessantes, será necessário remover algumas variáveis e criar outras
(como uma dummy para indicar a MP e as variáveis em primeira diferença,
caso seja necessário lidar com estacionariedade).


```r
base_regressao <- base %>% 
  ungroup() %>% # desagrupando base
  select(cnpj, data, mes_ano, dec, custo_acr, custo_acl, satisfacao, valor_percebido, index_brl,
         tarifa_uso, tarifa_energia, ano, ena, uf, regiao)  %>% # selecionando variáveis de interesse 
  arrange(cnpj, data) %>% # ordenando dados
  filter(ano <= 2020) %>% # selecionando período de interesse para análise
  mutate(d_mp = case_when(data >= '2012-09-01' ~ 1, T ~ 0), # criação da dummy para MP
         d_tarifaco = case_when(ano == '2015' ~ 1, T ~ 0),
         custo_acr = custo_acr/100,
         custo_acl = custo_acl/100) %>%  # criação da dummy para o traifaço
  select(-c(ano)) %>% # retirando variável não interessante
  distinct() %>% # removendo possíveis duplicatas
  drop_na() %>% # removendo valores não existentes
  group_by(cnpj) %>% # agrupando por índividuo
  mutate(diff_custo_acr = c(NA, diff(custo_acr)), # tomando diferenças
         diff_custo_acl = c(NA, diff(custo_acl)),
         diff_tarifa_uso = c(NA, diff(tarifa_uso)),
         diff_tarifa_energia = c(NA, diff(tarifa_energia)),
         diff_satisfacao = c(NA, diff(satisfacao)),
         diff_dec = c(NA, diff(dec)),
         diff_ena = c(NA, diff(ena)),
         diff_valor = c(NA, diff(valor_percebido))) %>%
  ungroup() # desagrupando base

# Contagem da frequência de cada indíviduo
freq <- base_regressao %>%
  group_by(cnpj) %>%
  summarise(n = n()) %>%
  ungroup()

# Filtrando indíviduos que mais aparecem na base
cnpj_selecionado <- freq %>%
  filter(n > 128) %>%
  pull(cnpj)

# Filtrando indíviduos que mais aparecem
base_regressao <- base_regressao %>%
  filter(cnpj %in% cnpj_selecionado) 

# Criando index para cada data

datas_unicas <- base_regressao %>% 
  select(mes_ano) %>% 
  distinct() %>% 
  arrange(mes_ano) %>% 
  mutate(tendencia = row_number())

base_regressao %<>%
  right_join(., datas_unicas, by = 'mes_ano')

# Verificando dimensões do painel
pdim(base_regressao, index = c('cnpj', 'mes_ano'))
```

```
## Unbalanced Panel: n = 65, T = 129-248, N = 9895
```

# Descritivas
## Evolução temporal

```r
font_add_google("Roboto", "Roboto")
font <- "Roboto"
tema <- theme(text = element_text(family = font),
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              legend.text = element_text(size = 12),
              axis.text.y = element_text(angle = 0, size = 12, face = 'plain'),
              axis.text.x = element_text(angle = 90, size = 12, face = 'plain'),
              axis.title = element_text(size = 14, face = 'plain'),
              axis.ticks.x = element_blank(),
              legend.title = element_text(size = 10),
              panel.grid = element_blank(), 
              plot.background = element_rect(fill = alpha("#ADD8E6", 0.25), 
                                       color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              legend.background = element_rect(fill = "transparent", color = NA))

#Gráfico do Custo Médio no ACL

base_regressao %>%
  group_by(data) %>%
  summarise(custo_acl = mean(custo_acl, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(as.POSIXct(data), custo_acl), size = 1.1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
  geom_hline(yintercept = c(2, 4, 6, 8), linetype = "dashed", 
             color = "gray50", size = 0.25) +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Livre',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#Gráfico do Custo Médio no ACR

base_regressao %>% 
  group_by(data) %>% 
  summarise(custo_acr = mean(custo_acr, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, custo_acr), size = 1.1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
   geom_hline(yintercept = c(1, 1.5, 2, 2.5), linetype = "dashed", 
             color = "gray50", size = 0.25) +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Regulada',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
#Gráfico da TE média

base_regressao %>% 
  group_by(data) %>% 
  summarise(tarifa_energia = mean(tarifa_energia, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, tarifa_energia), size = 1.1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
  geom_hline(yintercept = c(100, 150, 200, 250), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Tarifa de Energia (TE)',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Tarifa (R$/MWh)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
#Gráfico da TUSD média

base_regressao %>% 
  group_by(data) %>% 
  summarise(tarifa_uso = mean(tarifa_uso, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, tarifa_uso), size = 1.1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
  geom_hline(yintercept = c(150, 175, 200, 225), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Tarifa de Uso (TUSD)',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Tarifa (R$/MWh)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-4.png)<!-- -->

```r
#Gráfico da média mensal de satisfação

base_regressao %>% 
  group_by(data) %>% 
  summarise(satisfacao = mean(satisfacao, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, satisfacao), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")),
             color = "orange", size = 1) +
  geom_hline(yintercept = c(64, 68, 72, 76), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Satisfação',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Satisfação (0-100)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-5.png)<!-- -->

```r
#Gráfico da média mensal DEC

base_regressao %>% 
  group_by(data) %>% 
  summarise(dec = mean(dec, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, dec), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
   geom_hline(yintercept = c(1, 1.5, 2, 2.5), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Duração Equivalente de Interrupção 
       por Unidade Consumidora (DEC)',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'DEC',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-6.png)<!-- -->

```r
#Gráfico da média mensal ENA

base_regressao %>%  
  group_by(data) %>% 
  summarise(ena = mean(ena, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, ena), size = 1, color = "black") + 
  geom_hline(yintercept = c(1000, 2000, 3000, 4000), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal do ENA',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'ENA (MW/mês)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-12-7.png)<!-- -->

## Evolucão temporal (escala em diferenças)


```r
#Gráfico da primeira Diferença da Média mensal do custo no Ambiente de Contratação Livre

base_regressao %>% 
  group_by(data) %>% 
  summarise(custo_acl = mean(diff_custo_acl, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, custo_acl), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-2.5, 0, 2.5), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Livre',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média mensal do custo no Ambiente de Contratação Regulada

base_regressao %>% 
  group_by(data) %>% 
  summarise(custo_acr = mean(diff_custo_acr, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, custo_acr), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-0.1, 0, 0.1, 0.2), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Regulada',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média Mensal da TE

base_regressao %>% 
  group_by(data) %>% 
  summarise(tarifa_energia = mean(diff_tarifa_energia, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, tarifa_energia), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-10, 0, 10), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Tarifa de Energia (TE)',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'Tarifa (R$/MWh)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média Mensal da TUSD

base_regressao %>% 
  group_by(data) %>% 
  summarise(tarifa_uso = mean(diff_tarifa_uso, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, tarifa_uso), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-20,-10, 0, 10, 20), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Tarifa de Uso (TUSD)',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'Tarifa (R$/MWh$)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média Mensal da Satisfação

base_regressao %>% 
  group_by(data) %>% 
  summarise(satisfacao = mean(diff_satisfacao, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, satisfacao), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")),
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-1.5, -1, -0.5, 0, 0.5), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Satisfação',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'Satisfação',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-5.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média Mensal da DEC

base_regressao %>% 
  group_by(data) %>% 
  summarise(dec = mean(diff_dec, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, dec), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             size = 1, color = "orange") +
  geom_hline(yintercept = c(-1, -0.5, 0, 0.5, 1), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal da Duração Equivalente de Interrupção por Unidade 
       Consumidora (DEC)',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'DEC',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-6.png)<!-- -->

```r
#Gráfico da primeira Diferença da Média Mensal do ENA

base_regressao %>% 
  group_by(data) %>% 
  summarise(ena = mean(diff_ena, na.rm = T)) %>%
  ungroup() %>% 
  ggplot() + 
  geom_line(aes(data, ena), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), size = 1, 
             color = "orange") +
    geom_hline(yintercept = c(-3000, -2000, -1000, 0, 1000, 2000), 
               linetype = "dashed", 
             color = "gray60", size = 0.25) +
  labs(title = 'Média mensal do ENA',
       subtitle = 'Dados referentes a todo o Brasil, na primeira diferença',
       x = '',
       y = 'ENA (MW/mês)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-13-7.png)<!-- -->
## Descritivas 
#Variação dos dados


```r
# Subset dos dados antes de 2012-09-11

dados_pre <- subset(base_regressao, as.Date(data) < as.Date("2012-09-11"))

# Subset dos dados depois de 2012-09-11

dados_pos <- subset(base_regressao, as.Date(data) >= as.Date("2012-09-11"))

# Calcula as estatísticas resumidas para cada variável

media <- c(mean(dados_pre$custo_acl), mean(dados_pos$custo_acl),
           mean(dados_pre$custo_acr), mean(dados_pos$custo_acr),
           mean(dados_pre$tarifa_energia), mean(dados_pos$tarifa_energia), 
           mean(dados_pre$tarifa_uso), mean(dados_pos$tarifa_uso),
           mean(dados_pre$satisfacao), mean(dados_pos$satisfacao), 
           mean(dados_pre$dec), mean(dados_pos$dec), 
           mean(dados_pre$ena), mean(dados_pos$ena))

desvio_padrao <- c(sd(dados_pre$custo_acl), sd(dados_pos$custo_acl), 
                   sd(dados_pre$custo_acr), sd(dados_pos$custo_acr), 
                   sd(dados_pre$tarifa_energia), sd(dados_pos$tarifa_energia),
                   sd(dados_pre$tarifa_uso), sd(dados_pos$tarifa_uso), 
                   sd(dados_pre$satisfacao), sd(dados_pos$satisfacao), 
                   sd(dados_pre$dec), sd(dados_pos$dec), 
                   sd(dados_pre$ena), sd(dados_pos$ena))

mediana <- c(median(dados_pre$custo_acl), median(dados_pos$custo_acl), 
             median(dados_pre$custo_acr), median(dados_pos$custo_acr), 
             median(dados_pre$tarifa_energia), median(dados_pos$tarifa_energia), 
             median(dados_pre$tarifa_uso), median(dados_pos$tarifa_uso), 
             median(dados_pre$satisfacao), median(dados_pos$satisfacao), 
             median(dados_pre$dec), median(dados_pos$dec), 
             median(dados_pre$ena), median(dados_pos$ena))

# Cria uma tabela com as estatísticas resumidas

tabela <- data.frame(
  Variavel = c("custo_acl", "custo_acl_pos", "custo_acr", "custo_acr_pos", 
               "tarifa_energia", "tarifa_energia_pos", "tarifa_uso", "tarifa_uso_pos", 
               "satisfacao", "satisfacao_pos", "dec", "dec_pos", "ena", "ena_pos"),
  Periodo = c("Pré-MP-579", "Pós-MP-579", "Pré-MP-579", "Pós-MP-579",
              "Pré-MP-579", "Pós-MP-579", "Pré-MP-579", "Pós-MP-579",
              "Pré-MP-579", "Pós-MP-579", "Pré-MP-579", "Pós-MP-579",
              "Pré-MP-579", "Pós-MP-579"))


tabela <- cbind(tabela, Media = media, Desvio_Padrao = desvio_padrao, 
                Mediana = mediana)
tabela
```

```
##              Variavel    Periodo        Media Desvio_Padrao      Mediana
## 1           custo_acl Pré-MP-579    0.7203044     0.5715548    0.4555000
## 2       custo_acl_pos Pós-MP-579    2.8564533     1.9446825    2.3529000
## 3           custo_acr Pré-MP-579    1.1847511     0.2161415    1.2657000
## 4       custo_acr_pos Pós-MP-579    1.8310017     0.5253334    1.9600000
## 5      tarifa_energia Pré-MP-579  136.8657009    20.9214219  136.2800000
## 6  tarifa_energia_pos Pós-MP-579  169.9002379    76.4561601  176.5700000
## 7          tarifa_uso Pré-MP-579  216.5676340    49.0229136  212.1900000
## 8      tarifa_uso_pos Pós-MP-579  190.8000458    64.0518587  185.1400000
## 9          satisfacao Pré-MP-579   67.6176404    12.4282234   68.6311409
## 10     satisfacao_pos Pós-MP-579   69.1643551    10.2534801   68.7520166
## 11                dec Pré-MP-579    1.6607031     2.5267702    1.1025373
## 12            dec_pos Pós-MP-579    1.3249625     1.3940297    0.9401601
## 13                ena Pré-MP-579 2363.9677186  1650.7221535 1901.5888889
## 14            ena_pos Pós-MP-579 1938.3515377  1471.5625070 1586.4838710
```

```r
#Gráfico com média antes e depois para ACL

base_regressao %>%
  group_by(data) %>%
  summarise(custo_acl = mean(custo_acl, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(as.POSIXct(data), custo_acl), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
  geom_hline(yintercept = c(2, 4, 6, 8), linetype = "dashed", 
             color = "gray50", size = 0.25) +
  geom_segment(aes(x = as.POSIXct(min(base_regressao$data)), y = media[1], 
                  xend = as.POSIXct("2012-09-11"), 
                  yend = media[1]),
              color = "blue", size = 0.75, linetype = "longdash") +
  geom_segment(aes(x = as.POSIXct("2012-09-11"), y = media[2], 
                  xend = as.POSIXct(max(base_regressao$data)), yend = media[2]),
              color = "red", size = 0.75, linetype = "longdash") +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Livre',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
#Gráfico com média antes e depois para ACR

base_regressao %>%
  group_by(data) %>%
  summarise(custo_acr = mean(custo_acr, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(as.POSIXct(data), custo_acr), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")), 
             color = "orange", size = 1) +
  geom_hline(yintercept = c(1, 1.5, 2, 2.5), linetype = "dashed", 
             color = "gray50", size = 0.25) +
  geom_segment(aes(x = as.POSIXct(min(base_regressao$data)), y = media[3], 
                  xend = as.POSIXct("2012-09-11"), 
                  yend = media[3]),
              color = "blue", size = 0.75, linetype = "longdash") +
  geom_segment(aes(x = as.POSIXct("2012-09-11"), y = media[4], 
                  xend = as.POSIXct(max(base_regressao$data)), yend = media[4]),
              color = "red", size = 0.75, linetype = "longdash") +
  labs(title = 'Média mensal do Custo no Ambiente de Contratação Regulada',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Centenas de R$/MWh',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
#Gráfico com média antes e depois para Satisfação

base_regressao %>% 
  group_by(data) %>% 
  summarise(satisfacao = mean(satisfacao, na.rm = T)) %>%
  ggplot() + 
  geom_line(aes(data, satisfacao), size = 1, color = "black") + 
  geom_vline(xintercept = as.integer(as.POSIXct("2012-09-11")),
             color = "orange", size = 1) +
  geom_hline(yintercept = c(64, 68, 72, 76), linetype = "dashed", 
             color = "gray60", size = 0.25) +
  geom_segment(aes(x = as.POSIXct(min(base_regressao$data)), y = media[9], 
                  xend = as.POSIXct("2012-09-11"), 
                  yend = media[9]),
              color = "blue", size = 0.75, linetype = "longdash") +
  geom_segment(aes(x = as.POSIXct("2012-09-11"), y = media[10], 
                  xend = as.POSIXct(max(base_regressao$data)),
                  yend = media[10]),
              color = "red", size = 0.75, linetype = "longdash") +
  labs(title = 'Média mensal da Satisfação',
       subtitle = 'Dados referentes a todo o Brasil',
       x = '',
       y = 'Satisfação (0-100)',
       caption = 'Fonte: Agência Nacional de Energia Elétrica',
       fill = 'Distribuidora') +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b-%y") +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-14-3.png)<!-- -->


# Estratégia empírica

## Testes estatísticos

Para prosseguir com a criação do modelo econométrico é necessário
garantir que as variáveis numéricas sejam estacionárias. 

Como inicialmente o painel é extremamente desbalanceado, é necessário
deixá-lo mais balanceado. Deste modo, filtra-se os indivíduos que mais
aparecem, tendo como objetivo manter o maior valor de indivíduos de modo
que seja ainda possível realizar as funções para verificação de raiz unitária.

Para isso, foi utilizado o teste de raiz unitária proposto por [Im,Pesaran and Shin(2003)](https://www.sciencedirect.com/science/article/pii/S0304407603000927),
o qual também pode ser aplicável para painéis desbalanceados (ver
[Panel-Data Unit RootTests](https://www.stata.com/manuals/xtxtunitroot.pdf) 
O teste consiste
em tomar uma regressão de Dickey-Fuller no seguinte formato:\
$$\Delta y_{i,t}=\phi_iy_{i,t-1}+\mathbf{z´}_{i,t}\gamma_i+\varepsilon_{i,t}$$
Com a regressão em mãos, o objetivo será testar:\
$$H_0:\text{Todos painéis têm raiz unitária}\\
H_a:\text{Alguns painéis são estacionários}$$

Por fim, caso seja encontradas simultaneamente raiz unitária na variável dependente 
e raiz unitária em qualquer um dos regressores, pode-se verificar por meio de
um teste de cointegração, como proposto por [Pedroni (1999)](https://onlinelibrary.wiley.com/doi/10.1111/1468-0084.0610s1653), entre 
as variáveis e sendo encontrada cointegração, pode-se utilizar variável no nível
ao realizar a regressão, de modo a manter a interpretação original do coeficiente.


```r
# Criando painel para realizar testes de raiz unitária

painel_regressao <- pdata.frame(base_regressao, index = c("cnpj", "mes_ano"))

# Testes de raiz unitária utilizando o teste Im-Pesaran-Shin

purtest(custo_acl ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  custo_acl ~ 1
## Wtbar = -15.082, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(custo_acr ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  custo_acr ~ 1
## Wtbar = -0.015947, p-value = 0.4936
## alternative hypothesis: stationarity
```

```r
purtest(tarifa_uso ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  tarifa_uso ~ 1
## Wtbar = -1.5414, p-value = 0.06161
## alternative hypothesis: stationarity
```

```r
purtest(tarifa_energia ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  tarifa_energia ~ 1
## Wtbar = 3.4146, p-value = 0.9997
## alternative hypothesis: stationarity
```

```r
purtest(dec ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  dec ~ 1
## Wtbar = -37.909, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(ena ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  ena ~ 1
## Wtbar = -37.297, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(satisfacao ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  satisfacao ~ 1
## Wtbar = -12.466, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(valor_percebido ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  valor_percebido ~ 1
## Wtbar = 0.7224, p-value = 0.765
## alternative hypothesis: stationarity
```

Tendo em mãos as variáveis não estacionárias e tendo a primeira
diferença delas, precisamos garantir que esta será estacionária.\


```r
# Testes de raiz unitária utilizando o teste Im-Pesaran-Shin

purtest(diff_custo_acr ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  diff_custo_acr ~ 1
## Wtbar = -111.89, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(diff_tarifa_uso ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  diff_tarifa_uso ~ 1
## Wtbar = -79.359, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(diff_tarifa_energia ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  diff_tarifa_energia ~ 1
## Wtbar = -94.661, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

```r
purtest(diff_valor ~ 1, 
        data = painel_regressao, 
        index = c("cnpj", "mes_ano"), 
        lags = 'AIC', 
        test = "ips", 
        exo = 'trend')
```

```
## 
## 	Im-Pesaran-Shin Unit-Root Test (ex. var.: Individual Intercepts)
## 
## data:  diff_valor ~ 1
## Wtbar = -13.606, p-value < 2.2e-16
## alternative hypothesis: stationarity
```

## Estimação

### Especificação e resultados

Neste momento, como metodologia, será utilizada uma regressão de efeitos
fixos para indivíduos e tempo. 

Seja:  

-   A satisfação ($satisfacao$) é a medida de satisfação média dos consumidores para cada
    empresa. 

-   O custo ACL ($custo\_acl$), que é o custo, em centenas, no ACL (ambiente de contratação livre). 

-   A dummy MP ($d\_mp$) é o indicador para o período em que a MP
    esteve ativa.

-   A primeira diferença do custo ACR ($\Delta\_custo\_acr$), que é o custo, em centenas, no ACR (ambiente de contratação regulada). 

-   A primeira diferença da tarifa de energia ($\Delta\_tarifa\_energia$), que é a parcela da tarifa paga pelo cosumidor que corresponde aos repasses de custos de compra de energia pela distribuidora. 

-   A primeira diferença da tarifa de uso a ($\Delta\_tarifa\_uso$), que é a parcela da tarifa paga pelo cosumidor que corresponde aos repasses de custos com infraestrutura pela distribuidora. 

-   A DEC ($dec$), medida de qualidade média da empresa.  
-   O ENA ($dec$), medida de potencial de geração elétrica das bacias
    hidroelétricas na região daquela empresa.

Será então estimada uma regressão com estas variáveis, considerando
efeitos de índividuos ($\alpha_i$), efeitos de tempo ($\delta_t$), efeitos fixos de região ($\text{uf}_i$) e uma
variável numérica para controlar a tendência ($\text{tendencia}$).\

$$satifacao_{i,t} = \beta_1 + \beta_2custo\_acl_{i,t} + \beta_3custo\_acl_{i,t}\cdot d\_MP_t +\\\
\beta_3\Delta custo\_acr_{i,t} + \beta_4\Delta tarifa\_energia_{i,t} + \\
\beta_5DEC{i,t}+\beta_6ENA_{i,t}+\alpha_i+\delta_t+\text{uf}_i+regiao_i+\text{tendencia}+\varepsilon_{i,t}$$


```r
# Definição do modelo de regressão com efeitos fixos de cnpj e mês/ano
modelo_ef_tempo_diff <- plm(satisfacao ~ custo_acl*d_mp + diff_custo_acr + 
                         diff_tarifa_energia + dec + ena + uf + regiao + tendencia, 
                         data = base_regressao, # Base de dados utilizada
                         model = 'within', # Modelo de efeitos fixos
                         index = c('cnpj', 'mes_ano'), # Índices utilizados
                         effect = 'twoways' # Efeitos fixos de tempo
                 )
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```r
# Sumário do modelo para avaliar seus resultados
summary(modelo_ef_tempo_diff)
```

```
## Twoways effects Within Model
## 
## Call:
## plm(formula = satisfacao ~ custo_acl * d_mp + diff_custo_acr + 
##     diff_tarifa_energia + dec + ena + uf + regiao + tendencia, 
##     data = base_regressao, effect = "twoways", model = "within", 
##     index = c("cnpj", "mes_ano"))
## 
## Unbalanced Panel: n = 65, T = 128-247, N = 9830
## 
## Residuals:
##      Min.   1st Qu.    Median   3rd Qu.      Max. 
## -23.38138  -3.76605  -0.05225   3.47680  30.53020 
## 
## Coefficients:
##                        Estimate  Std. Error t-value  Pr(>|t|)    
## custo_acl            3.5451e+00  1.7240e+00  2.0564   0.03977 *  
## diff_custo_acr       1.0055e-01  3.6758e-01  0.2736   0.78443    
## diff_tarifa_energia -2.6248e-02  6.1420e-03 -4.2735 1.943e-05 ***
## dec                  2.5629e-02  4.3341e-02  0.5913   0.55430    
## ena                 -1.2066e-04  5.9877e-05 -2.0151   0.04392 *  
## custo_acl:d_mp      -3.7929e+00  1.7363e+00 -2.1845   0.02895 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    335070
## Residual Sum of Squares: 334100
## R-Squared:      0.0028888
## Adj. R-Squared: -0.017716
## F-statistic: 4.65002 on 6 and 9630 DF, p-value: 9.96e-05
```

Podemos testar também se é útil adicionar ou não a variável de tarifa de
uso.

Usando um teste F, pode-se verificar isso. Caso rejeitemos a hipótese
nula do teste, temos que o modelo sem o custo da tarifa de uso é uma
melhor escolha.\


```r
# Definição do modelo de regressão com efeitos fixos de cnpj e mês/ano
modelo_ef_tempo_diff2 <- plm(satisfacao ~ custo_acl*d_mp + diff_custo_acr + 
                         diff_tarifa_energia + diff_tarifa_uso + dec + ena + uf + regiao + tendencia, 
                         data = base_regressao, # Base de dados utilizada
                         model = 'within', # Modelo de efeitos fixos
                         index = c('cnpj', 'mes_ano'), # Índices utilizados
                         effect = 'twoways' # Efeitos fixos de tempo
                 )
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```r
# Sumário do modelo para avaliar seus resultados
summary(modelo_ef_tempo_diff2)
```

```
## Twoways effects Within Model
## 
## Call:
## plm(formula = satisfacao ~ custo_acl * d_mp + diff_custo_acr + 
##     diff_tarifa_energia + diff_tarifa_uso + dec + ena + uf + 
##     regiao + tendencia, data = base_regressao, effect = "twoways", 
##     model = "within", index = c("cnpj", "mes_ano"))
## 
## Unbalanced Panel: n = 65, T = 128-247, N = 9830
## 
## Residuals:
##       Min.    1st Qu.     Median    3rd Qu.       Max. 
## -23.367546  -3.759797  -0.044445   3.472813  30.360301 
## 
## Coefficients:
##                        Estimate  Std. Error t-value  Pr(>|t|)    
## custo_acl            3.5294e+00  1.7231e+00  2.0483 0.0405594 *  
## diff_custo_acr       9.4764e-02  3.6739e-01  0.2579 0.7964601    
## diff_tarifa_energia -2.7242e-02  6.1463e-03 -4.4322 9.431e-06 ***
## diff_tarifa_uso      1.8335e-02  5.5702e-03  3.2917 0.0009993 ***
## dec                  2.5324e-02  4.3319e-02  0.5846 0.5588362    
## ena                 -1.2691e-04  5.9877e-05 -2.1195 0.0340733 *  
## custo_acl:d_mp      -3.7815e+00  1.7354e+00 -2.1790 0.0293524 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    335070
## Residual Sum of Squares: 333730
## R-Squared:      0.0040096
## Adj. R-Squared: -0.016678
## F-statistic: 5.53774 on 7 and 9629 DF, p-value: 2.2332e-06
```

```r
# Teste de significância do modelo em relação ao modelo anterior (modelo_ef_tempo_diff)
pFtest(modelo_ef_tempo_diff2, modelo_ef_tempo_diff) 
```

```
## 
## 	F test for twoways effects
## 
## data:  satisfacao ~ custo_acl * d_mp + diff_custo_acr + diff_tarifa_energia +  ...
## F = 10.836, df1 = 1, df2 = 9629, p-value = 0.0009993
## alternative hypothesis: significant effects
```

```r
# Remoção do modelo da memória
rm(modelo_ef_tempo_diff2)
```

Podemos também verificar se a interação de outras variáveis com a MP faz
sentido estatístico.\


```r
# Definição do modelo de regressão com efeitos fixos de cnpj e mês/ano
modelo_ef_tempo_diff2 <- plm(satisfacao ~ custo_acl*d_mp + diff_custo_acr*d_mp + 
                         diff_tarifa_energia*d_mp + dec + ena + uf + regiao + tendencia, 
                         data = base_regressao, # Base de dados utilizada
                         model = 'within', # Modelo de efeitos fixos
                         index = c('cnpj', 'mes_ano'), # Índices utilizados
                         effect = 'twoways' # Efeitos fixos de tempo
                 )
```

```
## Warning in pdata.frame(data, index): duplicate couples (id-time) in resulting pdata.frame
##  to find out which, use, e.g., table(index(your_pdataframe), useNA = "ifany")
```

```r
# Sumário do modelo para avaliar seus resultados
summary(modelo_ef_tempo_diff2)
```

```
## Twoways effects Within Model
## 
## Call:
## plm(formula = satisfacao ~ custo_acl * d_mp + diff_custo_acr * 
##     d_mp + diff_tarifa_energia * d_mp + dec + ena + uf + regiao + 
##     tendencia, data = base_regressao, effect = "twoways", model = "within", 
##     index = c("cnpj", "mes_ano"))
## 
## Unbalanced Panel: n = 65, T = 128-247, N = 9830
## 
## Residuals:
##       Min.    1st Qu.     Median    3rd Qu.       Max. 
## -23.763575  -3.781087  -0.021483   3.475406  30.493278 
## 
## Coefficients:
##                             Estimate  Std. Error t-value Pr(>|t|)  
## custo_acl                 3.4238e+00  1.7245e+00  1.9854  0.04713 *
## diff_custo_acr           -2.7185e+00  1.5091e+00 -1.8014  0.07168 .
## diff_tarifa_energia      -6.5143e-02  3.0215e-02 -2.1560  0.03111 *
## dec                       2.5898e-02  4.3335e-02  0.5976  0.55011  
## ena                      -1.1286e-04  5.9961e-05 -1.8822  0.05984 .
## custo_acl:d_mp           -3.6629e+00  1.7369e+00 -2.1088  0.03498 *
## d_mp:diff_custo_acr       2.9964e+00  1.5561e+00  1.9256  0.05418 .
## d_mp:diff_tarifa_energia  4.0549e-02  3.0863e-02  1.3139  0.18893  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    335070
## Residual Sum of Squares: 333910
## R-Squared:      0.00345
## Adj. R-Squared: -0.017355
## F-statistic: 4.16643 on 8 and 9628 DF, p-value: 5.4732e-05
```

```r
# Teste de significância do modelo em relação ao modelo anterior (modelo_ef_tempo_diff)
pFtest(modelo_ef_tempo_diff2, modelo_ef_tempo_diff) 
```

```
## 
## 	F test for twoways effects
## 
## data:  satisfacao ~ custo_acl * d_mp + diff_custo_acr * d_mp + diff_tarifa_energia *  ...
## F = 2.7107, df1 = 2, df2 = 9628, p-value = 0.06654
## alternative hypothesis: significant effects
```

```r
# Remoção do modelo da memória
rm(modelo_ef_tempo_diff2)
```

Com o objetivo de dar uma aproximação causal ao projeto, buscou-se
utilizar a estratégia proposta por [Callaway, Goodman-Bacon & SantAnna(2021)](https://ideas.repec.org/p/arx/papers/2107.02637.html).

Deste modo, para garantir o uso de um modelo de diferenças em diferenças
com tratamento contínuo é necessário garantir a condição forte de
tendências paralelas. Para isso, tomamos a mesma estratégia adotada por
[Korovkin & Makarin(2023)](https://www.aeaweb.org/articles?id=10.1257/aer.20191701):

$$satifacao_{i,t} = \beta_1 + \beta_2custo\_acl_{i,t} + \beta_3custo\_acl_{i,t}*\delta_t +\\\
\beta_3\Delta custo\_acr_{i,t} + \beta_4\Delta tarifa\_energia_{i,t} + \\
\beta_5DEC{i,t}+\beta_6ENA_{i,t}+\alpha_i+\delta_t+\text{uf}_i+\text{tendencia}+\varepsilon_{i,t}$$

Logo, caso antes do início do período de tratamento os coeficientes de
interação da variável de tratamento com a variável de tempo sejam
irrelevantes estatisticamente, pode-se obter evidências favoráveis à
suposição forte de tendências paralelas, o que permite o uso da
metodologia de diferenças em diferenças com tratamento contínuo.


```r
# Ajustando o modelo com interação efeitos fixos de tempo e de entidade
modelo_trends <- lm(satisfacao~custo_acl*relevel(mes_ano, ref = "2012-09")+
                      diff_custo_acr+diff_tarifa_energia+
                      dec+ena+tendencia+regiao+uf+tendencia+
                      relevel(mes_ano, ref = "2012-09")+cnpj, # definindo variáveis
                         data = base_regressao # definindo base
                         )

# Obtendo as estimativas do modelo e filtrando apenas as variáveis relacionadas à interação
df_modelo <- sjPlot::get_model_data(
  modelo_trends,
  type = "est",
  digits = 2) %>% 
  select(term, estimate, conf.low, conf.high, p.value) %>% 
  mutate(estimate = round(estimate, 4),
         conf.low = round(conf.low, 4),
         conf.high = round(conf.high, 4),
         p.value = round(p.value, 4),
         relevance = case_when(p.value <= 0.05 ~ 'relevant', T ~ 'not relevant')) %>% 
  filter(grepl("custo_acl:relevel",term))
```

```
## Model matrix is rank deficient. Parameters `tendencia, ufRJ, ufTO, ufPI,
##   cnpj10835932000108, cnpj15139629000194, cnpj08324196000181,
##   cnpj27485069000109, cnpj19527639000158, cnpj07047251000170,
##   cnpj09095183000140, cnpj13017462000163, cnpj25086034000171,
##   cnpj06272793000184, cnpj06840748000189, cnpj79850574000109,
##   cnpj85318640000105, cnpj46598678000119, cnpj31465487000101,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2011-04,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2011-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2012-07,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2012-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2013-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2014-08,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2014-09,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2014-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2014-12,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2015-01,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2015-02,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2015-06,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2015-08,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2015-09,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2016-10,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2016-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2016-12,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2017-07,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2017-08,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2017-09,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2017-10,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2018-07,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2018-08,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2018-10,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2018-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2019-06,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2019-10,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2019-11,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2019-12,
##   custo_acl:relevel(mes_ano, ref = "2012-09")2020-04` were not estimable.
```

```r
# Tratando as informações para gerar o gráfico
df_modelo$term <- gsub("custo_acl:relevel\\(mes_ano, ref = \"2012-09\"\\)", "", df_modelo$term)
df_modelo %<>% 
  mutate(ano = year(ymd(paste0(term, "-01"))),
         mes = month(ymd(paste0(term, "-01"))),
         mes_ano = paste(ano, sprintf("%02d", mes), sep = "-"),
         data = as.Date(paste0(mes_ano, "-01")))

# Incluindo uma linha para a data de referência da interação
df_modelo <- bind_rows(df_modelo, tibble(
  data = as.Date("2012-09-01"),
  estimate = 0,
  conf.low = 0,
  conf.high = 0
))

# Gerando o gráfico com os resultados das estimativas
df_modelo %>% 
  filter(ano <= 2016) %>% 
  ggplot(aes(data, estimate)) +
  geom_point(size = 1, fill = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, 
                color = "black", size = 0.8) +
  geom_vline(xintercept = as.Date("2012-09-11"), size = 1, color = "orange") +
  geom_hline(yintercept = 0, colour = 'red') +
  labs(title = 'Estimativas da interação entre o custo do ACL e 
       a variável de tempo',
       x = '',
       y = 'Estimativa') +
  theme_light() +
  tema
```

![](script_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

## Resultados e testes de robustez

Ao analisar os resultados obtidos pelo modelo de efeitos fixos, nota-se
que o parâmetro de interesse é negativo e relevante, o que mostra que
sob a MP, o custo no mercado livre de energia traz efeitos negativos
sobre a satisfação do consumidor.


```r
summary(modelo_ef_tempo_diff)
```

```
## Twoways effects Within Model
## 
## Call:
## plm(formula = satisfacao ~ custo_acl * d_mp + diff_custo_acr + 
##     diff_tarifa_energia + dec + ena + uf + regiao + tendencia, 
##     data = base_regressao, effect = "twoways", model = "within", 
##     index = c("cnpj", "mes_ano"))
## 
## Unbalanced Panel: n = 65, T = 128-247, N = 9830
## 
## Residuals:
##      Min.   1st Qu.    Median   3rd Qu.      Max. 
## -23.38138  -3.76605  -0.05225   3.47680  30.53020 
## 
## Coefficients:
##                        Estimate  Std. Error t-value  Pr(>|t|)    
## custo_acl            3.5451e+00  1.7240e+00  2.0564   0.03977 *  
## diff_custo_acr       1.0055e-01  3.6758e-01  0.2736   0.78443    
## diff_tarifa_energia -2.6248e-02  6.1420e-03 -4.2735 1.943e-05 ***
## dec                  2.5629e-02  4.3341e-02  0.5913   0.55430    
## ena                 -1.2066e-04  5.9877e-05 -2.0151   0.04392 *  
## custo_acl:d_mp      -3.7929e+00  1.7363e+00 -2.1845   0.02895 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    335070
## Residual Sum of Squares: 334100
## R-Squared:      0.0028888
## Adj. R-Squared: -0.017716
## F-statistic: 4.65002 on 6 and 9630 DF, p-value: 9.96e-05
```

### Teste de heterocedasticidade

Dando ínicio aos testes do modelo escolhido, pode-se testar se os erros
do modelo estimado são homocedásticos ou não. 

Usando um teste de
Breusch-Pagan pode-se verificar se a hipótese nula de homocedasticidade
é falseável ou não.


```r
lmtest::bptest(satisfacao~custo_acl*d_mp+diff_custo_acr+ 
                         diff_tarifa_energia+
                         dec+ena+tendencia+mes_ano+cnpj+regiao+uf, 
               data = base_regressao, 
               studentize=F)
```

```
## 
## 	Breusch-Pagan test
## 
## data:  satisfacao ~ custo_acl * d_mp + diff_custo_acr + diff_tarifa_energia +     dec + ena + tendencia + mes_ano + cnpj + regiao + uf
## BP = 4539.7, df = 199, p-value < 2.2e-16
```

Perante os resultados do teste, pode-se utilizar métodos que nos dê
estimadores consistentes.


```r
lmtest::coeftest(modelo_ef_tempo_diff, 
                 vcovHC(modelo_ef_tempo_diff, method = "arellano"))
```

```
## 
## t test of coefficients:
## 
##                        Estimate  Std. Error t value Pr(>|t|)    
## custo_acl            3.5451e+00  1.9870e+00  1.7841 0.074433 .  
## diff_custo_acr       1.0055e-01  1.6201e-01  0.6207 0.534833    
## diff_tarifa_energia -2.6248e-02  7.3266e-03 -3.5825 0.000342 ***
## dec                  2.5629e-02  1.0861e-01  0.2360 0.813463    
## ena                 -1.2066e-04  7.3866e-05 -1.6335 0.102399    
## custo_acl:d_mp      -3.7929e+00  2.0971e+00 -1.8086 0.070542 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
