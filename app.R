library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)
library(readxl)
library(stringi)
library(shinyWidgets)
library(officer)
library(ggplot2)
library(scales)
library(rvg)
library(dplyr)
library(sf)
library(shinycssloaders)
library(writexl)

#### CAMINHOS ####

# O painel lê sempre de uma cópia local (dados/processados), para funcionar
# tanto local quanto publicado no shinyapps.io (que não tem acesso às pastas
# irmãs). Essa pasta é ignorada pelo .gitignore — nunca vai para o GitHub.
DADOS_LOCAIS <- file.path("dados", "processados")
dir.create(DADOS_LOCAIS, recursive = TRUE, showWarnings = FALSE)

DIR_TABELAS_CIRURGIA <- DADOS_LOCAIS
DIR_RESULT_OCI       <- DADOS_LOCAIS

# Projetos irmãos (só existem em execução local; ausentes quando publicado).
CIRURGIA_DIR_ORIGEM <- normalizePath(file.path("..", "Cirurgia"), mustWork = FALSE)
OCI_DIR_ORIGEM      <- normalizePath(file.path("..", "OCI"), mustWork = FALSE)
SEMAFORO_DIR_ORIGEM <- normalizePath(file.path("..", "Analise_Espacial", "app_semaforo", "dados"), mustWork = FALSE)
CONSULTAS_EXAMES_DIR_ORIGEM <- normalizePath(file.path("..", "Consultas_Exames"), mustWork = FALSE)

DIR_RESULT_CONSULTAS_EXAMES <- DADOS_LOCAIS

DIR_SEMAFORO <- file.path(DADOS_LOCAIS, "semaforo")
dir.create(DIR_SEMAFORO, recursive = TRUE, showWarnings = FALSE)

MES_LABELS <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

# Abreviações de mês em minúsculo (pt-BR) usadas pela coluna MÊS da base
# da Portaria 9.810.
MESES_ABREV_PT <- tolower(MES_LABELS)

# Rótulo da "UF" que agrupa os registros de OCI sem UF de atendimento.
OCI_UF_NAO_INFORMADA <- "NÃO INFORMADA"

# Aba "Variação Cirurgias" (mapas do Analise_Espacial). Desligada por enquanto,
# até os mapas serem regenerados com os meses mais recentes; TRUE religa a aba.
MOSTRAR_VARIACAO_CIRURGIAS <- FALSE

# Quantas das últimas competências de OCI ainda não fecharam (a competência
# fechada mais recente é a última disponível menos este número de meses).
OCI_MESES_PRELIMINARES <- 2L
MESES_PT <- c(
  "janeiro", "fevereiro", "março", "abril", "maio", "junho",
  "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
)

# Rótulos "Mês/AA" em português para eixos de data (o Plotly só formata
# datas em inglês por padrão, sem carregar um locale de JS à parte).
rotular_mes_ano_pt <- function(datas) {
  paste0(MES_LABELS[as.integer(format(datas, "%m"))], "/", format(datas, "%y"))
}

# Formatação numérica brasileira (milhar "." / decimal ",") para eixos,
# rótulos de dados e textos de hover — usada em todos os gráficos ggplot.
label_pt_num   <- scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
label_pt_moeda <- scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)

CORES_CLASSIFICACAO <- c(
  "Esperado"                           = "#479139",
  "Acima do esperado"                  = "#4DA3FF",
  "Acima do limite esperado"           = "#0040FF",
  "Atenção"                            = "#EAFF00",
  "Crítico abaixo do limite esperado"  = "#FF0000"
)

DATA_VIRADA_OCI <- as.Date("2026-01-01")

# Rótulos por indicador de Cirurgias, usados no título dos gráficos e na
# tabela — mesmo texto do radioButtons "Indicador" no sidebar.
ROTULOS_INDICADOR_CIRURGIA <- c(
  rol       = "Cirurgias Eletivas (MAC e FAEC) do ROL",
  total     = "Cirurgias Eletivas (MAC e FAEC) totais",
  total_pab = "Cirurgias Eletivas (MAC, FAEC e PAB)",
  pnrf      = "Cirurgias Eletivas do Programa (PNRF)"
)

# Quantos procedimentos entram no ranking (sub-aba "Ranking de
# Procedimentos") e códigos do subgrupo SIGTAP 04.15 (Cirurgia Múltipla) —
# mesmos valores do script 01_cirurgias_sam.R (projeto SAM) e do script
# 11_ranking_procedimentos_cirurgia.R (projeto Cirurgia).
TOP_N_RANKING_CIRURGIA <- 20L
CODIGOS_PROCEDIMENTO_MULTIPLO_CIRURGIA <- c(415010012L, 415020034L, 415020050L)

ORDEM_ESPECIALIDADES <- c(
  "Cardiologia", "Oftalmologia", "Oncologia",
  "Ortopedia", "Otorrinolaringologia", "Saúde Mulher"
)

# Quantos procedimentos entram no gráfico "por mês de atendimento" da
# sub-aba "Especialidade e Procedimentos" (a tabela ao lado mostra todos).
TOP_N_PROCEDIMENTOS_OCI <- 7L

# Mesma ideia, para o gráfico de linha "mais realizados por mês" da sub-aba
# "Ranking de Exames" — a tabela de ranking ao lado mostra todos os 27
# exames estratégicos.
TOP_N_EXAMES_CE <- 5L

# Agrupamento dos exames estratégicos eletivos, mesma classificação usada no
# projeto Consultas_Exames (script 03_exportar_painel.R, a partir dos
# comentários de 02_monitor_consultas_exames.R) — "Ressonância Magnética" e
# "Tomografia Computadorizada" são modalidades de exame, não especialidades
# médicas, mas o projeto de origem já agrupava assim.
ORDEM_ESPECIALIDADES_CE <- c(
  "Cardiologia", "Ginecologia", "Oftalmologia", "Oncologia", "Ortopedia",
  "Otorrinolaringologia", "Ressonância Magnética", "Tomografia Computadorizada"
)

# Ordem de região da "Tabela Geral" de Consultas e Exames — mesma ordem da
# planilha de referência do projeto Consultas_Exames (Centro-Oeste antes de
# Sudeste), diferente da ordem padrão do resto do painel (UF_REF).
ORDEM_REGIAO_CE <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

# Mesma paleta usada no script de origem (Monitoramento_oci_uf_2025_2026.R),
# para manter a identidade visual entre o relatório estático e o painel.
CORES_ESPECIALIDADE <- c(
  "Cardiologia"          = "#ff0035",
  "Oftalmologia"         = "#1e96fc",
  "Oncologia"            = "#ffbc42",
  "Ortopedia"            = "#386641",
  "Otorrinolaringologia" = "#044389",
  "Saúde Mulher"         = "#ff4d6d"
)

# Paleta institucional para gráficos com uma linha/barra por ano: do cinza
# muito claro (anos mais antigos) ao verde/turquesa institucional (ano mais
# recente, destacado) — em vez de cores saturadas concorrendo entre si.
# Fixa por ano (não cíclica); anos fora da faixa mapeada caem no cinza de
# fallback, para nunca ficar sem cor mesmo se a base ganhar um ano novo.
CORES_ANOS_INSTITUCIONAL <- c(
  "2022" = "#D9DEE3", "2023" = "#AEB8C2", "2024" = "#7C93AC",
  "2025" = "#1769AA", "2026" = "#18B997"
)
COR_ANO_FALLBACK <- "#AEB8C2"

# Componentes/modalidades da OCI (novidade do Dataset.csv em relação ao
# antigo Planilhão, que só trazia o Componente Ambulatorial) — mesmos
# rótulos usados pelo script de origem (Monitoramento_oci_uf_2025_2026.R).
ORDEM_COMPONENTES_OCI <- c(
  "Componente Ambulatorial", "Carretas", "Créditos Financeiros", "Equipes Volantes"
)

# Mesmas cores do gráfico de referência do projeto OCI (por
# componente/modalidade) — mantidas exatamente iguais nos gráficos do
# painel que quebram por componente.
CORES_COMPONENTE_OCI <- c(
  # "Total geral" em cinza-escuro: o azul de Carretas (#0C3988) é escuro
  # demais para conviver com um segundo azul na linha do total.
  "Total geral de OCI"    = "#3D4B5A",
  "Componente Ambulatorial" = "#F5C242",
  "Carretas"                = "#0C3988",
  "Créditos Financeiros"    = "#39DAC7",
  "Equipes Volantes"        = "#60A6C4"
)

# Mesma ideia, para o detalhamento FAEC/Outros componentes dentro da OCI
# nos gráficos de Consultas/Teleconsultas e Exames Estratégicos Eletivos
# (coluna empilhada "Fora da OCI" + os dois sub-componentes).
CORES_OCI_CE <- c(
  "Fora da OCI"                     = "#1769AA",
  "Dentro OCI (FAEC)"               = "#D64545",
  "Dentro OCI — Outros componentes" = "#F5C242"
)

# Escolhas do filtro de componente na subaba "Série histórica OCI por
# especialidade": "geral" soma os 4 componentes, os demais valores batem
# exatamente com a coluna COMPONENTE dos exports.
COMPONENTES_OCI_FILTRO <- c(
  "OCI geral"             = "geral",
  "Componente Ambulatorial" = "Componente Ambulatorial",
  "Carretas"                = "Carretas",
  "Créditos Financeiros"    = "Créditos Financeiros",
  "Equipes Volantes"        = "Equipes Volantes"
)

# Anos com dados no export atual do projeto OCI (mesmo escopo do script
# Monitoramento_oci_uf_2025_2026.R) — filtro exclusivo do gráfico "OCI por
# especialidade e componente".
ANOS_OCI <- c(2025, 2026)

cores_para_anos <- function(anos) {
  anos <- sort(unique(anos))
  anos_chr <- as.character(anos)
  cores <- unname(CORES_ANOS_INSTITUCIONAL[anos_chr])
  cores[is.na(cores)] <- COR_ANO_FALLBACK
  setNames(cores, anos)
}

# Tabela de referência de UF/região. NM_UF_OCI segue a grafia acentuada usada
# na planilha de OCI; NM_UF_CIRURGIA é a versão sem acento usada nas tabelas
# de cirurgia (mesma convenção dos dois projetos irmãos).
UF_REF <- fread(text = "
SG_UF;NM_UF_OCI;REGIAO;ORDEM_REGIAO;ORDEM_UF
RO;RONDÔNIA;Norte;1;1
AC;ACRE;Norte;1;2
AM;AMAZONAS;Norte;1;3
RR;RORAIMA;Norte;1;4
PA;PARÁ;Norte;1;5
AP;AMAPÁ;Norte;1;6
TO;TOCANTINS;Norte;1;7
MA;MARANHÃO;Nordeste;2;1
PI;PIAUÍ;Nordeste;2;2
CE;CEARÁ;Nordeste;2;3
RN;RIO GRANDE DO NORTE;Nordeste;2;4
PB;PARAÍBA;Nordeste;2;5
PE;PERNAMBUCO;Nordeste;2;6
AL;ALAGOAS;Nordeste;2;7
SE;SERGIPE;Nordeste;2;8
BA;BAHIA;Nordeste;2;9
MG;MINAS GERAIS;Sudeste;3;1
ES;ESPÍRITO SANTO;Sudeste;3;2
RJ;RIO DE JANEIRO;Sudeste;3;3
SP;SÃO PAULO;Sudeste;3;4
PR;PARANÁ;Sul;4;1
SC;SANTA CATARINA;Sul;4;2
RS;RIO GRANDE DO SUL;Sul;4;3
MS;MATO GROSSO DO SUL;Centro-Oeste;5;1
MT;MATO GROSSO;Centro-Oeste;5;2
GO;GOIÁS;Centro-Oeste;5;3
DF;DISTRITO FEDERAL;Centro-Oeste;5;4
", sep = ";", encoding = "UTF-8")

UF_REF[, NM_UF_CIRURGIA := toupper(stri_trans_general(NM_UF_OCI, "Latin-ASCII"))]

REGIOES <- unique(UF_REF[order(ORDEM_REGIAO)]$REGIAO)

#### FUNÇÕES DE LEITURA (sempre a partir das planilhas já prontas) ####

localizar_arquivo <- function(diretorio, padrao) {
  arquivos <- list.files(diretorio, pattern = padrao, full.names = TRUE)
  if (length(arquivos) == 0L) {
    return(NA_character_)
  }
  arquivos[which.max(file.info(arquivos)$mtime)]
}

carregar_serie_cirurgia <- function(indicador) {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    paste0("^serie_completa_", indicador, "_.*\\.csv$")
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[]
}

carregar_serie_anos_cirurgia <- function(indicador) {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    paste0("^serie_anos_", indicador, "_.*\\.csv$")
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[]
}

# Série multianual por município (Comparação Anos com filtro de Município).
# Mesmo esquema de carregar_serie_anos_cirurgia(), mais granular.
carregar_serie_municipio_cirurgia <- function(indicador) {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    paste0("^serie_anos_municipio_", indicador, "_.*\\.csv$")
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[, municipio_atendimento := toupper(municipio_atendimento)]
  dt[]
}

# Série por município e tipo de gestão (Estadual/Municipal/Dupla), usada só
# em "Comparação Anos" quando o filtro de Gestão está ativo. Gerada pelo
# script 10_series_gestao_comparacao_anos.R do projeto Cirurgia.
carregar_serie_gestao_cirurgia <- function(indicador) {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    paste0("^serie_anos_municipio_gestao_", indicador, "_.*\\.csv$")
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[, municipio_atendimento := toupper(municipio_atendimento)]
  dt[, gestao := toupper(gestao)]
  dt[]
}

# Série por UF e procedimento, só do indicador ROL — usada pelos filtros de
# Especialidade/Procedimento em "Comparação Anos". Cruza pelo código SIGTAP
# com a planilha de mapeamento (ver carregar_mapa_especialidade_rol()).
carregar_serie_procedimento_rol <- function() {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    "^cirurgias_mensal_procedimento_rol_.*\\.csv$"
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[, codigo_procedimento_principal := as.character(as.integer(codigo_procedimento_principal))]
  dt[]
}

# Ranking de procedimentos por UF x ano (Eletivas totais e do ROL), grão
# procedimento PRINCIPAL — usado pela sub-aba "Ranking de Procedimentos" da
# aba Cirurgias. Gerado pelo script 11_ranking_procedimentos_cirurgia.R do
# projeto Cirurgia (mesma regra das análises do SAM).
carregar_ranking_procedimento_cirurgia <- function() {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    "^cirurgias_ranking_procedimento_uf_.*\\.csv$"
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[]
}

# Tabela auxiliar por UF x ano x procedimento SECUNDÁRIO, só para os códigos
# de Cirurgia Múltipla (CODIGOS_PROCEDIMENTO_MULTIPLO_CIRURGIA) — usada para
# substituir esses códigos pelo secundário mais frequente dentro da seleção
# de Região/UF do usuário (ver carregar_ranking_procedimento_cirurgia()).
carregar_ranking_secundario_cirurgia <- function() {

  arquivo <- localizar_arquivo(
    DIR_TABELAS_CIRURGIA,
    "^cirurgias_ranking_secundario_uf_.*\\.csv$"
  )

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, encoding = "UTF-8")
  dt[, uf_atendimento := toupper(uf_atendimento)]
  dt[]
}

# Mapeamento código SIGTAP -> Especialidade, mantido manualmente em
# dados/Relacao_cirugiasROL.xlsx (não é sincronizado do projeto Cirurgia —
# é uma planilha de referência própria do painel).
carregar_mapa_especialidade_rol <- function() {

  arquivo <- file.path("dados", "Relacao_cirugiasROL.xlsx")

  if (!file.exists(arquivo)) {
    return(NULL)
  }

  mapa <- setDT(as.data.frame(read_excel(arquivo)))
  setnames(mapa, c("codigo_procedimento_principal", "nome_procedimento", "especialidade"))

  mapa[, codigo_procedimento_principal := as.character(as.integer(codigo_procedimento_principal))]
  mapa[, especialidade := stri_trans_totitle(especialidade)]
  mapa[, nome_procedimento := stri_trans_totitle(nome_procedimento)]
  mapa[]
}

# Pagamentos da Portaria nº 9.810 (Valor Líquido), mantido manualmente em
# dados/Base_pagamento/ (não vem de projeto irmão — é uma planilha de
# referência própria do painel, extraída do Painel FAF/InvestSUS — ver nota
# na sidebar da aba). Pega sempre o .xlsx mais recente da pasta, já que o
# nome do arquivo muda a cada nova exportação. Regra fixa da aba: o arquivo
# bruto traz outras portarias misturadas, então só entram linhas com
# NU_PORTARIA 09810/9810 — a Portaria GM/MS nº 12.174/2026, que alterou o
# limite da 9.810, não trouxe um novo código de portaria na execução
# financeira (conferido na extração de 03/10/2026: só aparecem "09810" e
# "9810"), então esse filtro continua correto. Data de pagamento = ANO +
# MÊS (coluna de texto, ex. "set").
# Resume os valores da coluna PROGRAMA (nomes longos e técnicos) nos 5
# rótulos usados como filtro de Componente no painel. FAEC - PMAE e FAEC
# PNRF levam "*" porque são Despesa de Exercício Anterior (ver nota na
# sidebar da aba).
mapear_programa_portaria9810 <- function(programa) {

  programa_norm <- stri_trans_general(toupper(trimws(programa)), "Latin-ASCII")

  fcase(
    stri_detect_fixed(programa_norm, "COMPONENTE AMBULATORIAL"), "Componente Ambulatorial",
    stri_detect_fixed(programa_norm, "COMPONENTE CIRURGICO"), "Componente Cirúrgico",
    stri_detect_fixed(programa_norm, "MUTIRAO"), "Mutirão",
    stri_detect_fixed(programa_norm, "PMAE"), "FAEC - PMAE*",
    stri_detect_fixed(programa_norm, "REDUCAO DAS FILAS"), "FAEC PNRF*",
    default = programa_norm
  )
}

carregar_portaria9810 <- function() {

  arquivo <- localizar_arquivo(file.path("dados", "Base_pagamento"), "\\.xlsx$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  bruto <- setDT(as.data.frame(read_excel(arquivo, sheet = "Sheet1")))
  bruto <- bruto[NU_PORTARIA %chin% c("09810", "9810")]

  dt <- bruto[, .(
    SG_UF = toupper(trimws(UF)),
    CO_MUNICIPIO_IBGE,
    MUNICIPIO = toupper(trimws(MUNICIPIO)),
    TIPO_GESTAO = stri_trans_general(toupper(trimws(TP_REPASSE)), "Latin-ASCII"),
    COMPONENTE = mapear_programa_portaria9810(PROGRAMA),
    ANO = as.integer(ANO),
    MES_ABREV = tolower(trimws(`MÊS`)),
    VALOR_LIQUIDO = suppressWarnings(as.numeric(`Valor Liquido`))
  )]

  dt[, MES := match(MES_ABREV, MESES_ABREV_PT)]
  dt <- dt[!is.na(SG_UF) & SG_UF != "" & !is.na(ANO) & !is.na(MES)]
  dt[, DATA_PAGAMENTO := as.Date(sprintf("%04d-%02d-01", ANO, MES))]

  dt <- merge(dt, UF_REF[, .(SG_UF, NM_UF_OCI, REGIAO)], by = "SG_UF", all.x = TRUE)
  setnames(dt, "NM_UF_OCI", "NM_UF")
  dt[, NM_UF := factor(NM_UF, levels = UF_REF$NM_UF_OCI)]

  dt[]
}

# Limite de repasse por UF — Anexo da Portaria nº 9.810 com a redação dada
# pela Portaria GM/MS nº 12.174/2026 (ver nota na aba), mantido manualmente
# em dados/PORTARIA_12.174_UF.xlsx.
carregar_limite_portaria9810 <- function() {

  arquivo <- file.path("dados", "PORTARIA_12.174_UF.xlsx")

  if (!file.exists(arquivo)) {
    return(NULL)
  }

  dt <- setDT(as.data.frame(read_excel(arquivo, sheet = "Planilha1")))
  setnames(dt, c("SG_UF", "POPULACAO_ESTIMADA", "VALOR_LIMITE"))

  dt[, SG_UF := toupper(trimws(SG_UF))]
  # A planilha traz uma linha "TOTAL" e rodapés (título/fonte da portaria)
  # abaixo da tabela de UFs — descarta tudo que não for uma sigla válida.
  dt <- dt[SG_UF %chin% UF_REF$SG_UF]

  dt[, VALOR_LIMITE := suppressWarnings(as.numeric(VALOR_LIMITE))]
  dt[, POPULACAO_ESTIMADA := suppressWarnings(as.numeric(POPULACAO_ESTIMADA))]

  dt <- merge(dt, UF_REF[, .(SG_UF, NM_UF_OCI, REGIAO)], by = "SG_UF", all.x = TRUE)
  setnames(dt, "NM_UF_OCI", "NM_UF")
  dt[, NM_UF := factor(NM_UF, levels = UF_REF$NM_UF_OCI)]

  dt[]
}

carregar_serie_oci <- function() {

  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^planilha_OCI_UF_mes_.*\\.xlsx$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  wide <- setDT(as.data.frame(read_excel(arquivo)))

  longo <- melt(
    wide,
    id.vars = "NM_UF",
    variable.name = "competencia",
    value.name = "oci"
  )

  longo[, NM_UF := toupper(NM_UF)]
  longo[, competencia := as.Date(paste0(as.character(competencia), "-01"))]
  longo <- merge(
    longo,
    UF_REF[, .(NM_UF_OCI, SG_UF, REGIAO)],
    by.x = "NM_UF",
    by.y = "NM_UF_OCI",
    all.x = TRUE
  )
  setorder(longo, NM_UF, competencia)
  longo[]
}

# Quebra por UF/especialidade/componente-modalidade (Componente Ambulatorial,
# Carretas, Créditos Financeiros, Equipes Volantes), usada pelo gráfico de
# componentes em "Série histórica OCI" e pela subaba "Por especialidade".
carregar_oci_especialidade_componente_uf <- function() {

  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_mensal_especialidade_componente_uf\\.csv$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, sep = ";", dec = ",", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
  dt[]
}

# Mesmo esquema de carregar_oci_especialidade_componente_uf(), granularidade
# de município.
carregar_oci_especialidade_componente_municipio <- function() {

  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_mensal_especialidade_componente_municipio\\.csv$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, sep = ";", dec = ",", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt[, MUNICIPIO := toupper(MUNICIPIO)]
  dt[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
  dt[]
}

# Ranking de procedimentos de OCI por Especialidade, grão UF x ano x mês x
# procedimento — usado pela sub-aba "Especialidade e Procedimentos".
# Gerado por OCI/ranking_procedimentos_oci.R.
carregar_oci_ranking_procedimento_uf <- function() {

  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_ranking_procedimento_uf\\.csv$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, sep = ";", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt[]
}

# Mesmo esquema de carregar_oci_ranking_procedimento_uf(), granularidade de
# município (sem a região "Não informada" — contatos sem UF/atendimento não
# têm município).
carregar_oci_ranking_procedimento_municipio <- function() {

  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_ranking_procedimento_municipio\\.csv$")

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, sep = ";", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt[, MUNICIPIO := toupper(MUNICIPIO)]
  dt[]
}

# Consultas/Teleconsultas e Exames Estratégicos Eletivos (projeto
# Consultas_Exames, script 03_exportar_painel.R) — grão UF x ano x mês x
# [TIPO ou ESPECIALIDADE]. `padrao` casa o nome do arquivo; o resultado já
# vem com NM_UF (nome por extenso, cruzado com UF_REF) e, no grão
# município, MUNICIPIO em maiúsculo (mesma convenção dos demais loaders).
carregar_consultas_exames <- function(padrao) {

  arquivo <- localizar_arquivo(DIR_RESULT_CONSULTAS_EXAMES, padrao)

  if (is.na(arquivo)) {
    return(NULL)
  }

  dt <- fread(arquivo, sep = ";", encoding = "UTF-8")
  dt <- merge(dt, UF_REF[, .(SG_UF, NM_UF_OCI)], by.x = "UF", by.y = "SG_UF", all.x = TRUE)
  setnames(dt, "NM_UF_OCI", "NM_UF")
  dt[, NM_UF := toupper(NM_UF)]
  if ("MUNICIPIO" %in% names(dt)) {
    dt[, MUNICIPIO := toupper(MUNICIPIO)]
  }
  dt[]
}

# Copia as planilhas mais recentes dos projetos irmãos para dados/processados.
# Só roda quando esses projetos existem localmente (dev no RStudio); no
# shinyapps.io eles não existem e a função não faz nada, mantendo a última
# cópia publicada.
sincronizar_dados_locais <- function() {

  if (!dir.exists(CIRURGIA_DIR_ORIGEM) || !dir.exists(OCI_DIR_ORIGEM)) {
    return(invisible(FALSE))
  }

  origem_cirurgia <- file.path(
    CIRURGIA_DIR_ORIGEM, "resultados", "02_monitoramento", "tabelas"
  )
  # O projeto Cirurgia passou a separar as bases em bases_processadas/fisico
  # e /financeiro; se a subpasta não existir, cai na pasta antiga.
  origem_cirurgia_bases <- file.path(
    CIRURGIA_DIR_ORIGEM, "resultados", "bases_processadas", "fisico"
  )
  if (!dir.exists(origem_cirurgia_bases)) {
    origem_cirurgia_bases <- file.path(
      CIRURGIA_DIR_ORIGEM, "resultados", "bases_processadas"
    )
  }
  origem_oci <- file.path(OCI_DIR_ORIGEM, "resultados")
  origem_consultas_exames <- file.path(
    CONSULTAS_EXAMES_DIR_ORIGEM, "resultados", "consultas_exames", "tabelas"
  )

  arquivos <- c(
    localizar_arquivo(origem_cirurgia, "^serie_completa_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_completa_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_completa_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_completa_financeiro_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_completa_financeiro_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_completa_financeiro_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_pab_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_pab_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_gestao_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_gestao_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_gestao_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_gestao_pab_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia_bases, "^cirurgias_mensal_procedimento_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^cirurgias_ranking_procedimento_uf_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^cirurgias_ranking_secundario_uf_.*\\.csv$"),
    localizar_arquivo(origem_oci, "^planilha_OCI_UF_mes_.*\\.xlsx$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_componente_uf\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_componente_municipio\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_ranking_procedimento_uf\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_ranking_procedimento_municipio\\.csv$"),
    localizar_arquivo(origem_consultas_exames, "^painel_consultas_uf\\.csv$"),
    localizar_arquivo(origem_consultas_exames, "^painel_consultas_municipio\\.csv$"),
    localizar_arquivo(origem_consultas_exames, "^painel_exames_estrategicos_uf\\.csv$"),
    localizar_arquivo(origem_consultas_exames, "^painel_exames_estrategicos_municipio\\.csv$"),
    localizar_arquivo(origem_consultas_exames, "^painel_exames_ranking_procedimento_uf\\.csv$")
  )
  arquivos <- arquivos[!is.na(arquivos)]

  # Remove versões antigas das séries de cirurgia (ex.: ..._base_2022_2024.csv
  # quando já existe ..._base_2022_2025.csv) para não irem parar no deploy.
  obsoletos <- list.files(DADOS_LOCAIS, pattern = "^serie_(completa|anos).*\\.csv$", full.names = TRUE)
  prefixo <- function(x) sub("_[0-9]{4}_.*$", "", basename(x))
  obsoletos <- obsoletos[
    prefixo(obsoletos) %in% prefixo(arquivos) &
      !basename(obsoletos) %in% basename(arquivos)
  ]
  if (length(obsoletos) > 0L) file.remove(obsoletos)

  file.copy(arquivos, DADOS_LOCAIS, overwrite = TRUE)

  # Data em que as séries de Cirurgia foram geradas no projeto de origem —
  # gravada num arquivo próprio porque a data de modificação do arquivo não
  # sobrevive ao deploy no shinyapps.io.
  serie_referencia <- localizar_arquivo(origem_cirurgia, "^serie_anos_total_.*\\.csv$")
  if (!is.na(serie_referencia)) {
    writeLines(
      format(file.info(serie_referencia)$mtime, "%Y-%m-%d"),
      file.path(DADOS_LOCAIS, "atualizacao_cirurgia.txt")
    )
  }

  invisible(TRUE)
}

# Soma duas séries de cirurgia (mesmo esquema uf/[município/gestão]/ano/mês
# + quantidade/valor) — usada para derivar o indicador combinado "MAC, FAEC
# e PAB" (total_pab) a partir de *_total + *_pab, sem duplicar a leitura dos
# arquivos nem criar um CSV extra no projeto Cirurgia (PAB já tem valor = 0
# em todas as séries, então o financeiro do combinado bate com o do total).
somar_series_cirurgia <- function(a, b) {
  if (is.null(a) || is.null(b)) {
    return(NULL)
  }
  chaves <- setdiff(names(a), c("quantidade", "valor"))
  rbind(a, b, use.names = TRUE, fill = TRUE)[
    ,
    .(quantidade = sum(quantidade, na.rm = TRUE), valor = sum(valor, na.rm = TRUE)),
    by = chaves
  ]
}

carregar_tudo <- function() {
  sincronizar_dados_locais()
  dados <- list(
    cirurgia_rol = carregar_serie_cirurgia("rol"),
    cirurgia_total = carregar_serie_cirurgia("total"),
    cirurgia_pnrf = carregar_serie_cirurgia("pnrf"),
    cirurgia_financeiro_rol = carregar_serie_cirurgia("financeiro_rol"),
    cirurgia_financeiro_total = carregar_serie_cirurgia("financeiro_total"),
    cirurgia_financeiro_pnrf = carregar_serie_cirurgia("financeiro_pnrf"),
    cirurgia_anos_rol = carregar_serie_anos_cirurgia("rol"),
    cirurgia_anos_total = carregar_serie_anos_cirurgia("total"),
    cirurgia_anos_pnrf = carregar_serie_anos_cirurgia("pnrf"),
    cirurgia_anos_pab = carregar_serie_anos_cirurgia("pab"),
    cirurgia_municipio_rol = carregar_serie_municipio_cirurgia("rol"),
    cirurgia_municipio_total = carregar_serie_municipio_cirurgia("total"),
    cirurgia_municipio_pnrf = carregar_serie_municipio_cirurgia("pnrf"),
    cirurgia_municipio_pab = carregar_serie_municipio_cirurgia("pab"),
    cirurgia_gestao_rol = carregar_serie_gestao_cirurgia("rol"),
    cirurgia_gestao_total = carregar_serie_gestao_cirurgia("total"),
    cirurgia_gestao_pnrf = carregar_serie_gestao_cirurgia("pnrf"),
    cirurgia_gestao_pab = carregar_serie_gestao_cirurgia("pab"),
    cirurgia_procedimento_rol = carregar_serie_procedimento_rol(),
    cirurgia_ranking_procedimento = carregar_ranking_procedimento_cirurgia(),
    cirurgia_ranking_secundario = carregar_ranking_secundario_cirurgia(),
    mapa_especialidade_rol = carregar_mapa_especialidade_rol(),
    portaria9810 = carregar_portaria9810(),
    portaria9810_limite = carregar_limite_portaria9810(),
    oci_serie = carregar_serie_oci(),
    oci_especialidade_componente_uf = carregar_oci_especialidade_componente_uf(),
    oci_especialidade_componente_municipio = carregar_oci_especialidade_componente_municipio(),
    oci_ranking_procedimento_uf = carregar_oci_ranking_procedimento_uf(),
    oci_ranking_procedimento_municipio = carregar_oci_ranking_procedimento_municipio(),
    ce_consultas_uf = carregar_consultas_exames("^painel_consultas_uf\\.csv$"),
    ce_consultas_municipio = carregar_consultas_exames("^painel_consultas_municipio\\.csv$"),
    ce_exames_uf = carregar_consultas_exames("^painel_exames_estrategicos_uf\\.csv$"),
    ce_exames_municipio = carregar_consultas_exames("^painel_exames_estrategicos_municipio\\.csv$"),
    ce_exames_ranking_uf = carregar_consultas_exames("^painel_exames_ranking_procedimento_uf\\.csv$")
  )

  dados$cirurgia_anos_total_pab <- somar_series_cirurgia(dados$cirurgia_anos_total, dados$cirurgia_anos_pab)
  dados$cirurgia_municipio_total_pab <- somar_series_cirurgia(dados$cirurgia_municipio_total, dados$cirurgia_municipio_pab)
  dados$cirurgia_gestao_total_pab <- somar_series_cirurgia(dados$cirurgia_gestao_total, dados$cirurgia_gestao_pab)

  dados
}

# Rótulo do agregado (opção "BRASIL" no seletor de UF), que reage à região
# selecionada: nacional de verdade quando todas as regiões estão marcadas,
# ou a soma das regiões escolhidas caso contrário.
rotulo_agregado <- function(regioes_selecionadas) {
  if (length(regioes_selecionadas) == 0) {
    "Nenhuma região selecionada"
  } else if (length(regioes_selecionadas) == length(REGIOES)) {
    "BRASIL"
  } else if (length(regioes_selecionadas) == 1) {
    paste0(regioes_selecionadas, " (agregado)")
  } else {
    "Regiões selecionadas (agregado)"
  }
}

# Soma a série de cirurgias das UFs das regiões selecionadas (quantidade e
# faixa histórica), reclassificando o ano de monitoramento com a mesma regra
# usada no script de origem. Usado quando o agregado não corresponde ao
# Brasil inteiro (senão a linha "BRASIL" já calculada é usada diretamente).
agregar_cirurgia_regiao <- function(serie, regioes) {

  ufs <- UF_REF[REGIAO %in% regioes]$NM_UF_CIRURGIA

  agregado <- serie[
    uf_atendimento %chin% ufs,
    .(
      quantidade = sum(quantidade, na.rm = TRUE),
      q1_historico = sum(q1_historico, na.rm = TRUE),
      mediana_historica = sum(mediana_historica, na.rm = TRUE),
      q3_historico = sum(q3_historico, na.rm = TRUE),
      limite_inferior = sum(limite_inferior, na.rm = TRUE),
      limite_superior = sum(limite_superior, na.rm = TRUE)
    ),
    by = .(ano, mes)
  ]

  if (nrow(agregado) == 0) {
    return(agregado[, uf_atendimento := character()][, classificacao := character()])
  }

  agregado[, uf_atendimento := "BRASIL"]

  agregado[
    , classificacao := fcase(
      ano != max(ano), NA_character_,
      quantidade < limite_inferior, "Crítico abaixo do limite esperado",
      quantidade < q1_historico, "Atenção",
      quantidade > limite_superior, "Acima do limite esperado",
      quantidade > q3_historico, "Acima do esperado",
      default = "Esperado"
    )
  ]

  agregado[]
}

# Soma a série multianual (Comparação Anos) das UFs das regiões
# selecionadas. Mais simples que agregar_cirurgia_regiao() porque aqui não
# há faixa histórica nem classificação — só a quantidade por ano/mês.
agregar_anos_regiao <- function(serie, regioes) {
  ufs <- UF_REF[REGIAO %in% regioes]$NM_UF_CIRURGIA
  serie[
    uf_atendimento %chin% ufs,
    .(quantidade = sum(quantidade, na.rm = TRUE), valor = sum(valor, na.rm = TRUE)),
    by = .(ano, mes)
  ]
}

# Filtra a série por procedimento (só ROL, físico) para o recorte de
# Região/UF da aba, e por Especialidade/Procedimento (cruzando com o mapa).
# Município não está disponível nessa série (só UF).
# procedimentos_sel: vetor de códigos (0+ selecionados). Vazio = especialidade
# inteira; um ou mais códigos = soma só desses procedimentos, juntando os
# dados de cada um no mesmo total por ano/mês.
filtrar_procedimento_rol <- function(serie_proc, mapa, uf_sel, regioes, especialidade_sel, procedimentos_sel) {

  d <- serie_proc

  if (uf_sel != "BRASIL") {
    d <- d[uf_atendimento == uf_sel]
  } else if (!setequal(regioes, REGIOES)) {
    ufs <- UF_REF[REGIAO %in% regioes]$NM_UF_CIRURGIA
    d <- d[uf_atendimento %chin% ufs]
  }

  if (length(procedimentos_sel) > 0) {
    d <- d[codigo_procedimento_principal %chin% procedimentos_sel]
  } else if (especialidade_sel != "Todas") {
    codigos <- mapa[especialidade == especialidade_sel]$codigo_procedimento_principal
    d <- d[codigo_procedimento_principal %chin% codigos]
  }

  d[, .(quantidade = sum(qt_total_rol, na.rm = TRUE), valor = NA_real_), by = .(ano, mes)]
}

# Mesmos anos usados no script 02_monitoramento_diagrama_controle.R do
# projeto Cirurgia (base 2022-2025, comparação 2025, monitoramento 2026). O
# ano de comparação (2025) agora também entra na série histórica que gera as
# faixas de quartil — mesma regra do script de origem.
DIAGRAMA_ANO_INICIAL_BASE <- 2022L
DIAGRAMA_ANO_FINAL_BASE <- 2025L
DIAGRAMA_ANO_COMPARACAO <- 2025L
DIAGRAMA_ANO_MONITORAMENTO <- 2026L

# Diagrama de controle (quartis) para uma Especialidade do ROL, no recorte
# de Região/UF selecionado. Reproduz a mesma lógica estatística de
# executar_monitoramento_quartis() (script 02 do projeto Cirurgia), mas
# calculada aqui porque "especialidade" só existe no painel (cruzamento com
# Relacao_cirugiasROL.xlsx, que não faz parte do projeto Cirurgia).
diagrama_especialidade_rol <- function(serie_proc, mapa, uf_sel, regioes, especialidade_sel) {

  base_indicador <- filtrar_procedimento_rol(
    serie_proc, mapa, uf_sel, regioes, especialidade_sel, procedimentos_sel = character(0)
  )

  base_historica <- base_indicador[ano %between% c(DIAGRAMA_ANO_INICIAL_BASE, DIAGRAMA_ANO_FINAL_BASE)]

  validate(need(nrow(base_historica) > 0, "Sem dados históricos suficientes para essa especialidade na seleção atual."))

  faixas <- base_historica[
    ,
    .(
      q1_historico = quantile(quantidade, 0.25, na.rm = TRUE, type = 7),
      mediana_historica = quantile(quantidade, 0.50, na.rm = TRUE, type = 7),
      q3_historico = quantile(quantidade, 0.75, na.rm = TRUE, type = 7)
    ),
    by = mes
  ]
  faixas[
    ,
    `:=`(
      limite_inferior = pmax(q1_historico - 1.5 * (q3_historico - q1_historico), 0),
      limite_superior = q3_historico + 1.5 * (q3_historico - q1_historico)
    )
  ]

  comparacao <- merge(base_indicador[ano == DIAGRAMA_ANO_COMPARACAO], faixas, by = "mes", all.x = TRUE)
  monitoramento <- merge(base_indicador[ano == DIAGRAMA_ANO_MONITORAMENTO], faixas, by = "mes", all.x = TRUE)

  comparacao[, classificacao := NA_character_]
  monitoramento[
    ,
    classificacao := fcase(
      is.na(quantidade) | is.na(q1_historico), NA_character_,
      quantidade < limite_inferior, "Crítico abaixo do limite esperado",
      quantidade < q1_historico, "Atenção",
      quantidade > limite_superior, "Acima do limite esperado",
      quantidade > q3_historico, "Acima do esperado",
      default = "Esperado"
    )
  ]

  dados_uf <- rbind(comparacao, monitoramento, use.names = TRUE)
  validate(need(nrow(dados_uf) > 0, "Sem dados para a seleção atual."))
  setorder(dados_uf, ano, mes)
  dados_uf[]
}

#### GRÁFICOS ####

# Aplicado a todo gráfico do painel: configura o botão de download nativo do
# Plotly (ícone de câmera) para exportar PNG em alta resolução em vez do
# tamanho de tela padrão.
alta_resolucao <- function(p, nome_arquivo = "grafico") {
  config(
    p,
    toImageButtonOptions = list(
      format = "png",
      filename = nome_arquivo,
      width = 1600,
      height = 900,
      scale = 3
    )
  )
}

# Retângulo cinza cobrindo os últimos 3 meses de uma série mensal (datas no
# 1º dia do mês), para sinalizar visualmente que os meses mais recentes de
# OCI ainda são dados preliminares (defasagem de fechamento da base). Some
# de vez se a série tiver menos de 1 mês.
sombra_dados_preliminares <- function(datas_unicas) {

  datas_unicas <- sort(unique(datas_unicas))
  n <- length(datas_unicas)

  if (n < 1) {
    return(NULL)
  }

  idx_inicio <- max(1, n - 2)
  x0 <- datas_unicas[idx_inicio]
  x1 <- seq(datas_unicas[n], by = "1 month", length.out = 2)[2]

  list(
    type = "rect", xref = "x", yref = "paper",
    x0 = x0, x1 = x1, y0 = 0, y1 = 1,
    fillcolor = "rgba(128,128,128,0.18)",
    line = list(width = 0),
    layer = "below"
  )
}

# Rótulo "Dados preliminares", centralizado na sombra acima, perto do topo
# do gráfico — sem isso, o retângulo cinza sozinho não deixa claro o motivo.
anotacao_dados_preliminares <- function(datas_unicas) {

  sombra <- sombra_dados_preliminares(datas_unicas)

  if (is.null(sombra)) {
    return(NULL)
  }

  list(
    x = mean(c(sombra$x0, sombra$x1)), y = 0.97,
    xref = "x", yref = "paper",
    text = "Dados preliminares", showarrow = FALSE,
    font = list(size = 11, color = "#666666"),
    xanchor = "center"
  )
}

# Versão ggplot2 do mesmo retângulo + rótulo "Dados preliminares" acima
# (equivalente ao par shapes/annotations do plotly nativo) — usada nos
# gráficos convertidos para ggplot. Retorna uma lista de camadas, somada
# com "+" a um ggplot como qualquer outra camada.
#
# Usa geom_rect()/geom_text() com data frame próprio (inherit.aes = FALSE),
# em vez de annotate(): o ggplotly() descarta silenciosamente camadas
# annotate() (shapes/annotations saem vazios do lado do plotly). E o
# ymin/ymax precisa ser um número finito, não -Inf/Inf: o ggplotly()
# converte Inf em NA nas coordenadas do trace, o que apaga o retângulo
# (renderiza sem erro, só que invisível). Por isso recebe y_max — o maior
# valor de Y realmente plotado no gráfico — para cobrir toda a área visível
# com folga (1.08x) em vez de "infinito".
sombra_dados_preliminares_gg <- function(datas_unicas, y_max) {

  datas_unicas <- sort(unique(datas_unicas))
  n <- length(datas_unicas)

  if (n < 1 || !is.finite(y_max)) {
    return(list())
  }

  idx_inicio <- max(1, n - 2)
  x0 <- datas_unicas[idx_inicio]
  x1 <- seq(datas_unicas[n], by = "1 month", length.out = 2)[2]
  x_meio <- as.Date(mean(c(as.numeric(x0), as.numeric(x1))), origin = "1970-01-01")

  y_topo <- y_max * 1.08

  list(
    geom_rect(
      data = data.frame(xmin = x0, xmax = x1),
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y_topo),
      inherit.aes = FALSE, fill = "grey50", alpha = 0.18
    ),
    geom_text(
      data = data.frame(x = x_meio, y = y_topo, label = "Dados preliminares"),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, vjust = 1.3, size = 3.3, colour = "#666666"
    )
  )
}

# Dados brutos por trás de qualquer gráfico, em CSV (";" + decimal ",",
# mesmo padrão dos demais arquivos do painel).
handler_csv <- function(dados_fn, nome_arquivo) {
  downloadHandler(
    filename = function() paste0(nome_arquivo, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) fwrite(dados_fn(), file, sep = ";", dec = ",", bom = TRUE)
  )
}

# Exporta o gráfico ggplot como slide de PowerPoint com rvg::dml(): o
# desenho vira vetor editável nativamente no PowerPoint (não uma imagem).
handler_pptx <- function(plot_fn, nome_arquivo) {
  downloadHandler(
    filename = function() paste0(nome_arquivo, "_", format(Sys.Date(), "%Y%m%d"), ".pptx"),
    content = function(file) {
      grafico <- plot_fn()
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(
        doc,
        value = rvg::dml(code = print(grafico), width = 10, height = 7.5),
        location = ph_location_fullsize()
      )
      print(doc, target = file)
    }
  )
}

# O ggplotly() às vezes monta o nome de cada trace concatenando atributos
# internos do build do ggplot (linewidth, índice do grupo) além do rótulo
# da legenda — aparecendo na tela como "(Rótulo,1,NA)". Aqui cortamos cada
# nome até a primeira vírgula, mantendo só o rótulo de verdade. Aplicado
# logo após todo ggplotly() do painel.
limpar_legenda_plotly <- function(p) {
  p$x$data <- lapply(p$x$data, function(tr) {
    if (!is.null(tr$name)) {
      tr$name <- sub("^\\(([^,]+),.*\\)$", "\\1", tr$name)
    }
    tr
  })
  p
}

# Só o ano de monitoramento é desenhado: o ano anterior (2025) entra na faixa
# histórica (base 2022-2025), como no script 02 do projeto Cirurgia, e não
# mais como linha separada.
grafico_cirurgia <- function(dados_uf, ano_monitoramento, titulo, metrica = "fisico") {

  d <- dados_uf[order(ano, mes)]

  faixa <- unique(
    d[, .(mes, mediana_historica, q1_historico, q3_historico, limite_inferior, limite_superior)]
  )
  setorder(faixa, mes)

  moni <- d[ano == ano_monitoramento]

  # A classificação (Esperado/Acima do esperado/...) foi desenhada para
  # produção física; para valores financeiros (R$) ela não se aplica, então
  # o modo financeiro mostra só a linha de produção, sem cor por ponto nem
  # legenda de classificação.
  com_classificacao <- metrica != "financeiro"

  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  fmt_valor   <- if (metrica == "financeiro") label_pt_moeda else label_pt_num

  moni[, texto := paste0(
    "Mês: ", MES_LABELS[mes], "<br>", rotulo_eixo, ": ", fmt_valor(quantidade),
    if (com_classificacao) paste0("<br>", classificacao) else ""
  )]

  rotulo_serie_moni <- paste0("Produção ", ano_monitoramento)

  # Todas as cores (as duas linhas de produção + os níveis de classificação)
  # ficam numa ÚNICA escala de cor (uma só scale_colour_manual no final),
  # combinando variáveis diferentes mapeadas em camadas diferentes. Isso
  # evita o ggnewscale, que o ggplotly (usado na tela) não converte bem —
  # com ele, as camadas anteriores à segunda escala somem na versão
  # interativa (mesmo aparecendo certo no PPTX, que não passa pelo ggplotly).
  moni[, serie := rotulo_serie_moni]

  niveis_classificacao <- c(
    "Esperado", "Acima do esperado", "Acima do limite esperado",
    "Atenção", "Crítico abaixo do limite esperado"
  )

  if (com_classificacao) {
    moni[, classificacao := factor(classificacao, levels = niveis_classificacao)]
    valores_cor <- c(setNames("black", rotulo_serie_moni), CORES_CLASSIFICACAO)
    quebras_legenda <- c(rotulo_serie_moni, niveis_classificacao)
  } else {
    valores_cor <- setNames("black", rotulo_serie_moni)
    quebras_legenda <- rotulo_serie_moni
  }

  p <- ggplot() +
    geom_ribbon(
      data = faixa,
      aes(x = mes, ymin = limite_inferior, ymax = limite_superior, fill = "Faixa histórica (min-máx)")
    ) +
    geom_ribbon(
      data = faixa,
      aes(x = mes, ymin = q1_historico, ymax = q3_historico, fill = "Faixa histórica (Q1-Q3)")
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("Faixa histórica (min-máx)" = "grey85", "Faixa histórica (Q1-Q3)" = "grey65")
    ) +
    geom_line(
      data = faixa, aes(x = mes, y = mediana_historica, linetype = "Mediana histórica"),
      colour = "black", linewidth = 0.6
    ) +
    scale_linetype_manual(name = NULL, values = c("Mediana histórica" = "dashed")) +
    geom_line(
      data = moni, aes(x = mes, y = quantidade, colour = serie, group = serie),
      linewidth = 1.1
    )

  if (com_classificacao) {
    p <- p + geom_point(
      data = moni, aes(x = mes, y = quantidade, colour = classificacao, text = texto),
      size = 3.2
    )
  } else {
    p <- p + geom_point(
      data = moni, aes(x = mes, y = quantidade, colour = serie, text = texto),
      size = 2.6
    )
  }

  p +
    scale_colour_manual(name = NULL, values = valores_cor, breaks = quebras_legenda) +
    scale_x_continuous(breaks = 1:12, labels = MES_LABELS) +
    scale_y_continuous(labels = fmt_valor) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = rotulo_eixo) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

grafico_comparacao_anos <- function(dados, titulo, metrica = "fisico") {

  d <- dados[order(ano, mes)]
  anos <- sort(unique(d$ano))
  cores <- cores_para_anos(anos)
  # O ano mais recente (tipicamente 2026) fica com a linha mais grossa, para
  # se destacar visualmente dos anos anteriores sem precisar de mais uma cor
  # saturada — mesma ideia da paleta em CORES_ANOS_INSTITUCIONAL.
  larguras <- setNames(ifelse(anos == max(anos), 2.4, 1.1), anos)

  coluna_y <- if (metrica == "financeiro") "valor" else "quantidade"
  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  fmt_valor   <- if (metrica == "financeiro") label_pt_moeda else label_pt_num

  d[, y_plot := get(coluna_y)]
  d[, ano_fct := factor(ano, levels = anos)]
  d[, texto := paste0(ano, " — Mês: ", MES_LABELS[mes], "<br>", rotulo_eixo, ": ", fmt_valor(y_plot))]

  ggplot(d, aes(x = mes, y = y_plot, colour = ano_fct, group = ano_fct)) +
    geom_line(aes(linewidth = ano_fct)) +
    geom_point(aes(text = texto), size = 1.6) +
    scale_colour_manual(name = NULL, values = cores) +
    scale_linewidth_manual(values = larguras, guide = "none") +
    scale_x_continuous(breaks = 1:12, labels = MES_LABELS) +
    scale_y_continuous(labels = fmt_valor) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = rotulo_eixo) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

# Gráfico "OCI geral": onda de área com o Total geral de OCI (mesmo estilo
# visual do gráfico original) mais uma linha por componente/modalidade
# (Componente Ambulatorial, Carretas, Créditos Financeiros, Equipes
# Volantes) — clicar na legenda do Plotly liga/desliga cada linha.
grafico_oci_componente <- function(dados_geral, dados_componente, titulo) {

  dg <- dados_geral[order(competencia)]
  dg[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  dg[, texto := paste0("Total geral de OCI<br>", rotulo_mes, "<br>OCI: ", label_pt_num(oci))]
  ultimo <- dg[which.max(competencia)]

  datas_unicas <- sort(unique(dg$competencia))

  dc <- dados_componente[COMPONENTE %in% ORDEM_COMPONENTES_OCI][order(competencia)]
  dc[, COMPONENTE := factor(as.character(COMPONENTE), levels = ORDEM_COMPONENTES_OCI)]
  dc[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  dc[, texto := paste0(COMPONENTE, "<br>", rotulo_mes, "<br>OCI: ", label_pt_num(OCI))]

  cores_legenda <- CORES_COMPONENTE_OCI[c("Total geral de OCI", ORDEM_COMPONENTES_OCI)]
  y_max <- max(dg$oci, dc$OCI, 0, na.rm = TRUE)

  ggplot() +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_area(
      data = dg, aes(x = competencia, y = oci),
      fill = CORES_COMPONENTE_OCI[["Total geral de OCI"]], alpha = 0.10
    ) +
    geom_line(
      data = dg, aes(x = competencia, y = oci, colour = "Total geral de OCI", group = 1),
      linewidth = 1.1
    ) +
    geom_point(
      data = dg, aes(x = competencia, y = oci, text = texto),
      colour = CORES_COMPONENTE_OCI[["Total geral de OCI"]], size = 0.01, alpha = 0
    ) +
    geom_point(
      data = ultimo, aes(x = competencia, y = oci),
      shape = 21, colour = "#D85A30", fill = "white", stroke = 1.4, size = 3.2
    ) +
    geom_line(
      data = dc, aes(x = competencia, y = OCI, colour = COMPONENTE, group = COMPONENTE),
      linetype = "dotted", linewidth = 0.8
    ) +
    geom_point(
      data = dc, aes(x = competencia, y = OCI, text = texto, colour = COMPONENTE),
      size = 0.01, alpha = 0
    ) +
    geom_vline(
      xintercept = as.numeric(DATA_VIRADA_OCI),
      linetype = "dashed", colour = "#D85A30", linewidth = 0.6
    ) +
    scale_colour_manual(name = NULL, values = cores_legenda) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de atendimento", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Barras empilhadas HORIZONTAIS por ano — total acumulado por
# componente/modalidade, mesmo estilo do gráfico de referência (SIA/SUS e
# CMD). Fica em cima, com o gráfico por mês logo abaixo.
grafico_oci_componente_ano <- function(dados, titulo) {

  d <- dados[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ANO, COMPONENTE)]
  d <- d[COMPONENTE %in% ORDEM_COMPONENTES_OCI]
  d[, COMPONENTE := factor(as.character(COMPONENTE), levels = ORDEM_COMPONENTES_OCI)]

  ordem_ano <- as.character(sort(unique(d$ANO)))
  d[, ano_fct := factor(as.character(ANO), levels = ordem_ano)]
  d[, texto := paste0(COMPONENTE, "<br>", ANO, "<br>OCI: ", label_pt_num(OCI))]

  # Os valores ficam na tabela logo abaixo do gráfico (e no tooltip), não
  # mais como rótulo fixo na ponta das barras.
  ggplot(d, aes(x = ano_fct, y = OCI, fill = COMPONENTE, text = texto)) +
    geom_col(width = 0.65) +
    scale_fill_manual(name = NULL, values = CORES_COMPONENTE_OCI, breaks = ORDEM_COMPONENTES_OCI) +
    scale_y_continuous(labels = label_pt_num, expand = expansion(mult = c(0, 0.02))) +
    coord_flip() +
    labs(title = titulo, x = NULL, y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

# Mesmo esquema de colunas empilhadas por componente/modalidade, mas por
# mês de atendimento (toda a série, não só o total do ano) — fica logo
# abaixo do gráfico por ano.
grafico_oci_componente_mes <- function(dados, titulo) {

  d <- dados[COMPONENTE %in% ORDEM_COMPONENTES_OCI][order(competencia)]
  d[, COMPONENTE := factor(as.character(COMPONENTE), levels = ORDEM_COMPONENTES_OCI)]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  d[, texto := paste0(COMPONENTE, "<br>", rotulo_mes, "<br>OCI: ", label_pt_num(OCI))]

  datas_unicas <- sort(unique(d$competencia))
  y_max <- max(d[, .(total = sum(OCI, na.rm = TRUE)), by = competencia]$total, 0, na.rm = TRUE)

  ggplot(d, aes(x = competencia, y = OCI, fill = COMPONENTE, text = texto)) +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_col(width = 25) +
    scale_fill_manual(name = NULL, values = CORES_COMPONENTE_OCI) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    labs(title = titulo, x = "Mês de atendimento", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Colunas empilhadas por mês de atendimento, uma cor por procedimento — só
# os TOP_N_PROCEDIMENTOS_OCI procedimentos mais frequentes da especialidade
# escolhida (o corte já vem pronto de dados_oci_procedimento_mes(), no
# servidor — esta função só desenha).
grafico_oci_procedimento_mes <- function(dados, titulo) {

  d <- dados[order(competencia)]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  d[, texto := paste0(NOME_PROCEDIMENTO, "<br>", rotulo_mes, "<br>OCI: ", label_pt_num(OCI))]

  datas_unicas <- sort(unique(d$competencia))
  y_max <- max(d[, .(total = sum(OCI, na.rm = TRUE)), by = competencia]$total, 0, na.rm = TRUE)

  ggplot(d, aes(x = competencia, y = OCI, fill = NOME_PROCEDIMENTO, text = texto)) +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_col(width = 25) +
    scale_fill_brewer(name = NULL, palette = "Set2") +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    labs(title = titulo, x = "Mês de atendimento", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Linha por exame (um ponto por mês de competência) — os TOP_N_EXAMES_CE
# exames estratégicos mais realizados no ano escolhido, na sub-aba
# "Ranking de Exames" (o corte já vem pronto no servidor; esta função só
# desenha). Sempre um único ano (12 pontos por linha, no máximo).
grafico_exames_ranking_mes <- function(dados, titulo) {

  d <- dados[order(competencia)]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  d[, texto := paste0(NOME_PROCEDIMENTO, "<br>", rotulo_mes, "<br>Quantidade: ", label_pt_num(QTD_TOTAL))]

  # Legenda com nome curto (sem o código SIGTAP na frente, truncado em 40
  # caracteres) — alguns nomes de procedimento são longos demais e, por
  # inteiro, estouram a legenda horizontal e colidem com o título do eixo X.
  # O tooltip (`texto`, acima) continua com o nome completo.
  d[, NOME_CURTO := sub("^\\d{8,10}\\s+", "", NOME_PROCEDIMENTO)]
  d[, NOME_CURTO := fifelse(nchar(NOME_CURTO) > 40, paste0(substr(NOME_CURTO, 1, 40), "…"), NOME_CURTO)]

  datas_unicas <- sort(unique(d$competencia))
  y_max <- max(d$QTD_TOTAL, 0, na.rm = TRUE)

  ggplot(d, aes(x = competencia, y = QTD_TOTAL, colour = NOME_CURTO, group = NOME_CURTO)) +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_line(linewidth = 1) +
    geom_point(aes(text = texto), size = 0.01, alpha = 0) +
    scale_colour_brewer(name = NULL, palette = "Set2") +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = "Quantidade") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Colunas empilhadas (Fora da OCI / Dentro OCI FAEC / Dentro OCI — Outros
# componentes) por mês de competência, com uma linha de % dentro da OCI no
# eixo secundário — mesmo estilo visual do grafico_oci_componente_mes(),
# adaptado para as duas primeiras sub-abas de "Consultas e Exames"
# (Consultas/Teleconsultas e Exames Estratégicos Eletivos). `dados` tem
# competencia (Date), TOTAL, FAEC, OUTROS e PCT_OCI, sempre em quantidade
# (Físico) — o Financeiro não entra aqui porque as linhas "Secund. de OCI"
# (FAEC/MAC) vêm com valor_federal = R$0 na base de origem (o valor fica
# todo no procedimento principal). FAEC/OUTROS só têm dado real a partir
# de 2025 — antes disso o trecho empilhado fica zerado e a barra é só
# "Fora da OCI". A linha de % usa traço tracejado e cor neutra (mais clara
# que as barras) para não competir visualmente com elas.
grafico_consultas_exames_oci_mes <- function(dados, titulo, rotulo_eixo, fmt_valor) {

  d <- dados[order(competencia)]
  d[, FORA := TOTAL - FAEC - OUTROS]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]

  dl <- melt(
    d[, .(
      competencia, rotulo_mes,
      `Fora da OCI` = FORA,
      `Dentro OCI (FAEC)` = FAEC,
      `Dentro OCI — Outros componentes` = OUTROS
    )],
    id.vars = c("competencia", "rotulo_mes"), variable.name = "CATEGORIA", value.name = "valor"
  )
  dl[, CATEGORIA := factor(as.character(CATEGORIA), levels = names(CORES_OCI_CE))]
  dl[, texto := paste0(CATEGORIA, "<br>", rotulo_mes, "<br>", rotulo_eixo, ": ", fmt_valor(valor))]

  fmt_pct <- scales::label_number(accuracy = 0.1, decimal.mark = ",", suffix = "%")
  d[, texto_pct := paste0(rotulo_mes, "<br>% dentro da OCI: ", fmt_pct(PCT_OCI))]

  datas_unicas <- sort(unique(d$competencia))
  y_max <- max(d$TOTAL, 0, na.rm = TRUE)
  pct_max <- max(d$PCT_OCI, 0, na.rm = TRUE)
  # Reescala a % para caber no eixo primário (até 95% da barra mais alta,
  # com folga abaixo do teto do sombreado de "Dados preliminares"); o
  # sec.axis() abaixo desfaz a conta só para os rótulos do eixo direito.
  fator <- if (pct_max > 0) (y_max * 0.95) / pct_max else 1
  d[, pct_y := PCT_OCI * fator]

  ggplot() +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_col(data = dl, aes(x = competencia, y = valor, fill = CATEGORIA, text = texto), width = 25) +
    geom_line(
      data = d, aes(x = competencia, y = pct_y, group = 1),
      colour = "#8292A3", linewidth = 0.8, linetype = "dashed"
    ) +
    geom_point(
      data = d, aes(x = competencia, y = pct_y, text = texto_pct),
      colour = "#8292A3", size = 0.01, alpha = 0
    ) +
    scale_fill_manual(name = NULL, values = CORES_OCI_CE) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(
      labels = fmt_valor,
      sec.axis = sec_axis(~ . / fator, name = "% dentro da OCI", labels = fmt_pct)
    ) +
    labs(title = titulo, x = "Mês de competência", y = rotulo_eixo) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Depois do ggplotly(), substitui os 2 últimos traces (a linha de % e o
# ponto invisível que carrega o tooltip — sempre as 2 últimas camadas
# adicionadas no ggplot, nessa ordem) por um eixo Y secundário de verdade:
# o ggplotly() não converte sec_axis() do ggplot2 de forma confiável, então
# o valor real (não reescalado) e o eixo y2 são aplicados à mão, só na
# versão interativa — o PPTX usa o sec_axis() do ggplot2 direto, que
# funciona bem no render estático (rvg::dml).
eixo_secundario_percentual <- function(p, pct_valores, titulo_eixo = "% dentro da OCI") {
  n <- length(p$x$data)
  for (i in (n - 1):n) {
    p$x$data[[i]]$y <- pct_valores
    p$x$data[[i]]$yaxis <- "y2"
  }
  p$x$layout$yaxis2 <- list(
    overlaying = "y", side = "right", title = list(text = titulo_eixo, font = list(color = "#8292A3", size = 12)),
    tickfont = list(color = "#8292A3", size = 11),
    ticksuffix = "%", showgrid = FALSE, zeroline = FALSE, rangemode = "tozero"
  )
  p
}

# Tabela "Geral" da aba Consultas e Exames: uma linha por UF, com um
# cabeçalho de Região intercalado antes de cada grupo (linha sem valor) e
# uma linha de total Brasil no fim — mesmo layout da planilha de referência
# do projeto Consultas_Exames. `dados` já vem filtrado a um único ano.
tabela_geral_consultas_exames <- function(dados, col_total, col_faec, col_outros, col_oci) {

  d <- dados[
    , .(
      TOTAL = sum(get(col_total)), FAEC = sum(get(col_faec)),
      OUTROS = sum(get(col_outros)), OCI = sum(get(col_oci))
    ),
    by = .(REGIAO, UF)
  ]

  linhas <- list()
  for (reg in ORDEM_REGIAO_CE) {
    sub <- d[REGIAO == reg]
    if (nrow(sub) == 0L) next
    setorder(sub, UF)
    linhas[[length(linhas) + 1L]] <- data.table(
      Linha = toupper(reg), TOTAL = NA_real_, FAEC = NA_real_, OUTROS = NA_real_, OCI = NA_real_
    )
    linhas[[length(linhas) + 1L]] <- sub[, .(Linha = UF, TOTAL, FAEC, OUTROS, OCI)]
  }
  linhas[[length(linhas) + 1L]] <- data.table(
    Linha = "BRASIL", TOTAL = sum(d$TOTAL), FAEC = sum(d$FAEC), OUTROS = sum(d$OUTROS), OCI = sum(d$OCI)
  )

  rbindlist(linhas)
}

grafico_oci_especialidade <- function(dados_geral, dados_especialidade, titulo) {

  dg <- dados_geral[order(competencia)]
  dg[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  dg[, texto := paste0("Geral<br>", rotulo_mes, "<br>OCI: ", label_pt_num(oci))]

  datas_unicas <- sort(unique(dg$competencia))
  y_max <- max(dg$oci, dados_especialidade$OCI, 0, na.rm = TRUE)

  p <- ggplot() +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_line(
      data = dg, aes(x = competencia, y = oci, colour = "Geral", group = 1),
      linewidth = 1.3
    ) +
    geom_point(
      data = dg, aes(x = competencia, y = oci, text = texto),
      colour = "#12283D", size = 0.01, alpha = 0
    )

  especialidades <- if (nrow(dados_especialidade) > 0) {
    sort(unique(as.character(dados_especialidade$ESPECIALIDADE)))
  } else {
    character()
  }

  cores_legenda <- c("Geral" = "#12283D")

  if (length(especialidades) > 0) {
    dados_especialidade <- dados_especialidade[order(ESPECIALIDADE, competencia)]
    dados_especialidade[, rotulo_mes := rotular_mes_ano_pt(competencia)]
    dados_especialidade[, texto := paste0(ESPECIALIDADE, "<br>", rotulo_mes, "<br>OCI: ", label_pt_num(OCI))]

    p <- p +
      geom_line(
        data = dados_especialidade,
        aes(x = competencia, y = OCI, colour = ESPECIALIDADE, group = ESPECIALIDADE),
        linetype = "dotted", linewidth = 0.8
      ) +
      geom_point(
        data = dados_especialidade,
        aes(x = competencia, y = OCI, text = texto, colour = ESPECIALIDADE),
        size = 0.01, alpha = 0
      )

    cores_legenda <- c(cores_legenda, CORES_ESPECIALIDADE[especialidades])
  }

  p +
    scale_colour_manual(name = NULL, values = cores_legenda) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de atendimento", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Colunas empilhadas por especialidade e componente/modalidade — total
# acumulado do período filtrado, ordenado por total decrescente. Sem rótulo
# fixo nos segmentos: os valores ficam no tooltip e nas tabelas abaixo do
# gráfico.
grafico_oci_especialidade_componente <- function(dados, titulo) {

  d <- dados[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ESPECIALIDADE, COMPONENTE)]

  totais_especialidade <- d[, .(total = sum(OCI, na.rm = TRUE)), by = ESPECIALIDADE]
  setorder(totais_especialidade, -total)
  ordem_especialidade <- as.character(totais_especialidade$ESPECIALIDADE)
  d[, ESPECIALIDADE := factor(as.character(ESPECIALIDADE), levels = ordem_especialidade)]

  # Ordem de empilhamento (de baixo para cima), igual ao gráfico de
  # referência — mas só os componentes que sobraram no filtro (ver
  # dados_oci_especialidade_componente_agregada()), para a legenda não
  # listar um componente sem nenhuma barra.
  ordem_pilha <- c("Carretas", "Componente Ambulatorial", "Créditos Financeiros", "Equipes Volantes")
  ordem_presente <- intersect(ordem_pilha, as.character(unique(d$COMPONENTE)))

  d <- d[COMPONENTE %in% ordem_presente]
  d[, COMPONENTE := factor(as.character(COMPONENTE), levels = rev(ordem_presente))]
  d[, texto := paste0(COMPONENTE, "<br>", ESPECIALIDADE, "<br>OCI: ", label_pt_num(OCI))]

  ggplot(d, aes(x = ESPECIALIDADE, y = OCI, fill = COMPONENTE, text = texto)) +
    geom_col(width = 0.7) +
    scale_fill_manual(name = NULL, values = CORES_COMPONENTE_OCI, breaks = ordem_presente) +
    scale_y_continuous(labels = label_pt_num) +
    labs(title = titulo, x = NULL, y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

# Tabela por especialidade x componente (com total por linha e uma linha de
# total geral) para acompanhar o gráfico de colunas empilhadas. `coluna` é
# "OCI" (físico) ou "VALOR" (financeiro, em R$).
tabela_oci_especialidade_componente <- function(dados, coluna, moeda = FALSE) {

  d <- dados[
    COMPONENTE %in% ORDEM_COMPONENTES_OCI,
    .(valor = sum(get(coluna), na.rm = TRUE)), by = .(ESPECIALIDADE, COMPONENTE)
  ]

  larga <- dcast(d, ESPECIALIDADE ~ as.character(COMPONENTE), value.var = "valor", fill = 0)
  colunas <- intersect(ORDEM_COMPONENTES_OCI, names(larga))
  larga[, Total := rowSums(.SD), .SDcols = colunas]
  setorder(larga, -Total)

  totais <- larga[, lapply(.SD, sum), .SDcols = c(colunas, "Total")]
  totais[, ESPECIALIDADE := "Total"]
  larga <- rbind(larga[, ESPECIALIDADE := as.character(ESPECIALIDADE)], totais, use.names = TRUE)
  setnames(larga, "ESPECIALIDADE", "Especialidade")
  setcolorder(larga, c("Especialidade", colunas, "Total"))

  tabela <- datatable(
    larga,
    rownames = FALSE,
    selection = "none",
    fillContainer = FALSE,
    height = "auto",
    options = list(dom = "t", ordering = FALSE, paging = FALSE)
  )

  tabela <- if (moeda) {
    formatCurrency(tabela, c(colunas, "Total"), currency = "R$ ", interval = 3, mark = ".", digits = 0)
  } else {
    formatRound(tabela, c(colunas, "Total"), digits = 0, mark = ".", interval = 3)
  }

  formatStyle(
    tabela, c("Especialidade", colunas, "Total"),
    valueColumns = "Especialidade",
    fontWeight = styleEqual("Total", "bold")
  )
}

CORES_TIPO_GESTAO <- c("ESTADUAL" = "#185FA5", "MUNICIPAL" = "#D85A30")

# Colunas empilhadas por mês de pagamento, separadas por tipo de gestão
# (Estadual/Municipal).
grafico_portaria9810_mensal <- function(dados, titulo) {

  d <- dados[TIPO_GESTAO %in% names(CORES_TIPO_GESTAO)][order(DATA_PAGAMENTO)]
  d[, tipo_rotulo := factor(stri_trans_totitle(TIPO_GESTAO), levels = stri_trans_totitle(names(CORES_TIPO_GESTAO)))]
  d[, texto := paste0(tipo_rotulo, "<br>", rotular_mes_ano_pt(DATA_PAGAMENTO), "<br>", label_pt_moeda(valor))]

  datas_unicas <- sort(unique(d$DATA_PAGAMENTO))
  cores_legenda <- setNames(unname(CORES_TIPO_GESTAO), stri_trans_totitle(names(CORES_TIPO_GESTAO)))
  y_max <- max(d[, .(total = sum(valor, na.rm = TRUE)), by = DATA_PAGAMENTO]$total, 0, na.rm = TRUE)

  ggplot(d, aes(x = DATA_PAGAMENTO, y = valor, fill = tipo_rotulo, text = texto)) +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_col(width = 25) +
    scale_fill_manual(name = NULL, values = cores_legenda) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_moeda) +
    labs(title = titulo, x = "Mês de pagamento", y = "Valor líquido (R$)") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

CORES_PROGRAMA_PORTARIA9810 <- c(
  "Componente Ambulatorial" = "#185FA5",
  "Componente Cirúrgico"    = "#D85A30",
  "Mutirão"                 = "#c9a227",
  "FAEC - PMAE*"            = "#2e8b57",
  "FAEC PNRF*"              = "#a4508b"
)

# Uma linha por Componente (categoria resumida da coluna PROGRAMA — ver
# mapear_programa_portaria9810()), por mês de pagamento.
grafico_portaria9810_programa <- function(dados, titulo) {

  d <- dados[order(DATA_PAGAMENTO)]
  d[, COMPONENTE := as.character(COMPONENTE)]
  datas_unicas <- sort(unique(d$DATA_PAGAMENTO))
  y_max <- max(d$valor, 0, na.rm = TRUE)

  categorias <- unique(c(names(CORES_PROGRAMA_PORTARIA9810), d$COMPONENTE))
  categorias <- categorias[categorias %in% d$COMPONENTE]

  # Categorias fora da paleta fixa (CORES_PROGRAMA_PORTARIA9810) recebem cor
  # gerada automaticamente, para nunca quebrar o scale_colour_manual.
  cores_legenda <- CORES_PROGRAMA_PORTARIA9810[categorias]
  faltantes <- categorias[is.na(cores_legenda)]
  if (length(faltantes) > 0) {
    cores_legenda[faltantes] <- scales::hue_pal()(length(faltantes))
  }
  names(cores_legenda) <- categorias

  d[, COMPONENTE := factor(COMPONENTE, levels = categorias)]
  d[, texto := paste0(COMPONENTE, "<br>", rotular_mes_ano_pt(DATA_PAGAMENTO), "<br>", label_pt_moeda(valor))]

  ggplot(d, aes(x = DATA_PAGAMENTO, y = valor, colour = COMPONENTE, group = COMPONENTE, text = texto)) +
    sombra_dados_preliminares_gg(datas_unicas, y_max) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    scale_colour_manual(name = NULL, values = cores_legenda) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_moeda) +
    labs(title = titulo, x = "Mês de pagamento", y = "Valor líquido (R$)") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

# Barras horizontais com o % do limite da Portaria 9.810 já utilizado por
# UF (pago / limite), ordenado do maior para o menor, com marcação em 100%.
grafico_portaria9810_limite <- function(dados, titulo) {

  d <- dados[order(percentual)]
  d[, NM_UF := factor(as.character(NM_UF), levels = as.character(NM_UF))]

  cores_status <- c(
    "Dentro do limite" = "#479139",
    "Ultrapassou o limite" = "#FF0000"
  )

  d[, texto := paste0(
    NM_UF, "<br>Pago: ", label_pt_moeda(valor_pago),
    "<br>Limite: ", label_pt_moeda(VALOR_LIMITE),
    "<br>", label_pt_num(percentual * 100), "% do limite"
  )]

  ggplot(d, aes(x = NM_UF, y = percentual, fill = status, text = texto)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "black", linewidth = 0.6) +
    scale_fill_manual(name = NULL, values = cores_status) +
    scale_y_continuous(labels = label_percent(accuracy = 1, decimal.mark = ",")) +
    coord_flip() +
    labs(title = titulo, x = NULL, y = "% do limite utilizado") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 13))
}

#### SEMÁFORO CIRÚRGICO (mapas de variação 2025-2026, do projeto Analise_Espacial) ####

# Copia os 3 .gpkg mais recentes do projeto irmão Analise_Espacial/app_semaforo
# para dados/processados/semaforo — mesmo padrão de sincronizar_dados_locais().
sincronizar_dados_semaforo <- function() {

  if (!dir.exists(SEMAFORO_DIR_ORIGEM)) {
    return(invisible(FALSE))
  }

  arquivos <- c(
    localizar_arquivo(SEMAFORO_DIR_ORIGEM, "^tab_br.*\\.gpkg$"),
    localizar_arquivo(SEMAFORO_DIR_ORIGEM, "^tab_regiao.*\\.gpkg$"),
    localizar_arquivo(SEMAFORO_DIR_ORIGEM, "^tab_municipio.*\\.gpkg$")
  )
  arquivos <- arquivos[!is.na(arquivos)]

  file.copy(arquivos, DIR_SEMAFORO, overwrite = TRUE)

  invisible(TRUE)
}

carregar_gpkg_semaforo <- function(termo) {
  arquivo <- localizar_arquivo(DIR_SEMAFORO, paste0("^tab_", termo, ".*\\.gpkg$"))
  if (is.na(arquivo)) {
    return(NULL)
  }
  st_read(arquivo, quiet = TRUE)
}

SEMAFORO_CORES_CATEGORIA <- c(
  "Queda forte (≤ -10%)"                = "#D73027",
  "Queda leve (-10% a 0%)"               = "#FC8D59",
  "Estabilidade / leve alta (0% a 10%)"  = "#FFFFBF",
  "Alta moderada (10% a 20%)"            = "#91CF60",
  "Alta forte (≥ 20%)"                   = "#1A9850"
)

# Carregados uma única vez, compartilhados por todas as sessões (mapas de
# referência, não mudam por usuário) — mesmo espírito de UF_REF acima.
# Envolvido em tryCatch para não derrubar o painel inteiro (Cirurgias/OCI/
# Portaria 9810) se os .gpkg estiverem ausentes ou corrompidos: nesse caso a
# aba "Variação Cirurgias" mostra uma mensagem, e o resto do painel funciona normal.
sincronizar_dados_semaforo()
SEMAFORO_MAPA_BR <- tryCatch(carregar_gpkg_semaforo("br"), error = function(e) NULL)

# Data em que os mapas foram gerados: o script do Analise_Espacial grava
# Sys.Date() no nome do arquivo (tab_br2026-08-03.gpkg). Sem data no nome,
# cai na data de modificação do arquivo.
SEMAFORO_DATA_GERACAO <- tryCatch({
  arquivo <- localizar_arquivo(DIR_SEMAFORO, "^tab_br.*\\.gpkg$")
  data_nome <- regmatches(basename(arquivo), regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", basename(arquivo)))
  if (length(data_nome) == 1L) as.Date(data_nome) else as.Date(file.info(arquivo)$mtime)
}, error = function(e) as.Date(NA))
SEMAFORO_MAPA_REGIAO <- tryCatch(carregar_gpkg_semaforo("regiao"), error = function(e) NULL)
SEMAFORO_MAPA_MUNICIPIO <- tryCatch(carregar_gpkg_semaforo("municipio"), error = function(e) NULL)

#### UI ####

# Bloco fixo com a origem dos dados, exibido abaixo dos filtros nas duas abas.
info_fonte_dados <- function() {
  div(
    class = "text-muted small mt-3",
    style = "line-height: 1.4;",
    p(
      "Dados públicos disponibilizados pelo Ministério da Saúde por meio do painel ",
      tags$strong("SUS360"), ":"
    ),
    tags$ul(
      style = "padding-left: 1.1em;",
      tags$li(tags$a(
        href = "https://sus360.saude.gov.br/#painel/componente-ambulatorial",
        target = "_blank", rel = "noopener noreferrer",
        "Componente Ambulatorial (OCI)"
      )),
      tags$li(tags$a(
        href = "https://sus360.saude.gov.br/painel/cirurgias/",
        target = "_blank", rel = "noopener noreferrer",
        "Cirurgias Eletivas"
      ))
    ),
    p(tags$em("Sujeito a alterações."))
  )
}

# Nota de fonte específica da aba OCI (DRAC/SAES/MS, com data de
# atualização e datas de extração por sistema de origem).
info_fonte_dados_oci <- function() {
  div(
    class = "text-muted small mt-3",
    style = "line-height: 1.4;",
    p("Fonte: DRAC/SAES/MS. Atualizado em 23/09/2026."),
    p("Fonte: SIA (extração em 12/09/2026), e CMD (extração em 23/09/2026)."),
    p(tags$em("Sujeito a alterações."))
  )
}

info_fonte_dados_ce <- function() {
  div(
    class = "text-muted small mt-3",
    style = "line-height: 1.4;",
    p("Fonte: SIA/SUS e SIH/SUS. Painel SUS360 — Panorama Clínico. Extração em 26/09/2026."),
    p("Consultas/Teleconsultas e Exames Estratégicos: somente produção eletiva."),
    p(tags$em("Sujeito a alterações."))
  )
}

# Linha compacta de exportação abaixo de cada gráfico: dados brutos em CSV e
# o mesmo gráfico como slide de PowerPoint totalmente editável (rvg::dml).
# O PNG em alta resolução continua no ícone de câmera do Plotly.
barra_downloads <- function(id) {
  div(
    class = "mb-3 mt-1",
    downloadButton(paste0(id, "_csv"), "Dados (CSV)", class = "btn-sm btn-outline-secondary"),
    downloadButton(paste0(id, "_pptx"), "Slide editável (PPTX)", class = "btn-sm btn-outline-secondary")
  )
}

#### COMPONENTES DE UI (tema institucional) ####

# Linha "Filtros / Limpar filtros" no topo de cada sidebar.
sidebar_cabecalho <- function(limpar_id) {
  div(
    class = "sidebar-cabecalho",
    div(class = "titulo", HTML("&#9660;&nbsp;Filtros")),
    actionLink(limpar_id, "Limpar filtros", class = "limpar-filtros")
  )
}

# Caixa única no fim da sidebar, substituindo as várias notas explicativas
# soltas entre os filtros — recebe um ou mais parágrafos de texto.
caixa_info_sidebar <- function(...) {
  itens <- list(...)
  div(
    class = "caixa-info-sidebar",
    tagList(lapply(itens, function(texto) {
      tags$p(HTML("&#9432;"), " ", texto)
    }))
  )
}

# Cabeçalho do conteúdo de cada aba: título + subtítulo à esquerda, card
# pequeno com a data de atualização à direita (quando output_data_id existe).
cabecalho_conteudo <- function(titulo, subtitulo, output_data_id = NULL, output_atualizacao_id = NULL) {
  div(
    class = "cabecalho-conteudo",
    div(
      div(class = "titulo-principal", titulo),
      div(class = "subtitulo", subtitulo)
    ),
    if (!is.null(output_data_id)) {
      div(
        class = "card-data-atualizacao",
        "Dados até a competência", br(),
        span(class = "valor", textOutput(output_data_id, inline = TRUE)),
        if (!is.null(output_atualizacao_id)) {
          div(class = "atualizacao", textOutput(output_atualizacao_id, inline = TRUE))
        }
      )
    }
  )
}

# Um card de indicador-resumo (KPI). `valor_id` é um textOutput simples;
# `linha_id`, se dado, é um uiOutput — o servidor decide lá (via renderUI)
# se mostra nota, tendência com seta/cor, ou as duas linhas, conforme o KPI.
kpi_card <- function(label, valor_id, linha_id = NULL) {
  div(
    class = "kpi-card",
    div(class = "kpi-label", label),
    div(class = "kpi-valor", textOutput(valor_id, inline = TRUE)),
    if (!is.null(linha_id)) uiOutput(linha_id)
  )
}

# Helper para o servidor montar a(s) linha(s) auxiliares de um kpi_card().
# `texto` pode ser um vetor com mais de um item (vira uma div por linha).
# `classe` opcional colore a primeira linha (positiva/negativa), para as
# tendências com seta.
kpi_linha <- function(texto, classe = NULL) {
  texto <- texto[nzchar(texto)]
  if (length(texto) == 0) {
    return(NULL)
  }
  tagList(lapply(seq_along(texto), function(i) {
    classes <- if (i == 1 && !is.null(classe)) paste("kpi-nota kpi-tendencia", classe) else "kpi-nota"
    div(class = classes, texto[i])
  }))
}

kpi_grid <- function(...) div(class = "kpi-grid", ...)

ui <- page_navbar(
  title = div(
    class = "titulo-painel",
    div(class = "titulo-principal", "Monitoramento de Produção"),
    div(class = "titulo-org", "COQAE/DEEQAE/SAES/MS")
  ),
  id = "navbar",
  theme = bs_theme(
    version = 5,
    bg = "#F7F9FB", fg = "#152536",
    primary = "#1769AA", success = "#00875A", danger = "#D64545",
    base_font = font_google("Inter"),
    font_scale = 0.95
  ),
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo_institucional.css"),
    # Gráficos plotly dentro de abas do Bootstrap nascem com largura zero
    # (a aba fica com display:none até ser selecionada). O evento nativo
    # shown.bs.tab dispara só depois que a aba já está visível, então
    # chamar Plotly.Plots.resize() nesse momento corrige o tamanho.
    tags$script(HTML(
      "document.addEventListener('shown.bs.tab', function (e) {
         [
           'grafico_cirurgia', 'grafico_comparacao_anos',
           'grafico_oci_componente', 'grafico_oci_componente_ano', 'grafico_oci_componente_mes',
           'grafico_oci_especialidade', 'grafico_oci_especialidade_componente',
           'grafico_oci_procedimento_mes',
           'grafico_consultas_ce', 'grafico_exames_ce', 'grafico_ranking_exames_ce',
           'grafico_portaria9810_mensal', 'grafico_portaria9810_programa', 'grafico_portaria9810_limite'
         ].forEach(function (id) {
           var el = document.getElementById(id);
           if (el && window.Plotly) { Plotly.Plots.resize(el); }
         });
       });"
    ))
  ),

  nav_panel(
    "Cirurgias eletivas",
    layout_sidebar(
      sidebar = sidebar(
        open = "always", width = "310px",
        sidebar_cabecalho("limpar_cirurgia"),
        selectInput(
          "indicador_cirurgia", "Cirurgias Eletivas",
          choices = c(
            "MAC e FAEC totais" = "total",
            "MAC e FAEC do Rol" = "rol",
            "MAC, FAEC e PAB" = "total_pab",
            "Ciru. PATE (PNRF)" = "pnrf"
          ),
          selected = "rol"
        ),
        fluidRow(
          column(
            6,
            pickerInput(
              "regiao_cirurgia", "Região",
              choices = REGIOES, selected = REGIOES, multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE, selectedTextFormat = "count > 2",
                countSelectedText = "{0} regiões", noneSelectedText = "Nenhuma região"
              )
            )
          ),
          column(
            6,
            selectInput("uf_cirurgia", "UF", choices = "BRASIL", selected = "BRASIL")
          )
        ),
        selectInput(
          "municipio_cirurgia", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        selectInput(
          "gestao_cirurgia", "Gestão",
          choices = c("Todas" = "Todas", "Estadual" = "ESTADUAL", "Municipal" = "MUNICIPAL", "Dupla" = "DUPLA"),
          selected = "Todas"
        ),
        selectInput(
          "especialidade_cirurgia", "Especialidade",
          choices = c("Todas" = "Todas"), selected = "Todas"
        ),
        selectizeInput(
          "procedimento_cirurgia", "Procedimento",
          choices = character(0), selected = character(0), multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Todos (especialidade inteira)")
        ),
        caixa_info_sidebar(
          "Indicador \"MAC, FAEC e PAB\" (com financeiro) vale só em \"Comparação Anos\" — sem Diagrama de monitoramento.",
          "Filtros de Município e Gestão valem só para \"Comparação Anos\" — o Diagrama de monitoramento continua por UF/Região, sem separar por gestão.",
          "Gestão = gestão do estabelecimento. Gestão dupla não tem valor financeiro (só Físico). Gestão não se combina com Especialidade/Procedimento.",
          "Indicador ROL — Físico: Especialidade vale para \"Comparação Anos\" e \"Diagrama de monitoramento\"; Procedimento só para \"Comparação Anos\".",
          "Ranking de Procedimentos segue Região/UF/Especialidade/Procedimento (não Município/Gestão) e tem indicador, ano e mês próprios, dentro da aba."
        ),
        info_fonte_dados()
      ),
      cabecalho_conteudo(
        "Cirurgias eletivas",
        textOutput("subtitulo_cirurgia", inline = TRUE),
        "data_atualizacao_cirurgia",
        "data_processamento_cirurgia"
      ),
      kpi_grid(
        kpi_card(textOutput("kpi_cirurgia_titulo1", inline = TRUE), "kpi_cirurgia_valor1"),
        kpi_card(textOutput("kpi_cirurgia_titulo2", inline = TRUE), "kpi_cirurgia_valor2"),
        kpi_card(textOutput("kpi_cirurgia_titulo3", inline = TRUE), "kpi_cirurgia_valor3", "kpi_cirurgia_linha3"),
        kpi_card(textOutput("kpi_cirurgia_titulo4", inline = TRUE), "kpi_cirurgia_valor4", "kpi_cirurgia_linha4")
      ),
      div(class = "kpi-rodape", textOutput("kpi_cirurgia_rodape", inline = TRUE)),
      tabsetPanel(
        id = "subaba_cirurgia",
        type = "tabs",
        tabPanel(
          "Comparação Anos",
          br(),
          radioButtons(
            "metrica_cirurgia_anos", NULL,
            choices = c("Físico" = "fisico", "Financeiro (R$)" = "financeiro"),
            selected = "fisico", inline = TRUE
          ),
          plotlyOutput("grafico_comparacao_anos", height = "68vh"),
          barra_downloads("grafico_comparacao_anos")
        ),
        tabPanel(
          "Diagrama de monitoramento",
          br(),
          radioButtons(
            "metrica_cirurgia_diagrama", NULL,
            choices = c("Físico" = "fisico", "Financeiro (R$)" = "financeiro"),
            selected = "fisico", inline = TRUE
          ),
          plotlyOutput("grafico_cirurgia", height = "62vh"),
          barra_downloads("grafico_cirurgia")
        ),
        tabPanel(
          "Ranking de Procedimentos",
          br(),
          radioButtons(
            "indicador_ranking_cirurgia", NULL,
            choices = c("Eletivas totais" = "totais", "Eletivas do ROL" = "rol"),
            selected = "totais", inline = TRUE
          ),
          fluidRow(
            column(
              6,
              selectInput("ano_ranking_cirurgia", "Ano", choices = NULL, width = "140px")
            ),
            column(
              6,
              selectInput(
                "mes_ranking_cirurgia", "Mês de competência",
                choices = c("Ano inteiro" = "0"), selected = "0", width = "180px"
              )
            )
          ),
          div(class = "subtitulo mb-2", textOutput("subtitulo_ranking_cirurgia", inline = TRUE)),
          DTOutput("tabela_ranking_cirurgia", height = "auto"),
          div(
            class = "mb-3 mt-2",
            downloadButton("tabela_ranking_cirurgia_csv", "Dados (CSV)", class = "btn-sm btn-outline-secondary")
          )
        )
      )
    )
  ),

  if (MOSTRAR_VARIACAO_CIRURGIAS) nav_panel(
    "Variação Cirurgias",
    div(
      class = "p-3",
      tags$head(tags$style(HTML(
        "
        .selectize-dropdown { z-index: 9999 !important; }
        .selectize-control { z-index: 9999 !important; }
        .shiny-input-container { z-index: auto !important; }
        .card-body { overflow: visible !important; }
        .card { overflow: visible !important; }
        "
      ))),
      cabecalho_conteudo(
        "Variação Cirurgias", textOutput("subtitulo_semaforo", inline = TRUE),
        "data_atualizacao_semaforo", "data_processamento_semaforo"
      ),
      kpi_grid(
        kpi_card("UFs em alta", "kpi_semaforo_alta", "kpi_semaforo_alta_linha"),
        kpi_card("UFs em queda", "kpi_semaforo_queda", "kpi_semaforo_queda_linha"),
        kpi_card("Maior alta", "kpi_semaforo_maior_alta", "kpi_semaforo_maior_alta_linha"),
        kpi_card("Maior queda", "kpi_semaforo_maior_queda", "kpi_semaforo_maior_queda_linha")
      ),

      card(
        card_header(class = "bg-primary text-white", "Brasil - Variação por UF"),
        plotOutput("semaforo_map_br", height = "600px") |> withSpinner(color = "#0dc5c1"),
        layout_column_wrap(
          width = 1 / 2,
          card(
            downloadButton(
              "semaforo_download_ppt_br", "Baixar Mapa (PPT)",
              class = "btn-warning btn-lg w-100", icon = icon("file-powerpoint")
            ),
            class = "text-center p-2"
          ),
          card(
            downloadButton(
              "semaforo_download_tabela_br", "Baixar Tabela (Excel)",
              class = "btn-success btn-lg w-100", icon = icon("file-excel")
            ),
            class = "text-center p-2"
          )
        )
      ),
      br(),
      card(
        card_header(style = "background-color:#294158; color:#fff;", "Regiões de Saúde - Variação"),
        layout_column_wrap(
          width = 1 / 3,
          card(
            selectInput("semaforo_uf_regiao", "Selecione a UF:", choices = NULL, selected = NULL),
            class = "p-2"
          )
        ),
        plotOutput("semaforo_map_regiao", height = "600px") |> withSpinner(color = "#0dc5c1"),
        layout_column_wrap(
          width = 1 / 2,
          card(
            downloadButton(
              "semaforo_download_ppt_regiao", "Baixar Mapa (PPT - % e nomes)",
              class = "btn-warning btn-lg w-100", icon = icon("file-powerpoint")
            ),
            class = "text-center p-2"
          ),
          card(
            downloadButton(
              "semaforo_download_tabela_regiao", "Baixar Tabela (Excel)",
              class = "btn-success btn-lg w-100", icon = icon("file-excel")
            ),
            class = "text-center p-2"
          )
        )
      ),
      br(),
      card(
        card_header(style = "background-color:#18B997; color:#fff;", "Municípios - Variação"),
        layout_column_wrap(
          width = 1 / 3,
          card(
            selectInput("semaforo_uf_municipio", "Selecione a UF:", choices = NULL, selected = NULL),
            class = "p-2"
          ),
          card(
            selectInput(
              "semaforo_municipio_selecionado", "Selecione o(s) Município(s):",
              choices = NULL, selected = NULL, multiple = TRUE
            ),
            class = "p-2"
          )
        ),
        plotOutput("semaforo_map_municipio", height = "600px") |> withSpinner(color = "#0dc5c1"),
        layout_column_wrap(
          width = 1 / 3,
          card(
            downloadButton(
              "semaforo_download_ppt_municipio", "Baixar Mapa (PPT - %)",
              class = "btn-warning btn-lg w-100", icon = icon("file-powerpoint")
            ),
            class = "text-center p-2"
          ),
          card(
            downloadButton(
              "semaforo_download_ppt_municipio_nome", "Baixar (PPT - % e nomes)",
              class = "btn-warning btn-lg w-100", icon = icon("file-powerpoint")
            ),
            class = "text-center p-2"
          ),
          card(
            downloadButton(
              "semaforo_download_tabela_municipio", "Baixar Tabela (Excel)",
              class = "btn-success btn-lg w-100", icon = icon("file-excel")
            ),
            class = "text-center p-2"
          )
        )
      )
    )
  ),

  nav_panel(
    "OCI",
    layout_sidebar(
      sidebar = sidebar(
        open = "always", width = "310px",
        sidebar_cabecalho("limpar_oci"),
        pickerInput(
          "regiao_oci", "Região",
          choices = REGIOES, selected = REGIOES, multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, selectedTextFormat = "count > 2",
            countSelectedText = "{0} regiões", noneSelectedText = "Nenhuma região"
          )
        ),
        selectInput("uf_oci", "UF", choices = "BRASIL", selected = "BRASIL"),
        selectInput(
          "municipio_oci", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        caixa_info_sidebar(
          "Filtro de Município vale para todas as abas de OCI.",
          "Registros sem UF de atendimento (contatos do CMD) entram só no total Brasil, com as 5 regiões marcadas."
        ),
        info_fonte_dados_oci()
      ),
      cabecalho_conteudo("OCI realizadas", textOutput("subtitulo_oci", inline = TRUE)),
      kpi_grid(
        kpi_card(textOutput("kpi_oci_titulo1", inline = TRUE), "kpi_oci_valor1", "kpi_oci_linha1"),
        kpi_card(textOutput("kpi_oci_titulo2", inline = TRUE), "kpi_oci_valor2", "kpi_oci_linha2"),
        kpi_card("OCI total", "kpi_oci_valor3", "kpi_oci_linha3"),
        kpi_card("Taxa de expansão anual", "kpi_oci_valor4", "kpi_oci_linha4")
      ),
      div(class = "kpi-rodape", textOutput("kpi_oci_rodape", inline = TRUE)),
      tabsetPanel(
        id = "subaba_oci",
        type = "tabs",
        tabPanel(
          "Série histórica OCI",
          br(),
          h5("OCI geral"),
          plotlyOutput("grafico_oci_componente", height = "44vh"),
          barra_downloads("grafico_oci_componente"),
          br(),
          h5("OCI por componente — por ano"),
          plotlyOutput("grafico_oci_componente_ano", height = "38vh"),
          barra_downloads("grafico_oci_componente_ano"),
          DTOutput("tabela_oci_componente_ano", height = "auto"),
          br(),
          h5("OCI por componente — por mês de atendimento"),
          plotlyOutput("grafico_oci_componente_mes", height = "40vh"),
          barra_downloads("grafico_oci_componente_mes")
        ),
        tabPanel(
          "Série histórica OCI por especialidade",
          br(),
          fluidRow(
            column(
              8,
              checkboxGroupInput(
                "especialidades_oci_sub", "Especialidades",
                choices = ORDEM_ESPECIALIDADES, selected = ORDEM_ESPECIALIDADES,
                inline = TRUE
              )
            ),
            column(
              4,
              selectInput(
                "componente_oci_sub", "Componente",
                choices = COMPONENTES_OCI_FILTRO, selected = "geral"
              )
            )
          ),
          plotlyOutput("grafico_oci_especialidade", height = "50vh"),
          barra_downloads("grafico_oci_especialidade"),
          br(),
          h5("OCI por especialidade e componente"),
          checkboxGroupInput(
            "anos_oci_especialidade_componente", "Ano",
            choices = ANOS_OCI, selected = ANOS_OCI, inline = TRUE
          ),
          checkboxGroupInput(
            "componentes_oci_especialidade_componente", "Componente",
            choices = ORDEM_COMPONENTES_OCI, selected = ORDEM_COMPONENTES_OCI, inline = TRUE
          ),
          plotlyOutput("grafico_oci_especialidade_componente", height = "50vh"),
          barra_downloads("grafico_oci_especialidade_componente"),
          h6("Físico — OCI realizadas", style = "margin-top: 16px;"),
          DTOutput("tabela_oci_especialidade_componente_fisico", height = "auto"),
          h6("Financeiro — valor federal de referência (R$)", style = "margin-top: 16px;"),
          DTOutput("tabela_oci_especialidade_componente_financeiro", height = "auto")
        ),
        tabPanel(
          "Especialidade e Procedimentos",
          br(),
          fluidRow(
            column(
              4,
              selectInput(
                "especialidade_oci_procedimento", "Especialidade",
                choices = ORDEM_ESPECIALIDADES, selected = ORDEM_ESPECIALIDADES[1]
              )
            ),
            column(
              4,
              selectInput("ano_oci_procedimento", "Ano", choices = NULL)
            ),
            column(
              4,
              selectInput(
                "mes_oci_procedimento", "Mês de atendimento",
                choices = c("Ano inteiro" = "0"), selected = "0"
              )
            )
          ),
          div(class = "subtitulo mb-2", textOutput("subtitulo_oci_procedimento", inline = TRUE)),
          DTOutput("tabela_oci_procedimento", height = "auto"),
          div(
            class = "mb-3 mt-2",
            downloadButton("tabela_oci_procedimento_csv", "Dados (CSV)", class = "btn-sm btn-outline-secondary")
          ),
          br(),
          h5(paste0("Top ", TOP_N_PROCEDIMENTOS_OCI, " procedimentos — por mês de atendimento")),
          plotlyOutput("grafico_oci_procedimento_mes", height = "44vh"),
          barra_downloads("grafico_oci_procedimento_mes")
        )
      )
    )
  ),

  nav_panel(
    "Consultas e Exames",
    layout_sidebar(
      sidebar = sidebar(
        open = "always", width = "310px",
        sidebar_cabecalho("limpar_ce"),
        pickerInput(
          "regiao_ce", "Região",
          choices = REGIOES, selected = REGIOES, multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, selectedTextFormat = "count > 2",
            countSelectedText = "{0} regiões", noneSelectedText = "Nenhuma região"
          )
        ),
        selectInput("uf_ce", "UF", choices = "BRASIL", selected = "BRASIL"),
        selectInput(
          "municipio_ce", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        selectInput(
          "especialidade_ce", "Especialidade",
          choices = c("Todas" = "Todas", ORDEM_ESPECIALIDADES_CE), selected = "Todas"
        ),
        caixa_info_sidebar(
          "Filtro de Especialidade vale só para \"Exames Estratégicos\" (Consultas/Teleconsultas não tem essa quebra).",
          "\"Ressonância Magnética\" e \"Tomografia Computadorizada\" são modalidades de exame, não especialidades médicas — mantidas como o projeto de origem já agrupava."
        ),
        info_fonte_dados_ce()
      ),
      cabecalho_conteudo("Consultas e Exames", textOutput("subtitulo_ce", inline = TRUE)),
      kpi_grid(
        kpi_card(textOutput("kpi_ce_titulo1", inline = TRUE), "kpi_ce_valor1", "kpi_ce_linha1"),
        kpi_card("Dentro da OCI", "kpi_ce_valor2", "kpi_ce_linha2"),
        kpi_card("% dentro da OCI", "kpi_ce_valor3", "kpi_ce_linha3"),
        kpi_card("Taxa de expansão", "kpi_ce_valor4", "kpi_ce_linha4")
      ),
      div(class = "kpi-rodape", textOutput("kpi_ce_rodape", inline = TRUE)),
      tabsetPanel(
        id = "subaba_ce",
        type = "tabs",
        tabPanel(
          "Consultas e Teleconsultas",
          br(),
          fluidRow(
            column(3, selectInput("ano_fim_consultas_ce", "Até o ano", choices = NULL, width = "100%")),
            column(4, selectInput("mes_fim_consultas_ce", "Até o mês", choices = NULL, width = "100%"))
          ),
          plotlyOutput("grafico_consultas_ce", height = "54vh"),
          barra_downloads("grafico_consultas_ce")
        ),
        tabPanel(
          "Exames Estratégicos Eletivos",
          br(),
          fluidRow(
            column(3, selectInput("ano_fim_exames_ce", "Até o ano", choices = NULL, width = "100%")),
            column(4, selectInput("mes_fim_exames_ce", "Até o mês", choices = NULL, width = "100%"))
          ),
          plotlyOutput("grafico_exames_ce", height = "54vh"),
          barra_downloads("grafico_exames_ce"),
          br(),
          h5(paste0("Top ", TOP_N_EXAMES_CE, " exames mais realizados — por mês de competência")),
          plotlyOutput("grafico_ranking_exames_ce", height = "44vh"),
          barra_downloads("grafico_ranking_exames_ce")
        ),
        tabPanel(
          "Ranking de Exames",
          br(),
          fluidRow(
            column(3, selectInput("ano_ranking_exames_ce", "Ano", choices = NULL, width = "100%"))
          ),
          div(class = "subtitulo mb-2", textOutput("subtitulo_ranking_exames_ce", inline = TRUE)),
          DTOutput("tabela_ranking_exames_ce", height = "auto"),
          div(
            class = "mb-3 mt-2",
            downloadButton("tabela_ranking_exames_ce_csv", "Dados (CSV)", class = "btn-sm btn-outline-secondary")
          )
        ),
        tabPanel(
          "Tabela Geral",
          br(),
          fluidRow(
            column(
              4,
              radioButtons(
                "indicador_tabela_ce", NULL,
                choices = c("Exames Estratégicos" = "exames", "Consultas/Teleconsultas" = "consultas"),
                selected = "exames", inline = TRUE
              )
            ),
            column(
              4,
              selectInput("ano_tabela_ce", "Ano", choices = NULL, width = "140px")
            ),
            column(
              4,
              radioButtons(
                "metrica_tabela_ce", NULL,
                choices = c("Físico" = "fisico", "Financeiro (R$)" = "financeiro"),
                selected = "fisico", inline = TRUE
              )
            )
          ),
          div(class = "subtitulo mb-2", textOutput("subtitulo_tabela_ce", inline = TRUE)),
          DTOutput("tabela_geral_ce", height = "auto"),
          div(
            class = "mb-3 mt-2",
            downloadButton("tabela_geral_ce_csv", "Dados (CSV)", class = "btn-sm btn-outline-secondary")
          )
        )
      )
    )
  ),

  nav_panel(
    "Pagamento Port.",
    layout_sidebar(
      sidebar = sidebar(
        open = "always", width = "310px",
        sidebar_cabecalho("limpar_portaria9810"),
        selectInput("uf_portaria9810", "UF", choices = "BRASIL", selected = "BRASIL"),
        selectInput(
          "municipio_portaria9810", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        checkboxGroupInput(
          "tipo_gestao_portaria9810", "Tipo de Gestão",
          choices = c("Estadual", "Municipal"), selected = c("Estadual", "Municipal")
        ),
        checkboxGroupInput(
          "componente_portaria9810", "Componente",
          choices = character(0), selected = character(0)
        ),
        caixa_info_sidebar(
          "Considera somente pagamentos da Portaria nº 9.810 (Valor Líquido).",
          "* Despesa de Exercício Anterior.",
          tagList(
            "Fonte da execução financeira (Pagamentos): Painel FAF/InvestSUS",
            tags$a(
              href = "https://investsuspaineis.saude.gov.br/extensions/CGIN_Painel_FAF/CGIN_Painel_FAF.html",
              target = "_blank", "(acessar)"
            ),
            ", extração em 03/10/2026 — filtros considerados: Ano 2026, os 5 códigos de Programa do Agora Tem",
            " Especialistas (Componente Ambulatorial, Componente Cirúrgico, Mutirão, FAEC - PMAE e FAEC -",
            " Redução das Filas — mesma classificação de Componente usada neste painel), todas as UF, com",
            " filtro para a Portaria 9.810."
          )
        )
      ),
      cabecalho_conteudo(
        "Limite financeiro para execução do Programa Agora Tem Especialistas",
        textOutput("subtitulo_portaria9810", inline = TRUE)
      ),
      kpi_grid(
        kpi_card("Valor pago no período", "kpi_portaria_valor_periodo", "kpi_portaria_valor_periodo_linha"),
        kpi_card("Maior pagamento mensal", "kpi_portaria_maior_mes", "kpi_portaria_maior_mes_linha"),
        kpi_card("Último mês de pagamento", "kpi_portaria_ultimo_mes", "kpi_portaria_ultimo_mes_linha"),
        kpi_card("Componente com maior pagamento", "kpi_portaria_componente_top", "kpi_portaria_componente_top_linha")
      ),
      tabsetPanel(
        id = "subaba_portaria9810",
        type = "tabs",
        tabPanel(
          "Pagamentos",
          br(),
          h5("Pagamentos por mês"),
          plotlyOutput("grafico_portaria9810_mensal", height = "42vh"),
          barra_downloads("grafico_portaria9810_mensal"),
          br(),
          h5("Pagamentos por mês e por Componente"),
          plotlyOutput("grafico_portaria9810_programa", height = "42vh"),
          barra_downloads("grafico_portaria9810_programa"),
          br(),
          h5("Detalhamento por UF"),
          DTOutput("tabela_portaria9810_uf")
        ),
        tabPanel(
          "Limite da Portaria",
          br(),
          div(
            class = "text-muted small mb-2", style = "line-height: 1.3;",
            "O limite da Portaria é definido por UF inteira (todos os municípios e tipos de gestão) — os filtros de Município e Tipo de Gestão não se aplicam aqui."
          ),
          div(
            class = "text-muted small mb-2", style = "line-height: 1.3;",
            tags$em(
              "Nota: este limite considera a Portaria GM/MS nº 12.174, de 21 de setembro de 2026, que altera o",
              " Anexo da Portaria GM/MS nº 9.810, de 27 de dezembro de 2025, que estabelece o limite financeiro",
              " para execução do Programa Agora Tem Especialistas — Componentes Ambulatorial e Cirúrgico, em 2026."
            )
          ),
          h5("Valor pago x limite por UF"),
          plotlyOutput("grafico_portaria9810_limite", height = "58vh"),
          barra_downloads("grafico_portaria9810_limite"),
          br(),
          h5("Tabela de acompanhamento"),
          DTOutput("tabela_portaria9810_limite")
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    div(
      class = "logo-item",
      img(src = "logo_sus_ms.png", class = "logo-institucional", alt = "SUS + Ministério da Saúde")
    )
  )
)

#### SERVER ####

server <- function(input, output, session) {

  dados <- reactiveVal(carregar_tudo())

  # Card do cabeçalho de Cirurgias: última competência com produção na série
  # de Cirurgias (não a da OCI) e a data em que essa base foi gerada.
  texto_data_atualizacao <- reactive({
    serie <- dados()$cirurgia_anos_total
    if (is.null(serie)) {
      return("")
    }
    ultima <- serie[uf_atendimento == "BRASIL" & quantidade > 0][order(ano, mes)][.N]
    sprintf("%02d/%d", ultima$mes, ultima$ano)
  })
  output$data_atualizacao_cirurgia <- renderText({ texto_data_atualizacao() })

  output$data_processamento_cirurgia <- renderText({
    dados()
    arquivo <- file.path(DADOS_LOCAIS, "atualizacao_cirurgia.txt")
    data <- if (file.exists(arquivo)) {
      suppressWarnings(as.Date(readLines(arquivo, n = 1L, warn = FALSE)))
    } else {
      as.Date(NA)
    }
    if (is.na(data)) {
      return("")
    }
    paste0("Atualizado em ", MESES_PT[as.integer(format(data, "%m"))], " de ", format(data, "%Y"))
  })

  ## ---- Cirurgias ----

  # Física, independente do toggle Físico/Financeiro do Diagrama — usada pela
  # cascata de UF (que não segue esse toggle).
  serie_cirurgia_indicador_fisica <- reactive({
    req(input$indicador_cirurgia)
    dados()[[paste0("cirurgia_", input$indicador_cirurgia)]]
  })

  # Física ou financeira conforme o toggle do Diagrama de monitoramento —
  # só usada por dados_diagrama_cirurgia()/output$grafico_cirurgia.
  serie_cirurgia_indicador <- reactive({
    req(input$indicador_cirurgia)
    req(input$metrica_cirurgia_diagrama)
    prefixo <- if (input$metrica_cirurgia_diagrama == "financeiro") "cirurgia_financeiro_" else "cirurgia_"
    dados()[[paste0(prefixo, input$indicador_cirurgia)]]
  })

  observeEvent(list(input$regiao_cirurgia, serie_cirurgia_indicador_fisica()), {

    ufs_regiao <- sort(UF_REF[REGIAO %in% input$regiao_cirurgia]$NM_UF_CIRURGIA)
    escolhas <- setNames(
      c("BRASIL", ufs_regiao),
      c(rotulo_agregado(input$regiao_cirurgia), ufs_regiao)
    )

    selecionado <- if (isTRUE(input$uf_cirurgia %in% escolhas)) input$uf_cirurgia else "BRASIL"

    updateSelectInput(session, "uf_cirurgia", choices = escolhas, selected = selecionado)
  })

  dados_diagrama_cirurgia <- reactive({

    req(input$indicador_cirurgia, input$uf_cirurgia)
    validate(need(length(input$regiao_cirurgia) > 0, "Selecione ao menos uma região."))

    especialidade_sel <- if (is.null(input$especialidade_cirurgia)) "Todas" else input$especialidade_cirurgia

    if (input$indicador_cirurgia == "rol" && especialidade_sel != "Todas") {

      validate(need(
        input$metrica_cirurgia_diagrama == "fisico",
        "Não há valor financeiro por especialidade ainda — troque para \"Físico\" para usar o filtro de Especialidade no diagrama."
      ))

      serie_proc <- dados()$cirurgia_procedimento_rol
      mapa <- dados()$mapa_especialidade_rol
      validate(need(
        !is.null(serie_proc) && !is.null(mapa),
        "Série por procedimento não encontrada. Rode novamente o script 01 do projeto Cirurgia."
      ))

      return(diagrama_especialidade_rol(serie_proc, mapa, input$uf_cirurgia, input$regiao_cirurgia, especialidade_sel))
    }

    validate(need(
      !input$indicador_cirurgia %in% c("pnrf", "total_pab"),
      if (isTRUE(input$indicador_cirurgia == "total_pab")) {
        "Diagrama de monitoramento não disponível para \"MAC, FAEC e PAB\" — as faixas de controle são calculadas só para MAC e FAEC (totais/ROL). Use \"Comparação Anos\"."
      } else {
        "Diagrama de monitoramento não disponível para o Programa (PNRF) — o histórico ainda é curto demais para gerar faixas de controle confiáveis. Use \"Comparação Anos\"."
      }
    ))

    serie <- serie_cirurgia_indicador()
    validate(need(!is.null(serie), "Série de cirurgias não encontrada. Rode o script 02_monitoramento_diagrama_controle.R no projeto Cirurgia."))

    todas_regioes <- setequal(input$regiao_cirurgia, REGIOES)

    dados_uf <- if (input$uf_cirurgia == "BRASIL") {
      if (todas_regioes) serie[uf_atendimento == "BRASIL"] else agregar_cirurgia_regiao(serie, input$regiao_cirurgia)
    } else {
      serie[uf_atendimento == input$uf_cirurgia]
    }
    validate(need(nrow(dados_uf) > 0, "Sem dados para a UF selecionada."))
    dados_uf
  })

  plot_cirurgia <- reactive({

    dados_uf <- dados_diagrama_cirurgia()

    ano_monitoramento <- max(dados_uf$ano)

    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[input$indicador_cirurgia]]
    rotulo_uf <- if (input$uf_cirurgia == "BRASIL") rotulo_agregado(input$regiao_cirurgia) else input$uf_cirurgia

    especialidade_sel <- input$especialidade_cirurgia
    sufixo_especialidade <- if (isTRUE(input$indicador_cirurgia == "rol") && isTRUE(especialidade_sel != "Todas")) {
      paste0(" — ", especialidade_sel)
    } else {
      ""
    }

    grafico_cirurgia(
      dados_uf, ano_monitoramento,
      titulo = paste0(rotulo_indicador, " — ", rotulo_uf, sufixo_especialidade),
      metrica = input$metrica_cirurgia_diagrama
    )
  })

  output$grafico_cirurgia <- renderPlotly({
    ggplotly(plot_cirurgia(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.2), margin = list(b = 90)) |>
      alta_resolucao("diagrama_monitoramento_cirurgias")
  })

  ## ---- Cirurgias: Ranking de Procedimentos ----
  # Segue a mesma regra de análise usada no SAM (script 01_cirurgias_sam.R
  # do projeto SAM): ranking pelo procedimento PRINCIPAL, com os códigos de
  # Cirurgia Múltipla (subgrupo SIGTAP 04.15) substituídos pelo secundário
  # mais frequente. Diferença: aqui isso é recalculado dentro da seleção de
  # Região/UF do usuário, não fixo em "Brasil" — ver script
  # 11_ranking_procedimentos_cirurgia.R (projeto Cirurgia).

  observeEvent(dados(), {
    ranking <- dados()$cirurgia_ranking_procedimento
    if (is.null(ranking)) return()
    anos <- sort(unique(ranking$ano), decreasing = TRUE)
    updateSelectInput(session, "ano_ranking_cirurgia", choices = anos, selected = anos[1])
    escolhas_mes <- c("Ano inteiro" = "0", setNames(as.character(1:12), MES_LABELS))
    updateSelectInput(session, "mes_ranking_cirurgia", choices = escolhas_mes, selected = "0")
  }, once = TRUE)

  ranking_cirurgia_top <- reactive({

    req(input$uf_cirurgia, input$ano_ranking_cirurgia, input$indicador_ranking_cirurgia, input$mes_ranking_cirurgia)
    validate(need(length(input$regiao_cirurgia) > 0, "Selecione ao menos uma região."))

    base <- dados()$cirurgia_ranking_procedimento
    validate(need(!is.null(base), "Ranking de procedimentos não encontrado. Rode o script 11_ranking_procedimentos_cirurgia.R no projeto Cirurgia."))

    ano_sel <- as.integer(input$ano_ranking_cirurgia)
    mes_sel <- as.integer(input$mes_ranking_cirurgia)
    coluna_qtd <- if (input$indicador_ranking_cirurgia == "rol") "qtd_eletivas_rol" else "qtd_eletivas_totais"

    ufs_sel <- if (input$uf_cirurgia == "BRASIL") {
      UF_REF[REGIAO %in% input$regiao_cirurgia]$NM_UF_CIRURGIA
    } else {
      input$uf_cirurgia
    }

    base_sel <- base[ano == ano_sel & uf_atendimento %chin% ufs_sel]
    if (mes_sel > 0) {
      base_sel <- base_sel[mes == mes_sel]
    }

    # Segue o mesmo filtro de Especialidade/Procedimento da lateral esquerda
    # (cruza pelo código SIGTAP com dados()$mapa_especialidade_rol, igual
    # a filtrar_procedimento_rol()) — vale para "Eletivas totais" e "do
    # ROL", independente do indicador principal do sidebar.
    especialidade_sel <- if (is.null(input$especialidade_cirurgia)) "Todas" else input$especialidade_cirurgia
    if (isTRUE(especialidade_sel != "Todas")) {

      mapa <- dados()$mapa_especialidade_rol
      validate(need(!is.null(mapa), "Mapa de Especialidade não encontrado. Confira dados/Relacao_cirugiasROL.xlsx."))

      codigos_sel <- mapa[especialidade == especialidade_sel]$codigo_procedimento_principal
      procedimentos_sel <- input$procedimento_cirurgia
      if (length(procedimentos_sel) > 0) {
        codigos_sel <- intersect(codigos_sel, procedimentos_sel)
      }

      base_sel <- base_sel[as.character(codigo_procedimento) %chin% codigos_sel]
    }

    validate(need(nrow(base_sel) > 0, "Sem dados para a seleção atual."))

    ranking <- base_sel[
      , .(quantidade = sum(get(coluna_qtd), na.rm = TRUE)),
      by = .(codigo_procedimento, nome_procedimento)
    ]
    ranking <- ranking[quantidade > 0]
    validate(need(nrow(ranking) > 0, "Sem dados para a seleção atual."))
    setorder(ranking, -quantidade)
    ranking <- head(ranking, TOP_N_RANKING_CIRURGIA)
    ranking[, ranking := .I]

    # Substituição da Cirurgia Múltipla, recalculada dentro da mesma
    # seleção de Região/UF/Ano — mesma lógica do SAM, ver comentário acima.
    secundario <- dados()$cirurgia_ranking_secundario
    idx_multiplo <- which(ranking$codigo_procedimento %in% CODIGOS_PROCEDIMENTO_MULTIPLO_CIRURGIA)

    if (!is.null(secundario) && length(idx_multiplo) > 0) {
      for (i in idx_multiplo) {

        codigo_original <- ranking$codigo_procedimento[i]
        nome_original <- ranking$nome_procedimento[i]

        secs_filtro <- secundario[
          ano == ano_sel & uf_atendimento %chin% ufs_sel & codigo_procedimento_principal == codigo_original
        ]
        if (mes_sel > 0) {
          secs_filtro <- secs_filtro[mes == mes_sel]
        }
        secs <- secs_filtro[
          , .(qtd_sec = sum(get(coluna_qtd), na.rm = TRUE)),
          by = .(codigo_procedimento_secundario, nome_procedimento_secundario)
        ]
        secs <- secs[qtd_sec > 0]
        if (nrow(secs) == 0L) next

        setorder(secs, -qtd_sec)
        mais_frequente <- secs[1]

        set(ranking, i, "codigo_procedimento", mais_frequente$codigo_procedimento_secundario)
        set(
          ranking, i, "nome_procedimento",
          paste0(
            mais_frequente$nome_procedimento_secundario,
            " (secundário mais frequente em ", nome_original, ")"
          )
        )
      }
    }

    setcolorder(ranking, c("ranking", "codigo_procedimento", "nome_procedimento", "quantidade"))
    ranking[]
  })

  rotulo_ranking_cirurgia <- reactive({
    if (isTRUE(input$indicador_ranking_cirurgia == "rol")) {
      "Cirurgias Eletivas do ROL"
    } else {
      "Cirurgias Eletivas totais"
    }
  })

  # Mesmo sufixo " — Especialidade — Procedimento(s)" de
  # rotulo_procedimento_cirurgia(), mas sem o gate de indicador == "rol":
  # o Ranking aplica Especialidade/Procedimento em "Eletivas totais" e
  # "do ROL" (ver ranking_cirurgia_top()).
  rotulo_especialidade_ranking <- reactive({

    especialidade_sel <- input$especialidade_cirurgia
    if (is.null(especialidade_sel) || especialidade_sel == "Todas") {
      return("")
    }

    procedimentos_sel <- input$procedimento_cirurgia
    if (length(procedimentos_sel) > 0) {
      mapa <- dados()$mapa_especialidade_rol
      nomes <- if (!is.null(mapa)) mapa[codigo_procedimento_principal %chin% procedimentos_sel]$nome_procedimento else procedimentos_sel
      sufixo <- if (length(nomes) <= 2) paste(nomes, collapse = "; ") else paste0(length(nomes), " procedimentos selecionados")
      paste0(" — ", especialidade_sel, " — ", sufixo)
    } else {
      paste0(" — ", especialidade_sel)
    }
  })

  output$subtitulo_ranking_cirurgia <- renderText({
    req(input$mes_ranking_cirurgia)
    mes_sel <- as.integer(input$mes_ranking_cirurgia)
    rotulo_periodo <- if (mes_sel > 0) {
      paste0(MES_LABELS[mes_sel], "/", input$ano_ranking_cirurgia)
    } else {
      input$ano_ranking_cirurgia
    }
    paste0(
      rotulo_ranking_cirurgia(), " — ", rotulo_periodo, " — ", rotulo_local_cirurgia(),
      rotulo_especialidade_ranking()
    )
  })

  output$tabela_ranking_cirurgia_csv <- handler_csv(ranking_cirurgia_top, "ranking_procedimentos_cirurgia")

  output$tabela_ranking_cirurgia <- renderDT({
    d <- ranking_cirurgia_top()
    datatable(
      d,
      colnames = c("#", "Código", "Procedimento", "Quantidade"),
      rownames = FALSE,
      selection = "none",
      fillContainer = FALSE,
      height = "auto",
      options = list(dom = "t", ordering = FALSE, paging = FALSE, pageLength = TOP_N_RANKING_CIRURGIA)
    ) |>
      formatRound("quantidade", digits = 0, mark = ".", interval = 3)
  })

  # Indicador efetivo de "Comparação Anos" — hoje é sempre
  # input$indicador_cirurgia (o combinado "MAC, FAEC e PAB" já está no
  # próprio dropdown; ver total_pab em carregar_tudo()/somar_series_cirurgia()).
  indicador_comparacao_anos <- reactive({
    input$indicador_cirurgia
  })

  serie_anos_cirurgia_indicador <- reactive({
    req(indicador_comparacao_anos())
    dados()[[paste0("cirurgia_anos_", indicador_comparacao_anos())]]
  })

  serie_municipio_cirurgia_indicador <- reactive({
    req(indicador_comparacao_anos())
    dados()[[paste0("cirurgia_municipio_", indicador_comparacao_anos())]]
  })

  # Cascata UF -> Município: só populado quando uma UF específica está
  # selecionada (em "BRASIL" ficaria com 5000+ opções, sem sentido).
  observeEvent(list(input$uf_cirurgia, serie_municipio_cirurgia_indicador()), {

    serie_mun <- serie_municipio_cirurgia_indicador()

    if (is.null(input$uf_cirurgia) || input$uf_cirurgia == "BRASIL" || is.null(serie_mun)) {
      updateSelectInput(session, "municipio_cirurgia", choices = c("Selecione uma UF" = "Todos"), selected = "Todos")
      return()
    }

    municipios <- sort(unique(serie_mun[uf_atendimento == input$uf_cirurgia]$municipio_atendimento))
    escolhas <- setNames(c("Todos", municipios), c("Todos (UF inteira)", municipios))

    selecionado <- if (isTRUE(input$municipio_cirurgia %in% escolhas)) input$municipio_cirurgia else "Todos"

    updateSelectInput(session, "municipio_cirurgia", choices = escolhas, selected = selecionado)
  })

  # Popula as opções de Especialidade a partir da planilha de mapeamento
  # (só existe para o indicador ROL — ver carregar_mapa_especialidade_rol()).
  observeEvent(dados(), {

    mapa <- dados()$mapa_especialidade_rol

    if (is.null(mapa)) {
      return()
    }

    especialidades <- sort(unique(mapa$especialidade))
    escolhas <- setNames(c("Todas", especialidades), c("Todas", especialidades))

    selecionado <- if (isTRUE(input$especialidade_cirurgia %in% escolhas)) input$especialidade_cirurgia else "Todas"

    updateSelectInput(session, "especialidade_cirurgia", choices = escolhas, selected = selecionado)
  }, once = TRUE)

  # Cascata Especialidade -> Procedimento: só populado quando uma
  # especialidade específica está selecionada. Multisseleção — nenhum
  # procedimento marcado significa "especialidade inteira" (ver
  # dados_comparacao_anos() / filtrar_procedimento_rol()).
  observeEvent(list(input$especialidade_cirurgia, dados()$mapa_especialidade_rol), {

    mapa <- dados()$mapa_especialidade_rol

    if (is.null(mapa) || is.null(input$especialidade_cirurgia) || input$especialidade_cirurgia == "Todas") {
      updateSelectizeInput(session, "procedimento_cirurgia", choices = character(0), selected = character(0))
      return()
    }

    procedimentos <- mapa[especialidade == input$especialidade_cirurgia][order(nome_procedimento)]
    escolhas <- setNames(procedimentos$codigo_procedimento_principal, procedimentos$nome_procedimento)

    selecionado <- intersect(input$procedimento_cirurgia, escolhas)

    updateSelectizeInput(session, "procedimento_cirurgia", choices = escolhas, selected = selecionado)
  })

  # Dados por trás do gráfico "Comparação Anos" e da tabela abaixo dele —
  # reactive compartilhada para não duplicar a lógica de filtro.
  dados_comparacao_anos <- reactive({

    req(input$indicador_cirurgia, input$uf_cirurgia)
    validate(need(length(input$regiao_cirurgia) > 0, "Selecione ao menos uma região."))

    indicador_sel <- indicador_comparacao_anos()

    especialidade_sel <- if (is.null(input$especialidade_cirurgia)) "Todas" else input$especialidade_cirurgia
    procedimentos_sel <- input$procedimento_cirurgia
    filtro_procedimento_ativo <- especialidade_sel != "Todas" || length(procedimentos_sel) > 0
    gestao_ativa <- isTRUE(!is.null(input$gestao_cirurgia) && input$gestao_cirurgia != "Todas")

    if (filtro_procedimento_ativo) {

      validate(need(
        indicador_sel == "rol",
        "Filtros de Especialidade/Procedimento valem só para o indicador ROL. Selecione ROL (e desmarque PAB), ou volte a Especialidade/Procedimento para \"Todas\"/\"Todos\"."
      ))
      validate(need(
        input$metrica_cirurgia_anos == "fisico",
        "Não há valor financeiro — troque para \"Físico\" para usar os filtros de Especialidade/Procedimento."
      ))
      validate(need(
        !gestao_ativa,
        "O filtro de Gestão não se combina com Especialidade/Procedimento. Volte Gestão para \"Todas\" ou Especialidade/Procedimento para \"Todas\"/\"Todos\"."
      ))

      serie_proc <- dados()$cirurgia_procedimento_rol
      mapa <- dados()$mapa_especialidade_rol
      validate(need(
        !is.null(serie_proc) && !is.null(mapa),
        "Série por procedimento não encontrada. Rode novamente o script 01 do projeto Cirurgia."
      ))

      dados_uf <- filtrar_procedimento_rol(
        serie_proc, mapa,
        uf_sel = input$uf_cirurgia,
        regioes = input$regiao_cirurgia,
        especialidade_sel = especialidade_sel,
        procedimentos_sel = procedimentos_sel
      )

    } else {

      validate(need(
        !(gestao_ativa && input$gestao_cirurgia == "DUPLA" && input$metrica_cirurgia_anos == "financeiro"),
        "Não há valor financeiro para gestão dupla — selecione \"Físico\"."
      ))

      serie <- serie_anos_cirurgia_indicador()
      validate(need(!is.null(serie), "Série multianual não encontrada. Rode o script 02_monitoramento_diagrama_controle.R no projeto Cirurgia."))

      municipio_sel <- input$municipio_cirurgia
      usar_municipio <- isTRUE(input$uf_cirurgia != "BRASIL") && !is.null(municipio_sel) && municipio_sel != "Todos"

      if (gestao_ativa) {
        serie_gest <- dados()[[paste0("cirurgia_gestao_", indicador_sel)]]
        validate(need(!is.null(serie_gest), "Série por gestão não encontrada. Rode o script 10_series_gestao_comparacao_anos.R no projeto Cirurgia."))

        sg <- serie_gest[gestao == input$gestao_cirurgia]
        if (usar_municipio) {
          sg <- sg[uf_atendimento == input$uf_cirurgia & municipio_atendimento == municipio_sel]
        } else if (input$uf_cirurgia != "BRASIL") {
          sg <- sg[uf_atendimento == input$uf_cirurgia]
        } else if (!setequal(input$regiao_cirurgia, REGIOES)) {
          sg <- sg[uf_atendimento %chin% UF_REF[REGIAO %in% input$regiao_cirurgia]$NM_UF_CIRURGIA]
        }

        # Meses sem produção não têm linha no arquivo por gestão: completa com
        # zero usando os mesmos ano/mês da série sem gestão (evita "buracos").
        grade <- unique(serie[uf_atendimento == "BRASIL", .(ano, mes)])
        dados_uf <- sg[, .(quantidade = sum(quantidade), valor = sum(valor)), by = .(ano, mes)]
        dados_uf <- dados_uf[grade, on = .(ano, mes)]
        dados_uf[is.na(quantidade), quantidade := 0]
        dados_uf[is.na(valor), valor := 0]
        setorder(dados_uf, ano, mes)
      } else if (usar_municipio) {
        serie_mun <- serie_municipio_cirurgia_indicador()
        validate(need(!is.null(serie_mun), "Série por município não encontrada. Rode novamente os scripts 01 e 02 do projeto Cirurgia."))
        dados_uf <- serie_mun[
          uf_atendimento == input$uf_cirurgia & municipio_atendimento == municipio_sel,
          .(ano, mes, quantidade, valor)
        ]
      } else {
        todas_regioes <- setequal(input$regiao_cirurgia, REGIOES)
        dados_uf <- if (input$uf_cirurgia == "BRASIL") {
          if (todas_regioes) {
            serie[uf_atendimento == "BRASIL", .(ano, mes, quantidade, valor)]
          } else {
            agregar_anos_regiao(serie, input$regiao_cirurgia)
          }
        } else {
          serie[uf_atendimento == input$uf_cirurgia, .(ano, mes, quantidade, valor)]
        }
      }
    }

    validate(need(nrow(dados_uf) > 0, "Sem dados para a seleção atual."))
    dados_uf
  })

  rotulo_local_cirurgia <- reactive({
    if (isTRUE(input$municipio_cirurgia != "Todos")) {
      paste0(input$municipio_cirurgia, " (", input$uf_cirurgia, ")")
    } else if (input$uf_cirurgia == "BRASIL") {
      rotulo_agregado(input$regiao_cirurgia)
    } else {
      input$uf_cirurgia
    }
  })

  rotulo_gestao_cirurgia <- reactive({
    if (isTRUE(!is.null(input$gestao_cirurgia) && input$gestao_cirurgia != "Todas")) {
      paste0(" — Gestão ", tolower(input$gestao_cirurgia))
    } else {
      ""
    }
  })

  # Sufixo do título com Especialidade/Procedimento, quando um filtro
  # estiver ativo (só se aplica com ROL selecionado — ver dados_comparacao_anos()).
  rotulo_procedimento_cirurgia <- reactive({

    especialidade_sel <- input$especialidade_cirurgia
    procedimentos_sel <- input$procedimento_cirurgia

    if (isTRUE(input$indicador_cirurgia != "rol") || is.null(especialidade_sel) || especialidade_sel == "Todas") {
      return("")
    }

    if (length(procedimentos_sel) > 0) {
      mapa <- dados()$mapa_especialidade_rol
      nomes <- if (!is.null(mapa)) mapa[codigo_procedimento_principal %chin% procedimentos_sel]$nome_procedimento else procedimentos_sel

      sufixo <- if (length(nomes) <= 2) {
        paste(nomes, collapse = "; ")
      } else {
        paste0(length(nomes), " procedimentos selecionados")
      }

      paste0(" — ", especialidade_sel, " — ", sufixo)
    } else {
      paste0(" — ", especialidade_sel)
    }
  })

  plot_comparacao_anos <- reactive({

    dados_uf <- dados_comparacao_anos()
    req(input$metrica_cirurgia_anos)

    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[indicador_comparacao_anos()]]

    grafico_comparacao_anos(
      dados_uf,
      titulo = paste0(
        rotulo_indicador, " — Comparação entre anos — ", rotulo_local_cirurgia(),
        rotulo_procedimento_cirurgia(), rotulo_gestao_cirurgia()
      ),
      metrica = input$metrica_cirurgia_anos
    )
  })

  output$grafico_comparacao_anos <- renderPlotly({
    ggplotly(plot_comparacao_anos(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.2), margin = list(b = 90)) |>
      alta_resolucao("comparacao_anos_cirurgias")
  })


  ## ---- Cirurgias: cabeçalho institucional (subtítulo, "limpar filtros" e KPIs) ----

  output$subtitulo_cirurgia <- renderText({
    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[indicador_comparacao_anos()]]
    rotulo_metrica <- if (isTRUE(input$metrica_cirurgia_anos == "financeiro")) "Produção financeira" else "Produção física"
    paste0(rotulo_indicador, " | ", rotulo_metrica)
  })

  observeEvent(input$limpar_cirurgia, {
    updateSelectInput(session, "indicador_cirurgia", selected = "rol")
    updatePickerInput(session, "regiao_cirurgia", selected = REGIOES)
    updateSelectInput(session, "uf_cirurgia", selected = "BRASIL")
    updateSelectInput(session, "municipio_cirurgia", selected = "Todos")
    updateSelectInput(session, "gestao_cirurgia", selected = "Todas")
    updateSelectInput(session, "especialidade_cirurgia", selected = "Todas")
    updateSelectizeInput(session, "procedimento_cirurgia", selected = character(0))
    updateRadioButtons(session, "metrica_cirurgia_anos", selected = "fisico")
    updateRadioButtons(session, "metrica_cirurgia_diagrama", selected = "fisico")
    updateRadioButtons(session, "indicador_ranking_cirurgia", selected = "totais")
    updateSelectInput(session, "mes_ranking_cirurgia", selected = "0")
  })

  # KPIs sempre a partir da mesma base já usada pelo gráfico "Comparação
  # Anos" (dados_comparacao_anos()) — segue o toggle Físico/Financeiro dali,
  # sem recalcular filtro nenhum de novo.
  # Card 1 e 2 são totais fixos do ano mais antigo e do penúltimo ano
  # disponíveis (hoje: 2022 e 2025); card 3 é o ano corrente até a última
  # competência (com a mesma variação vs. período do ano anterior de antes);
  # card 4 é a taxa de expansão entre o ano-base e o ano corrente. Usa
  # min(anos)/max(anos) em vez de anos fixos para não quebrar quando a base
  # ganhar mais um ano.
  kpi_cirurgia <- reactive({

    d <- dados_comparacao_anos()
    req(nrow(d) > 0)

    coluna_y <- if (isTRUE(input$metrica_cirurgia_anos == "financeiro")) "valor" else "quantidade"
    fmt_valor <- if (isTRUE(input$metrica_cirurgia_anos == "financeiro")) label_pt_moeda else label_pt_num
    d[, y_plot := get(coluna_y)]

    anos <- sort(unique(d$ano))
    ano_base <- min(anos)
    ano_atual <- max(anos)
    ano_anterior <- ano_atual - 1

    total_ano_base <- sum(d[ano == ano_base]$y_plot, na.rm = TRUE)
    total_ano_anterior <- if (ano_anterior %in% anos) sum(d[ano == ano_anterior]$y_plot, na.rm = TRUE) else NA_real_

    d_atual <- d[ano == ano_atual]
    total_ano_atual <- sum(d_atual$y_plot, na.rm = TRUE)
    meses_disponiveis <- sort(unique(d_atual$mes))

    d_periodo_anterior <- d[ano == ano_anterior & mes %in% meses_disponiveis]
    total_periodo_anterior <- sum(d_periodo_anterior$y_plot, na.rm = TRUE)
    variacao_periodo <- if (total_periodo_anterior > 0) (total_ano_atual / total_periodo_anterior - 1) * 100 else NA_real_

    # Taxa anual: ano corrente (parcial) sobre o ano-base INTEIRO — a leitura
    # "cheia" da expansão, mesmo comparando um ano completo com um parcial.
    tx_expansao <- if (isTRUE(total_ano_base > 0) && ano_atual != ano_base) {
      (total_ano_atual / total_ano_base - 1) * 100
    } else {
      NA_real_
    }

    # Taxa por período: mesmo recorte de meses em ambas as pontas (ex.:
    # Jan–Jun/2026 vs. Jan–Jun/2022) — compara maçã com maçã, sem o viés de
    # comparar ano corrente parcial com ano-base inteiro.
    d_periodo_base <- d[ano == ano_base & mes %in% meses_disponiveis]
    total_periodo_base <- sum(d_periodo_base$y_plot, na.rm = TRUE)
    tx_expansao_periodo <- if (isTRUE(total_periodo_base > 0) && ano_atual != ano_base) {
      (total_ano_atual / total_periodo_base - 1) * 100
    } else {
      NA_real_
    }

    list(
      fmt = fmt_valor,
      ano_base = ano_base, ano_anterior = ano_anterior, ano_atual = ano_atual,
      meses_disponiveis = meses_disponiveis,
      total_ano_base = total_ano_base, total_ano_anterior = total_ano_anterior, total_ano_atual = total_ano_atual,
      variacao_periodo = variacao_periodo, tx_expansao = tx_expansao, tx_expansao_periodo = tx_expansao_periodo
    )
  })

  output$kpi_cirurgia_rodape <- renderText({
    k <- kpi_cirurgia()
    paste0("*", k$ano_atual, ": dados disponíveis até a última competência.")
  })

  output$kpi_cirurgia_titulo1 <- renderText({
    k <- kpi_cirurgia()
    paste("Produção", k$ano_base)
  })
  output$kpi_cirurgia_valor1 <- renderText({
    k <- kpi_cirurgia()
    k$fmt(k$total_ano_base)
  })

  output$kpi_cirurgia_titulo2 <- renderText({
    k <- kpi_cirurgia()
    paste("Produção", k$ano_anterior)
  })
  output$kpi_cirurgia_valor2 <- renderText({
    k <- kpi_cirurgia()
    if (is.na(k$total_ano_anterior)) "—" else k$fmt(k$total_ano_anterior)
  })

  output$kpi_cirurgia_titulo3 <- renderText({
    k <- kpi_cirurgia()
    paste0("Produção ", k$ano_atual, "*")
  })
  output$kpi_cirurgia_valor3 <- renderText({
    k <- kpi_cirurgia()
    k$fmt(k$total_ano_atual)
  })
  output$kpi_cirurgia_linha3 <- renderUI({
    k <- kpi_cirurgia()
    if (is.na(k$variacao_periodo)) return(NULL)
    seta <- if (k$variacao_periodo >= 0) "↑" else "↓"
    sinal <- if (k$variacao_periodo >= 0) "+" else ""
    classe <- if (k$variacao_periodo >= 0) "positiva" else "negativa"
    rotulo_meses <- paste0(MES_LABELS[min(k$meses_disponiveis)], "–", MES_LABELS[max(k$meses_disponiveis)])
    kpi_linha(
      c(
        paste0(seta, " ", sinal, label_pt_num(k$variacao_periodo), "% em relação a ", k$ano_anterior),
        paste0("(", rotulo_meses, "/", k$ano_atual, " vs. ", rotulo_meses, "/", k$ano_anterior, ")")
      ),
      classe = classe
    )
  })

  output$kpi_cirurgia_titulo4 <- renderText({
    "Taxa de Expansão (PPA/PNS)"
  })
  output$kpi_cirurgia_valor4 <- renderText({
    k <- kpi_cirurgia()
    if (is.na(k$tx_expansao)) return("—")
    sinal <- if (k$tx_expansao >= 0) "+" else ""
    paste0(sinal, label_pt_num(k$tx_expansao), "%")
  })
  # Estrutura do card: valor principal (anual) já vem do kpi_valor4; aqui só
  # a legenda dele ("Anual · base → atual*"), depois — se der pra calcular —
  # uma divisória e a taxa por período em destaque (verde/vermelho), que é o
  # número que efetivamente explica o porquê do anual poder vir negativo
  # mesmo com produção crescendo mês a mês.
  output$kpi_cirurgia_linha4 <- renderUI({
    k <- kpi_cirurgia()

    partes <- list(
      div(class = "kpi-nota", paste0("Anual · ", k$ano_base, " → ", k$ano_atual, "*"))
    )

    if (!is.na(k$tx_expansao_periodo)) {
      sinal <- if (k$tx_expansao_periodo >= 0) "+" else ""
      classe <- if (k$tx_expansao_periodo >= 0) "positiva" else "negativa"
      rotulo_meses <- paste0(MES_LABELS[min(k$meses_disponiveis)], "–", MES_LABELS[max(k$meses_disponiveis)])
      partes <- c(partes, list(
        hr(class = "kpi-divisor"),
        div(
          class = paste("kpi-destaque-secundario", classe),
          paste0(sinal, label_pt_num(k$tx_expansao_periodo), "% no período equivalente")
        ),
        div(class = "kpi-nota", paste0(rotulo_meses, "/", k$ano_base, " → ", rotulo_meses, "/", k$ano_atual))
      ))
    }

    tagList(partes)
  })

  ## ---- Semáforo Cirúrgico (mapas de variação, do projeto Analise_Espacial) ----

  observe({
    req(SEMAFORO_MAPA_REGIAO)
    ufs <- SEMAFORO_MAPA_REGIAO %>% st_drop_geometry() %>% distinct(UF) %>% pull(UF) %>% sort()
    updateSelectInput(session, "semaforo_uf_regiao", choices = c("Todas" = "", ufs), selected = "")
  })

  observe({
    req(SEMAFORO_MAPA_MUNICIPIO)
    ufs <- SEMAFORO_MAPA_MUNICIPIO %>% st_drop_geometry() %>% distinct(UF) %>% pull(UF) %>% sort()
    updateSelectInput(session, "semaforo_uf_municipio", choices = c("Todas" = "", ufs), selected = "")
  })

  observe({
    req(SEMAFORO_MAPA_MUNICIPIO)
    uf_selecionada <- input$semaforo_uf_municipio

    base <- SEMAFORO_MAPA_MUNICIPIO %>% st_drop_geometry()
    if (isTRUE(uf_selecionada != "")) {
      base <- base %>% filter(UF == uf_selecionada)
    }

    municipios <- base %>% distinct(municipio) %>% pull(municipio) %>% sort()
    updateSelectInput(session, "semaforo_municipio_selecionado", choices = municipios, selected = NULL)
  })

  semaforo_dados_regiao_filtrados <- reactive({
    req(SEMAFORO_MAPA_REGIAO)
    dados_reg <- SEMAFORO_MAPA_REGIAO
    if (!is.null(input$semaforo_uf_regiao) && input$semaforo_uf_regiao != "") {
      dados_reg <- dados_reg %>% filter(UF == input$semaforo_uf_regiao)
    }
    dados_reg
  })

  semaforo_dados_municipio_filtrados <- reactive({
    req(SEMAFORO_MAPA_MUNICIPIO)
    dados_mun <- SEMAFORO_MAPA_MUNICIPIO
    if (!is.null(input$semaforo_uf_municipio) && input$semaforo_uf_municipio != "") {
      dados_mun <- dados_mun %>% filter(UF == input$semaforo_uf_municipio)
    }
    if (!is.null(input$semaforo_municipio_selecionado) && length(input$semaforo_municipio_selecionado) > 0) {
      dados_mun <- dados_mun %>% filter(municipio %in% input$semaforo_municipio_selecionado)
    }
    dados_mun
  })

  criar_mapa_semaforo_br <- function() {
    ggplot(SEMAFORO_MAPA_BR, aes(fill = categoria, geometry = geom)) +
      geom_sf(color = "white", size = 0.3) +
      geom_sf_text(aes(label = paste0(UF, "\n", round(dif_porcentagem, 1), "%")),
                   size = 3, color = "black", fontface = "bold") +
      scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
      theme_minimal() +
      theme(
        legend.position = "right", legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10), panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }

  criar_mapa_semaforo_regiao_tela <- function() {
    dados_reg <- semaforo_dados_regiao_filtrados()
    if (nrow(dados_reg) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Nenhum dado disponível para a UF selecionada", size = 6, color = "red") + theme_void())
    }
    ggplot(dados_reg, aes(fill = categoria, geometry = geom)) +
      geom_sf(color = "white", size = 0.3) +
      geom_sf_text(aes(label = paste0(round(dif_porcentagem, 1), "%")),
                   size = 2.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
      theme_minimal() +
      theme(
        legend.position = "right", legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10), panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }

  criar_mapa_semaforo_regiao_download <- function() {
    dados_reg <- semaforo_dados_regiao_filtrados()
    if (nrow(dados_reg) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Nenhum dado disponível para a UF selecionada", size = 6, color = "red") + theme_void())
    }
    ggplot(dados_reg, aes(fill = categoria, geometry = geom)) +
      geom_sf(color = "white", size = 0.3) +
      geom_sf_text(aes(label = paste0(regiao_saude, "\n", round(dif_porcentagem, 1), "%")),
                   size = 2.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
      theme_minimal() +
      theme(
        legend.position = "right", legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10), panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }

  criar_mapa_semaforo_municipio_tela <- function() {
    dados_mun <- semaforo_dados_municipio_filtrados()
    if (nrow(dados_mun) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Nenhum dado disponível para os filtros selecionados", size = 6, color = "red") + theme_void())
    }
    if (nrow(dados_mun) > 20) {
      ggplot(dados_mun, aes(fill = categoria, geometry = geom)) +
        geom_sf(color = "white", size = 0.1) +
        scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
        theme_minimal() +
        theme(
          legend.position = "right", legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 10), panel.grid = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )
    } else {
      ggplot(dados_mun, aes(fill = categoria, geometry = geom)) +
        geom_sf(color = "white", size = 0.2) +
        geom_sf_text(aes(label = paste0(round(dif_porcentagem, 1), "%")),
                     size = 2, color = "black", fontface = "bold") +
        scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
        theme_minimal() +
        theme(
          legend.position = "right", legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 10), panel.grid = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )
    }
  }

  criar_mapa_semaforo_municipio_download <- function() {
    dados_mun <- semaforo_dados_municipio_filtrados()
    if (nrow(dados_mun) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Nenhum dado disponível para os filtros selecionados", size = 6, color = "red") + theme_void())
    }
    ggplot(dados_mun, aes(fill = categoria, geometry = geom)) +
      geom_sf(color = "white", size = 0.2) +
      geom_sf_text(aes(label = paste0(round(dif_porcentagem, 1), "%")),
                   size = 2.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
      theme_minimal() +
      theme(
        legend.position = "right", legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10), panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }

  criar_mapa_semaforo_municipio_download_nome <- function() {
    dados_mun <- semaforo_dados_municipio_filtrados()
    if (nrow(dados_mun) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "Nenhum dado disponível para os filtros selecionados", size = 6, color = "red") + theme_void())
    }
    ggplot(dados_mun, aes(fill = categoria, geometry = geom)) +
      geom_sf(color = "white", size = 0.2) +
      geom_sf_text(aes(label = paste0(municipio, "\n", round(dif_porcentagem, 1), "%")),
                   size = 2.5, color = "black", fontface = "bold") +
      scale_fill_manual(values = SEMAFORO_CORES_CATEGORIA, name = "Variação") +
      theme_minimal() +
      theme(
        legend.position = "right", legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10), panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
  }

  output$semaforo_map_br <- renderPlot({
    validate(need(!is.null(SEMAFORO_MAPA_BR), "Mapa do Brasil não encontrado em dados/processados/semaforo."))
    criar_mapa_semaforo_br()
  })

  output$semaforo_map_regiao <- renderPlot({
    validate(need(!is.null(SEMAFORO_MAPA_REGIAO), "Mapa de regiões de saúde não encontrado em dados/processados/semaforo."))
    criar_mapa_semaforo_regiao_tela()
  })

  output$semaforo_map_municipio <- renderPlot({
    validate(need(!is.null(SEMAFORO_MAPA_MUNICIPIO), "Mapa de municípios não encontrado em dados/processados/semaforo."))
    criar_mapa_semaforo_municipio_tela()
  })

  output$semaforo_download_ppt_br <- downloadHandler(
    filename = function() paste0("mapa_br_", Sys.Date(), ".pptx"),
    content = function(file) {
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(doc, value = rvg::dml(code = print(criar_mapa_semaforo_br()), width = 10, height = 7.5), location = ph_location_fullsize())
      print(doc, target = file)
    }
  )

  output$semaforo_download_tabela_br <- downloadHandler(
    filename = function() paste0("tabela_br_", Sys.Date(), ".xlsx"),
    content = function(file) {
      SEMAFORO_MAPA_BR %>%
        st_drop_geometry() %>%
        select(UF, X2025, X2026, Variação....) %>%
        rename("UF" = UF, "2025" = X2025, "2026" = X2026, "Variação (%)" = Variação....) %>%
        writexl::write_xlsx(file)
    }
  )

  output$semaforo_download_ppt_regiao <- downloadHandler(
    filename = function() {
      uf <- ifelse(is.null(input$semaforo_uf_regiao) || input$semaforo_uf_regiao == "", "BR", input$semaforo_uf_regiao)
      paste0("mapa_regiao_", uf, "_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(doc, value = rvg::dml(code = print(criar_mapa_semaforo_regiao_download()), width = 10, height = 7.5), location = ph_location_fullsize())
      print(doc, target = file)
    }
  )

  output$semaforo_download_tabela_regiao <- downloadHandler(
    filename = function() {
      uf <- ifelse(is.null(input$semaforo_uf_regiao) || input$semaforo_uf_regiao == "", "BR", input$semaforo_uf_regiao)
      paste0("tabela_regiao_", uf, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      semaforo_dados_regiao_filtrados() %>%
        st_drop_geometry() %>%
        select(regiao_saude, X2025, X2026, Variação....) %>%
        rename("Região" = regiao_saude, "2025" = X2025, "2026" = X2026, "Variação (%)" = Variação....) %>%
        writexl::write_xlsx(file)
    }
  )

  output$semaforo_download_ppt_municipio <- downloadHandler(
    filename = function() {
      uf <- ifelse(is.null(input$semaforo_uf_municipio) || input$semaforo_uf_municipio == "", "BR", input$semaforo_uf_municipio)
      paste0("mapa_municipio_", uf, "_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(doc, value = rvg::dml(code = print(criar_mapa_semaforo_municipio_download()), width = 10, height = 7.5), location = ph_location_fullsize())
      print(doc, target = file)
    }
  )

  output$semaforo_download_ppt_municipio_nome <- downloadHandler(
    filename = function() {
      uf <- ifelse(is.null(input$semaforo_uf_municipio) || input$semaforo_uf_municipio == "", "BR", input$semaforo_uf_municipio)
      paste0("mapa_municipio_nome_", uf, "_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
      doc <- ph_with(doc, value = rvg::dml(code = print(criar_mapa_semaforo_municipio_download_nome()), width = 10, height = 7.5), location = ph_location_fullsize())
      print(doc, target = file)
    }
  )

  output$semaforo_download_tabela_municipio <- downloadHandler(
    filename = function() {
      uf <- ifelse(is.null(input$semaforo_uf_municipio) || input$semaforo_uf_municipio == "", "BR", input$semaforo_uf_municipio)
      paste0("tabela_municipio_", uf, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      semaforo_dados_municipio_filtrados() %>%
        st_drop_geometry() %>%
        select(municipio, X2025, X2026, Variação....) %>%
        rename("Município" = municipio, "2025" = X2025, "2026" = X2026, "Variação (%)" = Variação....) %>%
        writexl::write_xlsx(file)
    }
  )

  # Os .gpkg do semáforo não guardam o período comparado (jan-mai, jan-jul...).
  # Descobre pelo total: o mês k em que o acumulado de 2025 da série de
  # Cirurgias (MAC+FAEC, Brasil) fica mais próximo do total 2025 do mapa. Se
  # não houver correspondência (diferença > 1%), o subtítulo sai sem período.
  # Mês final do período comparado no mapa (NA se não der pra deduzir).
  semaforo_mes_ref <- reactive({
    serie <- dados()$cirurgia_anos_total
    if (is.null(SEMAFORO_MAPA_BR) || is.null(serie)) {
      return(NA_integer_)
    }
    total_2025_mapa <- sum(st_drop_geometry(SEMAFORO_MAPA_BR)[["X2025"]], na.rm = TRUE)
    acumulado <- serie[uf_atendimento == "BRASIL" & ano == 2025][order(mes)]
    acumulado[, acum := cumsum(quantidade)]
    k <- acumulado[which.min(abs(acum - total_2025_mapa))]
    if (nrow(k) == 0L || abs(k$acum - total_2025_mapa) / total_2025_mapa > 0.01 || k$mes < 2) {
      return(NA_integer_)
    }
    as.integer(k$mes)
  })

  output$subtitulo_semaforo <- renderText({
    base <- "Variação percentual de procedimentos — 2025 vs. 2026, por UF"
    mes <- semaforo_mes_ref()
    if (is.na(mes)) {
      return(base)
    }
    paste0(base, " (jan–", tolower(MES_LABELS[mes]), "/2026 vs. jan–", tolower(MES_LABELS[mes]), "/2025)")
  })

  # Card do cabeçalho: última competência do período comparado e data em que
  # os mapas foram gerados.
  output$data_atualizacao_semaforo <- renderText({
    mes <- semaforo_mes_ref()
    if (is.na(mes)) "" else sprintf("%02d/2026", mes)
  })
  output$data_processamento_semaforo <- renderText({
    if (is.na(SEMAFORO_DATA_GERACAO)) {
      return("")
    }
    paste0(
      "Atualizado em ", MESES_PT[as.integer(format(SEMAFORO_DATA_GERACAO, "%m"))],
      " de ", format(SEMAFORO_DATA_GERACAO, "%Y")
    )
  })

  # KPIs desta aba são de natureza diferente das outras (não é série
  # temporal, é um mapa) — contagem de UFs em alta/queda e os extremos,
  # direto do mapa Brasil (SEMAFORO_MAPA_BR), carregado uma vez só.
  kpi_semaforo <- reactive({
    req(!is.null(SEMAFORO_MAPA_BR))
    d <- SEMAFORO_MAPA_BR %>% st_drop_geometry()
    list(
      n_alta = sum(d$dif_porcentagem > 0, na.rm = TRUE),
      n_queda = sum(d$dif_porcentagem < 0, na.rm = TRUE),
      maior_alta = d[which.max(d$dif_porcentagem), ],
      maior_queda = d[which.min(d$dif_porcentagem), ]
    )
  })

  output$kpi_semaforo_alta <- renderText({
    k <- kpi_semaforo()
    paste(k$n_alta, "UFs")
  })
  output$kpi_semaforo_alta_linha <- renderUI({
    kpi_linha("em alta na variação 2025 → 2026", classe = "positiva")
  })

  output$kpi_semaforo_queda <- renderText({
    k <- kpi_semaforo()
    paste(k$n_queda, "UFs")
  })
  output$kpi_semaforo_queda_linha <- renderUI({
    kpi_linha("em queda na variação 2025 → 2026", classe = "negativa")
  })

  output$kpi_semaforo_maior_alta <- renderText({
    k <- kpi_semaforo()
    paste0(k$maior_alta$UF, " +", label_pt_num(k$maior_alta$dif_porcentagem), "%")
  })
  output$kpi_semaforo_maior_alta_linha <- renderUI({
    kpi_linha("maior alta entre as UFs")
  })

  output$kpi_semaforo_maior_queda <- renderText({
    k <- kpi_semaforo()
    paste0(k$maior_queda$UF, " ", label_pt_num(k$maior_queda$dif_porcentagem), "%")
  })
  output$kpi_semaforo_maior_queda_linha <- renderUI({
    kpi_linha("maior queda entre as UFs")
  })

  ## ---- OCI ----

  observeEvent(list(input$regiao_oci, dados()$oci_serie), {

    ufs_regiao <- sort(UF_REF[REGIAO %in% input$regiao_oci]$NM_UF_OCI)
    escolhas <- setNames(
      c("BRASIL", ufs_regiao),
      c(rotulo_agregado(input$regiao_oci), ufs_regiao)
    )

    selecionado <- if (isTRUE(input$uf_oci %in% escolhas)) input$uf_oci else "BRASIL"

    updateSelectInput(session, "uf_oci", choices = escolhas, selected = selecionado)
  })

  # Cascata UF -> Município (mesma lógica da aba Cirurgias): só populado
  # quando uma UF específica está selecionada.
  observeEvent(list(input$uf_oci, dados()$oci_especialidade_componente_municipio), {

    base_mun <- dados()$oci_especialidade_componente_municipio

    if (is.null(input$uf_oci) || input$uf_oci == "BRASIL" || is.null(base_mun)) {
      updateSelectInput(session, "municipio_oci", choices = c("Selecione uma UF" = "Todos"), selected = "Todos")
      return()
    }

    municipios <- sort(unique(base_mun[NM_UF == input$uf_oci]$MUNICIPIO))
    escolhas <- setNames(c("Todos", municipios), c("Todos (UF inteira)", municipios))

    selecionado <- if (isTRUE(input$municipio_oci %in% escolhas)) input$municipio_oci else "Todos"

    updateSelectInput(session, "municipio_oci", choices = escolhas, selected = selecionado)
  })

  # Registros de OCI sem UF de atendimento (ver script do projeto OCI) não
  # pertencem a nenhuma região: entram só no total Brasil, com as 5 regiões
  # marcadas — assim o total bate com o painel oficial do SUS360.
  oci_todas_regioes <- reactive({
    setequal(input$regiao_oci, REGIOES)
  })

  municipio_oci_ativo <- reactive({
    isTRUE(input$uf_oci != "BRASIL") && !is.null(input$municipio_oci) && input$municipio_oci != "Todos"
  })

  rotulo_local_oci <- reactive({
    if (municipio_oci_ativo()) {
      paste0(input$municipio_oci, " (", input$uf_oci, ")")
    } else if (input$uf_oci == "BRASIL") {
      rotulo_agregado(input$regiao_oci)
    } else {
      input$uf_oci
    }
  })

  # Série geral (mesma agregação usada pelo diagrama de monitoramento),
  # reaproveitada pelo gráfico por componente (linha "Total geral") e pelo
  # comparativo mensal 2025 x 2026. Quando um município está selecionado,
  # deriva o "geral" somando especialidades e componentes conhecidos na
  # base de município (não há export separado de OCI geral por município).
  oci_geral_filtrada <- reactive({

    if (municipio_oci_ativo()) {
      base_mun <- dados()$oci_especialidade_componente_municipio
      validate(need(!is.null(base_mun), "Tabela de OCI por especialidade, componente e município não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
      return(
        base_mun[
          NM_UF == input$uf_oci & MUNICIPIO == input$municipio_oci,
          .(oci = sum(OCI, na.rm = TRUE)),
          by = competencia
        ]
      )
    }

    serie <- dados()$oci_serie
    validate(need(!is.null(serie), "Planilha de OCI por UF/mês não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
    req(input$uf_oci)
    validate(need(length(input$regiao_oci) > 0, "Selecione ao menos uma região."))

    if (input$uf_oci == "BRASIL") {
      serie[
        NM_UF %chin% c(UF_REF[REGIAO %in% input$regiao_oci]$NM_UF_OCI, if (oci_todas_regioes()) OCI_UF_NAO_INFORMADA),
        .(oci = sum(oci, na.rm = TRUE)),
        by = competencia
      ]
    } else {
      serie[NM_UF == input$uf_oci, .(competencia, oci)]
    }
  })

  # Mesma cascata de UF/região/município usada pelas demais bases de OCI,
  # a partir do export com quebra por componente/modalidade (Componente
  # Ambulatorial, Carretas, Créditos Financeiros, Equipes Volantes). Usada
  # pelo gráfico "Por componente" em "Série histórica OCI" e pela subaba
  # "Série histórica OCI por especialidade".
  oci_especialidade_componente_filtrada <- reactive({

    if (municipio_oci_ativo()) {
      base_mun <- dados()$oci_especialidade_componente_municipio
      validate(need(!is.null(base_mun), "Tabela de OCI por especialidade, componente e município não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
      return(base_mun[NM_UF == input$uf_oci & MUNICIPIO == input$municipio_oci])
    }

    base <- dados()$oci_especialidade_componente_uf
    validate(need(!is.null(base), "Tabela de OCI por especialidade e componente não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
    req(input$uf_oci)
    validate(need(length(input$regiao_oci) > 0, "Selecione ao menos uma região."))

    if (input$uf_oci == "BRASIL") {
      base[REGIAO %in% c(input$regiao_oci, if (oci_todas_regioes()) "Não informada")]
    } else {
      base[NM_UF == input$uf_oci]
    }
  })

  # "Série histórica OCI" — "OCI geral": onda de área com o Total geral de
  # OCI mais uma linha por componente/modalidade, somando todas as
  # especialidades.
  dados_oci_componente_agregada <- reactive({

    agregada <- oci_especialidade_componente_filtrada()[
      ,
      .(OCI = sum(OCI, na.rm = TRUE)),
      by = .(ANO, MES, COMPONENTE)
    ]
    agregada[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
    agregada[]
  })

  plot_oci_componente <- reactive({

    dados_geral <- oci_geral_filtrada()
    validate(need(nrow(dados_geral) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente(
      dados_geral, dados_oci_componente_agregada(),
      titulo = paste0("OCI realizadas — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_componente <- renderPlotly({
    ggplotly(plot_oci_componente(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3), margin = list(b = 110)) |>
      alta_resolucao("oci_geral_por_componente")
  })

  plot_oci_componente_ano <- reactive({

    dados_grafico <- dados_oci_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente_ano(
      dados_grafico, titulo = paste0("Por ano — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_componente_ano <- renderPlotly({
    ggplotly(plot_oci_componente_ano(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.6), margin = list(b = 120)) |>
      alta_resolucao("oci_componente_ano")
  })

  # Tabela com os valores do gráfico: uma linha por ano, uma coluna por
  # componente e o total.
  output$tabela_oci_componente_ano <- renderDT({

    d <- dados_oci_componente_agregada()[
      COMPONENTE %in% ORDEM_COMPONENTES_OCI,
      .(OCI = sum(OCI, na.rm = TRUE)), by = .(ANO, COMPONENTE)
    ]
    validate(need(nrow(d) > 0, ""))

    larga <- dcast(d, ANO ~ as.character(COMPONENTE), value.var = "OCI", fill = 0)
    colunas <- intersect(ORDEM_COMPONENTES_OCI, names(larga))
    larga <- larga[order(-ANO), c("ANO", colunas), with = FALSE]
    larga[, Total := rowSums(.SD), .SDcols = colunas]
    setnames(larga, "ANO", "Ano")
    larga[, Ano := as.character(Ano)]

    datatable(
      larga,
      rownames = FALSE,
      selection = "none",
      fillContainer = FALSE,
      height = "auto",
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    ) |>
      formatRound(c(colunas, "Total"), digits = 0, mark = ".", interval = 3) |>
      formatStyle("Total", fontWeight = "bold")
  })

  plot_oci_componente_mes <- reactive({

    dados_grafico <- dados_oci_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente_mes(
      dados_grafico, titulo = paste0("Por mês de atendimento — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_componente_mes <- renderPlotly({
    ggplotly(plot_oci_componente_mes(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.35), margin = list(b = 110)) |>
      alta_resolucao("oci_componente_mes")
  })

  # Subaba "Série histórica OCI por especialidade": aplica o filtro de
  # componente (todos, ou um só) antes de somar por especialidade.
  oci_especialidade_sub_base <- reactive({

    base <- oci_especialidade_componente_filtrada()

    if (isTRUE(input$componente_oci_sub != "geral")) {
      base <- base[COMPONENTE == input$componente_oci_sub]
    }

    base
  })

  # Linha "Geral" do gráfico da subaba: soma de todas as especialidades já
  # dentro do componente selecionado (bate com a soma das linhas abaixo).
  dados_oci_especialidade_sub_geral <- reactive({

    oci_especialidade_sub_base()[, .(oci = sum(OCI, na.rm = TRUE)), by = competencia]
  })

  dados_oci_especialidade_sub_agregada <- reactive({

    especialidades_sel <- input$especialidades_oci_sub
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))

    agregada <- oci_especialidade_sub_base()[
      ESPECIALIDADE %chin% especialidades_sel,
      .(OCI = sum(OCI, na.rm = TRUE)),
      by = .(ANO, MES, ESPECIALIDADE)
    ]
    agregada[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
    agregada[]
  })

  rotulo_componente_oci_sub <- reactive({
    if (isTRUE(input$componente_oci_sub == "geral")) "todos os componentes" else input$componente_oci_sub
  })

  plot_oci_especialidade <- reactive({

    dados_geral <- dados_oci_especialidade_sub_geral()
    validate(need(nrow(dados_geral) > 0, "Sem dados para a seleção atual."))

    grafico_oci_especialidade(
      dados_geral, dados_oci_especialidade_sub_agregada(),
      titulo = paste0(
        "OCI por especialidade — ", rotulo_componente_oci_sub(), " — ", rotulo_local_oci()
      )
    )
  })

  output$grafico_oci_especialidade <- renderPlotly({
    ggplotly(plot_oci_especialidade(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3), margin = list(b = 110)) |>
      alta_resolucao("oci_por_especialidade")
  })

  # "OCI por especialidade e componente": respeita o filtro de Especialidade
  # da sub-aba e tem Ano e Componente próprios (só deste gráfico — este
  # último resolve o problema de clicar na legenda do Plotly para esconder
  # um componente e sobrar um espaço vazio enorme no eixo Y: aqui o
  # componente já sai dos dados, então o gráfico reescala sozinho).
  dados_oci_especialidade_componente_agregada <- reactive({

    especialidades_sel <- input$especialidades_oci_sub
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))

    anos_sel <- input$anos_oci_especialidade_componente
    validate(need(length(anos_sel) > 0, "Selecione ao menos um ano."))

    componentes_sel <- input$componentes_oci_especialidade_componente
    validate(need(length(componentes_sel) > 0, "Selecione ao menos um componente."))

    oci_especialidade_componente_filtrada()[
      ESPECIALIDADE %chin% especialidades_sel &
        ANO %in% as.integer(anos_sel) &
        COMPONENTE %chin% componentes_sel
    ]
  })

  plot_oci_especialidade_componente <- reactive({

    dados_grafico <- dados_oci_especialidade_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    rotulo_anos <- paste(sort(input$anos_oci_especialidade_componente), collapse = " e ")

    grafico_oci_especialidade_componente(
      dados_grafico,
      titulo = paste0("OCI por especialidade e componente — ", rotulo_anos, " — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_especialidade_componente <- renderPlotly({
    ggplotly(plot_oci_especialidade_componente(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3), margin = list(b = 90)) |>
      alta_resolucao("oci_especialidade_componente")
  })

  output$tabela_oci_especialidade_componente_fisico <- renderDT({
    d <- dados_oci_especialidade_componente_agregada()
    validate(need(nrow(d) > 0, ""))
    tabela_oci_especialidade_componente(d, "OCI")
  })
  output$tabela_oci_especialidade_componente_financeiro <- renderDT({
    d <- dados_oci_especialidade_componente_agregada()
    validate(need(nrow(d) > 0, ""))
    tabela_oci_especialidade_componente(d, "VALOR", moeda = TRUE)
  })

  # Base de OCI (todos os componentes) usada pelos KPIs do topo da aba —
  # soma por ano/mês, sem os filtros de Especialidade/Componente das outras
  # sub-abas (KPI é sempre a visão geral da seleção de Região/UF/Município).
  dados_oci_mensal_anos <- reactive({

    dados_grafico <- oci_especialidade_componente_filtrada()[
      , .(oci = sum(OCI, na.rm = TRUE)), by = .(ano = ANO, mes = MES)
    ]
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    dados_grafico
  })

  ## ---- OCI: cabeçalho institucional (subtítulo, "limpar filtros" e KPIs) ----

  output$subtitulo_oci <- renderText({
    rotulo_local_oci()
  })

  observeEvent(input$limpar_oci, {
    updatePickerInput(session, "regiao_oci", selected = REGIOES)
    updateSelectInput(session, "uf_oci", selected = "BRASIL")
    updateSelectInput(session, "municipio_oci", selected = "Todos")
    updateCheckboxGroupInput(session, "especialidades_oci_sub", selected = ORDEM_ESPECIALIDADES)
    updateSelectInput(session, "componente_oci_sub", selected = "geral")
    updateCheckboxGroupInput(session, "anos_oci_especialidade_componente", selected = ANOS_OCI)
    updateCheckboxGroupInput(session, "componentes_oci_especialidade_componente", selected = ORDEM_COMPONENTES_OCI)
    updateSelectInput(session, "especialidade_oci_procedimento", selected = ORDEM_ESPECIALIDADES[1])
    anos_oci_ranking <- dados()$oci_ranking_procedimento_uf
    if (!is.null(anos_oci_ranking)) {
      updateSelectInput(session, "ano_oci_procedimento", selected = max(anos_oci_ranking$ano))
    }
    updateSelectInput(session, "mes_oci_procedimento", selected = "0")
  })

  ## ---- OCI: Especialidade e Procedimentos ----
  # Cascata Região/UF/Município igual às demais reactives de OCI, no grão
  # de procedimento (código SIGTAP) — ver OCI/ranking_procedimentos_oci.R.
  oci_ranking_procedimento_filtrada <- reactive({

    if (municipio_oci_ativo()) {
      base_mun <- dados()$oci_ranking_procedimento_municipio
      validate(need(!is.null(base_mun), "Ranking de procedimentos de OCI (município) não encontrado. Rode ranking_procedimentos_oci.R no projeto OCI."))
      return(base_mun[NM_UF == input$uf_oci & MUNICIPIO == input$municipio_oci])
    }

    base <- dados()$oci_ranking_procedimento_uf
    validate(need(!is.null(base), "Ranking de procedimentos de OCI não encontrado. Rode ranking_procedimentos_oci.R no projeto OCI."))
    req(input$uf_oci)
    validate(need(length(input$regiao_oci) > 0, "Selecione ao menos uma região."))

    if (input$uf_oci == "BRASIL") {
      base[REGIAO %in% c(input$regiao_oci, if (oci_todas_regioes()) "Não informada")]
    } else {
      base[NM_UF == input$uf_oci]
    }
  })

  observeEvent(dados(), {
    ranking <- dados()$oci_ranking_procedimento_uf
    if (is.null(ranking)) return()
    anos <- sort(unique(ranking$ano), decreasing = TRUE)
    updateSelectInput(session, "ano_oci_procedimento", choices = anos, selected = anos[1])
    escolhas_mes <- c("Ano inteiro" = "0", setNames(as.character(1:12), MES_LABELS))
    updateSelectInput(session, "mes_oci_procedimento", choices = escolhas_mes, selected = "0")
  }, once = TRUE)

  # Recorte de Ano/Mês, próprio desta sub-aba — reaproveitado pela tabela
  # (soma o recorte inteiro) e pelo gráfico por mês (mantém ano/mês).
  oci_procedimento_periodo <- reactive({

    req(input$ano_oci_procedimento, input$mes_oci_procedimento)
    ano_sel <- as.integer(input$ano_oci_procedimento)
    mes_sel <- as.integer(input$mes_oci_procedimento)

    d <- oci_ranking_procedimento_filtrada()[ano == ano_sel]
    if (mes_sel > 0) {
      d <- d[mes == mes_sel]
    }
    d
  })

  # Tabela: soma o Ano/Mês selecionados, só a especialidade escolhida — é o
  # "número de procedimentos realizados dentro daquela OCI".
  oci_procedimento_tabela <- reactive({

    req(input$especialidade_oci_procedimento)

    d <- oci_procedimento_periodo()[ESPECIALIDADE == input$especialidade_oci_procedimento]
    validate(need(nrow(d) > 0, "Sem dados para a seleção atual."))

    agregada <- d[
      , .(Quantidade = sum(OCI, na.rm = TRUE), Financeiro = sum(VALOR, na.rm = TRUE)),
      by = .(Codigo = CODIGO_PROCEDIMENTO, Procedimento = NOME_PROCEDIMENTO)
    ]
    setorder(agregada, -Quantidade)
    agregada[]
  })

  output$subtitulo_oci_procedimento <- renderText({
    req(input$mes_oci_procedimento)
    mes_sel <- as.integer(input$mes_oci_procedimento)
    rotulo_periodo <- if (mes_sel > 0) {
      paste0(MES_LABELS[mes_sel], "/", input$ano_oci_procedimento)
    } else {
      input$ano_oci_procedimento
    }
    paste0(input$especialidade_oci_procedimento, " — ", rotulo_periodo, " — ", rotulo_local_oci())
  })

  output$tabela_oci_procedimento_csv <- handler_csv(oci_procedimento_tabela, "oci_procedimentos_especialidade")

  output$tabela_oci_procedimento <- renderDT({
    d <- oci_procedimento_tabela()
    datatable(
      d,
      colnames = c("Código", "Procedimento", "Quantidade", "Financeiro (R$)"),
      rownames = FALSE,
      selection = "none",
      fillContainer = FALSE,
      height = "auto",
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    ) |>
      formatRound("Quantidade", digits = 0, mark = ".", interval = 3) |>
      formatCurrency("Financeiro", currency = "R$ ", interval = 3, mark = ".", digits = 0)
  })

  # Gráfico: mesma especialidade, por mês de atendimento — só os
  # TOP_N_PROCEDIMENTOS_OCI procedimentos com mais OCI no período (o mesmo
  # corte que já está no topo da tabela acima).
  dados_oci_procedimento_mes <- reactive({

    tabela <- oci_procedimento_tabela()
    codigos_top <- head(tabela$Codigo, TOP_N_PROCEDIMENTOS_OCI)

    d <- oci_procedimento_periodo()[
      ESPECIALIDADE == input$especialidade_oci_procedimento & CODIGO_PROCEDIMENTO %in% codigos_top
    ]

    agregada <- d[
      , .(OCI = sum(OCI, na.rm = TRUE)),
      by = .(ano, mes, CODIGO_PROCEDIMENTO, NOME_PROCEDIMENTO)
    ]
    agregada[, competencia := as.Date(sprintf("%04d-%02d-01", ano, mes))]
    validate(need(nrow(agregada) > 0, "Sem dados para a seleção atual."))

    agregada[]
  })

  plot_oci_procedimento_mes <- reactive({
    grafico_oci_procedimento_mes(
      dados_oci_procedimento_mes(),
      titulo = paste0(
        "Top ", TOP_N_PROCEDIMENTOS_OCI, " procedimentos — ", input$especialidade_oci_procedimento,
        " — ", input$ano_oci_procedimento, " — ", rotulo_local_oci()
      )
    )
  })

  output$grafico_oci_procedimento_mes <- renderPlotly({
    ggplotly(plot_oci_procedimento_mes(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.4), margin = list(b = 100)) |>
      alta_resolucao("oci_procedimentos_mes")
  })

  output$grafico_oci_procedimento_mes_csv <- handler_csv(dados_oci_procedimento_mes, "oci_procedimentos_mes")
  output$grafico_oci_procedimento_mes_pptx <- handler_pptx(plot_oci_procedimento_mes, "oci_procedimentos_mes")

  # Mesma base do gráfico "Produção mensal — 2025 vs 2026"
  # (dados_oci_mensal_anos()) — OCI só tem contagem física, sem toggle.
  # Card 1: total do ano anterior; card 2: ano corrente até a última
  # competência; card 3: total acumulado da série; card 4: taxa de expansão
  # anual = 2026 até a última competência FECHADA (as últimas
  # OCI_MESES_PRELIMINARES ainda podem ser revisadas e ficam de fora) sobre o
  # ano anterior INTEIRO.
  kpi_oci <- reactive({

    d <- dados_oci_mensal_anos()
    req(nrow(d) > 0)

    ano_atual <- max(d$ano)
    ano_anterior <- ano_atual - 1

    d_atual <- d[ano == ano_atual]
    ultima <- d_atual[which.max(mes)]

    # Valor financeiro (R$) dos mesmos anos, da mesma base filtrada do gráfico.
    valor_ano <- oci_especialidade_componente_filtrada()[, .(valor = sum(VALOR, na.rm = TRUE)), by = .(ano = ANO)]
    valor_do_ano <- function(a) {
      v <- valor_ano[ano == a]$valor
      if (length(v) == 0L) NA_real_ else v
    }

    # Última competência fechada = última competência disponível menos
    # OCI_MESES_PRELIMINARES meses.
    idx_fechada <- ultima$ano * 12 + ultima$mes - OCI_MESES_PRELIMINARES
    ano_fechado <- (idx_fechada - 1) %/% 12
    mes_fechado <- (idx_fechada - 1) %% 12 + 1

    tx_expansao <- NA_real_
    if (ano_fechado == ano_atual) {
      total_fechado_atual <- sum(d[ano == ano_atual & mes <= mes_fechado]$oci, na.rm = TRUE)
      total_anterior_inteiro <- sum(d[ano == ano_anterior]$oci, na.rm = TRUE)
      if (total_anterior_inteiro > 0) {
        tx_expansao <- (total_fechado_atual / total_anterior_inteiro - 1) * 100
      }
    }

    list(
      ano_atual = ano_atual, ano_anterior = ano_anterior,
      total_anterior = sum(d[ano == ano_anterior]$oci, na.rm = TRUE),
      total_atual = sum(d_atual$oci, na.rm = TRUE),
      total_geral = sum(d$oci, na.rm = TRUE),
      valor_anterior = valor_do_ano(ano_anterior),
      valor_atual = valor_do_ano(ano_atual),
      valor_total = sum(valor_ano$valor, na.rm = TRUE),
      mes_ultima = ultima$mes, mes_fechado = mes_fechado, ano_fechado = ano_fechado,
      ultima_ano = ultima$ano,
      tx_expansao = tx_expansao
    )
  })

  output$kpi_oci_titulo1 <- renderText({
    paste("OCI realizadas em", kpi_oci()$ano_anterior)
  })
  output$kpi_oci_valor1 <- renderText({
    label_pt_num(kpi_oci()$total_anterior)
  })

  output$kpi_oci_linha1 <- renderUI({
    k <- kpi_oci()
    if (is.na(k$valor_anterior)) return(NULL)
    kpi_linha(c(
      paste0("Total do ano ", k$ano_anterior),
      paste0("Financeiro: ", label_pt_moeda(k$valor_anterior))
    ))
  })

  output$kpi_oci_titulo2 <- renderText({
    paste0("OCI realizadas em ", kpi_oci()$ano_atual, "*")
  })
  output$kpi_oci_valor2 <- renderText({
    label_pt_num(kpi_oci()$total_atual)
  })
  output$kpi_oci_linha2 <- renderUI({
    k <- kpi_oci()
    kpi_linha(c(
      paste0("Jan–", MES_LABELS[k$mes_ultima], "/", k$ano_atual),
      if (!is.na(k$valor_atual)) paste0("Financeiro: ", label_pt_moeda(k$valor_atual))
    ))
  })

  output$kpi_oci_valor3 <- renderText({
    label_pt_num(kpi_oci()$total_geral)
  })
  output$kpi_oci_linha3 <- renderUI({
    k <- kpi_oci()
    kpi_linha(c(
      paste0(k$ano_anterior, " + ", k$ano_atual, "*"),
      paste0("Financeiro: ", label_pt_moeda(k$valor_total))
    ))
  })

  output$kpi_oci_valor4 <- renderText({
    k <- kpi_oci()
    if (is.na(k$tx_expansao)) return("—")
    sinal <- if (k$tx_expansao >= 0) "+" else ""
    paste0(sinal, label_pt_num(k$tx_expansao), "%")
  })
  output$kpi_oci_linha4 <- renderUI({
    k <- kpi_oci()
    if (is.na(k$tx_expansao)) return(NULL)
    kpi_linha(c(
      paste0("Anual · ", k$ano_anterior, " → Jan–", MES_LABELS[k$mes_fechado], "/", k$ano_atual),
      paste0("Até a última competência fechada (", MES_LABELS[k$mes_fechado], "/", k$ano_fechado, ")")
    ))
  })

  output$kpi_oci_rodape <- renderText({
    k <- kpi_oci()
    paste0(
      "*", k$ano_atual, ": dados disponíveis até ", MES_LABELS[k$mes_ultima], "/", k$ultima_ano,
      " (os últimos ", OCI_MESES_PRELIMINARES, " meses ainda são preliminares e ficam de fora da taxa de expansão).",
      " Financeiro = valor federal de referência (valor aprovado menos o complemento do gestor local; só SIA)."
    )
  })

  ## ---- Consultas e Exames ----
  # Mesma cascata Região/UF/Município da aba OCI. Especialidade só existe
  # na base de Exames (coluna ESPECIALIDADE); Consultas ignora esse filtro
  # (só tem 2 tipos fixos — Consulta/Teleconsulta —, sem quebra por área).

  observeEvent(list(input$regiao_ce, dados()$ce_exames_uf), {
    ufs_regiao <- sort(UF_REF[REGIAO %in% input$regiao_ce]$NM_UF_OCI)
    escolhas <- setNames(c("BRASIL", ufs_regiao), c(rotulo_agregado(input$regiao_ce), ufs_regiao))
    selecionado <- if (isTRUE(input$uf_ce %in% escolhas)) input$uf_ce else "BRASIL"
    updateSelectInput(session, "uf_ce", choices = escolhas, selected = selecionado)
  })

  observeEvent(list(input$uf_ce, dados()$ce_exames_municipio), {
    base_mun <- dados()$ce_exames_municipio
    if (is.null(input$uf_ce) || input$uf_ce == "BRASIL" || is.null(base_mun)) {
      updateSelectInput(session, "municipio_ce", choices = c("Selecione uma UF" = "Todos"), selected = "Todos")
      return()
    }
    municipios <- sort(unique(base_mun[NM_UF == input$uf_ce]$MUNICIPIO))
    escolhas <- setNames(c("Todos", municipios), c("Todos (UF inteira)", municipios))
    selecionado <- if (isTRUE(input$municipio_ce %in% escolhas)) input$municipio_ce else "Todos"
    updateSelectInput(session, "municipio_ce", choices = escolhas, selected = selecionado)
  })

  observeEvent(dados(), {
    base <- dados()$ce_exames_uf
    if (is.null(base)) return()
    anos <- sort(unique(base$ANO), decreasing = TRUE)
    updateSelectInput(session, "ano_tabela_ce", choices = anos, selected = anos[1])

    ultimo_ano <- max(anos)
    ultimo_mes <- max(base[ANO == ultimo_ano]$MES)
    escolhas_mes_ce <- setNames(as.character(1:12), MES_LABELS)
    escolhas_ano_ce <- sort(anos)
    updateSelectInput(session, "ano_fim_consultas_ce", choices = escolhas_ano_ce, selected = ultimo_ano)
    updateSelectInput(session, "mes_fim_consultas_ce", choices = escolhas_mes_ce, selected = as.character(ultimo_mes))
    updateSelectInput(session, "ano_fim_exames_ce", choices = escolhas_ano_ce, selected = ultimo_ano)
    updateSelectInput(session, "mes_fim_exames_ce", choices = escolhas_mes_ce, selected = as.character(ultimo_mes))
    updateSelectInput(session, "ano_ranking_exames_ce", choices = anos, selected = anos[1])
  }, once = TRUE)

  municipio_ce_ativo <- reactive({
    isTRUE(input$uf_ce != "BRASIL") && !is.null(input$municipio_ce) && input$municipio_ce != "Todos"
  })

  rotulo_local_ce <- reactive({
    if (municipio_ce_ativo()) {
      paste0(input$municipio_ce, " (", input$uf_ce, ")")
    } else if (input$uf_ce == "BRASIL") {
      rotulo_agregado(input$regiao_ce)
    } else {
      input$uf_ce
    }
  })

  # Base filtrada por Região/UF/Município — reaproveitada por Consultas e
  # Exames (`base_uf_nome`/`base_municipio_nome` escolhem qual das quatro
  # tabelas carregadas usar). `respeitar_especialidade` só se aplica a
  # Exames (única base com a coluna ESPECIALIDADE).
  ce_filtrada <- function(base_uf_nome, base_municipio_nome, respeitar_especialidade) {

    if (municipio_ce_ativo()) {
      base_mun <- dados()[[base_municipio_nome]]
      validate(need(!is.null(base_mun), "Tabela não encontrada. Rode o script 03_exportar_painel.R no projeto Consultas_Exames."))
      d <- base_mun[NM_UF == input$uf_ce & MUNICIPIO == input$municipio_ce]
    } else {
      base <- dados()[[base_uf_nome]]
      validate(need(!is.null(base), "Tabela não encontrada. Rode o script 03_exportar_painel.R no projeto Consultas_Exames."))
      req(input$uf_ce)
      validate(need(length(input$regiao_ce) > 0, "Selecione ao menos uma região."))
      d <- if (input$uf_ce == "BRASIL") base[REGIAO %in% input$regiao_ce] else base[NM_UF == input$uf_ce]
    }

    if (respeitar_especialidade && isTRUE(!is.null(input$especialidade_ce) && input$especialidade_ce != "Todas")) {
      d <- d[ESPECIALIDADE == input$especialidade_ce]
    }

    d
  }

  consultas_ce_filtrada <- reactive({
    ce_filtrada("ce_consultas_uf", "ce_consultas_municipio", respeitar_especialidade = FALSE)
  })

  exames_ce_filtrada <- reactive({
    ce_filtrada("ce_exames_uf", "ce_exames_municipio", respeitar_especialidade = TRUE)
  })

  # Série mensal (soma o que sobrar do filtro) — usada pelos gráficos de
  # barra e pelos KPIs do topo da aba.
  dados_consultas_mes <- reactive({
    d <- consultas_ce_filtrada()[
      , .(
        QTD_TOTAL = sum(QTD_TOTAL), QTD_OCI_FAEC = sum(QTD_OCI_FAEC),
        QTD_OCI_MAC = sum(QTD_OCI_MAC), QTD_OCI_TOTAL = sum(QTD_OCI_TOTAL)
      ),
      by = .(ano = ANO, mes = MES)
    ]
    validate(need(nrow(d) > 0, "Sem dados para a seleção atual."))
    d[, competencia := as.Date(sprintf("%04d-%02d-01", ano, mes))]
    d[]
  })

  dados_exames_mes <- reactive({
    d <- exames_ce_filtrada()[
      , .(
        QTD_TOTAL = sum(QTD_TOTAL), QTD_OCI_FAEC = sum(QTD_OCI_FAEC),
        QTD_OCI_MAC = sum(QTD_OCI_MAC), QTD_OCI_TOTAL = sum(QTD_OCI_TOTAL)
      ),
      by = .(ano = ANO, mes = MES)
    ]
    validate(need(nrow(d) > 0, "Sem dados para a seleção atual."))
    d[, competencia := as.Date(sprintf("%04d-%02d-01", ano, mes))]
    d[]
  })

  # Recorta uma série mensal para os 12 meses terminados em ano_fim/mes_fim
  # (inclusive) — os gráficos de Consultas/Exames não mostram mais a série
  # inteira (54+ meses), e sim uma janela de 1 ano que os filtros "Até o
  # ano"/"Até o mês" deslizam. `col_ano`/`col_mes` permitem reaproveitar em
  # tabelas com nomes de coluna diferentes (ex.: ANO/MES, maiúsculo).
  janela_12_meses <- function(d, ano_fim, mes_fim, col_ano = "ano", col_mes = "mes") {
    fim_idx <- ano_fim * 12L + mes_fim
    inicio_idx <- fim_idx - 11L
    d[(get(col_ano) * 12L + get(col_mes)) >= inicio_idx & (get(col_ano) * 12L + get(col_mes)) <= fim_idx]
  }

  # `dados_..._ce_grafico()` já aplica a janela de 12 meses — usada tanto
  # pelo gráfico (sempre em Físico) quanto pelo CSV, pra exportar
  # exatamente o que está desenhado.
  dados_consultas_ce_grafico <- reactive({
    req(input$ano_fim_consultas_ce, input$mes_fim_consultas_ce)
    d <- janela_12_meses(
      dados_consultas_mes(), as.integer(input$ano_fim_consultas_ce), as.integer(input$mes_fim_consultas_ce)
    )
    validate(need(nrow(d) > 0, "Sem dados para a janela selecionada."))
    d2 <- d[, .(competencia, TOTAL = QTD_TOTAL, FAEC = QTD_OCI_FAEC, OUTROS = QTD_OCI_MAC)]
    d2[, PCT_OCI := fifelse(TOTAL > 0, (FAEC + OUTROS) / TOTAL * 100, 0)]
    d2[]
  })

  plot_consultas_ce <- reactive({
    grafico_consultas_exames_oci_mes(
      dados_consultas_ce_grafico(), titulo = paste0("Consultas e Teleconsultas — ", rotulo_local_ce()),
      rotulo_eixo = "Quantidade", fmt_valor = label_pt_num
    )
  })

  output$grafico_consultas_ce <- renderPlotly({
    ggplotly(plot_consultas_ce(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      eixo_secundario_percentual(dados_consultas_ce_grafico()$PCT_OCI) |>
      layout(legend = list(orientation = "h", y = -0.35), margin = list(b = 110, r = 60)) |>
      alta_resolucao("consultas_teleconsultas")
  })

  output$grafico_consultas_ce_csv <- handler_csv(dados_consultas_ce_grafico, "consultas_teleconsultas")
  output$grafico_consultas_ce_pptx <- handler_pptx(plot_consultas_ce, "consultas_teleconsultas")

  dados_exames_ce_grafico <- reactive({
    req(input$ano_fim_exames_ce, input$mes_fim_exames_ce)
    d <- janela_12_meses(
      dados_exames_mes(), as.integer(input$ano_fim_exames_ce), as.integer(input$mes_fim_exames_ce)
    )
    validate(need(nrow(d) > 0, "Sem dados para a janela selecionada."))
    d2 <- d[, .(competencia, TOTAL = QTD_TOTAL, FAEC = QTD_OCI_FAEC, OUTROS = QTD_OCI_MAC)]
    d2[, PCT_OCI := fifelse(TOTAL > 0, (FAEC + OUTROS) / TOTAL * 100, 0)]
    d2[]
  })

  plot_exames_ce <- reactive({
    grafico_consultas_exames_oci_mes(
      dados_exames_ce_grafico(), titulo = paste0("Exames Estratégicos Eletivos — ", rotulo_local_ce()),
      rotulo_eixo = "Quantidade", fmt_valor = label_pt_num
    )
  })

  output$grafico_exames_ce <- renderPlotly({
    ggplotly(plot_exames_ce(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      eixo_secundario_percentual(dados_exames_ce_grafico()$PCT_OCI) |>
      layout(legend = list(orientation = "h", y = -0.35), margin = list(b = 110, r = 60)) |>
      alta_resolucao("exames_estrategicos")
  })

  output$grafico_exames_ce_csv <- handler_csv(dados_exames_ce_grafico, "exames_estrategicos")
  output$grafico_exames_ce_pptx <- handler_pptx(plot_exames_ce, "exames_estrategicos")

  ## ---- Consultas e Exames: Ranking de Exames ----
  # Fica no grão UF (não Município) — mesmo recorte que a aba OCI já usa na
  # sub-aba "Especialidade e Procedimentos" (Região/UF/Especialidade da
  # lateral; Município não se aplica aqui).

  ce_exames_ranking_filtrada <- reactive({
    base <- dados()$ce_exames_ranking_uf
    validate(need(
      !is.null(base),
      "Ranking de exames não encontrado. Rode o script 03_exportar_painel.R no projeto Consultas_Exames."
    ))
    req(input$uf_ce)
    validate(need(length(input$regiao_ce) > 0, "Selecione ao menos uma região."))
    d <- if (input$uf_ce == "BRASIL") base[REGIAO %in% input$regiao_ce] else base[NM_UF == input$uf_ce]
    if (isTRUE(!is.null(input$especialidade_ce) && input$especialidade_ce != "Todas")) {
      d <- d[ESPECIALIDADE == input$especialidade_ce]
    }
    d
  })

  # Tabela: soma o ano inteiro escolhido, um ranking por exame.
  ranking_exames_ano <- reactive({
    req(input$ano_ranking_exames_ce)
    d <- ce_exames_ranking_filtrada()[ANO == as.integer(input$ano_ranking_exames_ce)]
    validate(need(nrow(d) > 0, "Sem dados para a seleção atual."))
    agregada <- d[, .(Quantidade = sum(QTD_TOTAL)), by = .(Codigo = CODIGO, Procedimento = NOME_PROCEDIMENTO)]
    setorder(agregada, -Quantidade)
    agregada[]
  })

  output$subtitulo_ranking_exames_ce <- renderText({
    req(input$ano_ranking_exames_ce)
    esp <- if (isTRUE(!is.null(input$especialidade_ce) && input$especialidade_ce != "Todas")) {
      paste0(input$especialidade_ce, " — ")
    } else {
      ""
    }
    paste0(esp, input$ano_ranking_exames_ce, " — ", rotulo_local_ce())
  })

  output$tabela_ranking_exames_ce_csv <- handler_csv(ranking_exames_ano, "exames_ranking_procedimento")

  output$tabela_ranking_exames_ce <- renderDT({
    d <- ranking_exames_ano()
    datatable(
      d,
      colnames = c("Código", "Procedimento", "Quantidade"),
      rownames = FALSE, selection = "none", fillContainer = FALSE, height = "auto",
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    ) |> formatRound("Quantidade", digits = 0, mark = ".", interval = 3)
  })

  # Gráfico: mora na sub-aba "Exames Estratégicos Eletivos", logo abaixo do
  # gráfico de colunas empilhadas — segue a MESMA janela de 12 meses dele
  # ("Até o ano"/"Até o mês" daquela sub-aba), não o Ano fixo da tabela de
  # ranking acima. Só os TOP_N_EXAMES_CE exames mais realizados na janela.
  dados_ranking_exames_mes <- reactive({
    req(input$ano_fim_exames_ce, input$mes_fim_exames_ce)
    d_janela <- janela_12_meses(
      ce_exames_ranking_filtrada(), as.integer(input$ano_fim_exames_ce), as.integer(input$mes_fim_exames_ce),
      col_ano = "ANO", col_mes = "MES"
    )
    validate(need(nrow(d_janela) > 0, "Sem dados para a janela selecionada."))

    ranking <- d_janela[, .(QTD = sum(QTD_TOTAL)), by = .(CODIGO, NOME_PROCEDIMENTO)]
    setorder(ranking, -QTD)
    codigos_top <- head(ranking$CODIGO, TOP_N_EXAMES_CE)

    agregada <- d_janela[
      CODIGO %in% codigos_top,
      .(QTD_TOTAL = sum(QTD_TOTAL)),
      by = .(ano = ANO, mes = MES, NOME_PROCEDIMENTO)
    ]
    agregada[, competencia := as.Date(sprintf("%04d-%02d-01", ano, mes))]
    validate(need(nrow(agregada) > 0, "Sem dados para a seleção atual."))
    agregada[]
  })

  plot_ranking_exames_ce <- reactive({
    grafico_exames_ranking_mes(
      dados_ranking_exames_mes(),
      titulo = paste0("Top ", TOP_N_EXAMES_CE, " exames — ", rotulo_local_ce())
    )
  })

  output$grafico_ranking_exames_ce <- renderPlotly({
    ggplotly(plot_ranking_exames_ce(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.4), margin = list(b = 100)) |>
      alta_resolucao("exames_ranking_mes")
  })

  output$grafico_ranking_exames_ce_csv <- handler_csv(dados_ranking_exames_mes, "exames_ranking_mes")
  output$grafico_ranking_exames_ce_pptx <- handler_pptx(plot_ranking_exames_ce, "exames_ranking_mes")

  output$subtitulo_ce <- renderText({ rotulo_local_ce() })

  # KPIs seguem a sub-aba ativa (Consultas e Teleconsultas ou Exames
  # Estratégicos — nas sub-abas "Ranking de Exames"/"Tabela Geral", que não
  # têm indicador próprio, ficam em Exames) e o "Até o ano" já escolhido
  # naquela sub-aba ("ano selecionado"). Sequência: quantidade do ano,
  # quantidade dentro da OCI, % dentro da OCI, e taxa de expansão
  # comparando o mesmo intervalo de meses (Jan–até o último mês disponível)
  # entre o ano selecionado e o ano anterior.
  kpi_ce <- reactive({

    indicador_consultas <- isTRUE(input$subaba_ce == "Consultas e Teleconsultas")

    if (indicador_consultas) {
      req(input$ano_fim_consultas_ce)
      ano_sel <- as.integer(input$ano_fim_consultas_ce)
      d <- dados_consultas_mes()
      titulo <- "Consultas e Teleconsultas"
    } else {
      req(input$ano_fim_exames_ce)
      ano_sel <- as.integer(input$ano_fim_exames_ce)
      d <- dados_exames_mes()
      titulo <- "Exames Estratégicos Eletivos"
    }

    d_ano <- d[ano == ano_sel]
    validate(need(nrow(d_ano) > 0, "Sem dados para o ano selecionado."))

    qtd_total <- sum(d_ano$QTD_TOTAL)
    qtd_oci <- sum(d_ano$QTD_OCI_TOTAL)
    pct_oci <- if (qtd_total > 0) qtd_oci / qtd_total * 100 else NA_real_
    mes_ultimo <- max(d_ano$mes)

    ano_anterior <- ano_sel - 1L
    total_sel <- sum(d[ano == ano_sel & mes <= mes_ultimo, ]$QTD_TOTAL)
    total_anterior <- sum(d[ano == ano_anterior & mes <= mes_ultimo, ]$QTD_TOTAL)
    tx_expansao <- if (total_anterior > 0) (total_sel / total_anterior - 1) * 100 else NA_real_

    list(
      titulo = titulo, ano_sel = ano_sel, ano_anterior = ano_anterior, mes_ultimo = mes_ultimo,
      qtd_total = qtd_total, qtd_oci = qtd_oci, pct_oci = pct_oci, tx_expansao = tx_expansao
    )
  })

  output$kpi_ce_titulo1 <- renderText({ paste0(kpi_ce()$titulo, " (", kpi_ce()$ano_sel, "*)") })
  output$kpi_ce_valor1 <- renderText({ label_pt_num(kpi_ce()$qtd_total) })
  output$kpi_ce_linha1 <- renderUI({
    k <- kpi_ce()
    kpi_linha(paste0("Jan–", MES_LABELS[k$mes_ultimo], "/", k$ano_sel))
  })

  output$kpi_ce_valor2 <- renderText({ label_pt_num(kpi_ce()$qtd_oci) })
  output$kpi_ce_linha2 <- renderUI({
    k <- kpi_ce()
    kpi_linha(paste0("Jan–", MES_LABELS[k$mes_ultimo], "/", k$ano_sel))
  })

  output$kpi_ce_valor3 <- renderText({
    k <- kpi_ce()
    if (is.na(k$pct_oci)) return("—")
    paste0(label_pt_num(k$pct_oci), "%")
  })
  output$kpi_ce_linha3 <- renderUI({
    k <- kpi_ce()
    kpi_linha(paste0("Jan–", MES_LABELS[k$mes_ultimo], "/", k$ano_sel))
  })

  output$kpi_ce_valor4 <- renderText({
    k <- kpi_ce()
    if (is.na(k$tx_expansao)) return("—")
    sinal <- if (k$tx_expansao >= 0) "+" else ""
    paste0(sinal, label_pt_num(k$tx_expansao), "%")
  })
  output$kpi_ce_linha4 <- renderUI({
    k <- kpi_ce()
    if (is.na(k$tx_expansao)) return(NULL)
    kpi_linha(paste0(
      "Jan–", MES_LABELS[k$mes_ultimo], "/", k$ano_anterior, " → Jan–", MES_LABELS[k$mes_ultimo], "/", k$ano_sel
    ))
  })

  output$kpi_ce_rodape <- renderText({
    k <- kpi_ce()
    paste0(
      "*", k$ano_sel, ": dados disponíveis até ", MES_LABELS[k$mes_ultimo], "/", k$ano_sel, "."
    )
  })

  ## ---- Consultas e Exames: Tabela Geral ----

  tabela_geral_ce_dados <- reactive({

    req(input$ano_tabela_ce, input$indicador_tabela_ce, input$metrica_tabela_ce)
    validate(need(length(input$regiao_ce) > 0, "Selecione ao menos uma região."))
    ano_sel <- as.integer(input$ano_tabela_ce)
    indicador_exames <- isTRUE(input$indicador_tabela_ce == "exames")

    base <- if (indicador_exames) dados()$ce_exames_uf else dados()$ce_consultas_uf
    validate(need(!is.null(base), "Tabela não encontrada. Rode o script 03_exportar_painel.R no projeto Consultas_Exames."))

    # Tabela já é "por UF" — por isso não aplica o filtro de UF/Município da
    # lateral (ficaria uma tabela de 1 linha); Região e Especialidade (só em
    # Exames) continuam valendo.
    d <- base[ANO == ano_sel & REGIAO %chin% input$regiao_ce]
    if (indicador_exames && isTRUE(!is.null(input$especialidade_ce) && input$especialidade_ce != "Todas")) {
      d <- d[ESPECIALIDADE == input$especialidade_ce]
    }
    validate(need(nrow(d) > 0, "Sem dados para a seleção atual."))

    cols <- if (isTRUE(input$metrica_tabela_ce == "financeiro")) {
      c("VALOR_TOTAL", "VALOR_OCI_FAEC", "VALOR_OCI_MAC", "VALOR_OCI_TOTAL")
    } else {
      c("QTD_TOTAL", "QTD_OCI_FAEC", "QTD_OCI_MAC", "QTD_OCI_TOTAL")
    }

    tabela_geral_consultas_exames(d, cols[1], cols[2], cols[3], cols[4])
  })

  output$subtitulo_tabela_ce <- renderText({
    rotulo_ind <- if (input$indicador_tabela_ce == "exames") "Exames Estratégicos Eletivos" else "Consultas e Teleconsultas"
    paste0(rotulo_ind, " — ", input$ano_tabela_ce)
  })

  output$tabela_geral_ce_csv <- handler_csv(tabela_geral_ce_dados, "consultas_exames_tabela_geral")

  output$tabela_geral_ce <- renderDT({

    d <- tabela_geral_ce_dados()
    moeda <- isTRUE(input$metrica_tabela_ce == "financeiro")
    rotulo_total <- if (moeda) "Valor Total" else "Qtd. Total"
    rotulo_oci <- if (moeda) "Total dentro OCI (R$)" else "Total dentro OCI"

    tabela <- datatable(
      d,
      colnames = c("UF", rotulo_total, "Dentro OCI (FAEC)", "Dentro OCI — Outros componentes", rotulo_oci),
      rownames = FALSE, selection = "none", fillContainer = FALSE, height = "auto",
      options = list(dom = "t", ordering = FALSE, paging = FALSE, pageLength = -1)
    )

    tabela <- if (moeda) {
      formatCurrency(tabela, c("TOTAL", "FAEC", "OUTROS", "OCI"), currency = "R$ ", interval = 3, mark = ".", digits = 0)
    } else {
      formatRound(tabela, c("TOTAL", "FAEC", "OUTROS", "OCI"), digits = 0, mark = ".", interval = 3)
    }

    formatStyle(
      tabela, c("Linha", "TOTAL", "FAEC", "OUTROS", "OCI"),
      valueColumns = "Linha",
      fontWeight = styleEqual(
        c("BRASIL", toupper(ORDEM_REGIAO_CE)),
        rep("bold", 1 + length(ORDEM_REGIAO_CE))
      )
    )
  })

  observeEvent(input$limpar_ce, {
    updatePickerInput(session, "regiao_ce", selected = REGIOES)
    updateSelectInput(session, "uf_ce", selected = "BRASIL")
    updateSelectInput(session, "municipio_ce", selected = "Todos")
    updateSelectInput(session, "especialidade_ce", selected = "Todas")
    updateRadioButtons(session, "indicador_tabela_ce", selected = "exames")
    updateRadioButtons(session, "metrica_tabela_ce", selected = "fisico")
    anos_ce <- dados()$ce_exames_uf
    if (!is.null(anos_ce)) {
      ultimo_ano <- max(anos_ce$ANO)
      ultimo_mes <- max(anos_ce[ANO == ultimo_ano]$MES)
      updateSelectInput(session, "ano_tabela_ce", selected = ultimo_ano)
      updateSelectInput(session, "ano_fim_consultas_ce", selected = ultimo_ano)
      updateSelectInput(session, "mes_fim_consultas_ce", selected = as.character(ultimo_mes))
      updateSelectInput(session, "ano_fim_exames_ce", selected = ultimo_ano)
      updateSelectInput(session, "mes_fim_exames_ce", selected = as.character(ultimo_mes))
      updateSelectInput(session, "ano_ranking_exames_ce", selected = ultimo_ano)
    }
  })

  ## ---- Pagamento Portaria 9810 ----

  # Popula UF e Componente a partir da base carregada (só uma vez — a base
  # não muda durante a sessão, diferente de dados() no resto do painel, que
  # é atualizado por sincronizar_dados_locais()).
  observeEvent(dados(), {

    base <- dados()$portaria9810
    if (is.null(base)) {
      return()
    }

    ufs <- sort(unique(as.character(base$NM_UF)))
    escolhas_uf <- setNames(c("BRASIL", ufs), c("BRASIL (todos os estados)", ufs))
    updateSelectInput(session, "uf_portaria9810", choices = escolhas_uf, selected = "BRASIL")

    componentes <- sort(unique(base$COMPONENTE))
    updateCheckboxGroupInput(
      session, "componente_portaria9810", choices = componentes, selected = componentes
    )
  }, once = TRUE)

  # Cascata UF -> Município (mesma lógica das outras abas).
  observeEvent(list(input$uf_portaria9810, dados()$portaria9810), {

    base <- dados()$portaria9810

    if (is.null(input$uf_portaria9810) || input$uf_portaria9810 == "BRASIL" || is.null(base)) {
      updateSelectInput(session, "municipio_portaria9810", choices = c("Selecione uma UF" = "Todos"), selected = "Todos")
      return()
    }

    municipios <- sort(unique(base[NM_UF == input$uf_portaria9810]$MUNICIPIO))
    escolhas <- setNames(c("Todos", municipios), c("Todos (UF inteira)", municipios))

    selecionado <- if (isTRUE(input$municipio_portaria9810 %in% escolhas)) input$municipio_portaria9810 else "Todos"

    updateSelectInput(session, "municipio_portaria9810", choices = escolhas, selected = selecionado)
  })

  municipio_portaria9810_ativo <- reactive({
    isTRUE(input$uf_portaria9810 != "BRASIL") && !is.null(input$municipio_portaria9810) && input$municipio_portaria9810 != "Todos"
  })

  rotulo_local_portaria9810 <- reactive({
    if (municipio_portaria9810_ativo()) {
      paste0(input$municipio_portaria9810, " (", input$uf_portaria9810, ")")
    } else if (input$uf_portaria9810 == "BRASIL") {
      "BRASIL"
    } else {
      input$uf_portaria9810
    }
  })

  # Base filtrada por UF/Município/Tipo de Gestão/Componente — usada só na
  # subaba "Pagamentos" (a subaba de Limite ignora Município e Tipo de
  # Gestão de propósito, ver dados_portaria9810_limite()).
  portaria9810_filtrada <- reactive({

    base <- dados()$portaria9810
    validate(need(!is.null(base), "Base de pagamentos da Portaria 9.810 não encontrada em dados/Base_pagamento/."))

    validate(need(length(input$tipo_gestao_portaria9810) > 0, "Selecione ao menos um tipo de gestão."))
    validate(need(length(input$componente_portaria9810) > 0, "Selecione ao menos um componente."))

    tipos_sel <- toupper(stri_trans_general(input$tipo_gestao_portaria9810, "Latin-ASCII"))
    base <- base[TIPO_GESTAO %chin% tipos_sel & COMPONENTE %chin% input$componente_portaria9810]

    if (isTRUE(input$uf_portaria9810 != "BRASIL")) {
      base <- base[NM_UF == input$uf_portaria9810]
    }

    if (municipio_portaria9810_ativo()) {
      base <- base[MUNICIPIO == input$municipio_portaria9810]
    }

    base
  })

  # Tira os meses iniciais com valor líquido zerado (ex.: jan/2026, que só
  # teve 3 lançamentos municipais do Componente Cirúrgico com desconto
  # igual ao valor bruto — líquido R$0) — eles só deixavam a ponta do
  # gráfico vazia, sem nenhum pagamento de verdade pra mostrar.
  remover_meses_zerados_no_inicio <- function(d) {
    totais <- d[, .(total = sum(valor, na.rm = TRUE)), by = DATA_PAGAMENTO]
    meses_validos <- totais[total > 0]$DATA_PAGAMENTO
    if (length(meses_validos) == 0) {
      return(d)
    }
    d[DATA_PAGAMENTO >= min(meses_validos)]
  }

  dados_portaria9810_mensal <- reactive({

    base <- portaria9810_filtrada()
    validate(need(nrow(base) > 0, "Sem dados para a seleção atual."))

    d <- base[, .(valor = sum(VALOR_LIQUIDO, na.rm = TRUE)), by = .(DATA_PAGAMENTO, TIPO_GESTAO)]
    remover_meses_zerados_no_inicio(d)
  })

  plot_portaria9810_mensal <- reactive({
    grafico_portaria9810_mensal(
      dados_portaria9810_mensal(),
      titulo = paste0("Pagamentos Portaria 9.810 — ", rotulo_local_portaria9810())
    )
  })

  output$grafico_portaria9810_mensal <- renderPlotly({
    ggplotly(plot_portaria9810_mensal(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3), margin = list(b = 100)) |>
      alta_resolucao("portaria9810_pagamentos_mensal")
  })

  dados_portaria9810_programa <- reactive({

    base <- portaria9810_filtrada()
    validate(need(nrow(base) > 0, "Sem dados para a seleção atual."))

    d <- base[, .(valor = sum(VALOR_LIQUIDO, na.rm = TRUE)), by = .(DATA_PAGAMENTO, COMPONENTE)]
    remover_meses_zerados_no_inicio(d)
  })

  plot_portaria9810_programa <- reactive({
    grafico_portaria9810_programa(
      dados_portaria9810_programa(),
      titulo = paste0("Pagamentos por Componente — ", rotulo_local_portaria9810())
    )
  })

  output$grafico_portaria9810_programa <- renderPlotly({
    ggplotly(plot_portaria9810_programa(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3), margin = list(b = 100)) |>
      alta_resolucao("portaria9810_por_componente_programa")
  })

  output$tabela_portaria9810_uf <- renderDT({

    base <- portaria9810_filtrada()
    validate(need(nrow(base) > 0, ""))

    agregada <- base[, .(valor = sum(VALOR_LIQUIDO, na.rm = TRUE)), by = .(NM_UF, TIPO_GESTAO)]
    tabela <- dcast(
      agregada, NM_UF ~ TIPO_GESTAO,
      value.var = "valor", fun.aggregate = sum, fill = 0
    )
    colunas_tipo <- setdiff(names(tabela), "NM_UF")
    tabela[, Total := rowSums(.SD), .SDcols = colunas_tipo]
    colunas_valor <- c(colunas_tipo, "Total")

    datatable(
      tabela,
      colnames = c("UF", stri_trans_totitle(colunas_tipo), "Total"),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(length(colunas_valor), "desc")))
    ) |>
      formatCurrency(colunas_valor, currency = "R$ ", interval = 3, mark = ".", digits = 0)
  })

  # "Limite da Portaria": sempre por UF inteira (soma Estadual +
  # Municipal, todos os municípios e componentes) — o limite da portaria não
  # discrimina por tipo de gestão/município, então só o filtro de UF (para
  # focar em um estado) se aplica aqui.
  dados_portaria9810_limite <- reactive({

    base <- dados()$portaria9810
    limite <- dados()$portaria9810_limite
    validate(need(
      !is.null(base) && !is.null(limite),
      "Bases da Portaria 9.810 não encontradas em dados/."
    ))

    pago_uf <- base[, .(valor_pago = sum(VALOR_LIQUIDO, na.rm = TRUE)), by = .(SG_UF, NM_UF)]

    comparacao <- merge(limite, pago_uf, by = c("SG_UF", "NM_UF"), all.x = TRUE)
    comparacao[is.na(valor_pago), valor_pago := 0]
    comparacao[, percentual := valor_pago / VALOR_LIMITE]
    comparacao[, status := fifelse(valor_pago > VALOR_LIMITE, "Ultrapassou o limite", "Dentro do limite")]

    if (isTRUE(input$uf_portaria9810 != "BRASIL")) {
      comparacao <- comparacao[NM_UF == input$uf_portaria9810]
    }

    validate(need(nrow(comparacao) > 0, "Sem dados para a seleção atual."))
    setorder(comparacao, percentual)

    comparacao[]
  })

  rotulo_agregado_portaria9810 <- reactive({
    if (input$uf_portaria9810 == "BRASIL") "BRASIL" else input$uf_portaria9810
  })

  plot_portaria9810_limite <- reactive({
    grafico_portaria9810_limite(
      dados_portaria9810_limite(),
      titulo = paste0("Valor pago x limite — Portaria 9.810 — ", rotulo_agregado_portaria9810())
    )
  })

  output$grafico_portaria9810_limite <- renderPlotly({
    ggplotly(plot_portaria9810_limite(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      alta_resolucao("portaria9810_limite_uf")
  })

  output$tabela_portaria9810_limite <- renderDT({

    comparacao <- dados_portaria9810_limite()

    tabela <- comparacao[order(-percentual), .(NM_UF, valor_pago, VALOR_LIMITE, percentual, status)]

    datatable(
      tabela,
      colnames = c("UF", "Valor Pago", "Limite (R$)", "% do Limite", "Status"),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(3, "desc")))
    ) |>
      formatCurrency(c("valor_pago", "VALOR_LIMITE"), currency = "R$ ", interval = 3, mark = ".", digits = 0) |>
      formatPercentage("percentual", 1) |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Dentro do limite", "Ultrapassou o limite"), c("#d4edda", "#f8d7da")
        )
      )
  })

  ## ---- Portaria 9810: cabeçalho institucional (subtítulo, "limpar filtros" e KPIs) ----

  output$subtitulo_portaria9810 <- renderText({
    paste0("Componentes Ambulatorial e Cirúrgico, em 2026 — ", rotulo_local_portaria9810())
  })

  observeEvent(input$limpar_portaria9810, {
    updateSelectInput(session, "uf_portaria9810", selected = "BRASIL")
    updateSelectInput(session, "municipio_portaria9810", selected = "Todos")
    updateCheckboxGroupInput(session, "tipo_gestao_portaria9810", selected = c("Estadual", "Municipal"))
  })

  # A série da Portaria 9.810 é curta e cabe dentro de um único ano — por
  # isso os KPIs de mês aqui comparam primeiro x último mês disponível, em
  # vez de "mesmo mês, ano anterior" como nas outras abas (não faria
  # sentido com tão poucos meses de histórico).
  kpi_portaria9810 <- reactive({

    d <- dados_portaria9810_mensal()
    req(nrow(d) > 0)

    d_mes <- d[, .(valor = sum(valor, na.rm = TRUE)), by = DATA_PAGAMENTO]
    setorder(d_mes, DATA_PAGAMENTO)
    req(nrow(d_mes) > 0)

    total_periodo <- sum(d_mes$valor, na.rm = TRUE)
    maior_linha <- d_mes[which.max(valor)]
    ultima_linha <- d_mes[which.max(DATA_PAGAMENTO)]
    primeira_linha <- d_mes[which.min(DATA_PAGAMENTO)]

    d_componente <- dados_portaria9810_programa()[, .(valor = sum(valor, na.rm = TRUE)), by = COMPONENTE]
    componente_top <- if (nrow(d_componente) > 0) d_componente[which.max(valor)] else NULL

    list(
      total_periodo = total_periodo, maior_linha = maior_linha,
      ultima_linha = ultima_linha, primeira_linha = primeira_linha, componente_top = componente_top
    )
  })

  output$kpi_portaria_valor_periodo <- renderText({
    k <- kpi_portaria9810()
    label_pt_moeda(k$total_periodo)
  })
  output$kpi_portaria_valor_periodo_linha <- renderUI({
    k <- kpi_portaria9810()
    kpi_linha(paste0(
      rotular_mes_ano_pt(k$primeira_linha$DATA_PAGAMENTO), " – ",
      rotular_mes_ano_pt(k$ultima_linha$DATA_PAGAMENTO)
    ))
  })

  output$kpi_portaria_maior_mes <- renderText({
    k <- kpi_portaria9810()
    label_pt_moeda(k$maior_linha$valor)
  })
  output$kpi_portaria_maior_mes_linha <- renderUI({
    k <- kpi_portaria9810()
    kpi_linha(rotular_mes_ano_pt(k$maior_linha$DATA_PAGAMENTO))
  })

  output$kpi_portaria_ultimo_mes <- renderText({
    k <- kpi_portaria9810()
    label_pt_moeda(k$ultima_linha$valor)
  })
  output$kpi_portaria_ultimo_mes_linha <- renderUI({
    k <- kpi_portaria9810()
    kpi_linha(rotular_mes_ano_pt(k$ultima_linha$DATA_PAGAMENTO))
  })

  output$kpi_portaria_componente_top <- renderText({
    k <- kpi_portaria9810()
    if (is.null(k$componente_top)) return("—")
    as.character(k$componente_top$COMPONENTE)
  })
  output$kpi_portaria_componente_top_linha <- renderUI({
    k <- kpi_portaria9810()
    if (is.null(k$componente_top)) return(NULL)
    kpi_linha(paste0(
      label_pt_moeda(k$componente_top$valor), " · ",
      rotular_mes_ano_pt(k$primeira_linha$DATA_PAGAMENTO), " – ", rotular_mes_ano_pt(k$ultima_linha$DATA_PAGAMENTO)
    ))
  })

  ## ---- Exportação de dados (CSV) dos 8 gráficos ----
  # Cada gráfico expõe os mesmos dados usados para plotar (nenhum recálculo).

  output$grafico_cirurgia_csv <- handler_csv(dados_diagrama_cirurgia, "diagrama_monitoramento_cirurgias")
  output$grafico_cirurgia_pptx <- handler_pptx(plot_cirurgia, "diagrama_monitoramento_cirurgias")
  output$grafico_comparacao_anos_csv <- handler_csv(dados_comparacao_anos, "comparacao_anos_cirurgias")
  output$grafico_comparacao_anos_pptx <- handler_pptx(plot_comparacao_anos, "comparacao_anos_cirurgias")

  dados_oci_componente_export <- reactive({
    dg <- oci_geral_filtrada()[, .(competencia, valor = oci, serie = "Total geral de OCI")]
    ag <- dados_oci_componente_agregada()[, .(competencia, valor = OCI, serie = as.character(COMPONENTE))]
    rbind(dg, ag)
  })
  output$grafico_oci_componente_csv <- handler_csv(dados_oci_componente_export, "oci_geral")
  output$grafico_oci_componente_pptx <- handler_pptx(plot_oci_componente, "oci_geral")

  dados_oci_componente_ano_export <- reactive({
    dados_oci_componente_agregada()[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ANO, COMPONENTE)]
  })
  output$grafico_oci_componente_ano_csv <- handler_csv(dados_oci_componente_ano_export, "oci_componente_ano")
  output$grafico_oci_componente_ano_pptx <- handler_pptx(plot_oci_componente_ano, "oci_componente_ano")
  output$grafico_oci_componente_mes_csv <- handler_csv(dados_oci_componente_agregada, "oci_componente_mes")
  output$grafico_oci_componente_mes_pptx <- handler_pptx(plot_oci_componente_mes, "oci_componente_mes")

  dados_oci_especialidade_export <- reactive({
    dg <- dados_oci_especialidade_sub_geral()[, .(competencia, valor = oci, serie = "Geral")]
    ag <- dados_oci_especialidade_sub_agregada()[, .(competencia, valor = OCI, serie = as.character(ESPECIALIDADE))]
    rbind(dg, ag)
  })
  output$grafico_oci_especialidade_csv <- handler_csv(dados_oci_especialidade_export, "oci_por_especialidade")
  output$grafico_oci_especialidade_pptx <- handler_pptx(plot_oci_especialidade, "oci_por_especialidade")

  dados_oci_especialidade_componente_export <- reactive({
    dados_oci_especialidade_componente_agregada()[
      , .(OCI = sum(OCI, na.rm = TRUE)), by = .(ESPECIALIDADE, COMPONENTE)
    ]
  })
  output$grafico_oci_especialidade_componente_csv <- handler_csv(
    dados_oci_especialidade_componente_export, "oci_especialidade_componente"
  )
  output$grafico_oci_especialidade_componente_pptx <- handler_pptx(
    plot_oci_especialidade_componente, "oci_especialidade_componente"
  )


  output$grafico_portaria9810_mensal_csv <- handler_csv(dados_portaria9810_mensal, "portaria9810_pagamentos_mensal")
  output$grafico_portaria9810_mensal_pptx <- handler_pptx(plot_portaria9810_mensal, "portaria9810_pagamentos_mensal")
  output$grafico_portaria9810_programa_csv <- handler_csv(
    dados_portaria9810_programa, "portaria9810_por_componente"
  )
  output$grafico_portaria9810_programa_pptx <- handler_pptx(plot_portaria9810_programa, "portaria9810_por_componente")
  output$grafico_portaria9810_limite_csv <- handler_csv(dados_portaria9810_limite, "portaria9810_limite_uf")
  output$grafico_portaria9810_limite_pptx <- handler_pptx(plot_portaria9810_limite, "portaria9810_limite_uf")

  # Os botões de download ficam dentro de sub-abas que já nascem "ativas"
  # (a primeira de cada tabsetPanel). Como elas nunca disparam um evento de
  # troca de aba do Bootstrap, o Shiny não desconsidera a suspensão padrão
  # de outputs escondidos e o link de download nunca é calculado. Aqui isso
  # é desligado especificamente para esses 24 outputs (CSV + PPTX dos 12
  # gráficos — não afeta os gráficos/tabelas, que continuam suspensos até a
  # aba ser visitada).
  botoes_download <- c(
    "grafico_cirurgia_csv", "grafico_cirurgia_pptx",
    "tabela_ranking_cirurgia_csv",
    "grafico_comparacao_anos_csv", "grafico_comparacao_anos_pptx",
    "grafico_oci_componente_csv", "grafico_oci_componente_pptx",
    "grafico_oci_componente_ano_csv", "grafico_oci_componente_ano_pptx",
    "grafico_oci_componente_mes_csv", "grafico_oci_componente_mes_pptx",
    "grafico_oci_especialidade_csv", "grafico_oci_especialidade_pptx",
    "grafico_oci_especialidade_componente_csv", "grafico_oci_especialidade_componente_pptx",
    "tabela_oci_procedimento_csv",
    "grafico_oci_procedimento_mes_csv", "grafico_oci_procedimento_mes_pptx",
    "grafico_consultas_ce_csv", "grafico_consultas_ce_pptx",
    "grafico_exames_ce_csv", "grafico_exames_ce_pptx",
    "tabela_ranking_exames_ce_csv",
    "grafico_ranking_exames_ce_csv", "grafico_ranking_exames_ce_pptx",
    "tabela_geral_ce_csv",
    "grafico_portaria9810_mensal_csv", "grafico_portaria9810_mensal_pptx",
    "grafico_portaria9810_programa_csv", "grafico_portaria9810_programa_pptx",
    "grafico_portaria9810_limite_csv", "grafico_portaria9810_limite_pptx",
    "semaforo_download_ppt_br", "semaforo_download_tabela_br",
    "semaforo_download_ppt_regiao", "semaforo_download_tabela_regiao",
    "semaforo_download_ppt_municipio", "semaforo_download_ppt_municipio_nome",
    "semaforo_download_tabela_municipio"
  )
  for (id_botao in botoes_download) {
    outputOptions(output, id_botao, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
