library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)
library(readxl)
library(stringi)
library(shinyWidgets)

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

MES_LABELS <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

# Rótulos "Mês/AA" em português para eixos de data (o Plotly só formata
# datas em inglês por padrão, sem carregar um locale de JS à parte).
rotular_mes_ano_pt <- function(datas) {
  paste0(MES_LABELS[as.integer(format(datas, "%m"))], "/", format(datas, "%y"))
}

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
  rol   = "Cirurgias Eletivas (MAC e FAEC) do ROL",
  total = "Cirurgias Eletivas (MAC e FAEC) totais",
  pnrf  = "Cirurgias Eletivas do Programa (PNRF)",
  pab   = "Cirurgias Eletivas PAB"
)

ORDEM_ESPECIALIDADES <- c(
  "Cardiologia", "Oftalmologia", "Oncologia",
  "Ortopedia", "Otorrinolaringologia", "Saúde Mulher"
)

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

# Paleta cíclica para gráficos com uma linha/barra por ano (não é fixa em
# quantidade de anos: se a base ganhar mais um ano, a paleta só repete).
PALETA_ANOS <- c("#c9a227", "#7d7d7d", "#2e8b57", "#1b6fa8", "#a4508b", "#c1442d")

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
  "Total geral de OCI"    = "#185FA5",
  "Componente Ambulatorial" = "#FDB528",
  "Carretas"                = "#E4302B",
  "Créditos Financeiros"    = "#8FD9C4",
  "Equipes Volantes"        = "#9AD4E8"
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
  setNames(PALETA_ANOS[((seq_along(anos) - 1) %% length(PALETA_ANOS)) + 1], anos)
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

status_cirurgia <- function(serie) {

  if (is.null(serie)) {
    return(NULL)
  }

  s <- serie[!is.na(classificacao) & classificacao != ""]
  s <- s[s[, .I[mes == max(mes)], by = uf_atendimento]$V1]
  setorder(s, uf_atendimento)
  s[, .(uf_atendimento, mes, quantidade, classificacao)]
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
  origem_cirurgia_bases <- file.path(
    CIRURGIA_DIR_ORIGEM, "resultados", "bases_processadas"
  )
  origem_oci <- file.path(OCI_DIR_ORIGEM, "resultados")

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
    localizar_arquivo(origem_cirurgia_bases, "^cirurgias_mensal_procedimento_rol_.*\\.csv$"),
    localizar_arquivo(origem_oci, "^planilha_OCI_UF_mes_.*\\.xlsx$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_componente_uf\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_componente_municipio\\.csv$")
  )
  arquivos <- arquivos[!is.na(arquivos)]

  file.copy(arquivos, DADOS_LOCAIS, overwrite = TRUE)

  invisible(TRUE)
}

carregar_tudo <- function() {
  sincronizar_dados_locais()
  list(
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
    cirurgia_procedimento_rol = carregar_serie_procedimento_rol(),
    mapa_especialidade_rol = carregar_mapa_especialidade_rol(),
    oci_serie = carregar_serie_oci(),
    oci_especialidade_componente_uf = carregar_oci_especialidade_componente_uf(),
    oci_especialidade_componente_municipio = carregar_oci_especialidade_componente_municipio()
  )
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
# projeto Cirurgia (base 2022-2024, comparação 2025, monitoramento 2026).
DIAGRAMA_ANO_INICIAL_BASE <- 2022L
DIAGRAMA_ANO_FINAL_BASE <- 2024L
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

# Dados brutos por trás de qualquer gráfico, em CSV (";" + decimal ",",
# mesmo padrão dos demais arquivos do painel).
handler_csv <- function(dados_fn, nome_arquivo) {
  downloadHandler(
    filename = function() paste0(nome_arquivo, "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) fwrite(dados_fn(), file, sep = ";", dec = ",", bom = TRUE)
  )
}

grafico_cirurgia <- function(dados_uf, ano_comparacao, ano_monitoramento, titulo, metrica = "fisico") {

  d <- dados_uf[order(ano, mes)]

  faixa <- unique(
    d[, .(mes, mediana_historica, q1_historico, q3_historico, limite_inferior, limite_superior)]
  )
  setorder(faixa, mes)

  comp <- d[ano == ano_comparacao]
  moni <- d[ano == ano_monitoramento]

  # A classificação (Esperado/Acima do esperado/...) foi desenhada para
  # produção física; para valores financeiros (R$) ela não se aplica, então
  # o modo financeiro mostra só a linha de produção, sem cor por ponto nem
  # legenda de classificação.
  com_classificacao <- metrica != "financeiro"

  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  prefixo_hover <- if (metrica == "financeiro") "R$ " else ""

  p <- plot_ly() |>
    add_ribbons(
      data = faixa, x = ~mes, ymin = ~limite_inferior, ymax = ~limite_superior,
      name = "Faixa histórica (min-máx)", fillcolor = "rgba(120,120,120,0.15)",
      line = list(color = "transparent"), hoverinfo = "skip"
    ) |>
    add_ribbons(
      data = faixa, x = ~mes, ymin = ~q1_historico, ymax = ~q3_historico,
      name = "Faixa histórica (Q1-Q3)", fillcolor = "rgba(90,90,90,0.28)",
      line = list(color = "transparent"), hoverinfo = "skip"
    ) |>
    add_lines(
      data = faixa, x = ~mes, y = ~mediana_historica, name = "Mediana histórica",
      line = list(color = "black", dash = "dash", width = 1.3)
    ) |>
    add_lines(
      data = comp, x = ~mes, y = ~quantidade, name = paste0("Produção ", ano_comparacao),
      line = list(color = "#003366", width = 2.4),
      hovertemplate = paste0("Mês: %{x}<br>", rotulo_eixo, ": ", prefixo_hover, "%{y:,.0f}<extra></extra>")
    )

  if (com_classificacao) {

    cores_pontos <- unname(CORES_CLASSIFICACAO[moni$classificacao])

    p <- add_trace(
      p,
      data = moni, x = ~mes, y = ~quantidade, name = paste0("Produção ", ano_monitoramento),
      type = "scatter", mode = "lines+markers",
      line = list(color = "black", width = 2.4),
      marker = list(size = 9, color = cores_pontos, line = list(color = "black", width = 0.5)),
      text = ~classificacao,
      hovertemplate = paste0("Mês: %{x}<br>", rotulo_eixo, ": ", prefixo_hover, "%{y:,.0f}<br>%{text}<extra></extra>")
    )

    # Como os pontos de "Produção <ano>" são um único trace com cor variável
    # por classificação, o Plotly não gera legenda para cada cor sozinho.
    # Os traces abaixo existem só para aparecer na legenda (sem desenhar nada
    # no gráfico), facilitando a leitura rápida das cores de classificação.
    niveis_classificacao <- c(
      "Esperado", "Acima do esperado", "Acima do limite esperado",
      "Atenção", "Crítico abaixo do limite esperado"
    )

    for (nivel in niveis_classificacao) {
      p <- add_trace(
        p, x = list(NA), y = list(NA), type = "scatter", mode = "markers",
        marker = list(size = 9, color = CORES_CLASSIFICACAO[[nivel]]),
        name = nivel, showlegend = TRUE, hoverinfo = "skip"
      )
    }

  } else {

    p <- add_trace(
      p,
      data = moni, x = ~mes, y = ~quantidade, name = paste0("Produção ", ano_monitoramento),
      type = "scatter", mode = "lines+markers",
      line = list(color = "black", width = 2.4),
      marker = list(size = 9, color = "black", line = list(color = "black", width = 0.5)),
      hovertemplate = paste0("Mês: %{x}<br>", rotulo_eixo, ": ", prefixo_hover, "%{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      xaxis = list(title = "Mês de competência", tickmode = "array", tickvals = 1:12, ticktext = MES_LABELS),
      yaxis = list(
        title = rotulo_eixo, tickformat = ",.0f",
        tickprefix = if (metrica == "financeiro") "R$ " else "",
        rangemode = "tozero"
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2),
      margin = list(b = 90),
      separators = ",."
    ) |>
    alta_resolucao("diagrama_monitoramento_cirurgias")
}

grafico_comparacao_anos <- function(dados, titulo, metrica = "fisico") {

  d <- dados[order(ano, mes)]
  anos <- sort(unique(d$ano))
  cores <- cores_para_anos(anos)

  coluna_y <- if (metrica == "financeiro") "valor" else "quantidade"
  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  prefixo_hover <- if (metrica == "financeiro") "R$ " else ""

  p <- plot_ly()

  for (ano_atual in anos) {
    dd <- d[ano == ano_atual]
    dd[, y_plot := get(coluna_y)]
    dd[, rotulo_dado := format(round(y_plot), big.mark = ".", scientific = FALSE)]

    cor_ano <- cores[[as.character(ano_atual)]]

    # As linhas de 2025 e 2026 levam rótulo de dado fixo em cada ponto; os
    # demais anos ficam só com a linha (leitura via hover), mantendo o
    # gráfico limpo.
    if (ano_atual %in% c(2025, 2026)) {
      p <- add_trace(
        p, data = dd, x = ~mes, y = ~y_plot, type = "scatter", mode = "lines+markers+text",
        name = as.character(ano_atual),
        line = list(color = cor_ano, width = 2.6),
        marker = list(color = cor_ano, size = 6),
        text = ~rotulo_dado, textposition = "top center",
        textfont = list(size = 10, color = cor_ano),
        hovertemplate = paste0(ano_atual, " — Mês: %{x}<br>", rotulo_eixo, ": ", prefixo_hover, "%{y:,.0f}<extra></extra>")
      )
    } else {
      p <- add_trace(
        p, data = dd, x = ~mes, y = ~y_plot, type = "scatter", mode = "lines",
        name = as.character(ano_atual),
        line = list(color = cor_ano, width = 2.6),
        hovertemplate = paste0(ano_atual, " — Mês: %{x}<br>", rotulo_eixo, ": ", prefixo_hover, "%{y:,.0f}<extra></extra>")
      )
    }
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      xaxis = list(title = "Mês de competência", tickmode = "array", tickvals = 1:12, ticktext = MES_LABELS),
      yaxis = list(
        title = rotulo_eixo, tickformat = ",.0f",
        tickprefix = if (metrica == "financeiro") "R$ " else "",
        rangemode = "tozero"
      ),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2),
      margin = list(b = 90),
      separators = ",."
    ) |>
    alta_resolucao("comparacao_anos_cirurgias")
}

# Gráfico "OCI geral": onda de área com o Total geral de OCI (mesmo estilo
# visual do gráfico original) mais uma linha por componente/modalidade
# (Componente Ambulatorial, Carretas, Créditos Financeiros, Equipes
# Volantes) — clicar na legenda do Plotly liga/desliga cada linha.
grafico_oci_componente <- function(dados_geral, dados_componente, titulo) {

  dg <- dados_geral[order(competencia)]
  dg[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  ultimo <- dg[which.max(competencia)]

  datas_unicas <- sort(unique(dg$competencia))

  p <- plot_ly() |>
    add_trace(
      data = dg, x = ~competencia, y = ~oci, type = "scatter", mode = "lines",
      fill = "tozeroy", fillcolor = "rgba(24,95,165,0.10)",
      name = "Total geral de OCI",
      line = list(color = CORES_COMPONENTE_OCI[["Total geral de OCI"]], width = 2.2),
      text = ~rotulo_mes,
      hovertemplate = "Total geral de OCI<br>%{text}<br>OCI: %{y:,.0f}<extra></extra>"
    ) |>
    add_trace(
      data = ultimo, x = ~competencia, y = ~oci, type = "scatter", mode = "markers",
      marker = list(color = "white", line = list(color = "#D85A30", width = 2), size = 10),
      name = "Último mês", hoverinfo = "skip", showlegend = FALSE
    )

  for (componente_atual in ORDEM_COMPONENTES_OCI) {
    dd <- dados_componente[COMPONENTE == componente_atual][order(competencia)]
    if (nrow(dd) == 0) next
    dd[, rotulo_mes := rotular_mes_ano_pt(competencia)]
    p <- add_trace(
      p, data = dd, x = ~competencia, y = ~OCI, type = "scatter", mode = "lines",
      name = componente_atual,
      line = list(color = CORES_COMPONENTE_OCI[[componente_atual]], width = 2, dash = "dot"),
      text = ~rotulo_mes,
      hovertemplate = paste0(componente_atual, "<br>%{text}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      xaxis = list(
        title = "Mês de atendimento",
        tickmode = "array",
        tickvals = datas_unicas,
        ticktext = rotular_mes_ano_pt(datas_unicas),
        tickangle = -45
      ),
      yaxis = list(title = "OCI realizadas", tickformat = ",.0f", rangemode = "tozero"),
      shapes = list(list(
        type = "line", x0 = DATA_VIRADA_OCI, x1 = DATA_VIRADA_OCI, y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "#D85A30", dash = "dash", width = 1.2)
      )),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.3),
      margin = list(b = 110),
      separators = ",."
    ) |>
    alta_resolucao("oci_geral_por_componente")
}

# Barras empilhadas HORIZONTAIS por ano — total acumulado por
# componente/modalidade, mesmo estilo do gráfico de referência (SIA/SUS e
# CMD). Fica em cima, com o gráfico por mês logo abaixo.
grafico_oci_componente_ano <- function(dados, titulo) {

  d <- dados[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ANO, COMPONENTE)]
  totais_ano <- d[, .(total = sum(OCI, na.rm = TRUE)), by = ANO]
  setorder(totais_ano, ANO)

  p <- plot_ly()

  for (componente_atual in ORDEM_COMPONENTES_OCI) {
    dd <- d[COMPONENTE == componente_atual]
    if (nrow(dd) == 0) next
    p <- add_trace(
      p, data = dd, y = ~as.character(ANO), x = ~OCI, type = "bar", orientation = "h",
      name = componente_atual,
      marker = list(color = CORES_COMPONENTE_OCI[[componente_atual]]),
      hovertemplate = paste0(componente_atual, "<br>%{y}<br>OCI: %{x:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0, font = list(size = 13)),
      barmode = "stack",
      xaxis = list(title = "OCI realizadas", tickformat = ",.0f"),
      # Range fixo: com annotations usando xshift, o autorange do Plotly.js
      # num eixo categórico se confunde e infla a escala (bug conhecido) —
      # travando o range em [-0.5, n-0.5] cada categoria fica corretamente
      # espaçada.
      yaxis = list(
        title = "", type = "category",
        range = c(-0.5, nrow(totais_ano) - 0.5), autorange = FALSE
      ),
      annotations = lapply(seq_len(nrow(totais_ano)), function(i) {
        list(
          x = totais_ano$total[i], y = as.character(totais_ano$ANO[i]),
          text = format(totais_ano$total[i], big.mark = ".", scientific = FALSE),
          showarrow = FALSE, xanchor = "left", xshift = 8,
          font = list(size = 12, color = "#12283D")
        )
      }),
      showlegend = FALSE,
      separators = ",.",
      margin = list(l = 60, b = 40, t = 40)
    ) |>
    alta_resolucao("oci_componente_ano")
}

# Mesmo esquema de colunas empilhadas por componente/modalidade, mas por
# mês de atendimento (toda a série, não só o total do ano) — fica logo
# abaixo do gráfico por ano.
grafico_oci_componente_mes <- function(dados, titulo) {

  d <- dados[order(competencia)]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]

  datas_unicas <- sort(unique(d$competencia))

  p <- plot_ly()

  for (componente_atual in ORDEM_COMPONENTES_OCI) {
    dd <- d[COMPONENTE == componente_atual][order(competencia)]
    if (nrow(dd) == 0) next
    p <- add_trace(
      p, data = dd, x = ~competencia, y = ~OCI, type = "bar",
      name = componente_atual,
      marker = list(color = CORES_COMPONENTE_OCI[[componente_atual]]),
      hovertext = ~rotulo_mes,
      hovertemplate = paste0(componente_atual, "<br>%{hovertext}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      barmode = "stack",
      xaxis = list(
        title = "Mês de atendimento",
        tickmode = "array",
        tickvals = datas_unicas,
        ticktext = rotular_mes_ano_pt(datas_unicas),
        tickangle = -45
      ),
      yaxis = list(title = "OCI realizadas", tickformat = ",.0f"),
      legend = list(orientation = "h", y = -0.35),
      margin = list(b = 110),
      separators = ",."
    ) |>
    alta_resolucao("oci_componente_mes")
}

grafico_oci_especialidade <- function(dados_geral, dados_especialidade, titulo) {

  dg <- dados_geral[order(competencia)]
  dg[, rotulo_mes := rotular_mes_ano_pt(competencia)]

  datas_unicas <- sort(unique(dg$competencia))

  p <- plot_ly() |>
    add_trace(
      data = dg, x = ~competencia, y = ~oci, type = "scatter", mode = "lines",
      name = "Geral", line = list(color = "#12283D", width = 3),
      text = ~rotulo_mes,
      hovertemplate = "Geral<br>%{text}<br>OCI: %{y:,.0f}<extra></extra>"
    )

  especialidades <- if (nrow(dados_especialidade) > 0) {
    sort(unique(as.character(dados_especialidade$ESPECIALIDADE)))
  } else {
    character()
  }

  for (especialidade_atual in especialidades) {
    dd <- dados_especialidade[ESPECIALIDADE == especialidade_atual][order(competencia)]
    dd[, rotulo_mes := rotular_mes_ano_pt(competencia)]
    p <- add_trace(
      p, data = dd, x = ~competencia, y = ~OCI, type = "scatter", mode = "lines",
      name = especialidade_atual,
      line = list(color = CORES_ESPECIALIDADE[[especialidade_atual]], width = 2, dash = "dot"),
      text = ~rotulo_mes,
      hovertemplate = paste0(especialidade_atual, "<br>%{text}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      xaxis = list(
        title = "Mês de atendimento",
        tickmode = "array",
        tickvals = datas_unicas,
        ticktext = rotular_mes_ano_pt(datas_unicas),
        tickangle = -45
      ),
      yaxis = list(title = "OCI realizadas", tickformat = ",.0f", rangemode = "tozero"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.3),
      margin = list(b = 110),
      separators = ",."
    ) |>
    alta_resolucao("oci_por_especialidade")
}

# Colunas empilhadas por especialidade e componente/modalidade — total
# acumulado do período filtrado, ordenado por total decrescente e com
# rótulo de valor em cada segmento, igual ao gráfico de referência.
grafico_oci_especialidade_componente <- function(dados, titulo) {

  d <- dados[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ESPECIALIDADE, COMPONENTE)]

  totais_especialidade <- d[, .(total = sum(OCI, na.rm = TRUE)), by = ESPECIALIDADE]
  setorder(totais_especialidade, -total)
  ordem_especialidade <- as.character(totais_especialidade$ESPECIALIDADE)
  d[, ESPECIALIDADE := factor(ESPECIALIDADE, levels = ordem_especialidade)]

  # Ordem de empilhamento (de baixo para cima), igual ao gráfico de referência.
  ordem_pilha <- c("Carretas", "Componente Ambulatorial", "Créditos Financeiros", "Equipes Volantes")
  cor_texto_pilha <- c(
    "Carretas" = "white", "Componente Ambulatorial" = "#12283D",
    "Créditos Financeiros" = "#12283D", "Equipes Volantes" = "#12283D"
  )

  p <- plot_ly()

  for (componente_atual in ordem_pilha) {
    dd <- d[COMPONENTE == componente_atual]
    if (nrow(dd) == 0) next
    p <- add_trace(
      p, data = dd, x = ~ESPECIALIDADE, y = ~OCI, type = "bar",
      name = componente_atual,
      marker = list(color = CORES_COMPONENTE_OCI[[componente_atual]]),
      texttemplate = "%{y:,.0f}", textposition = "inside", insidetextanchor = "middle",
      textfont = list(size = 10, color = cor_texto_pilha[[componente_atual]]),
      hovertemplate = paste0(componente_atual, "<br>%{x}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      barmode = "stack",
      xaxis = list(title = "", categoryorder = "array", categoryarray = ordem_especialidade),
      yaxis = list(title = "OCI realizadas", tickformat = ",.0f"),
      legend = list(orientation = "h", y = -0.3),
      margin = list(b = 90),
      separators = ",."
    ) |>
    alta_resolucao("oci_especialidade_componente")
}

grafico_oci_fisico_especialidade <- function(dados, titulo) {

  d <- dados[order(ESPECIALIDADE, ANO)]
  anos <- sort(unique(d$ANO))
  cores <- cores_para_anos(anos)

  p <- plot_ly()

  for (ano_atual in anos) {
    dd <- d[ANO == ano_atual]
    p <- add_trace(
      p, data = dd, x = ~ESPECIALIDADE, y = ~OCI, type = "bar",
      name = as.character(ano_atual), marker = list(color = cores[[as.character(ano_atual)]]),
      hovertemplate = paste0(ano_atual, "<br>%{x}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      barmode = "group",
      xaxis = list(title = "Especialidade"),
      yaxis = list(title = "OCI realizadas (físico)", tickformat = ",.0f"),
      legend = list(orientation = "h", y = -0.25),
      margin = list(b = 90),
      separators = ",."
    ) |>
    alta_resolucao("oci_fisico_especialidade")
}

grafico_oci_mensal_anos <- function(dados, titulo) {

  d <- dados[order(ano, mes)]
  anos <- sort(unique(d$ano))
  cores <- cores_para_anos(anos)

  p <- plot_ly()

  for (ano_atual in anos) {
    dd <- d[ano == ano_atual]
    p <- add_trace(
      p, data = dd, x = ~mes, y = ~oci, type = "bar",
      name = as.character(ano_atual), marker = list(color = cores[[as.character(ano_atual)]]),
      hovertemplate = paste0(ano_atual, " — Mês: %{x}<br>OCI: %{y:,.0f}<extra></extra>")
    )
  }

  p |>
    layout(
      title = list(text = titulo, x = 0),
      barmode = "group",
      xaxis = list(title = "Mês de atendimento", tickmode = "array", tickvals = 1:12, ticktext = MES_LABELS),
      yaxis = list(title = "OCI realizadas", tickformat = ",.0f", rangemode = "tozero"),
      legend = list(orientation = "h", y = -0.2),
      margin = list(b = 80),
      separators = ",."
    ) |>
    alta_resolucao("oci_mensal_2025_2026")
}

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
    p("Fonte: DRAC/SAES/MS. Atualizado em 24/08/2026."),
    p("Fonte: SIA (extração em 12/08/2026), e CMD (extração em 24/08/2026)."),
    p(tags$em("Sujeito a alterações."))
  )
}

# Linha compacta de exportação abaixo de cada gráfico: dados brutos em CSV e
# o mesmo gráfico como objeto nativo/editável do PowerPoint (não uma
# imagem). O PNG em alta resolução continua no ícone de câmera do Plotly.
barra_downloads <- function(id) {
  div(
    class = "mb-3 mt-1",
    downloadButton(paste0(id, "_csv"), "Dados (CSV)", class = "btn-sm btn-outline-secondary")
  )
}

ui <- page_navbar(
  title = "Monitoramento de Produção",
  id = "navbar",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tagList(
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
           'grafico_oci_fisico_especialidade', 'grafico_oci_mensal_anos'
         ].forEach(function (id) {
           var el = document.getElementById(id);
           if (el && window.Plotly) { Plotly.Plots.resize(el); }
         });
       });"
    )),
    div(
      style = "padding: 6px 16px;",
      textOutput("data_atualizacao", inline = TRUE)
    )
  ),

  nav_panel(
    "Cirurgias eletivas",
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        selectInput(
          "indicador_cirurgia", "Cirurgias Eletivas",
          choices = c(
            "MAC e FAEC totais" = "total",
            "MAC e FAEC do Rol" = "rol",
            "Ciru. PATE (PNRF)" = "pnrf"
          ),
          selected = "rol"
        ),
        checkboxInput(
          "pab_cirurgia", "Cirurgias Eletivas PAB", value = FALSE
        ),
        div(
          class = "text-muted small mb-2", style = "line-height: 1.3;",
          "PAB vale só para \"Comparação Anos\" (sem valor financeiro) — substitui o indicador acima enquanto marcado."
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
        div(
          class = "text-muted small mb-2", style = "line-height: 1.3;",
          "Filtro de Município vale só para \"Comparação Anos\" — o Diagrama de monitoramento e a Tabela continuam por UF/Região."
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
        div(
          class = "text-muted small mb-2", style = "line-height: 1.3;",
          "Indicador ROL — Físico: Especialidade vale para \"Comparação Anos\" e \"Diagrama de monitoramento\"; Procedimento só para \"Comparação Anos\"."
        ),
        info_fonte_dados()
      ),
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
          "Tabela",
          br(),
          h5("Classificação no último mês monitorado"),
          DTOutput("tabela_status_cirurgia")
        )
      )
    )
  ),

  nav_panel(
    "OCI realizadas",
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        checkboxGroupInput(
          "regiao_oci", "Região",
          choices = REGIOES, selected = REGIOES
        ),
        selectInput("uf_oci", "UF", choices = "BRASIL", selected = "BRASIL"),
        selectInput(
          "municipio_oci", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        div(
          class = "text-muted small mb-2", style = "line-height: 1.3;",
          "Filtro de Município vale para \"Série histórica\" e \"Comparativos\"."
        ),
        info_fonte_dados_oci()
      ),
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
          plotlyOutput("grafico_oci_componente_ano", height = "26vh"),
          barra_downloads("grafico_oci_componente_ano"),
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
          plotlyOutput("grafico_oci_especialidade_componente", height = "50vh"),
          barra_downloads("grafico_oci_especialidade_componente")
        ),
        tabPanel(
          "Comparativos",
          br(),
          fluidRow(
            column(
              8,
              checkboxGroupInput(
                "especialidades_oci_comparativos", "Especialidades",
                choices = ORDEM_ESPECIALIDADES, selected = ORDEM_ESPECIALIDADES,
                inline = TRUE
              )
            ),
            column(
              4,
              selectInput(
                "componente_oci_comparativos", "Componente",
                choices = COMPONENTES_OCI_FILTRO, selected = "geral"
              )
            )
          ),
          br(),
          h5("Físico por especialidade"),
          plotlyOutput("grafico_oci_fisico_especialidade", height = "40vh"),
          barra_downloads("grafico_oci_fisico_especialidade"),
          br(),
          h5("Financeiro por especialidade (R$)"),
          DTOutput("tabela_oci_financeiro"),
          br(),
          h5("Produção mensal — 2025 vs 2026"),
          plotlyOutput("grafico_oci_mensal_anos", height = "38vh"),
          barra_downloads("grafico_oci_mensal_anos")
        )
      )
    )
  )
)

#### SERVER ####

server <- function(input, output, session) {

  dados <- reactiveVal(carregar_tudo())

  output$data_atualizacao <- renderText({
    # Não exibe na aba OCI — lá a data de atualização já vem na nota de
    # fonte específica (info_fonte_dados_oci()).
    if (isTRUE(input$navbar == "OCI realizadas")) {
      return("")
    }
    serie_oci <- dados()$oci_serie
    if (is.null(serie_oci)) {
      return("")
    }
    paste0("Dados até a competência ", format(max(serie_oci$competencia, na.rm = TRUE), "%m/%Y"))
  })

  ## ---- Cirurgias ----

  # Física, independente do toggle Físico/Financeiro do Diagrama — usada pela
  # cascata de UF e pela Tabela de status (que não segue esse toggle).
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
      input$indicador_cirurgia != "pnrf",
      "Diagrama de monitoramento não disponível para o Programa (PNRF) — o histórico ainda é curto demais para gerar faixas de controle confiáveis. Use \"Comparação Anos\" ou a \"Tabela\" para acompanhar o PNRF."
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

  output$grafico_cirurgia <- renderPlotly({

    dados_uf <- dados_diagrama_cirurgia()

    ano_comparacao   <- min(dados_uf$ano)
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
      dados_uf, ano_comparacao, ano_monitoramento,
      titulo = paste0(rotulo_indicador, " — ", rotulo_uf, sufixo_especialidade),
      metrica = input$metrica_cirurgia_diagrama
    )
  })

  # Indicador efetivo de "Comparação Anos": PAB (checkbox) substitui o
  # indicador do dropdown só aqui — Diagrama de monitoramento e Tabela
  # continuam sempre com input$indicador_cirurgia, sem PAB (não tem
  # diagrama de controle nem tabela de status calculados).
  indicador_comparacao_anos <- reactive({
    if (isTRUE(input$pab_cirurgia)) "pab" else input$indicador_cirurgia
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

    if (filtro_procedimento_ativo) {

      validate(need(
        indicador_sel == "rol",
        "Filtros de Especialidade/Procedimento valem só para o indicador ROL. Selecione ROL (e desmarque PAB), ou volte a Especialidade/Procedimento para \"Todas\"/\"Todos\"."
      ))
      validate(need(
        input$metrica_cirurgia_anos == "fisico",
        "Não há valor financeiro — troque para \"Físico\" para usar os filtros de Especialidade/Procedimento."
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
        indicador_sel != "pab" || input$metrica_cirurgia_anos == "fisico",
        "Não há valor financeiro para Cirurgias Eletivas PAB — selecione \"Físico\"."
      ))

      serie <- serie_anos_cirurgia_indicador()
      validate(need(!is.null(serie), "Série multianual não encontrada. Rode o script 02_monitoramento_diagrama_controle.R no projeto Cirurgia."))

      municipio_sel <- input$municipio_cirurgia
      usar_municipio <- isTRUE(input$uf_cirurgia != "BRASIL") && !is.null(municipio_sel) && municipio_sel != "Todos"

      if (usar_municipio) {
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

  output$grafico_comparacao_anos <- renderPlotly({

    dados_uf <- dados_comparacao_anos()
    req(input$metrica_cirurgia_anos)

    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[indicador_comparacao_anos()]]

    grafico_comparacao_anos(
      dados_uf,
      titulo = paste0(
        rotulo_indicador, " — Comparação entre anos — ", rotulo_local_cirurgia(),
        rotulo_procedimento_cirurgia()
      ),
      metrica = input$metrica_cirurgia_anos
    )
  })


  output$tabela_status_cirurgia <- renderDT({

    serie <- serie_cirurgia_indicador_fisica()
    validate(need(!is.null(serie), ""))
    validate(need(length(input$regiao_cirurgia) > 0, ""))

    todas_regioes <- setequal(input$regiao_cirurgia, REGIOES)
    ufs_regiao <- UF_REF[REGIAO %in% input$regiao_cirurgia]$NM_UF_CIRURGIA

    status <- status_cirurgia(serie)

    if (todas_regioes) {
      status <- status[uf_atendimento %chin% c("BRASIL", ufs_regiao)]
    } else {
      status_regiao <- status_cirurgia(agregar_cirurgia_regiao(serie, input$regiao_cirurgia))
      if (!is.null(status_regiao) && nrow(status_regiao) > 0) {
        status_regiao[, uf_atendimento := rotulo_agregado(input$regiao_cirurgia)]
      }
      status <- rbind(status_regiao, status[uf_atendimento %chin% ufs_regiao], fill = TRUE)
    }

    datatable(
      status,
      colnames = c("UF", "Mês", "Quantidade", "Classificação"),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(3, "asc")))
    ) |>
      formatRound("quantidade", digits = 0, mark = ".", interval = 3) |>
      formatStyle(
        "classificacao",
        backgroundColor = styleEqual(names(CORES_CLASSIFICACAO), CORES_CLASSIFICACAO)
      )
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
        NM_UF %chin% UF_REF[REGIAO %in% input$regiao_oci]$NM_UF_OCI,
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
      base[REGIAO %in% input$regiao_oci]
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

  output$grafico_oci_componente <- renderPlotly({

    dados_geral <- oci_geral_filtrada()
    validate(need(nrow(dados_geral) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente(
      dados_geral, dados_oci_componente_agregada(),
      titulo = paste0("OCI realizadas — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_componente_ano <- renderPlotly({

    dados_grafico <- dados_oci_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente_ano(
      dados_grafico, titulo = paste0("Por ano — ", rotulo_local_oci())
    )
  })

  output$grafico_oci_componente_mes <- renderPlotly({

    dados_grafico <- dados_oci_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    grafico_oci_componente_mes(
      dados_grafico, titulo = paste0("Por mês de atendimento — ", rotulo_local_oci())
    )
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

  output$grafico_oci_especialidade <- renderPlotly({

    dados_geral <- dados_oci_especialidade_sub_geral()
    validate(need(nrow(dados_geral) > 0, "Sem dados para a seleção atual."))

    grafico_oci_especialidade(
      dados_geral, dados_oci_especialidade_sub_agregada(),
      titulo = paste0(
        "OCI por especialidade — ", rotulo_componente_oci_sub(), " — ", rotulo_local_oci()
      )
    )
  })

  # "OCI por especialidade e componente": sempre quebra pelos 4 componentes
  # (ignora o filtro de Componente, que aqui não se aplica), respeita o
  # filtro de Especialidades e tem filtro de Ano próprio (só deste gráfico).
  dados_oci_especialidade_componente_agregada <- reactive({

    especialidades_sel <- input$especialidades_oci_sub
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))

    anos_sel <- input$anos_oci_especialidade_componente
    validate(need(length(anos_sel) > 0, "Selecione ao menos um ano."))

    oci_especialidade_componente_filtrada()[
      ESPECIALIDADE %chin% especialidades_sel & ANO %in% as.integer(anos_sel)
    ]
  })

  output$grafico_oci_especialidade_componente <- renderPlotly({

    dados_grafico <- dados_oci_especialidade_componente_agregada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    rotulo_anos <- paste(sort(input$anos_oci_especialidade_componente), collapse = " e ")

    grafico_oci_especialidade_componente(
      dados_grafico,
      titulo = paste0("OCI por especialidade e componente — ", rotulo_anos, " — ", rotulo_local_oci())
    )
  })

  # "Comparativos": aplica o filtro de componente (todos, ou um só) antes de
  # somar por especialidade/mês — mesmo padrão da subaba "Por especialidade".
  oci_comparativos_base <- reactive({

    base <- oci_especialidade_componente_filtrada()

    if (isTRUE(input$componente_oci_comparativos != "geral")) {
      base <- base[COMPONENTE == input$componente_oci_comparativos]
    }

    base
  })

  rotulo_componente_oci_comparativos <- reactive({
    if (isTRUE(input$componente_oci_comparativos == "geral")) {
      "todos os componentes"
    } else {
      input$componente_oci_comparativos
    }
  })

  # Base física + financeira por especialidade/ano, reaproveitada pelo
  # gráfico físico e pela tabela financeira em "Comparativos".
  oci_especialidade_por_ano <- reactive({

    especialidades_sel <- input$especialidades_oci_comparativos
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))

    agregada <- oci_comparativos_base()[
      ESPECIALIDADE %chin% especialidades_sel,
      .(OCI = sum(OCI, na.rm = TRUE), VALOR = sum(VALOR, na.rm = TRUE)),
      by = .(ANO, ESPECIALIDADE)
    ]
    validate(need(nrow(agregada) > 0, "Sem dados para a seleção atual."))

    agregada
  })

  output$grafico_oci_fisico_especialidade <- renderPlotly({

    grafico_oci_fisico_especialidade(
      oci_especialidade_por_ano(),
      titulo = paste0(
        "Físico por especialidade — ", rotulo_componente_oci_comparativos(), " — ", rotulo_local_oci()
      )
    )
  })

  output$tabela_oci_financeiro <- renderDT({

    agregada <- oci_especialidade_por_ano()

    tabela <- dcast(
      agregada, ESPECIALIDADE ~ ANO,
      value.var = "VALOR", fun.aggregate = sum, fill = 0
    )
    colunas_ano <- setdiff(names(tabela), "ESPECIALIDADE")

    datatable(
      tabela,
      colnames = c("Especialidade", colunas_ano),
      rownames = FALSE,
      options = list(dom = "t", pageLength = -1, ordering = FALSE)
    ) |>
      formatCurrency(colunas_ano, currency = "R$ ", interval = 3, mark = ".", digits = 0)
  })

  # Ignora o filtro de Especialidades (comparativo mensal usa a série geral
  # do escopo filtrado), mas respeita o filtro de Componente.
  dados_oci_mensal_anos <- reactive({

    dados_grafico <- oci_comparativos_base()[
      , .(oci = sum(OCI, na.rm = TRUE)), by = .(ano = ANO, mes = MES)
    ]
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))

    dados_grafico
  })

  output$grafico_oci_mensal_anos <- renderPlotly({

    grafico_oci_mensal_anos(
      dados_oci_mensal_anos(),
      titulo = paste0(
        "Produção mensal — 2025 vs 2026 — ", rotulo_componente_oci_comparativos(), " — ", rotulo_local_oci()
      )
    )
  })

  ## ---- Exportação de dados (CSV) dos 8 gráficos ----
  # Cada gráfico expõe os mesmos dados usados para plotar (nenhum recálculo).

  output$grafico_cirurgia_csv <- handler_csv(dados_diagrama_cirurgia, "diagrama_monitoramento_cirurgias")
  output$grafico_comparacao_anos_csv <- handler_csv(dados_comparacao_anos, "comparacao_anos_cirurgias")

  dados_oci_componente_export <- reactive({
    dg <- oci_geral_filtrada()[, .(competencia, valor = oci, serie = "Total geral de OCI")]
    ag <- dados_oci_componente_agregada()[, .(competencia, valor = OCI, serie = as.character(COMPONENTE))]
    rbind(dg, ag)
  })
  output$grafico_oci_componente_csv <- handler_csv(dados_oci_componente_export, "oci_geral")

  dados_oci_componente_ano_export <- reactive({
    dados_oci_componente_agregada()[, .(OCI = sum(OCI, na.rm = TRUE)), by = .(ANO, COMPONENTE)]
  })
  output$grafico_oci_componente_ano_csv <- handler_csv(dados_oci_componente_ano_export, "oci_componente_ano")
  output$grafico_oci_componente_mes_csv <- handler_csv(dados_oci_componente_agregada, "oci_componente_mes")

  dados_oci_especialidade_export <- reactive({
    dg <- dados_oci_especialidade_sub_geral()[, .(competencia, valor = oci, serie = "Geral")]
    ag <- dados_oci_especialidade_sub_agregada()[, .(competencia, valor = OCI, serie = as.character(ESPECIALIDADE))]
    rbind(dg, ag)
  })
  output$grafico_oci_especialidade_csv <- handler_csv(dados_oci_especialidade_export, "oci_por_especialidade")

  dados_oci_especialidade_componente_export <- reactive({
    dados_oci_especialidade_componente_agregada()[
      , .(OCI = sum(OCI, na.rm = TRUE)), by = .(ESPECIALIDADE, COMPONENTE)
    ]
  })
  output$grafico_oci_especialidade_componente_csv <- handler_csv(
    dados_oci_especialidade_componente_export, "oci_especialidade_componente"
  )

  output$grafico_oci_fisico_especialidade_csv <- handler_csv(oci_especialidade_por_ano, "oci_fisico_especialidade")
  output$grafico_oci_mensal_anos_csv <- handler_csv(dados_oci_mensal_anos, "oci_mensal_2025_2026")

  # Os botões de download ficam dentro de sub-abas que já nascem "ativas"
  # (a primeira de cada tabsetPanel). Como elas nunca disparam um evento de
  # troca de aba do Bootstrap, o Shiny não desconsidera a suspensão padrão
  # de outputs escondidos e o link de download nunca é calculado. Aqui isso
  # é desligado especificamente para esses 9 outputs (não afeta os
  # gráficos/tabelas, que continuam suspensos até a aba ser visitada).
  botoes_download <- c(
    "grafico_cirurgia_csv",
    "grafico_comparacao_anos_csv",
    "grafico_oci_componente_csv",
    "grafico_oci_componente_ano_csv",
    "grafico_oci_componente_mes_csv",
    "grafico_oci_especialidade_csv",
    "grafico_oci_especialidade_componente_csv",
    "grafico_oci_fisico_especialidade_csv",
    "grafico_oci_mensal_anos_csv"
  )
  for (id_botao in botoes_download) {
    outputOptions(output, id_botao, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
