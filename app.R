library(shiny)
library(bslib)
library(plotly)
library(DT)
library(data.table)
library(readxl)
library(stringi)
library(officer)
library(ggplot2)
library(scales)
library(rvg)

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

# Rótulos "Mês/AA" em português para eixos de data (evita locale de JS ou
# de sistema para formatar datas nos eixos).
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
  rol   = "Cirurgias Eletivas (MAC e FAEC) do ROL",
  total = "Cirurgias Eletivas (MAC e FAEC) totais",
  pnrf  = "Cirurgias Eletivas do Programa (PNRF)"
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

cores_para_anos <- function(anos) {
  anos <- sort(unique(anos))
  setNames(PALETA_ANOS[((seq_along(anos) - 1) %% length(PALETA_ANOS)) + 1], as.character(anos))
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

carregar_status_oci <- function() {
  
  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^tabela_status_OCI_planilhao_[0-9_]+\\.csv$")
  
  if (is.na(arquivo)) {
    return(NULL)
  }
  
  dt <- fread(arquivo, sep = ";", dec = ",", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt <- merge(dt, UF_REF[, .(NM_UF_OCI, REGIAO)], by.x = "NM_UF", by.y = "NM_UF_OCI", all.x = TRUE)
  dt[]
}

carregar_oci_especialidade <- function() {
  
  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_mensal_especialidade_uf\\.csv$")
  
  if (is.na(arquivo)) {
    return(NULL)
  }
  
  dt <- fread(arquivo, sep = ";", dec = ",", encoding = "UTF-8")
  dt[, NM_UF := toupper(NM_UF)]
  dt[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
  dt[]
}

# Mesmo esquema de carregar_oci_especialidade(), granularidade de município
# (usado pelo filtro de Município em "Série histórica" e "Comparativos").
carregar_oci_especialidade_municipio <- function() {
  
  arquivo <- localizar_arquivo(DIR_RESULT_OCI, "^oci_mensal_especialidade_municipio\\.csv$")
  
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
    CIRURGIA_DIR_ORIGEM, "resultados", "tabelas", "monitoramento_diagrama_controle"
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
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_rol_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_total_.*\\.csv$"),
    localizar_arquivo(origem_cirurgia, "^serie_anos_municipio_pnrf_.*\\.csv$"),
    localizar_arquivo(origem_oci, "^planilha_OCI_UF_mes_.*\\.xlsx$"),
    localizar_arquivo(origem_oci, "^tabela_status_OCI_planilhao_[0-9_]+\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_uf\\.csv$"),
    localizar_arquivo(origem_oci, "^oci_mensal_especialidade_municipio\\.csv$")
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
    cirurgia_municipio_rol = carregar_serie_municipio_cirurgia("rol"),
    cirurgia_municipio_total = carregar_serie_municipio_cirurgia("total"),
    cirurgia_municipio_pnrf = carregar_serie_municipio_cirurgia("pnrf"),
    oci_serie = carregar_serie_oci(),
    oci_status = carregar_status_oci(),
    oci_especialidade = carregar_oci_especialidade(),
    oci_especialidade_municipio = carregar_oci_especialidade_municipio()
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

#### GRÁFICOS ####
# Todos os gráficos agora são objetos ggplot puros (vetoriais). Na tela,
# viram interativos via ggplotly(); no PPTX, o MESMO objeto ggplot é
# exportado com rvg::dml() como um shape 100% editável (cor, texto,
# posição) dentro do PowerPoint.

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

# Configura o botão de download nativo do Plotly (ícone de câmera) para
# exportar PNG em alta resolução — aplicado após a conversão ggplotly().
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

grafico_cirurgia <- function(dados_uf, ano_comparacao, ano_monitoramento, titulo, metrica = "fisico") {
  
  d <- dados_uf[order(ano, mes)]
  
  faixa <- unique(
    d[, .(mes, mediana_historica, q1_historico, q3_historico, limite_inferior, limite_superior)]
  )
  setorder(faixa, mes)
  
  comp <- d[ano == ano_comparacao]
  moni <- d[ano == ano_monitoramento]
  
  com_classificacao <- metrica != "financeiro"
  
  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  fmt_valor   <- if (metrica == "financeiro") label_pt_moeda else label_pt_num
  
  comp[, texto := paste0("Mês: ", MES_LABELS[mes], "<br>", rotulo_eixo, ": ", fmt_valor(quantidade))]
  moni[, texto := paste0(
    "Mês: ", MES_LABELS[mes], "<br>", rotulo_eixo, ": ", fmt_valor(quantidade),
    if (com_classificacao) paste0("<br>", classificacao) else ""
  )]
  
  rotulo_serie_comp <- paste0("Produção ", ano_comparacao)
  rotulo_serie_moni <- paste0("Produção ", ano_monitoramento)
  
  # Todas as cores (as duas linhas de produção + os níveis de classificação)
  # ficam numa ÚNICA escala de cor (uma só scale_colour_manual no final),
  # combinando variáveis diferentes mapeadas em camadas diferentes. Isso
  # evita o ggnewscale, que o ggplotly (usado na tela) não converte bem —
  # com ele, as camadas anteriores à segunda escala somem na versão
  # interativa (mesmo aparecendo certo no PPTX, que não passa pelo ggplotly).
  comp[, serie := rotulo_serie_comp]
  moni[, serie := rotulo_serie_moni]
  
  niveis_classificacao <- c(
    "Esperado", "Acima do esperado", "Acima do limite esperado",
    "Atenção", "Crítico abaixo do limite esperado"
  )
  
  if (com_classificacao) {
    moni[, classificacao := factor(classificacao, levels = niveis_classificacao)]
    valores_cor <- c(
      setNames(c("#003366", "black"), c(rotulo_serie_comp, rotulo_serie_moni)),
      CORES_CLASSIFICACAO
    )
    quebras_legenda <- c(rotulo_serie_comp, rotulo_serie_moni, niveis_classificacao)
  } else {
    valores_cor <- setNames(c("#003366", "black"), c(rotulo_serie_comp, rotulo_serie_moni))
    quebras_legenda <- c(rotulo_serie_comp, rotulo_serie_moni)
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
      data = comp, aes(x = mes, y = quantidade, colour = serie, group = serie),
      linewidth = 1.1
    ) +
    geom_point(
      data = comp, aes(x = mes, y = quantidade, colour = serie, text = texto),
      size = 1.6
    ) +
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
  
  coluna_y <- if (metrica == "financeiro") "valor" else "quantidade"
  rotulo_eixo <- if (metrica == "financeiro") "Valor (R$)" else "Quantidade"
  fmt_valor   <- if (metrica == "financeiro") label_pt_moeda else label_pt_num
  
  d[, y_plot := get(coluna_y)]
  d[, ano_fct := factor(ano, levels = anos)]
  d[, rotulo_dado := fmt_valor(y_plot)]
  d[, texto := paste0(ano, " — Mês: ", MES_LABELS[mes], "<br>", rotulo_eixo, ": ", fmt_valor(y_plot))]
  
  destaque <- d[ano %in% c(2025, 2026)]
  
  ggplot(d, aes(x = mes, y = y_plot, colour = ano_fct, group = ano_fct)) +
    geom_line(linewidth = 1) +
    geom_point(aes(text = texto), size = 1.6) +
    geom_text(
      data = destaque, aes(label = rotulo_dado),
      vjust = -1, size = 3, show.legend = FALSE
    ) +
    scale_colour_manual(name = NULL, values = cores) +
    scale_x_continuous(breaks = 1:12, labels = MES_LABELS) +
    scale_y_continuous(labels = fmt_valor) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = rotulo_eixo) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

grafico_oci <- function(dados, titulo) {
  
  d <- dados[order(competencia)]
  d[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  d[, texto := paste0(rotulo_mes, "<br>OCI: ", label_pt_num(oci))]
  ultimo <- d[which.max(competencia)]
  
  datas_unicas <- sort(unique(d$competencia))
  
  ggplot(d, aes(x = competencia, y = oci)) +
    geom_area(fill = "#185FA5", alpha = 0.10) +
    geom_line(colour = "#185FA5", linewidth = 1.1) +
    geom_point(aes(text = texto), colour = "#185FA5", size = 0.01, alpha = 0) +
    geom_point(
      data = ultimo, aes(x = competencia, y = oci),
      shape = 21, colour = "#D85A30", fill = "white", stroke = 1.4, size = 3.2
    ) +
    geom_vline(
      xintercept = as.numeric(DATA_VIRADA_OCI),
      linetype = "dashed", colour = "#D85A30", linewidth = 0.6
    ) +
    scale_x_date(breaks = datas_unicas, labels = rotular_mes_ano_pt(datas_unicas)) +
    scale_y_continuous(labels = label_pt_num) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      plot.title = element_text(face = "bold", size = 13)
    )
}

grafico_oci_especialidade <- function(dados_geral, dados_especialidade, titulo) {
  
  dg <- dados_geral[order(competencia)]
  dg[, rotulo_mes := rotular_mes_ano_pt(competencia)]
  dg[, texto := paste0("Geral<br>", rotulo_mes, "<br>OCI: ", label_pt_num(oci))]
  
  datas_unicas <- sort(unique(dg$competencia))
  
  p <- ggplot() +
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
    labs(title = titulo, x = "Mês de competência", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = -45, hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13)
    )
}

grafico_oci_fisico_especialidade <- function(dados, titulo) {
  
  d <- dados[order(ESPECIALIDADE, ANO)]
  anos <- sort(unique(d$ANO))
  cores <- cores_para_anos(anos)
  d[, ano_fct := factor(ANO, levels = anos)]
  d[, ESPECIALIDADE := factor(ESPECIALIDADE, levels = ORDEM_ESPECIALIDADES)]
  d[, texto := paste0(ANO, "<br>", ESPECIALIDADE, "<br>OCI: ", label_pt_num(OCI))]
  
  ggplot(d, aes(x = ESPECIALIDADE, y = OCI, fill = ano_fct, text = texto)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(name = NULL, values = cores) +
    scale_y_continuous(labels = label_pt_num) +
    labs(title = titulo, x = "Especialidade", y = "OCI realizadas (físico)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
}

grafico_oci_mensal_anos <- function(dados, titulo) {
  
  d <- dados[order(ano, mes)]
  anos <- sort(unique(d$ano))
  cores <- cores_para_anos(anos)
  d[, ano_fct := factor(ano, levels = anos)]
  d[, mes_fct := factor(mes, levels = 1:12, labels = MES_LABELS)]
  d[, texto := paste0(ano, " — Mês: ", MES_LABELS[mes], "<br>OCI: ", label_pt_num(oci))]
  
  ggplot(d, aes(x = mes_fct, y = oci, fill = ano_fct, text = texto)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(name = NULL, values = cores) +
    scale_y_continuous(labels = label_pt_num) +
    expand_limits(y = 0) +
    labs(title = titulo, x = "Mês de competência", y = "OCI realizadas") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 13))
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

# Linha compacta de exportação abaixo de cada gráfico: dados brutos em CSV e
# o mesmo gráfico como slide de PowerPoint totalmente editável (rvg::dml).
barra_downloads <- function(id) {
  div(
    class = "mb-3 mt-1",
    downloadButton(paste0(id, "_csv"), "Dados (CSV)", class = "btn-sm btn-outline-secondary"),
    downloadButton(paste0(id, "_pptx"), "Slide editável (PPTX)", class = "btn-sm btn-outline-secondary")
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
           'grafico_oci', 'grafico_oci_especialidade',
           'grafico_oci_fisico_especialidade', 'grafico_oci_mensal_anos'
         ].forEach(function (id) {
           var el = document.getElementById(id);
           if (el && window.Plotly) { Plotly.Plots.resize(el); }
         });
       });"
    )),
    div(
      style = "padding: 6px 16px;",
      actionButton("atualizar", "Atualizar dados", icon = icon("rotate")),
      textOutput("data_atualizacao", inline = TRUE)
    )
  ),
  
  nav_panel(
    "Cirurgias eletivas",
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        radioButtons(
          "indicador_cirurgia", "Indicador",
          choices = c(
            "Cirurgias Eletivas (MAC e FAEC) do ROL" = "rol",
            "Cirurgias Eletivas (MAC e FAEC) totais" = "total",
            "Cirurgias Eletivas do Programa (PNRF)" = "pnrf"
          ),
          selected = "rol"
        ),
        checkboxGroupInput(
          "regiao_cirurgia", "Região",
          choices = REGIOES, selected = REGIOES
        ),
        selectInput("uf_cirurgia", "UF", choices = "BRASIL", selected = "BRASIL"),
        selectInput(
          "municipio_cirurgia", "Município",
          choices = c("Selecione uma UF" = "Todos"), selected = "Todos"
        ),
        div(
          class = "text-muted small mb-2", style = "line-height: 1.3;",
          "Filtro de Município vale só para \"Comparação Anos\" — o Diagrama de monitoramento e a Tabela continuam por UF/Região."
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
          "Filtro de Município vale para \"Série histórica\" e \"Comparativos\" — a Tabela de status continua por UF/Região."
        ),
        checkboxGroupInput(
          "especialidades_oci", "Especialidades",
          choices = ORDEM_ESPECIALIDADES, selected = ORDEM_ESPECIALIDADES
        ),
        info_fonte_dados()
      ),
      tabsetPanel(
        id = "subaba_oci",
        type = "tabs",
        tabPanel(
          "Série histórica OCI",
          br(),
          h5("OCI geral"),
          plotlyOutput("grafico_oci", height = "40vh"),
          barra_downloads("grafico_oci"),
          br(),
          h5("Por especialidade"),
          plotlyOutput("grafico_oci_especialidade", height = "44vh"),
          barra_downloads("grafico_oci_especialidade")
        ),
        tabPanel(
          "Comparativos",
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
        ),
        tabPanel(
          "Tabela",
          br(),
          h5("Status por UF (variação em relação ao mês anterior)"),
          DTOutput("tabela_status_oci")
        )
      )
    )
  )
)

#### SERVER ####

server <- function(input, output, session) {
  
  dados <- reactiveVal(carregar_tudo())
  
  observeEvent(input$atualizar, {
    dados(carregar_tudo())
  })
  
  output$data_atualizacao <- renderText({
    serie_oci <- dados()$oci_serie
    if (is.null(serie_oci)) {
      return("")
    }
    paste0(" | Dados até a competência ", format(max(serie_oci$competencia, na.rm = TRUE), "%m/%Y"))
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
    
    req(input$indicador_cirurgia)
    validate(need(
      input$indicador_cirurgia != "pnrf",
      "Diagrama de monitoramento não disponível para o Programa (PNRF) — o histórico ainda é curto demais para gerar faixas de controle confiáveis. Use \"Comparação Anos\" ou a \"Tabela\" para acompanhar o PNRF."
    ))
    
    serie <- serie_cirurgia_indicador()
    validate(need(!is.null(serie), "Série de cirurgias não encontrada. Rode o script 02_monitoramento_diagrama_controle.R no projeto Cirurgia."))
    req(input$uf_cirurgia)
    validate(need(length(input$regiao_cirurgia) > 0, "Selecione ao menos uma região."))
    
    todas_regioes <- setequal(input$regiao_cirurgia, REGIOES)
    
    dados_uf <- if (input$uf_cirurgia == "BRASIL") {
      if (todas_regioes) serie[uf_atendimento == "BRASIL"] else agregar_cirurgia_regiao(serie, input$regiao_cirurgia)
    } else {
      serie[uf_atendimento == input$uf_cirurgia]
    }
    validate(need(nrow(dados_uf) > 0, "Sem dados para a UF selecionada."))
    dados_uf
  })
  
  # Objeto ggplot do Diagrama de monitoramento, reativo e reutilizado tanto
  # pela tela (via ggplotly) quanto pelo downloadHandler do PPTX (rvg::dml).
  plot_cirurgia <- reactive({
    
    dados_uf <- dados_diagrama_cirurgia()
    
    ano_comparacao   <- min(dados_uf$ano)
    ano_monitoramento <- max(dados_uf$ano)
    
    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[input$indicador_cirurgia]]
    rotulo_uf <- if (input$uf_cirurgia == "BRASIL") rotulo_agregado(input$regiao_cirurgia) else input$uf_cirurgia
    
    grafico_cirurgia(
      dados_uf, ano_comparacao, ano_monitoramento,
      titulo = paste0(rotulo_indicador, " — ", rotulo_uf),
      metrica = input$metrica_cirurgia_diagrama
    )
  })
  
  output$grafico_cirurgia <- renderPlotly({
    ggplotly(plot_cirurgia(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.25)) |>
      alta_resolucao("diagrama_monitoramento_cirurgias")
  })
  
  serie_anos_cirurgia_indicador <- reactive({
    req(input$indicador_cirurgia)
    dados()[[paste0("cirurgia_anos_", input$indicador_cirurgia)]]
  })
  
  serie_municipio_cirurgia_indicador <- reactive({
    req(input$indicador_cirurgia)
    dados()[[paste0("cirurgia_municipio_", input$indicador_cirurgia)]]
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
  
  # Dados por trás do gráfico "Comparação Anos" e da tabela abaixo dele —
  # reactive compartilhada para não duplicar a lógica de filtro.
  dados_comparacao_anos <- reactive({
    
    serie <- serie_anos_cirurgia_indicador()
    validate(need(!is.null(serie), "Série multianual não encontrada. Rode o script 02_monitoramento_diagrama_controle.R no projeto Cirurgia."))
    req(input$uf_cirurgia)
    validate(need(length(input$regiao_cirurgia) > 0, "Selecione ao menos uma região."))
    
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
  
  # Objeto ggplot do gráfico "Comparação Anos", reativo e reutilizado tanto
  # pela tela (via ggplotly) quanto pelo downloadHandler do PPTX (rvg::dml).
  plot_comparacao_anos <- reactive({
    
    dados_uf <- dados_comparacao_anos()
    req(input$metrica_cirurgia_anos)
    
    rotulo_indicador <- ROTULOS_INDICADOR_CIRURGIA[[input$indicador_cirurgia]]
    
    grafico_comparacao_anos(
      dados_uf,
      titulo = paste0(rotulo_indicador, " — Comparação entre anos — ", rotulo_local_cirurgia()),
      metrica = input$metrica_cirurgia_anos
    )
  })
  
  output$grafico_comparacao_anos <- renderPlotly({
    ggplotly(plot_comparacao_anos(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.2)) |>
      alta_resolucao("comparacao_anos_cirurgias")
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
  observeEvent(list(input$uf_oci, dados()$oci_especialidade_municipio), {
    
    base_mun <- dados()$oci_especialidade_municipio
    
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
  # reaproveitada pelo gráfico por especialidade (linha "Geral") e pelo
  # comparativo mensal 2025 x 2026. Quando um município está selecionado,
  # deriva o "geral" somando as especialidades conhecidas na base de
  # município (não há export separado de OCI geral por município).
  oci_geral_filtrada <- reactive({
    
    if (municipio_oci_ativo()) {
      base_mun <- dados()$oci_especialidade_municipio
      validate(need(!is.null(base_mun), "Tabela de OCI por especialidade e município não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
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
  
  # Base de OCI por especialidade já filtrada por UF/região (ou município),
  # antes do filtro de especialidades marcadas (aplicado individualmente em
  # cada gráfico, já que "Comparativos" ignora o toggle de especialidade e
  # usa a série geral para o comparativo mensal).
  oci_especialidade_filtrada <- reactive({
    
    if (municipio_oci_ativo()) {
      base_mun <- dados()$oci_especialidade_municipio
      validate(need(!is.null(base_mun), "Tabela de OCI por especialidade e município não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
      return(base_mun[NM_UF == input$uf_oci & MUNICIPIO == input$municipio_oci])
    }
    
    base <- dados()$oci_especialidade
    validate(need(!is.null(base), "Tabela de OCI por especialidade não encontrada. Rode o script Monitoramento_oci_uf_2025_2026.R no projeto OCI."))
    req(input$uf_oci)
    validate(need(length(input$regiao_oci) > 0, "Selecione ao menos uma região."))
    
    if (input$uf_oci == "BRASIL") {
      base[REGIAO %in% input$regiao_oci]
    } else {
      base[NM_UF == input$uf_oci]
    }
  })
  
  # Objeto ggplot de OCI geral, reativo e reutilizado tanto pela tela (via
  # ggplotly) quanto pelo downloadHandler do PPTX (rvg::dml).
  plot_oci <- reactive({
    
    dados_grafico <- oci_geral_filtrada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))
    
    rotulo_uf <- rotulo_local_oci()
    
    grafico_oci(dados_grafico, titulo = paste0("OCI realizadas — ", rotulo_uf))
  })
  
  output$grafico_oci <- renderPlotly({
    ggplotly(plot_oci(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      alta_resolucao("diagrama_monitoramento_oci")
  })
  
  dados_oci_especialidade_agregada <- reactive({
    
    especialidades_sel <- input$especialidades_oci
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))
    
    agregada <- oci_especialidade_filtrada()[
      ESPECIALIDADE %chin% especialidades_sel,
      .(OCI = sum(OCI, na.rm = TRUE)),
      by = .(ANO, MES, ESPECIALIDADE)
    ]
    agregada[, competencia := as.Date(sprintf("%04d-%02d-01", ANO, MES))]
    agregada[]
  })
  
  # Objeto ggplot de OCI por especialidade, reativo e reutilizado tanto pela
  # tela (via ggplotly) quanto pelo downloadHandler do PPTX (rvg::dml).
  plot_oci_especialidade <- reactive({
    
    dados_geral <- oci_geral_filtrada()
    validate(need(nrow(dados_geral) > 0, "Sem dados para a seleção atual."))
    
    grafico_oci_especialidade(
      dados_geral, dados_oci_especialidade_agregada(),
      titulo = paste0("OCI por especialidade — ", rotulo_local_oci())
    )
  })
  
  output$grafico_oci_especialidade <- renderPlotly({
    ggplotly(plot_oci_especialidade(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.3)) |>
      alta_resolucao("oci_por_especialidade")
  })
  
  # Base física + financeira por especialidade/ano, reaproveitada pelo
  # gráfico físico e pela tabela financeira em "Comparativos".
  oci_especialidade_por_ano <- reactive({
    
    especialidades_sel <- input$especialidades_oci
    validate(need(length(especialidades_sel) > 0, "Selecione ao menos uma especialidade."))
    
    agregada <- oci_especialidade_filtrada()[
      ESPECIALIDADE %chin% especialidades_sel,
      .(OCI = sum(OCI, na.rm = TRUE), VALOR = sum(VALOR, na.rm = TRUE)),
      by = .(ANO, ESPECIALIDADE)
    ]
    validate(need(nrow(agregada) > 0, "Sem dados para a seleção atual."))
    
    agregada
  })
  
  # Objeto ggplot de OCI físico por especialidade, reativo e reutilizado
  # tanto pela tela (via ggplotly) quanto pelo downloadHandler do PPTX.
  plot_oci_fisico_especialidade <- reactive({
    
    rotulo_uf <- rotulo_local_oci()
    
    grafico_oci_fisico_especialidade(
      oci_especialidade_por_ano(),
      titulo = paste0("Físico por especialidade — ", rotulo_uf)
    )
  })
  
  output$grafico_oci_fisico_especialidade <- renderPlotly({
    ggplotly(plot_oci_fisico_especialidade(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.25)) |>
      alta_resolucao("oci_fisico_especialidade")
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
  
  dados_oci_mensal_anos <- reactive({
    
    dados_grafico <- oci_geral_filtrada()
    validate(need(nrow(dados_grafico) > 0, "Sem dados para a seleção atual."))
    
    dados_grafico[
      , .(oci = sum(oci, na.rm = TRUE)),
      by = .(
        ano = as.integer(format(competencia, "%Y")),
        mes = as.integer(format(competencia, "%m"))
      )
    ]
  })
  
  # Objeto ggplot do comparativo mensal 2025 vs 2026, reativo e reutilizado
  # tanto pela tela (via ggplotly) quanto pelo downloadHandler do PPTX.
  plot_oci_mensal_anos <- reactive({
    grafico_oci_mensal_anos(
      dados_oci_mensal_anos(),
      titulo = paste0("Produção mensal — 2025 vs 2026 — ", rotulo_local_oci())
    )
  })
  
  output$grafico_oci_mensal_anos <- renderPlotly({
    ggplotly(plot_oci_mensal_anos(), tooltip = "text") |>
      limpar_legenda_plotly() |>
      layout(legend = list(orientation = "h", y = -0.2)) |>
      alta_resolucao("oci_mensal_2025_2026")
  })
  
  output$tabela_status_oci <- renderDT({
    
    status <- dados()$oci_status
    validate(need(!is.null(status), ""))
    
    status <- status[REGIAO %in% input$regiao_oci]
    
    datatable(
      status[, .(NM_UF, ultimo, anterior, variacao_ultimo_mes, tendencia, semaforo, nivel)],
      colnames = c("UF", "Último mês", "Mês anterior", "Variação", "Tendência", "Semáforo", "Nível"),
      rownames = FALSE,
      options = list(pageLength = 10, order = list(list(1, "desc")))
    ) |>
      formatRound(c("ultimo", "anterior"), digits = 0, mark = ".", interval = 3) |>
      formatPercentage("variacao_ultimo_mes", 1)
  })
  
  ## ---- Exportação de dados (CSV) e slides (PPTX) dos 6 gráficos ----
  # Cada gráfico expõe os mesmos dados/objeto usados para plotar (nenhum
  # recálculo — os reactives plot_*() acima já foram usados no renderPlotly).
  
  output$grafico_cirurgia_csv <- handler_csv(dados_diagrama_cirurgia, "diagrama_monitoramento_cirurgias")
  output$grafico_cirurgia_pptx <- handler_pptx(plot_cirurgia, "diagrama_monitoramento_cirurgias")
  
  output$grafico_comparacao_anos_csv <- handler_csv(dados_comparacao_anos, "comparacao_anos_cirurgias")
  output$grafico_comparacao_anos_pptx <- handler_pptx(plot_comparacao_anos, "comparacao_anos_cirurgias")
  
  output$grafico_oci_csv <- handler_csv(oci_geral_filtrada, "oci_geral")
  output$grafico_oci_pptx <- handler_pptx(plot_oci, "oci_geral")
  
  dados_oci_especialidade_export <- reactive({
    dg <- oci_geral_filtrada()[, .(competencia, valor = oci, serie = "Geral")]
    ag <- dados_oci_especialidade_agregada()[, .(competencia, valor = OCI, serie = as.character(ESPECIALIDADE))]
    rbind(dg, ag)
  })
  output$grafico_oci_especialidade_csv <- handler_csv(dados_oci_especialidade_export, "oci_por_especialidade")
  output$grafico_oci_especialidade_pptx <- handler_pptx(plot_oci_especialidade, "oci_por_especialidade")
  
  output$grafico_oci_fisico_especialidade_csv <- handler_csv(oci_especialidade_por_ano, "oci_fisico_especialidade")
  output$grafico_oci_fisico_especialidade_pptx <- handler_pptx(plot_oci_fisico_especialidade, "oci_fisico_especialidade")
  
  output$grafico_oci_mensal_anos_csv <- handler_csv(dados_oci_mensal_anos, "oci_mensal_2025_2026")
  output$grafico_oci_mensal_anos_pptx <- handler_pptx(plot_oci_mensal_anos, "oci_mensal_2025_2026")
  
  # Os botões de download ficam dentro de sub-abas que já nascem "ativas"
  # (a primeira de cada tabsetPanel). Como elas nunca disparam um evento de
  # troca de aba do Bootstrap, o Shiny não desconsidera a suspensão padrão
  # de outputs escondidos e o link de download nunca é calculado. Aqui isso
  # é desligado especificamente para esses 12 outputs (CSV + PPTX de cada um
  # dos 6 gráficos) — não afeta os gráficos/tabelas, que continuam
  # suspensos até a aba ser visitada.
  botoes_download <- c(
    "grafico_cirurgia_csv", "grafico_cirurgia_pptx",
    "grafico_comparacao_anos_csv", "grafico_comparacao_anos_pptx",
    "grafico_oci_csv", "grafico_oci_pptx",
    "grafico_oci_especialidade_csv", "grafico_oci_especialidade_pptx",
    "grafico_oci_fisico_especialidade_csv", "grafico_oci_fisico_especialidade_pptx",
    "grafico_oci_mensal_anos_csv", "grafico_oci_mensal_anos_pptx"
  )
  for (id_botao in botoes_download) {
    outputOptions(output, id_botao, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)