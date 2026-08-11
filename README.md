# Painel para monitoramento da produção de Cirurgias Eletivas e OCI

Painel em **R Shiny** para acompanhar mensalmente a produção de **Cirurgias Eletivas** (ROL e Total) e de **Ofertas de Cuidados Integrados (OCI)**, comparando com parâmetros históricos e classificando o desempenho por UF.

## Funcionalidades

**Cirurgias eletivas**
- Diagrama de controle interativo: faixa histórica (min-máx e Q1-Q3), mediana, produção do ano de comparação e do ano monitorado;
- Classificação mensal por UF em 5 níveis (Esperado, Acima do esperado, Acima do limite esperado, Atenção, Crítico abaixo do limite esperado), com legenda de cores no gráfico;
- Filtros por indicador (ROL/Total), região e UF;
- Abas "Gráfico" e "Tabela" para alternar entre a visualização e a classificação detalhada por UF.

**OCI realizadas**
- Série mensal de produção, com marcação da virada de ano de referência;
- Tabela de status por UF (variação em relação ao mês anterior, tendência, semáforo, nível);
- Mesma lógica de filtro por região/UF e agregado dinâmico do painel de cirurgias.

## Fonte dos dados

Os dados têm origem pública, disponibilizados pelo Ministério da Saúde na plataforma **SUS360**:

- [Componente Ambulatorial (OCI)](https://sus360.saude.gov.br/#painel/componente-ambulatorial)
- [Cirurgias Eletivas](https://sus360.saude.gov.br/painel/cirurgias/)

Este painel **não acessa o SUS360 diretamente**. Ele consome apenas os arquivos já processados e padronizados pelos projetos irmãos `Cirurgia` e `OCI`, que fazem esse tratamento a partir das bases brutas.

## Como os dados chegam ao painel

```
Cirurgia/resultados/tabelas/monitoramento_diagrama_controle/serie_completa_{rol,total}_*.csv
OCI/resultados/planilha_OCI_UF_mes_*.xlsx
OCI/resultados/tabela_status_OCI_planilhao_*.csv
                          │
                          ▼   sincronizado ao abrir o app ou clicar em "Atualizar dados"
              Painel_Ciru_OCI/dados/processados/
```

`dados/processados/` é a única fonte que o `app.R` lê. Essa pasta é ignorada pelo Git (não vai para o GitHub) e existe em dois estados:

- **Local**: o app copia automaticamente a versão mais recente das planilhas dos projetos irmãos toda vez que abre, ou quando "Atualizar dados" é clicado — desde que `Cirurgia/` e `OCI/` estejam na mesma pasta pai deste projeto.
- **Publicado no shinyapps.io**: como o servidor não tem acesso às pastas irmãs, ele usa a última cópia enviada junto no deploy.

## Estrutura de pastas

```text
1. GitHub_SAES/
├── Cirurgia/
├── OCI/
└── Painel_Ciru_OCI/
    ├── app.R
    ├── dados/processados/     # gerado automaticamente, ignorado pelo Git
    ├── README.md
    ├── .gitignore
    └── .gitattributes
```

## Executar localmente

Na primeira vez, instale as dependências (inclui `officer`/`mschart`, usados na exportação de gráficos em PowerPoint editável):

```r
install.packages(c(
  "shiny", "bslib", "plotly", "DT", "data.table",
  "readxl", "stringi", "officer", "mschart"
))
```

Abra `Painel_Ciru_OCI.Rproj` no RStudio e rode:

```r
shiny::runApp()
```

Na primeira execução, se os projetos `Cirurgia` e `OCI` já tiverem sido processados (scripts `01_unificar_bases_cirurgias.R`/`02_monitoramento_diagrama_controle.R` no projeto Cirurgia, e `Monitoramento_oci_uf_2025_2026.R` no projeto OCI), os dados são sincronizados automaticamente para `dados/processados/`.

## Publicar / atualizar no shinyapps.io

1. Rode os scripts de tratamento em `Cirurgia` e `OCI` para atualizar a competência mais recente;
2. Abra e rode este app localmente uma vez, para sincronizar `dados/processados/`;
3. No RStudio, com `app.R` aberto, clique em **Publish** — como o app já existe no shinyapps.io, ele é atualizado no mesmo link;
4. Confirme que os 4 arquivos de `dados/processados/` estão marcados para envio (e nenhum `.RData`, se aparecer).

Publicado em `https://felipecotrim.shinyapps.io/Painel_Ciru_OCI/`

## Status

Painel em uso interno — dados sujeitos a alterações conforme atualização das bases do SUS360.
