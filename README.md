# Painel para monitoramento da produção física e financeira ATE

Painel em **R Shiny** para acompanhar mensalmente a produção física e financeira de **Cirurgias Eletivas** (ROL, Total e Programa PATE/PNRF), das **Ofertas de Cuidados Integrados (OCI)** e do **Pagamento Portaria 9810**.

## Funcionalidades

**Cirurgias eletivas**
- Diagrama de controle: faixa histórica 2022–2025 (min-máx e Q1-Q3), mediana e produção do ano monitorado (2026; 2025 já compõe a faixa histórica, sem linha própria), com classificação mensal em 5 níveis (Esperado → Crítico abaixo do limite). Disponível para ROL/Total (Físico e Financeiro) e, no ROL, também por Especialidade. PNRF (soma de `faec_rol_pnrf_sim`, `faec_naorol_pnrf` e `faec_rol_pnrf_nao`) não tem diagrama (histórico curto); o modo Financeiro não classifica por cor.
- Filtros: indicador (MAC e FAEC totais / Rol / Ciru. PATE-PNRF), Região (multisseleção) + UF, Município e Gestão Estadual/Municipal (só "Comparação Anos", Físico e Financeiro; gestão do estabelecimento (Estadual/Municipal/Dupla; Dupla só no Físico); não se combina com Especialidade/Procedimento e não gera diagrama), e — só no ROL — Especialidade/Procedimento, cruzando com `dados/Relacao_cirugiasROL.xlsx` (Especialidade vale para "Comparação Anos" e "Diagrama"; Procedimento, multisseleção, só para "Comparação Anos").
- Checkbox "Cirurgias Eletivas PAB" substitui o indicador do dropdown **só em "Comparação Anos"** (o Diagrama continua sem PAB); PAB não tem financeiro, então o toggle Financeiro fica bloqueado quando marcado.
- Abas "Comparação Anos" e "Diagrama de monitoramento".
- O card "Dados até a competência" mostra o último mês com produção na série de Cirurgias e, abaixo, o mês em que essa base foi gerada (`dados/processados/atualizacao_cirurgia.txt`, gravado no sync — vai junto no deploy).

**OCI realizadas**
- Usa mês de **atendimento** (`COMPETENCIA_ATENDIMENTO`), não mês de processamento.
- Gráficos mensais ("OCI geral", "Por mês de atendimento", "OCI por especialidade") sombreiam os últimos 3 meses com "Dados preliminares" — a base ainda não fechou essas competências (`sombra_dados_preliminares()`/`anotacao_dados_preliminares()` em `app.R`).
- "Série histórica OCI": gráfico "OCI geral" (Total + uma linha por componente/modalidade, liga/desliga pela legenda, com marcação da virada de ano) e, abaixo, dois empilhados por componente — "Por ano" e "Por mês de atendimento".
- "Série histórica OCI por especialidade": Geral + especialidade numa subaba própria, com filtros de Especialidade e Componente; abaixo, colunas empilhadas "Por especialidade e componente" (sempre pelos 4 componentes).
- "Comparativos": Físico/Financeiro por especialidade e produção mensal 2025 vs 2026, com filtros próprios de Especialidade e Componente (o comparativo mensal ignora Especialidade, mas respeita Componente).
- Mesma lógica de filtro por região/UF/município do painel de cirurgias.
- Todo valor financeiro de OCI é o **valor federal de referência** (valor aprovado − complemento do gestor local; só SIA — o CMD não tem valor). A análise é sempre por local/competência de **atendimento**; registros sem UF de atendimento (contatos do CMD) ficam em "NÃO INFORMADA" e entram só no total Brasil com as 5 regiões marcadas.

**Pagamento Portaria 9810**
- Repasses da Portaria GM/MS nº 9.810/2025 (Agora Tem Especialistas). Só entram linhas com `NU_PORTARIA` 09810/9810 e só o Valor Líquido.
- Filtros: UF, Município (cascata), Tipo de Gestão e Componente — este último resumido em 5 rótulos a partir da coluna `PROGRAMA` (ver `mapear_programa_portaria9810()`); os com "*" são Despesa de Exercício Anterior.
- Usa mês de **pagamento**, não mês de competência.
- "Pagamentos": total por mês empilhado por Tipo de Gestão, linha por Componente, tabela por UF (Estadual/Municipal/Total).
- "Limite da Portaria 9810": pago x limite por UF inteira (`dados/PORTARIA_9.810_UF.xlsx`, soma Estadual+Municipal — Município e Tipo de Gestão não se aplicam aqui), com destaque para quem ultrapassou.

**Variação Cirurgias**
- Aba trazida do projeto irmão `Analise_Espacial/app_semaforo` — mapas coropléticos da variação % de procedimentos entre 2025 e 2026, em 5 faixas de cor (Queda forte → Alta forte).
- O subtítulo mostra o período comparado (ex.: jan–mai/2026 vs. jan–mai/2025), deduzido comparando o total de 2025 do mapa com o acumulado da série de Cirurgias. Para atualizar até um mês novo, rode `Analise_Espacial/dados_preparar.R` com os CSVs mensais em `bases/cirur_360/`.
- 3 mapas: **Brasil** (por UF, sem filtro), **Regiões de Saúde** (filtro de UF) e **Municípios** (filtro de UF e Município, multisseleção — acima de 20 municípios, os rótulos de % somem para não poluir).
- Cada mapa tem seu próprio download em PPT editável e tabela em Excel.
- Renderizado como imagem estática (`renderPlot`, não Plotly).

## Exportação dos gráficos

Todo gráfico tem, logo abaixo, **Dados (CSV)** (dados brutos) e **Slide editável (PPTX)** — o gráfico como slide de PowerPoint totalmente editável, não uma imagem (`ggplot2` + `rvg::dml()` via `officer`). O ícone de câmera do gráfico continua exportando PNG em alta resolução.

## Fonte dos dados

Dados públicos do Ministério da Saúde, plataforma **SUS360**: [Componente Ambulatorial (OCI)](https://sus360.saude.gov.br/#painel/componente-ambulatorial) e [Cirurgias Eletivas](https://sus360.saude.gov.br/painel/cirurgias/).

Este painel **não acessa o SUS360 diretamente**: consome os arquivos já processados pelos projetos irmãos `Cirurgia` e `OCI`, com três exceções mantidas manualmente (não vêm do SUS360 nem de script): `dados/Relacao_cirugiasROL.xlsx` (Código SIGTAP → Especialidade), `dados/BaseValorliquidoPortaria9810.xlsx` e `dados/PORTARIA_9.810_UF.xlsx` (pagamentos e limite da Portaria 9.810).

A aba "Variação Cirurgias" é diferente: os 3 mapas (`.gpkg`) vêm de um terceiro projeto irmão, `Analise_Espacial/app_semaforo`, já prontos com geometria e variação % calculada — este painel só copia e lê.

## Como os dados chegam ao painel

```
Cirurgia/resultados/02_monitoramento/tabelas/      serie_completa_*, serie_anos_* (inclui serie_anos_municipio_gestao_*, do script 10)
Cirurgia/resultados/bases_processadas/fisico/      cirurgias_mensal_procedimento_rol_*.csv
OCI/resultados/                                    planilha_OCI_UF_mes_*.xlsx, oci_mensal_especialidade_componente_*.csv
Analise_Espacial/app_semaforo/dados/               tab_br/tab_regiao/tab_municipio *.gpkg
                          │
                          ▼   sincronizado toda vez que o app abre (sessão nova)
              Painel_Ciru_OCI/dados/processados/   (.gpkg ficam em processados/semaforo/)

Painel_Ciru_OCI/dados/Relacao_cirugiasROL.xlsx           ← mantido manualmente, não sincroniza sozinho
Painel_Ciru_OCI/dados/BaseValorliquidoPortaria9810.xlsx  ← mantido manualmente, não sincroniza sozinho
Painel_Ciru_OCI/dados/PORTARIA_9.810_UF.xlsx             ← mantido manualmente, não sincroniza sozinho
```

`dados/processados/` (ignorada pelo Git) é a principal fonte que o `app.R` lê:

- **Local**: sincronizada automaticamente a cada abertura do app, desde que `Cirurgia/`, `OCI/` e `Analise_Espacial/` estejam na mesma pasta pai deste projeto.
- **Publicado no shinyapps.io**: usa a última cópia enviada no deploy (sem acesso às pastas irmãs).

## Estrutura de pastas

```text
1. GitHub_SAES/
├── Cirurgia/
├── OCI/
├── Analise_Espacial/
│   └── app_semaforo/
│       └── dados/                             # tab_br/tab_regiao/tab_municipio *.gpkg
└── Painel_Ciru_OCI/
    ├── app.R
    ├── dados/
    │   ├── Relacao_cirugiasROL.xlsx           # mantido manualmente, não ignorado pelo Git
    │   ├── BaseValorliquidoPortaria9810.xlsx  # mantido manualmente, não ignorado pelo Git
    │   ├── PORTARIA_9.810_UF.xlsx             # mantido manualmente, não ignorado pelo Git
    │   └── processados/                       # gerado automaticamente, ignorado pelo Git
    │       └── semaforo/                      # os 3 .gpkg copiados de Analise_Espacial
    ├── README.md
    ├── .gitignore
    └── .gitattributes
```

## Executar localmente

Abra `Painel_Ciru_OCI.Rproj` no RStudio e rode:

```r
shiny::runApp()
```

Na primeira execução, se `Cirurgia` e `OCI` já tiverem sido processados (scripts `01_unificar_bases_cirurgias.R`/`02_monitoramento_diagrama_controle.R`/`10_series_gestao_comparacao_anos.R` e `Monitoramento_oci_uf_2025_2026.R`), os dados sincronizam automaticamente para `dados/processados/`.

Pacotes: shiny, bslib, plotly, DT, data.table, readxl, stringi, `shinyWidgets` (dropdown de Região), `officer`/`ggplot2`/`scales`/`rvg` (exportação PPTX) e `sf`/`dplyr`/`shinycssloaders`/`writexl` (mapas e Excel da aba "Variação Cirurgias").

## Publicar / atualizar no shinyapps.io

1. Rode os scripts de tratamento em `Cirurgia` e `OCI` para atualizar a competência mais recente;
2. Abra e rode este app localmente uma vez, para sincronizar `dados/processados/`;
3. No RStudio, com `app.R` aberto, clique em **Publish**. Numa conta nova (sem deploy anterior), use a seta ao lado do botão → **Other Destination** para escolher a conta antes de criar o app — clicar direto tende a reaproveitar a última conta/app usados;
4. Confirme que **todos os arquivos de `dados/processados/`** estão marcados — incluindo `processados/semaforo/` (3 `.gpkg`, ~23MB, fácil de esquecer) — mais os três xlsx manuais de `dados/` — e nenhum `.RData`, se aparecer.

Publicado em [`https://cginfo.shinyapps.io/Painel_Ciru_OCI/`](https://cginfo.shinyapps.io/Painel_Ciru_OCI/)

## Status

Painel em uso interno — dados sujeitos a alterações conforme atualização das bases do SUS360.
