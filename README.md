# Painel para monitoramento da produção de Cirurgias Eletivas e OCI

Painel em **R Shiny** para acompanhar mensalmente a produção de **Cirurgias Eletivas** (ROL, Total e Programa PATE/PNRF) e de **Ofertas de Cuidados Integrados (OCI)**, comparando com parâmetros históricos e classificando o desempenho por UF. Cada indicador de cirurgias tem visão Físico e Financeiro (R$).

## Funcionalidades

**Cirurgias eletivas**
- Diagrama de controle: faixa histórica (min-máx e Q1-Q3), mediana, produção do ano de comparação e do ano monitorado, com classificação mensal em 5 níveis (Esperado, Acima do esperado, Acima do limite esperado, Atenção, Crítico abaixo do limite esperado) — disponível para ROL e Total (Físico e Financeiro) e, dentro do ROL, também por Especialidade. PNRF não tem diagrama (histórico ainda curto demais para faixas confiáveis); o modo Financeiro não mostra classificação por cor (só a linha de produção).
- Filtros: indicador (MAC e FAEC totais / do Rol / Ciru. PATE-PNRF), Região (múltipla escolha em dropdown) e UF lado a lado, Município (só "Comparação Anos"), e — só para o indicador ROL — Especialidade e Procedimento, cruzando com `dados/Relacao_cirugiasROL.xlsx`. Especialidade vale para "Comparação Anos" e "Diagrama de monitoramento"; Procedimento (multisseleção, soma os selecionados) só para "Comparação Anos".
- Abas "Comparação Anos", "Diagrama de monitoramento" e "Tabela" (classificação do último mês por UF, sempre Físico, independente do toggle).

**OCI realizadas**
- Todas as análises usam mês de **atendimento** (`COMPETENCIA_ATENDIMENTO`, mês em que a OCI foi efetivamente realizada), não mês de processamento;
- Aba "Série histórica OCI": série mensal geral, com marcação da virada de ano de referência, e um gráfico "Por componente" logo abaixo — uma linha por componente/modalidade (Componente Ambulatorial, Carretas, Créditos Financeiros, Equipes Volantes) mais o Total geral, cada uma podendo ser ligada/desligada clicando na legenda;
- Aba "Série histórica OCI por especialidade": mesmo gráfico "Geral + por especialidade" de antes, mas isolado numa subaba própria, com filtro de Especialidade e um filtro de Componente (OCI geral ou um componente específico) que só existem ali;
- Aba "Comparativos": Físico/Financeiro por especialidade (com seu próprio filtro de Especialidade, independente do da subaba anterior) e produção mensal 2025 vs 2026;
- Aba "Tabela": status por UF (variação em relação ao mês anterior, tendência, semáforo, nível);
- Mesma lógica de filtro por região/UF/município e agregado dinâmico do painel de cirurgias.

## Fonte dos dados

Os dados têm origem pública, disponibilizados pelo Ministério da Saúde na plataforma **SUS360**:

- [Componente Ambulatorial (OCI)](https://sus360.saude.gov.br/#painel/componente-ambulatorial)
- [Cirurgias Eletivas](https://sus360.saude.gov.br/painel/cirurgias/)

Este painel **não acessa o SUS360 diretamente**. Ele consome os arquivos já processados e padronizados pelos projetos irmãos `Cirurgia` e `OCI`, que fazem esse tratamento a partir das bases brutas — com uma exceção: `dados/Relacao_cirugiasROL.xlsx` (mapeamento Código SIGTAP → Especialidade, usado nos filtros de Especialidade/Procedimento) é mantido manualmente dentro deste projeto, não vem do SUS360 nem é gerado por script.

## Como os dados chegam ao painel

```
Cirurgia/resultados/tabelas/monitoramento_diagrama_controle/
    serie_completa_{rol,total,pnrf}_*.csv
    serie_completa_financeiro_{rol,total,pnrf}_*.csv
    serie_anos_{rol,total,pnrf}_*.csv
    serie_anos_municipio_{rol,total,pnrf}_*.csv
Cirurgia/resultados/bases_processadas/
    cirurgias_mensal_procedimento_rol_*.csv
OCI/resultados/
    planilha_OCI_UF_mes_*.xlsx
    tabela_status_OCI_planilhao_*.csv
    oci_mensal_especialidade_{uf,municipio}.csv
    oci_mensal_especialidade_componente_{uf,municipio}.csv
                          │
                          ▼   sincronizado ao abrir o app ou clicar em "Atualizar dados"
              Painel_Ciru_OCI/dados/processados/

Painel_Ciru_OCI/dados/Relacao_cirugiasROL.xlsx   ← mantido manualmente, não sincroniza sozinho
```

`dados/processados/` é a principal fonte que o `app.R` lê (mais o `Relacao_cirugiasROL.xlsx` acima, que fica direto em `dados/`). A pasta `processados/` é ignorada pelo Git (não vai para o GitHub) e existe em dois estados:

- **Local**: o app copia automaticamente a versão mais recente das planilhas dos projetos irmãos toda vez que abre, ou quando "Atualizar dados" é clicado — desde que `Cirurgia/` e `OCI/` estejam na mesma pasta pai deste projeto.
- **Publicado no shinyapps.io**: como o servidor não tem acesso às pastas irmãs, ele usa a última cópia enviada junto no deploy.

## Estrutura de pastas

```text
1. GitHub_SAES/
├── Cirurgia/
├── OCI/
└── Painel_Ciru_OCI/
    ├── app.R
    ├── dados/
    │   ├── Relacao_cirugiasROL.xlsx   # mantido manualmente, não ignorado pelo Git
    │   └── processados/               # gerado automaticamente, ignorado pelo Git
    ├── README.md
    ├── .gitignore
    └── .gitattributes
```

## Executar localmente

Abra `Painel_Ciru_OCI.Rproj` no RStudio e rode:

```r
shiny::runApp()
```

Na primeira execução, se os projetos `Cirurgia` e `OCI` já tiverem sido processados (scripts `01_unificar_bases_cirurgias.R`/`02_monitoramento_diagrama_controle.R` no projeto Cirurgia, e `Monitoramento_oci_uf_2025_2026.R` no projeto OCI), os dados são sincronizados automaticamente para `dados/processados/`.

Depende do pacote `shinyWidgets` (dropdown de Região com múltipla escolha), além de shiny, bslib, plotly, DT, data.table, readxl e stringi.

## Publicar / atualizar no shinyapps.io

1. Rode os scripts de tratamento em `Cirurgia` e `OCI` para atualizar a competência mais recente;
2. Abra e rode este app localmente uma vez, para sincronizar `dados/processados/`;
3. No RStudio, com `app.R` aberto, clique em **Publish**. Se for a primeira vez numa conta nova (sem deploy anterior), use a seta ao lado do botão Publish → **Other Destination** para poder escolher a conta antes de criar o app — clicar direto no botão tende a reaproveitar a última conta/app usados;
4. Confirme que **todos os arquivos de `dados/processados/`** estão marcados para envio, mais o `dados/Relacao_cirugiasROL.xlsx` (ele não é gerado automaticamente, então só vai junto se for marcado manualmente) — e nenhum `.RData`, se aparecer.

Publicado em [`https://cginfo.shinyapps.io/Painel_Ciru_OCI/`](https://cginfo.shinyapps.io/Painel_Ciru_OCI/)

## Status

Painel em uso interno — dados sujeitos a alterações conforme atualização das bases do SUS360.
