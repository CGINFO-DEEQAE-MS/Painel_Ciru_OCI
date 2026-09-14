# Painel para monitoramento da produção física e financeira ATE

Painel em **R Shiny** para acompanhar mensalmente a produção física e financeira de **Cirurgias Eletivas** (ROL, Total e Programa PATE/PNRF), das **Ofertas de Cuidados Integrados (OCI)** e do **Pagamento Portaria 9810**

## Funcionalidades

**Cirurgias eletivas**
- Diagrama de controle: faixa histórica (min-máx e Q1-Q3), mediana, produção do ano de comparação e do ano monitorado, com classificação mensal em 5 níveis (Esperado, Acima do esperado, Acima do limite esperado, Atenção, Crítico abaixo do limite esperado) — disponível para ROL e Total (Físico e Financeiro) e, dentro do ROL, também por Especialidade. PNRF não tem diagrama (histórico ainda curto demais para faixas confiáveis); o modo Financeiro não mostra classificação por cor (só a linha de produção).
- Filtros: indicador (MAC e FAEC totais / do Rol / Ciru. PATE-PNRF), Região (múltipla escolha em dropdown) e UF lado a lado, Município (só "Comparação Anos"), e — só para o indicador ROL — Especialidade e Procedimento, cruzando com `dados/Relacao_cirugiasROL.xlsx`. Especialidade vale para "Comparação Anos" e "Diagrama de monitoramento"; Procedimento (multisseleção, soma os selecionados) só para "Comparação Anos".
- Checkbox "Cirurgias Eletivas PAB", abaixo do indicador: quando marcado, substitui o indicador do dropdown **só em "Comparação Anos"** (Diagrama de monitoramento e Tabela continuam sempre no indicador do dropdown, sem PAB — não há diagrama de controle nem tabela de status para PAB). PAB não tem valor financeiro nas extrações atuais (só MAC/FAEC têm); o toggle Financeiro fica bloqueado com aviso quando PAB está marcado.
- Abas "Comparação Anos", "Diagrama de monitoramento" e "Tabela" (classificação do último mês por UF, sempre Físico, independente do toggle).

**OCI realizadas**
- Todas as análises usam mês de **atendimento** (`COMPETENCIA_ATENDIMENTO`, mês em que a OCI foi efetivamente realizada), não mês de processamento;
- Os gráficos com série mensal por mês de atendimento ("OCI geral", "Por mês de atendimento" e "OCI por especialidade") destacam os últimos 3 meses com uma faixa cinza e o rótulo "Dados preliminares" — a base ainda não fechou totalmente essas competências (ver `sombra_dados_preliminares()`/`anotacao_dados_preliminares()` em `app.R`);
- Aba "Série histórica OCI": um único gráfico "OCI geral" — onda de área com o Total geral de OCI e marcação da virada de ano de referência, com uma linha sobreposta por componente/modalidade (Componente Ambulatorial, Carretas, Créditos Financeiros, Equipes Volantes), cada uma podendo ser ligada/desligada clicando na legenda — e, logo abaixo, dois gráficos empilhados por componente (mesmas cores do gráfico de referência do projeto OCI): "Por ano" (2025 vs 2026, barras horizontais) e, em seguida, "Por mês de atendimento" (colunas verticais);
- Aba "Série histórica OCI por especialidade": mesmo gráfico "Geral + por especialidade" de antes, mas isolado numa subaba própria, com filtro de Especialidade e um filtro de Componente (OCI geral ou um componente específico) que só existem ali; logo abaixo, um gráfico de colunas empilhadas "Por especialidade e componente" — total do período por especialidade (ordenado do maior para o menor), sempre quebrado pelos 4 componentes independente do filtro de Componente;
- Aba "Comparativos": Físico/Financeiro por especialidade e produção mensal 2025 vs 2026, com seu próprio filtro de Especialidade (independente do da subaba anterior) e um filtro de Componente igual ao da subaba anterior — o comparativo mensal ignora o filtro de Especialidade, mas respeita o de Componente;
- Mesma lógica de filtro por região/UF/município e agregado dinâmico do painel de cirurgias.

**Pagamento Portaria 9810**
- Acompanha os repasses da Portaria GM/MS nº 9.810/2025 (Programa Agora Tem Especialistas — Componentes Ambulatorial e Cirúrgico). Regra fixa da aba: só entram linhas com `NU_PORTARIA` 09810/9810 (a base bruta traz outras portarias misturadas) e só o Valor Líquido.
- Filtros: UF, Município (cascata a partir da UF), Tipo de Gestão (Estadual/Municipal) e Componente. O filtro de Componente vem da coluna `PROGRAMA` da base, resumida em 5 rótulos: "Componente Ambulatorial", "Componente Cirúrgico", "Mutirão", "FAEC - PMAE*" e "FAEC PNRF*" (ver `mapear_programa_portaria9810()` em `app.R`) — os dois com "*" são Despesa de Exercício Anterior (nota na sidebar da aba).
- Todas as análises usam mês de **pagamento** (ANO + MÊS da base), não mês de competência.
- Aba "Pagamentos": total por mês de pagamento empilhado por Tipo de Gestão; gráfico de linhas por mês de pagamento com uma linha por Componente; tabela de detalhamento por UF com colunas Estadual, Municipal e Total.
- Aba "Limite da Portaria 9810": valor pago x limite de repasse por UF (`dados/PORTARIA_9.810_UF.xlsx`), sempre por UF inteira (soma Estadual + Municipal, todos os municípios — os filtros de Município e Tipo de Gestão não se aplicam aqui), com destaque visual para UFs que já ultrapassaram o limite.

## Fonte dos dados

Os dados têm origem pública, disponibilizados pelo Ministério da Saúde na plataforma **SUS360**:

- [Componente Ambulatorial (OCI)](https://sus360.saude.gov.br/#painel/componente-ambulatorial)
- [Cirurgias Eletivas](https://sus360.saude.gov.br/painel/cirurgias/)

Este painel **não acessa o SUS360 diretamente**. Ele consome os arquivos já processados e padronizados pelos projetos irmãos `Cirurgia` e `OCI`, que fazem esse tratamento a partir das bases brutas — com três exceções mantidas manualmente dentro deste projeto (não vêm do SUS360 nem são geradas por script): `dados/Relacao_cirugiasROL.xlsx` (mapeamento Código SIGTAP → Especialidade, usado nos filtros de Especialidade/Procedimento), `dados/BaseValorliquidoPortaria9810.xlsx` (base de pagamentos da Portaria 9.810) e `dados/PORTARIA_9.810_UF.xlsx` (limite de repasse por UF da mesma portaria).

## Como os dados chegam ao painel

```
Cirurgia/resultados/02_monitoramento/tabelas/
    serie_completa_{rol,total,pnrf}_*.csv
    serie_completa_financeiro_{rol,total,pnrf}_*.csv
    serie_anos_{rol,total,pnrf,pab}_*.csv
    serie_anos_municipio_{rol,total,pnrf,pab}_*.csv
Cirurgia/resultados/bases_processadas/
    cirurgias_mensal_procedimento_rol_*.csv
OCI/resultados/
    planilha_OCI_UF_mes_*.xlsx
    oci_mensal_especialidade_componente_{uf,municipio}.csv
                          │
                          ▼   sincronizado toda vez que o app abre (sessão nova)
              Painel_Ciru_OCI/dados/processados/

Painel_Ciru_OCI/dados/Relacao_cirugiasROL.xlsx           ← mantido manualmente, não sincroniza sozinho
Painel_Ciru_OCI/dados/BaseValorliquidoPortaria9810.xlsx  ← mantido manualmente, não sincroniza sozinho
Painel_Ciru_OCI/dados/PORTARIA_9.810_UF.xlsx             ← mantido manualmente, não sincroniza sozinho
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
    │   ├── Relacao_cirugiasROL.xlsx           # mantido manualmente, não ignorado pelo Git
    │   ├── BaseValorliquidoPortaria9810.xlsx  # mantido manualmente, não ignorado pelo Git
    │   ├── PORTARIA_9.810_UF.xlsx             # mantido manualmente, não ignorado pelo Git
    │   └── processados/                       # gerado automaticamente, ignorado pelo Git
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
4. Confirme que **todos os arquivos de `dados/processados/`** estão marcados para envio, mais `dados/Relacao_cirugiasROL.xlsx`, `dados/BaseValorliquidoPortaria9810.xlsx` e `dados/PORTARIA_9.810_UF.xlsx` (nenhum desses três é gerado automaticamente, então só vão junto se forem marcados manualmente) — e nenhum `.RData`, se aparecer.

Publicado em [`https://cginfo.shinyapps.io/Painel_Ciru_OCI/`](https://cginfo.shinyapps.io/Painel_Ciru_OCI/)

## Status

Painel em uso interno — dados sujeitos a alterações conforme atualização das bases do SUS360.
