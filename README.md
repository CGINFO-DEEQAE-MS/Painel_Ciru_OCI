# Painel para monitoramento da produção de Cirurgias Eletivas e OCI

Painel desenvolvido em **R Shiny** para monitoramento da produção de **Cirurgias Eletivas** e **Ofertas de Cuidados Integrados (OCI)**.

## Funcionalidades

- Monitoramento mensal de cirurgias eletivas do ROL e totais;
- Comparação da produção com parâmetros históricos;
- Classificação do desempenho por UF;
- Monitoramento da produção mensal de OCI;
- Visualização da variação mensal e do status por UF;
- Filtros por região e unidade federativa;
- Atualização dos dados diretamente pelo painel.

## Fontes de dados

O painel consome dados processados pelos projetos irmãos:

- `Cirurgia`
- `OCI`

Esses projetos utilizam como fonte primária os dados públicos disponibilizados pelo Ministério da Saúde por meio da painel **SUS360**:

- Componente Ambulatorial (OCI): [SUS360 – Componente Ambulatorial](https://sus360.saude.gov.br/?utm_source=chatgpt.com#painel/componente-ambulatorial)
- Cirurgias Eletivas: [SUS360 – Cirurgias Eletivas](https://sus360.saude.gov.br/painel/cirurgias/?utm_source=chatgpt.com)

O painel Shiny não realiza extração direta dessas fontes, consumindo apenas os arquivos processados e padronizados gerados pelos projetos `Cirurgia` e `OCI`. citeturn0search0

## Estrutura esperada

```text
Projetos/
├── Cirurgia/
├── OCI/
└── Painel_Ciru_OCI/
    ├── app.R
    ├── README.md
    └── .gitignore
```

## Execução

Abra o projeto no RStudio e execute:

```r
shiny::runApp()
```

## Status

Painel interno em desenvolvimento.