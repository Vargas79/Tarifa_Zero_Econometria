# Tarifa_Zero_Econometria
Projeto para o curso de Econometria em R

## Scripts e estrutura do repositório

### Scripts (pasta `scripts/`)
- `01_analise_sul_sudeste_v1.R` – Versão original (antes do filtro de anos).
- `02_analise_sul_sudeste_v2.R` – Versão corrigida com filtro de anos (2015–2023), frota e MUNIC.
- `03_analise_brasil_v1.R` – Análise com todos os municípios brasileiros (controles básicos).
- `04_auditoria_outlier.R` – Auditoria da coorte 2018.
- `05_testes_robustez.R` – Testes de robustez adicionais.

### Resultados (pasta `resultados/`)
Os resultados estão organizados por recorte geográfico:
- `sul_sudeste/` – Tabelas e gráficos da análise principal.
- `brasil/` – Resultados da análise de robustez com todo o país.

### Como reproduzir
1. Baixe os dados (não incluídos no repositório) e coloque em `dados/`.
2. Execute os scripts em ordem numérica.
3. Os resultados serão salvos em `resultados/`.
