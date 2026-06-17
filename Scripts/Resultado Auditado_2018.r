# ==============================================================================
# AVALIAÇÃO DE IMPACTO DA TARIFA ZERO - VERSÃO CORRIGIDA (EXCLUINDO COORTE 2018)
# ==============================================================================
# Esta versão remove o município outlier da coorte 2018 (N=1) e reestima
# todos os modelos, gerando resultados robustos para o relatório final.
# ==============================================================================

# ── 0. PACOTES ──────────────────────────────────────────────────────────────
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  readxl, readODS, dplyr, tidyr, ggplot2, fixest, did, MatchIt,
  geobr, sf, patchwork
)

# ── 1. CARREGAMENTO DAS BASES ───────────────────────────────────────────────
cat("\n[1/8] Carregando bases de dados...\n")

arquivo_principal <- "Painel_Nacional_com_anos_implementacao.xlsx"
if (!file.exists(arquivo_principal))
  stop("Arquivo '", arquivo_principal, "' não encontrado.")

df_painel <- read_excel(arquivo_principal)

# PIB (robusto)
if (file.exists("PIB_Municipios_Deflacionado.xlsx")) {
  df_pib_raw <- read_excel("PIB_Municipios_Deflacionado.xlsx")
  names(df_pib_raw)[1:2] <- c("municipio_nome", "municipio_id")
  df_pib_def <- df_pib_raw %>%
    filter(!is.na(municipio_id), municipio_nome != "Brasil") %>%
    pivot_longer(cols = -c(municipio_nome, municipio_id),
                 names_to = "ano", values_to = "pib_deflacionado") %>%
    mutate(municipio_id = as.integer(substr(as.character(municipio_id), 1, 6)),
           ano = as.integer(ano)) %>%
    select(municipio_id, ano, pib_deflacionado)
  cat("   PIB carregado do arquivo externo.\n")
} else {
  if ("pib_deflacionado" %in% names(df_painel)) {
    df_pib_def <- df_painel %>% select(municipio_id, ano, pib_deflacionado)
    cat("   PIB obtido da coluna 'pib_deflacionado' do painel.\n")
  } else if ("pib" %in% names(df_painel)) {
    df_pib_def <- df_painel %>% select(municipio_id, ano, pib_deflacionado = pib)
    cat("   PIB obtido da coluna 'pib' do painel.\n")
  } else {
    df_pib_def <- tibble(municipio_id = integer(), ano = integer(), pib_deflacionado = numeric())
    cat("   PIB não encontrado (será ignorado).\n")
  }
}

# Distâncias (geobr) - com fallback
cat("\n[2/8] Calculando distâncias às capitais...\n")
distancia_disponivel <- FALSE
df_dist <- NULL

if (requireNamespace("geobr", quietly = TRUE) & requireNamespace("sf", quietly = TRUE)) {
  tryCatch({
    mun_geo <- geobr::read_municipality(year = 2020, showProgress = FALSE)
    if (!is.null(mun_geo)) {
      mun_geo <- sf::st_transform(mun_geo, 4326)
      df_geo <- mun_geo %>%
        mutate(municipio_id = as.integer(substr(as.character(code_muni), 1, 6)),
               uf_id = as.integer(code_state),
               lon = sf::st_coordinates(sf::st_centroid(.))[, 1],
               lat = sf::st_coordinates(sf::st_centroid(.))[, 2]) %>%
        sf::st_drop_geometry() %>%
        select(municipio_id, uf_id, name_muni, lon, lat)
      
      cod_capitais <- c(310620, 320530, 330455, 355030, 410690, 420540, 431490)
      df_caps <- df_geo %>% filter(municipio_id %in% cod_capitais) %>% select(uf_id, lon_cap = lon, lat_cap = lat)
      
      df_dist <- df_geo %>%
        left_join(df_caps, by = "uf_id") %>%
        filter(!is.na(lon_cap)) %>%
        mutate(p = pi/180,
               a = 0.5 - cos((lat_cap - lat)*p)/2 + cos(lat*p)*cos(lat_cap*p)*(1 - cos((lon_cap - lon)*p))/2,
               distancia_capital = 12742 * asin(sqrt(a))) %>%
        select(municipio_id, distancia_capital)
      distancia_disponivel <- TRUE
      cat("   Distâncias calculadas com sucesso.\n")
    }
  }, error = function(e) cat("   AVISO: geobr falhou, distâncias não serão usadas.\n"))
}

if (!distancia_disponivel) {
  df_dist <- tibble(municipio_id = integer(), distancia_capital = numeric())
}

# ── 2. BASE UNIFICADA E EXCLUSÃO DA COORTE 2018 ─────────────────────────────
cat("\n[3/8] Montando painel e excluindo coorte 2018...\n")

df_base_raw <- df_painel %>%
  mutate(municipio_id = as.integer(municipio_id), ano = as.integer(ano)) %>%
  left_join(df_pib_def, by = c("municipio_id", "ano")) %>%
  left_join(df_dist, by = "municipio_id") %>%
  mutate(
    codigo_uf = floor(municipio_id / 10000),
    ano_trat_num = as.integer(ano_tratamento),
    log_populacao = log(populacao + 1),
    log_pib = ifelse(!is.na(pib_deflacionado), log(pib_deflacionado + 1), NA_real_),
    g = ifelse(is.na(ano_trat_num), 0L, ano_trat_num)
  ) %>%
  filter(codigo_uf %in% c(31, 32, 33, 35, 41, 42, 43))  # Sul e Sudeste

# Identificar municípios da coorte 2018
coorte_2018_munis <- df_base_raw %>%
  filter(ano_trat_num == 2018) %>%
  distinct(municipio_id) %>%
  pull(municipio_id)

cat(sprintf("   Removendo coorte 2018: %d município(s)\n", length(coorte_2018_munis)))
df_base <- df_base_raw %>% filter(!municipio_id %in% coorte_2018_munis)

# Recalcular g (tratados agora só têm anos 2015, 2019+)
df_base <- df_base %>%
  mutate(g = ifelse(is.na(ano_trat_num), 0L, ano_trat_num)) %>%
  arrange(municipio_id, ano)

cat(sprintf("   Painel final (sem 2018): %d observações | %d municípios\n",
            nrow(df_base), n_distinct(df_base$municipio_id)))

# ── 3. PREPARAÇÃO PARA OS MODELOS ───────────────────────────────────────────
# Variável de tratamento interação
df_did <- df_base %>% filter(!is.na(var_anual_pib))  # para DiD clássico que precisa de var_anual_pib

# Para o PSM (ano base 2014)
df_2014 <- df_base %>%
  filter(ano == 2014, !is.na(distancia_capital), !is.na(pib_deflacionado))

# ── 4. DiD CLÁSSICO (TWFE) ──────────────────────────────────────────────────
cat("\n[4/8] Rodando DiD Clássico (TWFE)...\n")

did_mortalidade <- feols(
  taxa_mortalidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
  data = df_did, cluster = ~municipio_id
)

did_morbidade <- feols(
  taxa_morbidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
  data = df_did, cluster = ~municipio_id
)

# ── 5. PSM + DiD ────────────────────────────────────────────────────────────
cat("\n[5/8] Rodando PSM + DiD...\n")

# Fórmula do propensity score (usa variáveis disponíveis)
vars_psm <- c("pib_deflacionado", "populacao", "distancia_capital")
vars_psm <- vars_psm[vars_psm %in% names(df_2014)]
formula_psm <- as.formula(paste("grupo_trat ~", paste(vars_psm, collapse = " + ")))

modelo_psm <- matchit(formula_psm, data = df_2014, method = "nearest", distance = "glm")
df_pesos <- match.data(modelo_psm) %>% select(municipio_id, weights)

df_psm <- df_base %>%
  inner_join(df_pesos, by = "municipio_id") %>%
  filter(!is.na(var_anual_pib))

psm_mortalidade <- feols(
  taxa_mortalidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
  data = df_psm, weights = ~weights, cluster = ~municipio_id
)

psm_morbidade <- feols(
  taxa_morbidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
  data = df_psm, weights = ~weights, cluster = ~municipio_id
)

# ── 6. CALLAWAY & SANT'ANNA (2021) ──────────────────────────────────────────
cat("\n[6/8] Rodando Callaway & Sant'Anna (staggered DiD)...\n")

# Base para CS (completa, sem filtro de var_anual_pib)
df_cs <- df_base %>% filter(!is.na(log_populacao))

cs_mortalidade <- att_gt(
  yname = "taxa_mortalidade_100k",
  tname = "ano",
  idname = "municipio_id",
  gname = "g",
  xformla = ~ log_pib + log_populacao,
  data = df_cs,
  control_group = "nevertreated",
  bstrap = TRUE, biters = 500, clustervars = "municipio_id"
)

cs_morbidade <- att_gt(
  yname = "taxa_morbidade_100k",
  tname = "ano",
  idname = "municipio_id",
  gname = "g",
  xformla = ~ log_pib + log_populacao,
  data = df_cs,
  control_group = "nevertreated",
  bstrap = TRUE, biters = 500, clustervars = "municipio_id"
)

# Agregações
es_mortalidade <- aggte(cs_mortalidade, type = "dynamic", na.rm = TRUE)
es_morbidade   <- aggte(cs_morbidade,   type = "dynamic", na.rm = TRUE)
grp_mortalidade <- aggte(cs_mortalidade, type = "group", na.rm = TRUE)
grp_morbidade   <- aggte(cs_morbidade,   type = "group", na.rm = TRUE)

# ── 7. TABELA COMPARATIVA DE RESULTADOS ─────────────────────────────────────
cat("\n[7/8] Gerando tabela comparativa...\n")

extrair_feols <- function(modelo, indicador, metodo) {
  ct <- coeftable(modelo)
  linha <- ct["Efeito_Tarifa_Zero", ]
  tibble(
    Metodo    = metodo,
    Indicador = indicador,
    ATT       = round(linha["Estimate"], 4),
    SE        = round(linha["Std. Error"], 4),
    p_valor   = round(linha["Pr(>|t|)"], 4),
    IC_inf    = round(linha["Estimate"] - 1.96 * linha["Std. Error"], 4),
    IC_sup    = round(linha["Estimate"] + 1.96 * linha["Std. Error"], 4)
  )
}

tabela_final <- bind_rows(
  extrair_feols(did_mortalidade, "Mortalidade", "1. DiD Clássico (TWFE)"),
  extrair_feols(did_morbidade,   "Morbidade",   "1. DiD Clássico (TWFE)"),
  extrair_feols(psm_mortalidade, "Mortalidade", "2. PSM + DiD"),
  extrair_feols(psm_morbidade,   "Morbidade",   "2. PSM + DiD"),
  tibble(
    Metodo    = "3. Callaway & Sant'Anna (2021)",
    Indicador = "Mortalidade",
    ATT       = round(es_mortalidade$overall.att, 4),
    SE        = round(es_mortalidade$overall.se,  4),
    p_valor   = NA_real_,
    IC_inf    = round(es_mortalidade$overall.att - 1.96 * es_mortalidade$overall.se, 4),
    IC_sup    = round(es_mortalidade$overall.att + 1.96 * es_mortalidade$overall.se, 4)
  ),
  tibble(
    Metodo    = "3. Callaway & Sant'Anna (2021)",
    Indicador = "Morbidade",
    ATT       = round(es_morbidade$overall.att, 4),
    SE        = round(es_morbidade$overall.se,  4),
    p_valor   = NA_real_,
    IC_inf    = round(es_morbidade$overall.att - 1.96 * es_morbidade$overall.se, 4),
    IC_sup    = round(es_morbidade$overall.att + 1.96 * es_morbidade$overall.se, 4)
  )
)

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║     TABELA CORRIGIDA (SEM COORTE 2018) - IMPACTO DA TARIFA ZERO    ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
print(tabela_final, n = Inf)

write.csv(tabela_final, "resultados_corrigidos_sem2018.csv", row.names = FALSE)

# ── 8. GRÁFICOS ATUALIZADOS ─────────────────────────────────────────────────
cat("\n[8/8] Gerando gráficos corrigidos...\n")

# Event Study Mortalidade
p1 <- ggdid(es_mortalidade) +
  ggtitle("Event Study — Mortalidade (excluída coorte 2018)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

# Event Study Morbidade
p2 <- ggdid(es_morbidade) +
  ggtitle("Event Study — Morbidade (excluída coorte 2018)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

# Efeitos por coorte (group)
p3 <- ggdid(grp_mortalidade) +
  ggtitle("Efeito por Coorte de Adoção — Mortalidade") +
  theme_minimal(base_size = 12)

p4 <- ggdid(grp_morbidade) +
  ggtitle("Efeito por Coorte de Adoção — Morbidade") +
  theme_minimal(base_size = 12)

# Salvar individualmente
ggsave("cs_event_study_mortalidade_corrigido.png", p1, width = 9, height = 5)
ggsave("cs_event_study_morbidade_corrigido.png", p2, width = 9, height = 5)
ggsave("cs_group_mortalidade_corrigido.png", p3, width = 9, height = 5)
ggsave("cs_group_morbidade_corrigido.png", p4, width = 9, height = 5)

# Gráfico combinado para o relatório
combinado <- (p1 | p2) / (p3 | p4) +
  plot_annotation(title = "Resultados corrigidos - exclusão da coorte 2018",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))
ggsave("resultados_corrigidos_combinados.png", combinado, width = 12, height = 10)

cat("\n✅ Processamento concluído. Arquivos gerados:\n")
cat("   - resultados_corrigidos_sem2018.csv\n")
cat("   - cs_event_study_mortalidade_corrigido.png\n")
cat("   - cs_event_study_morbidade_corrigido.png\n")
cat("   - cs_group_mortalidade_corrigido.png\n")
cat("   - cs_group_morbidade_corrigido.png\n")
cat("   - resultados_corrigidos_combinados.png\n")