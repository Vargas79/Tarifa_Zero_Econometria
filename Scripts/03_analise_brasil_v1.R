# ==============================================================================
# AVALIAÇÃO DE IMPACTO DA TARIFA ZERO - BRASIL COMPLETO (VERSÃO SIMPLIFICADA)
# ==============================================================================
# Objetivo: Estimar o efeito da Tarifa Zero sobre mortalidade e morbidade
#           usando todos os municípios brasileiros (2014-2023).
# Controles: log_populacao, var_anual_pib, log_pib (CS) e distancia_capital (PSM)
# NOTA: Sem frota e MUNIC (dados incompletos para Norte/Nordeste).
# ==============================================================================

# ── 0. PACOTES ──────────────────────────────────────────────────────────────
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  readxl, dplyr, tidyr, ggplot2, fixest, did, MatchIt,
  geobr, sf
)

# ── 1. CARREGAMENTO DAS BASES ──────────────────────────────────────────────
cat("\n[1/6] Carregando bases de dados...\n")

arquivo_principal <- "Painel_Nacional_com_anos_implementacao.xlsx"
if (!file.exists(arquivo_principal))
  stop("Arquivo '", arquivo_principal, "' não encontrado.")

df_painel <- read_excel(arquivo_principal)

# PIB (tenta carregar de arquivo externo ou usa colunas do painel)
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
    cat("   PIB obtido do painel.\n")
  } else {
    df_pib_def <- tibble(municipio_id = integer(), ano = integer(), pib_deflacionado = numeric())
    cat("   PIB não encontrado (será ignorado).\n")
  }
}

# Distâncias (geobr) - todos os estados
cat("\n[2/6] Calculando distâncias às capitais (Brasil todo)...\n")
df_dist <- tibble(municipio_id = integer(), distancia_capital = numeric())

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
        select(municipio_id, uf_id, lon, lat)
      
      # Capitais de todos os estados (códigos IBGE)
      cod_capitais <- c(
        110020, 120040, 130260, 140010, 150140, 160030, 170210, 210530, 220770,
        230440, 240810, 250750, 260320, 270430, 280030, 290100, 310620, 320530,
        330455, 355030, 410690, 420540, 430510, 500270, 510340, 520870, 530010
      )
      df_caps <- df_geo %>% filter(municipio_id %in% cod_capitais) %>% 
        select(uf_id, lon_cap = lon, lat_cap = lat)
      
      df_dist <- df_geo %>%
        left_join(df_caps, by = "uf_id") %>%
        filter(!is.na(lon_cap)) %>%
        mutate(p = pi/180,
               a = 0.5 - cos((lat_cap - lat)*p)/2 + cos(lat*p)*cos(lat_cap*p)*(1 - cos((lon_cap - lon)*p))/2,
               distancia_capital = 12742 * asin(sqrt(a))) %>%
        select(municipio_id, distancia_capital)
      cat("   Distâncias calculadas.\n")
    }
  }, error = function(e) cat("   AVISO: geobr falhou. Distâncias não usadas.\n"))
}

# ── 2. MONTAGEM DO PAINEL (BRASIL TODO) ─────────────────────────────────────
cat("\n[3/6] Montando painel (Brasil todo)...\n")

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
  )

# ── 3. FILTROS (anos válidos e exclusão da coorte 2018) ────────────────────
cat("\n[4/6] Aplicando filtros...\n")

# 3.1 Anos de tratamento válidos (2015–2023)
ano_minimo <- 2015
ano_maximo <- 2023
df_base_raw <- df_base_raw %>%
  mutate(ano_invalido = ifelse(!is.na(ano_trat_num) & 
                                 (ano_trat_num < ano_minimo | ano_trat_num > ano_maximo), TRUE, FALSE))
df_base_raw <- df_base_raw %>% filter(!ano_invalido | is.na(ano_invalido))

# 3.2 Exclusão da coorte 2018 (outlier)
coorte_2018 <- df_base_raw %>% filter(ano_trat_num == 2018) %>% distinct(municipio_id) %>% pull()
df_base <- df_base_raw %>% filter(!municipio_id %in% coorte_2018)

# 3.3 Recalcular variáveis de tratamento
df_base <- df_base %>%
  mutate(g = ifelse(is.na(ano_trat_num), 0L, ano_trat_num)) %>%
  arrange(municipio_id, ano) %>%
  group_by(municipio_id) %>%
  mutate(var_anual_pib = ifelse(!is.na(pib_deflacionado) & !is.na(lag(pib_deflacionado)),
                                (pib_deflacionado - lag(pib_deflacionado)) / lag(pib_deflacionado) * 100,
                                NA_real_)) %>%
  ungroup() %>%
  mutate(
    grupo_trat = ifelse(!is.na(ano_trat_num), 1L, 0L),
    tempo_pos = ifelse(!is.na(ano_trat_num) & ano >= ano_trat_num, 1L, 0L),
    Efeito_Tarifa_Zero = grupo_trat * tempo_pos,
    uf = as.factor(codigo_uf)
  )

cat(sprintf("   Painel final: %d observações | %d municípios\n",
            nrow(df_base), n_distinct(df_base$municipio_id)))

# ── 4. REMOÇÃO DE NAs E PREPARAÇÃO DOS DATASETS ─────────────────────────────
cat("\n[5/6] Removendo NAs e preparando datasets...\n")

# 4.1 Variáveis base
vars_base <- c("taxa_mortalidade_100k", "var_anual_pib", "log_populacao")

# Adicionar morbidade se existir
if ("taxa_morbidade_100k" %in% names(df_base)) {
  vars_did <- c(vars_base, "taxa_morbidade_100k")
} else {
  vars_did <- vars_base
  cat("   AVISO: 'taxa_morbidade_100k' não encontrada. Modelos de morbidade ignorados.\n")
}

# 4.2 Dataset para DiD
df_did <- df_base %>%
  select(municipio_id, ano, uf, Efeito_Tarifa_Zero, all_of(vars_did)) %>%
  drop_na()

cat(sprintf("   DiD: %d observações\n", nrow(df_did)))

# 4.3 Dataset para PSM (ano 2014)
vars_psm <- c("pib_deflacionado", "populacao", "distancia_capital")
df_2014 <- df_base %>%
  filter(ano == 2014) %>%
  select(municipio_id, grupo_trat, all_of(vars_psm)) %>%
  drop_na()

cat(sprintf("   PSM: %d observações (ano 2014)\n", nrow(df_2014)))

# 4.4 Dataset para CS
vars_cs <- c("log_pib", "log_populacao")
df_cs <- df_base %>%
  select(municipio_id, ano, g, taxa_mortalidade_100k, all_of(vars_cs)) %>%
  drop_na()

cat(sprintf("   CS: %d observações\n", nrow(df_cs)))

# Verificar se há dados para morbidade no df_did
tem_morbidade <- "taxa_morbidade_100k" %in% names(df_did) && 
  sum(!is.na(df_did$taxa_morbidade_100k)) > 0

# ── 5. MODELOS ──────────────────────────────────────────────────────────────
cat("\n[6/6] Rodando modelos...\n")

# 5.1 DiD Clássico (TWFE)
if (nrow(df_did) > 0) {
  did_mort <- feols(
    taxa_mortalidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
    data = df_did, cluster = ~municipio_id
  )
  
  if (tem_morbidade) {
    did_morb <- feols(
      taxa_morbidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
      data = df_did, cluster = ~municipio_id
    )
  } else {
    did_morb <- NULL
    cat("   Morbidade: dados insuficientes (NAs). Pulando.\n")
  }
} else {
  stop("DiD não pode rodar: df_did vazio. Verifique os dados.")
}

# 5.2 PSM + DiD
psm_mort <- psm_morb <- NULL
if (nrow(df_2014) > 0 && n_distinct(df_2014$grupo_trat) > 1) {
  formula_psm <- as.formula(paste("grupo_trat ~", paste(vars_psm, collapse = " + ")))
  modelo_psm <- matchit(formula_psm, data = df_2014, method = "nearest", distance = "glm")
  df_pesos <- match.data(modelo_psm) %>% select(municipio_id, weights)
  
  df_psm <- df_base %>%
    inner_join(df_pesos, by = "municipio_id") %>%
    select(municipio_id, ano, uf, Efeito_Tarifa_Zero, all_of(vars_did)) %>%
    drop_na()
  
  if (nrow(df_psm) > 0) {
    psm_mort <- feols(
      taxa_mortalidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
      data = df_psm, weights = ~weights, cluster = ~municipio_id
    )
    if (tem_morbidade) {
      psm_morb <- feols(
        taxa_morbidade_100k ~ Efeito_Tarifa_Zero + var_anual_pib + log_populacao | uf + ano,
        data = df_psm, weights = ~weights, cluster = ~municipio_id
      )
    }
  }
}

# 5.3 Callaway & Sant'Anna
cs_mort <- NULL
if (length(unique(df_cs$g)) >= 2 && nrow(df_cs) > 100) {
  cs_mort <- att_gt(
    yname = "taxa_mortalidade_100k",
    tname = "ano",
    idname = "municipio_id",
    gname = "g",
    xformla = ~ log_pib + log_populacao,
    data = df_cs,
    control_group = "nevertreated",
    bstrap = TRUE, biters = 500, clustervars = "municipio_id"
  )
} else {
  cat("   Dados insuficientes para CS (mortalidade). Pulando.\n")
}

# ── 6. TABELA DE RESULTADOS ──────────────────────────────────────────────────
cat("\n[7/7] Gerando tabela de resultados...\n")

extrair_feols <- function(modelo, indicador, metodo) {
  if (is.null(modelo)) return(tibble())
  ct <- coeftable(modelo)
  if (!"Efeito_Tarifa_Zero" %in% rownames(ct)) return(tibble())
  linha <- ct["Efeito_Tarifa_Zero", ]
  tibble(
    Metodo = metodo,
    Indicador = indicador,
    ATT = round(linha["Estimate"], 4),
    SE = round(linha["Std. Error"], 4),
    p_valor = round(linha["Pr(>|t|)"], 4),
    IC_inf = round(linha["Estimate"] - 1.96 * linha["Std. Error"], 4),
    IC_sup = round(linha["Estimate"] + 1.96 * linha["Std. Error"], 4)
  )
}

tabela_final <- bind_rows(
  extrair_feols(did_mort, "Mortalidade", "1. DiD Clássico")
)
if (!is.null(did_morb)) {
  tabela_final <- bind_rows(tabela_final,
                            extrair_feols(did_morb, "Morbidade", "1. DiD Clássico")
  )
}
if (!is.null(psm_mort)) {
  tabela_final <- bind_rows(tabela_final,
                            extrair_feols(psm_mort, "Mortalidade", "2. PSM + DiD")
  )
  if (!is.null(psm_morb)) {
    tabela_final <- bind_rows(tabela_final,
                              extrair_feols(psm_morb, "Morbidade", "2. PSM + DiD")
    )
  }
}
if (!is.null(cs_mort)) {
  es <- aggte(cs_mort, type = "dynamic", na.rm = TRUE)
  tabela_final <- bind_rows(tabela_final,
                            tibble(
                              Metodo = "3. Callaway & Sant'Anna",
                              Indicador = "Mortalidade",
                              ATT = round(es$overall.att, 4),
                              SE = round(es$overall.se, 4),
                              p_valor = NA_real_,
                              IC_inf = round(es$overall.att - 1.96*es$overall.se, 4),
                              IC_sup = round(es$overall.att + 1.96*es$overall.se, 4)
                            )
  )
}

cat("\n=== RESULTADOS – BRASIL TODO ===\n")
print(tabela_final)
write.csv(tabela_final, "resultados_brasil_completo.csv", row.names = FALSE)

# ── 7. CONTAGEM DE MUNICÍPIOS ──────────────────────────────────────────────
n_total <- n_distinct(df_base$municipio_id)
n_tratados <- df_base %>%
  filter(!is.na(ano_trat_num) & ano_trat_num %in% 2015:2023) %>%
  distinct(municipio_id) %>% nrow()
n_controles <- n_total - n_tratados

cat("\n=== RESUMO DA AMOSTRA ===\n")
cat(sprintf("Total de municípios: %d\n", n_total))
cat(sprintf("Tratados (2015-2023): %d\n", n_tratados))
cat(sprintf("Controles (nunca tratados): %d\n", n_controles))

cat("\n✅ PROCESSAMENTO CONCLUÍDO. Arquivo gerado: resultados_brasil_completo.csv\n")
