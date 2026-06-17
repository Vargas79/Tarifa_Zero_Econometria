# ==============================================================================
# AUDITORIA DA COORTE 2018 - TARIFA ZERO (VERSÃO SEM DEPENDÊNCIA CRÍTICA DO GEOBR)
# ==============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, ggplot2, fixest, did)

# 1. CARREGAR PAINEL PRINCIPAL ------------------------------------------------
cat("\n[1/5] Carregando dados...\n")
df_painel <- read_excel("Painel_Nacional_com_anos_implementacao.xlsx")

# 2. PIB (tratamento robusto) ------------------------------------------------
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
  cat("   PIB: arquivo externo\n")
} else {
  if ("pib_deflacionado" %in% names(df_painel)) {
    df_pib_def <- df_painel %>% select(municipio_id, ano, pib_deflacionado)
    cat("   PIB: coluna 'pib_deflacionado' do painel\n")
  } else if ("pib" %in% names(df_painel)) {
    df_pib_def <- df_painel %>% select(municipio_id, ano, pib_deflacionado = pib)
    cat("   PIB: coluna 'pib' do painel\n")
  } else {
    df_pib_def <- tibble(municipio_id = integer(), ano = integer(), pib_deflacionado = numeric())
    cat("   PIB: NÃO ENCONTRADO (será ignorado)\n")
  }
}

# 3. TENTAR OBTER DISTÂNCIAS (GEOBR) - COM TRATAMENTO DE ERRO -----------------
cat("\n[2/5] Tentando calcular distâncias às capitais (geobr)...\n")
distancia_disponivel <- FALSE
df_dist <- NULL

# Verificar se o pacote geobr está instalado
if (requireNamespace("geobr", quietly = TRUE) & requireNamespace("sf", quietly = TRUE)) {
  # Tentar baixar os municípios
  tryCatch({
    mun_geo <- geobr::read_municipality(year = 2020, showProgress = FALSE)
    if (!is.null(mun_geo) & inherits(mun_geo, "sf")) {
      # Transformar e calcular distâncias
      mun_geo <- sf::st_transform(mun_geo, 4326)
      df_geo <- mun_geo %>%
        mutate(municipio_id = as.integer(substr(as.character(code_muni), 1, 6)),
               uf_id = as.integer(code_state),
               lon = sf::st_coordinates(sf::st_centroid(.))[, 1],
               lat = sf::st_coordinates(sf::st_centroid(.))[, 2]) %>%
        sf::st_drop_geometry() %>%
        select(municipio_id, uf_id, name_muni, lon, lat)
      
      # Capitais
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
      cat("   -> Distâncias calculadas com sucesso.\n")
    } else {
      cat("   -> AVISO: 'read_municipality' retornou NULL.\n")
    }
  }, error = function(e) {
    cat("   -> AVISO: Falha ao baixar dados do geobr: ", e$message, "\n")
  })
} else {
  cat("   -> Pacotes 'geobr' ou 'sf' não instalados. Pulando distâncias.\n")
}

# Se não conseguiu distâncias, cria coluna vazia
if (!distancia_disponivel) {
  df_dist <- tibble(municipio_id = integer(), distancia_capital = numeric())
  cat("   -> Distâncias não disponíveis. Variável será ignorada.\n")
}

# 4. BASE FINAL --------------------------------------------------------------
cat("\n[3/5] Montando painel final...\n")
df_base <- df_painel %>%
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
  filter(codigo_uf %in% c(31,32,33,35,41,42,43)) %>%
  arrange(municipio_id, ano)

cat(sprintf("Painel final: %d municípios, %d observações\n",
            n_distinct(df_base$municipio_id), nrow(df_base)))

# 5. DESCREVER COORTE 2018 ---------------------------------------------------
coorte_2018 <- df_base %>% filter(ano_trat_num == 2018) %>% distinct(municipio_id, .keep_all = TRUE)
cat(sprintf("Municípios na coorte 2018: %d\n", nrow(coorte_2018)))

descritivas <- df_base %>%
  filter(ano == 2014) %>%
  mutate(coorte = case_when(
    ano_trat_num == 2018 ~ "2018",
    ano_trat_num == 2015 ~ "2015",
    ano_trat_num %in% c(2019,2020,2021,2022,2023) ~ "Outras",
    is.na(ano_trat_num) ~ "Nunca tratado"
  )) %>%
  group_by(coorte) %>%
  summarise(
    n = n(),
    mortalidade_2014 = mean(taxa_mortalidade_100k, na.rm = TRUE),
    morbidade_2014 = mean(taxa_morbidade_100k, na.rm = TRUE),
    pop_media = mean(populacao, na.rm = TRUE),
    pib_media = mean(pib_deflacionado, na.rm = TRUE)
  )
cat("\n--- Comparação pré-tratamento (2014) ---\n")
print(descritivas)

# 6. TRAJETÓRIA DA MORTALIDADE -----------------------------------------------
df_traj <- df_base %>%
  filter(!is.na(taxa_mortalidade_100k)) %>%
  mutate(grupo = case_when(
    ano_trat_num == 2018 ~ "Coorte 2018",
    ano_trat_num == 2015 ~ "Coorte 2015",
    ano_trat_num %in% c(2019,2020,2021,2022,2023) ~ "Outras coortes",
    is.na(ano_trat_num) ~ "Nunca tratados"
  )) %>%
  group_by(ano, grupo) %>%
  summarise(m = mean(taxa_mortalidade_100k), se = sd(taxa_mortalidade_100k)/sqrt(n()), .groups = "drop")

p <- ggplot(df_traj, aes(x=ano, y=m, color=grupo)) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin=m-1.96*se, ymax=m+1.96*se, fill=grupo), alpha=0.2, color=NA) +
  geom_vline(xintercept=2018, linetype="dashed") +
  labs(title="Trajetória da mortalidade - Coorte 2018 vs demais", y="Taxa por 100k") +
  theme_minimal()
ggsave("auditoria_trajetoria.png", width=10, height=6)
cat("   Gráfico salvo: auditoria_trajetoria.png\n")

# 7. OUTLIERS NA COORTE 2018 -------------------------------------------------
df_var <- df_base %>%
  filter(municipio_id %in% coorte_2018$municipio_id) %>%
  arrange(municipio_id, ano) %>%
  group_by(municipio_id) %>%
  mutate(var_mort = taxa_mortalidade_100k - lag(taxa_mortalidade_100k)) %>%
  ungroup()

outliers <- df_var %>%
  group_by(municipio_id) %>%
  mutate(z = abs(var_mort - mean(var_mort, na.rm=TRUE)) / sd(var_mort, na.rm=TRUE)) %>%
  filter(z > 3 & !is.na(z)) %>%
  select(municipio_id, ano, taxa_mortalidade_100k, var_mort, z)

if(nrow(outliers) > 0) {
  # Tentar adicionar nome do município se tivermos a tabela de nomes
  if(exists("df_geo") && !is.null(df_geo) && "name_muni" %in% names(df_geo)) {
    outliers <- outliers %>%
      left_join(df_geo %>% select(municipio_id, name_muni), by="municipio_id")
  } else {
    # Fallback: usar nome a partir do painel se existir coluna 'municipio_nome'
    if("municipio_nome" %in% names(df_painel)) {
      df_nomes <- df_painel %>% select(municipio_id, municipio_nome) %>% distinct()
      outliers <- outliers %>% left_join(df_nomes, by="municipio_id")
    } else {
      outliers$name_muni <- NA
    }
  }
  write.csv(outliers, "outliers_coorte2018.csv", row.names=FALSE)
  cat(sprintf("   %d outliers salvos em outliers_coorte2018.csv\n", nrow(outliers)))
} else {
  cat("   Nenhum outlier severo (z>3) encontrado.\n")
}

# 8. TESTE DE SENSIBILIDADE (remover coorte 2018) ---------------------------
df_sem2018 <- df_base %>% filter(ano_trat_num != 2018) %>% mutate(g = ifelse(is.na(ano_trat_num), 0L, ano_trat_num))

# Verificar se há dados suficientes para rodar o CS
if(n_distinct(df_sem2018$g) >= 2 & nrow(df_sem2018) > 100) {
  cat("\n[4/5] Reestimando CS sem a coorte 2018...\n")
  # Usar apenas variáveis que existem
  form_x <- if("log_pib" %in% names(df_sem2018) && any(!is.na(df_sem2018$log_pib))) {
    ~ log_pib + log_populacao
  } else {
    ~ log_populacao
  }
  cs_mort_sem2018 <- att_gt(
    yname = "taxa_mortalidade_100k",
    tname = "ano",
    idname = "municipio_id",
    gname = "g",
    xformla = form_x,
    data = df_sem2018,
    control_group = "nevertreated",
    bstrap = TRUE, biters = 500, clustervars = "municipio_id"
  )
  es_sem2018 <- aggte(cs_mort_sem2018, type = "dynamic", na.rm=TRUE)
  att_sem2018 <- es_sem2018$overall.att
  se_sem2018 <- es_sem2018$overall.se
} else {
  cat("\n   Dados insuficientes para reestimar CS sem a coorte 2018.\n")
  att_sem2018 <- NA
  se_sem2018 <- NA
}

# Valores originais (do seu CSV)
att_original <- -1.7095
se_original <- 1.1898

cat("\n--- COMPARAÇÃO ---\n")
cat(sprintf("Original (com 2018): ATT = %.4f (SE = %.4f)\n", att_original, se_original))
if(!is.na(att_sem2018)) {
  cat(sprintf("Sem coorte 2018:    ATT = %.4f (SE = %.4f)\n", att_sem2018, se_sem2018))
} else {
  cat("Sem coorte 2018: não foi possível estimar.\n")
}

# Salvar sensibilidade
sens <- data.frame(
  Especificacao = c("Com coorte 2018", "Sem coorte 2018"),
  ATT = c(att_original, att_sem2018),
  SE = c(se_original, se_sem2018)
)
write.csv(sens, "sensibilidade_sem2018.csv", row.names=FALSE)

# 9. RELATÓRIO FINAL ---------------------------------------------------------
sink("auditoria_coorte2018_relatorio.txt")
cat("RELATÓRIO DE AUDITORIA - COORTE 2018\n")
cat("====================================\n\n")
cat("Número de municípios na coorte 2018:", nrow(coorte_2018), "\n\n")
cat("Comparação pré-tratamento (2014):\n")
print(descritivas)
cat("\nOutliers detectados:", nrow(outliers), "\n")
if(nrow(outliers) > 0) cat("Lista disponível em outliers_coorte2018.csv\n")
cat("\nSensibilidade:\n")
cat(sprintf("  ATT original: %.4f (SE %.4f)\n", att_original, se_original))
if(!is.na(att_sem2018)) {
  cat(sprintf("  ATT sem 2018: %.4f (SE %.4f)\n", att_sem2018, se_sem2018))
  if(abs(att_sem2018 - att_original) > 1) {
    cat("  -> IMPACTO SIGNIFICATIVO: a coorte 2018 altera substancialmente o resultado.\n")
  } else {
    cat("  -> A coorte 2018 não é a principal responsável pelo efeito estimado.\n")
  }
}
cat("\nRecomendações:\n")
cat("1. Verificar manualmente os outliers (especialmente saltos >20 pontos).\n")
cat("2. Repetir o evento study agregado excluindo a coorte 2018.\n")
cat("3. Considerar incluir interação entre coorte e tempo.\n")
sink()

cat("\n✅ Auditoria concluída. Arquivos gerados:\n")
cat("  - auditoria_trajetoria.png\n")
cat("  - outliers_coorte2018.csv (se houver)\n")
cat("  - sensibilidade_sem2018.csv\n")
cat("  - auditoria_coorte2018_relatorio.txt\n")