### FURTHER PREPROCESSING ###

## Net import (importo lordo - sconto) 
df_7_tic_clean_final$IMPORTO_NETTO <- 
  df_7_tic_clean_final$IMPORTO_LORDO - df_7_tic_clean_final$SCONTO
df_7_tic_clean_final$SCONTO_PERCENTUALE <-
  round(df_7_tic_clean_final$SCONTO/df_7_tic_clean_final$IMPORTO_LORDO, digits = 3)
## Dataframe without returns 
df_7_no_resi <- df_7_tic_clean_final[df_7_tic_clean_final$DIREZIONE==1 & df_7_tic_clean_final$IMPORTO_LORDO>=0,]


### EXPLORATORY ANALYSIS ###

# average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_reparto_avgimportosconto <- df_7_tic_clean %>% 
  group_by(COD_REPARTO) %>% 
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO),
            AVG_SCONTO = mean(SCONTO))
# ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
df7_artdistr <- df_7_tic_clean %>% 
  group_by(ID_ARTICOLO) %>%
  summarise(n_tics = n_distinct(ID_SCONTRINO))
# average IMPORTO_LORDO and average SCONTO per ID_CLI
df7_cliente_avgimportosconto <- df_7_tic_clean %>% 
  group_by(ID_CLI) %>% 
  summarise(AVG_IMPORTO_LORDO = round(mean(IMPORTO_LORDO), digits = 2),
            AVG_SCONTO = round(mean(SCONTO), digits = 2))
