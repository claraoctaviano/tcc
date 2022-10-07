# Instalação dos pacotes
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot","ggrepel","splines","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "beepr","Rcpp","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# Dataframe - Dados do Twitter
  # Contém tanto variáveis numéricas quanto categóricas
df_twitter <- read_excel("tbl_twitter_v2.xlsx")

# Mudando o campo Date para 'categórica'
df_twitter$Date <- as.character(df_twitter$Date)

# Coletando estatísticas descritivas da base de dados
summary(df_twitter)

# Tabela de frequência das categóricas
table(df_twitter$User)
table(df_twitter$Categoria_Predominante)
table(df_twitter$Ano)


# Criação das dummies
twitter_dummies <- dummy_columns(.data = df_twitter,
                                    select_columns = c("User", "Ano"),
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T) # Já removendo as colunas usadas para a construção da dummy
View(twitter_dummies)

df_aux <- twitter_dummies %>% 
          select(Total_Engajamento, Ano_2020, Ano_2022, Qtd_Caracteres, User_estadaoverifica, User_fatooufake, User_uolconfere,
                 Politica, Saude, Educacao, Seguranca, Meio_Ambiente, Economia)

cols <- c("Qtd_Caracteres", "Politica", "Saude", "Educacao", "Seguranca", "Meio_Ambiente", "Economia")
df_aux[cols] <- log(df_aux[cols])

df_aux[mapply(is.infinite, df_aux)] <- NA # Trocando infinito por NA
df_aux[is.na(df_aux)] <- 0 # Trocando NA por 0

# Estimação de um modelo de regressão  
  # Resposta: Total de Engajamento 

modelo_engaj_dummies <- glm(formula = Total_Engajamento ~ ., 
                                 data = df_aux)
summary(modelo_engaj_dummies)

# Procedimento de Stepwise
step_engaj <- step(modelo_engaj_dummies, k = 3.841459)

summary(step_engaj)

# Plotando os resíduos do modelo stepwise 
df_aux %>%
  mutate(residuos = step_engaj$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#### AJUSTANDO OS DADOS - ETAPA DE NORMALIZAÇÃO DAS VARIÁVEIS #### 
library(caret)
preproc <- preProcess(df_aux %>% select (-c(Total_Engajamento)), method = c("center","scale"))
norm <- predict(preproc, df_aux)
summary(norm)

model_engajamento <- glm(formula = Total_Engajamento ~ ., 
                            data = norm)
summary(model_engajamento)

# Stepwise
step_engajamento <- step(model_engajamento, k = 3.841459)

summary(step_engajamento)

# Plotando os resíduos do modelo step_engajamento 
norm %>%
  mutate(residuos = step_engajamento$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

norm %>%
  ggplot() +
  geom_density(aes(x = step_engajamento$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

# Histograma dos resíduos 
df_aux %>%
  mutate(residuos = model_engajamento$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(model_engajamento$residuals),
                            sd = sd(model_engajamento$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

df_aux %>%
  mutate(residuos = step_engajamento$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_engajamento$residuals),
                            sd = sd(step_engajamento$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


# Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(df_aux$Total_Engajamento) 
lambda_BC

# Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
df_aux$bc_engajamento <- (((df_aux$Total_Engajamento ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)


# write.csv(df_aux, "plotagem_boxplot_normalizado.csv")

#Estimando um novo modelo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bc_engajamento ~ Ano_2020 + Ano_2022 + Qtd_Caracteres + User_estadaoverifica
                + User_fatooufake + User_uolconfere + Politica + Saude + Educacao + Seguranca
                + Meio_Ambiente + Economia,
                data = df_aux)

summary(modelo_bc)

step_bc <- step(modelo_bc, k = 3.841459)

summary(step_bc)

step_bc$coefficients

# Comparando os modelos gerados
export_summs(model_engajamento, modelo_bc, scale = F, digits = 4)

export_summs(step_engajamento, modelo_bc, scale = F, digits = 4)

# Plot dos resíduos
df_aux %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc$residuals),
                            sd = sd(modelo_bc$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Fazendo predições com o Box-Cox
# Modelo Não Linear (Box-Cox):
predict(object = modelo_bc,
        data.frame(Ano_2020 = 1, Ano_2022 = 0, Qtd_Caracteres = 100,
                   User_estadaoverifica = 0, User_fatooufake = 1,
                   User_uolconfere = 0, Politica = 0, Saude = 0,
                   Educacao = 0, Seguranca = 0, Meio_Ambiente = 0,
                   Economia = 1),
        interval = "confidence", level = 0.95)

# Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
# value de Y (variável 'engajamento')
(((173.5356 * 2.659051) + 1)) ^ (1 / 2.659051)

# Salvando os fitted values dos dois modelos (modelo_linear e modelo_bc) no
df_aux$yhat_linear <- model_engajamento$fitted.values
df_aux$yhat_modelo_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                            1))^(1/(lambda_BC$lambda))

# Visualizando os fitted values dos dois modelos no dataset
df_aux %>%
  select(everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)
