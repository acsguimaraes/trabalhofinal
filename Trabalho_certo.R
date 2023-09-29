pacman::p_load(dplyr, ggplot2, ggThemeAssist, GGally, tidyverse, arules, arulesCBA, arulesViz, caret, rattle, lubridate, data.table, tidyverse, janitor, ade4,arules,forcats, tidyr, ggcorrplot, gt, gtExtras, gtsummary, kableExtra, plotly, ccaPP, lsa, minerva, Rfast, ade4, arules, forcats, utils, stringr, corrplot, gapminder)

mtcars2 <- mtcars %>% select(cyl, disp, hp, carb)

mtcars

str(mtcars)

#colocando título na primeira coluna
mtcars <- mtcars %>%
  tibble::rownames_to_column() %>%
  rename('Automobiles_models'= 'rowname')

mtcars3 <- mtcars

str(mtcars)

# Pré-processamento
particaoMTCARS = createDataPartition(1:nrow(mtcars), p=.70) # cria a partição 70-30 nrow o nuemro total de linha p.7 é igual a 70%

treinoMTCARS = mtcars[particaoMTCARS$Resample1, ] # treino é a base de aprendizagem a base q ela usa pra aprender

testeMTCARS = mtcars[-particaoMTCARS$Resample1, ] # - treino = teste validação base de teste a difereça é o -

# Mineração e predição com Árvores de Decisão
## Árvore de Decisão  ~ significa em função de alguma coisa   - regressão: aprendendo um número e essa é supervisionado - a preformace method rpart - trControl como o humano ajuada a máquina a aprender controle / controle da aprendizagem cv vali
MTCARS_RPART <- train(
  hp ~ disp + wt +qsec + mpg, 
  data = treinoMTCARS, 
  method = "rpart", 
  trControl = trainControl(method = "cv"))

fancyRpartPlot(MTCARS_RPART$finalModel) # desenho da árvore

plot(varImp(MTCARS_RPART)) # importância das variáveis

varImp(MTCARS_RPART, scale = T) # importância de cada variável

predicaoTree = predict(MTCARS_RPART, newdata = testeMTCARS)

postResample(testeMTCARS[ , 5], predicaoTree) # teste de performance da Árvore Condicional o 7 é a coluna da nota

# selecionando variáveis de interesse
mtcars <- mtcars %>% select(Automobiles_models, hp, disp, wt, qsec, mpg) 

# Pré-processamento de variáveis
mtcars[ , -c(1)] <- discretizeDF(mtcars[ , -c(1)]) # transforma variáveis numéricas em fatores transforma números em faixa

# SUPP é a porcetagem, 0.2 20% conf Confiança 0.5 é igual a 50% de confiança de está certo, o padrão que fica bom  maxlen é a quantidade de 10 ligações 
associacaoMTCARS<- apriori(mtcars[ , -1], parameter = list(supp = 0.2, conf = 0.5, maxlen = 10))

summary(associacaoMTCARS)
# é a função de mostra as regras, de visualizar as regras / usar support confience e count
inspect(associacaoMTCARS)

# o lift é a melhor medida de confiança pegando as melhores regras n=10 vai pegar 10 regras
associacaoMTCARSPrin <- head(associacaoMTCARS, n = 10, by = "lift")
#método coordenado / paracoord
plot(associacaoMTCARSPrin, method = "paracoord")

#método grafo
plot(head(sort(associacaoMTCARSPrin, by = "lift"), 10),
     method = "graph"
)

# Pré-processamento de base
particaoMTCARS = createDataPartition(1:nrow(mtcars), p=.7) # cria a partição 70-30
treinoMTCARS = mtcars[particaoMTCARS$Resample1, ] # treino
testeMTCARS = mtcars[-particaoMTCARS$Resample1, ] # - treino = teste

treinoMTCARS <- treinoMTCARS[ , -1]
testeMTCARS <- testeMTCARS[ , -1]

# Modelagem cba passo surpevisão ~ . diz que pode usar todas as colunas
regrasMTCARS = arulesCBA::CBA(hp ~ ., treinoMTCARS, supp=0.2, conf=0.5) 
inspect(regrasMTCARS$rules)
plot(regrasMTCARS$rules)

predicaoregrasMTCARS <- predict(regrasMTCARS, testeMTCARS)

confusionMatrix(predicaoregrasMTCARS, testeMTCARS$hp)

#correlacao das variaveis numericas



cor(mtcars2)

pairs(mtcars2)

mtcarsCor <- cor(mtcars2)
corrplot(mtcarsCor, method = "number", order = 'alphabet')
corrplot(mtcarsCor, order = 'alphabet') 

# pra tu copiar a partir daqui parte dos gráficos

theme_set(theme_light())

mtcars3 %>% ggplot(aes(y = mpg)) + geom_boxplot()


mtcars3 %>% ggplot(aes(x = hp, y = disp )) + geom_point(color='red', size=2) + geom_smooth() + labs(y = "Deslocamento", x = "Número de cilindros", title = "Relação entre o Número de cilindros e o Deslocamento") 

mtcars3 %>% ggplot(mapping = aes(x = wt)) + geom_histogram(bins = 10, fill ='Pink', color='black') + labs(x = 'Assaltos para cada 100 mil habitantes', y = 'Frequência de Assaltos', title = 'Assaltos em Estados Americanos') 

mtcars3 %>% ggplot(aes(x = Automobiles_models, y = hp, fill = Automobiles_models)) + geom_bar(stat = "identity")  +  labs( x = ' Modelos dos Automóveis', y = 'Potência', title = 'Potência dos Modelos dos carros') + scale_fill_discrete(name="Modelos") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

(plots=ggplot(mtcars3, aes(x = mpg)) +
    geom_line(aes(y = wt, 
                  colour = "Frequência de Estupros"))+  
    scale_x_continuous() + 
    geom_line(aes(y = drat, 
                  colour = "Frequência de Assassinatos"))) + labs(title="Relacão entre Frequência de Assassinatos e Estupros, baseado na População Urbana", x="População Urbana", y="") 

mtcars3 %>% ggplot(aes(x = hp, y = mpg)) + labs(title="Frequência de Assaltos, baseado na População Urbana", x="População Urbana", y="") + geom_line(col='red')
