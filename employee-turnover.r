library("readxl")
library("summarytools")
library("ggplot2")
library("fastDummies")
library("caret")
library("randomForest")
library("pROC")


# carregamento do conjunto de dados

data <- read_xlsx("dataset.xlsx")

# o dataset possui atualmente 686 linhas e 23 colunas
dim(data)

View(data)

# conversao de dados numericos para categoricos
# desligamento (stauts) = 1
data$status <- as.factor(data$status)
data$job_level <- as.factor(data$job_level)
data$no_of_promotions <- as.factor(data$no_of_promotions)
data$risk_of_attrition <- as.factor(data$risk_of_attrition)
data$potential_rating <- as.factor(data$potential_rating)

dfSummary(data)

# verificar quais colunas possuem valores faltantes
colSums(is.na(data))


# realizando algumas transformacoes de dados e criacao de novas features
# variacao entre a performace de 2018 a 2017
data$var_rating <- as.factor(data$performance_rating_2018 - data$performance_rating_2017)

# porcentagem de salario de 2018 para 2017
data$percent_salary_change <- (data$salary_2018 - data$salary_2017)/data$salary_2017 * 100

# criacao de uma coluna idade
data$age <- 2018 - data$year_of_birth

# remocao das colunas que foram utilizadas para transformacao dos dados
data[ ,c('year_of_birth','salary_2017','salary_2018', 'performace_rating_2017','performace_rating_2018','hre_date', 'e_code')] <- list(NULL)


# funcao que cria uma tabela de frequencia de referente aos atributos categoricos
# em relacao ao status do colaborador
analyses_categorical_data   <- function(attribute, data) {
  mosaicplot(~attribute + status, data = data, color=TRUE)
  tab2 <- table(data$status, attribute)
  print(round(prop.table(tab2,1) * 100, digits = 2)) # per row by group
  print(round(prop.table(tab2,2) * 100, digits = 2)) # per group by column
}

# ha uma dominacia do genero masculino no dataset
analyses_categorical_data(data$gender,data)

# ha uma dominacia de nao haver service_agrrement nos dados, isto eh,
# uma baixa relacao com a saida do colaborador
analyses_categorical_data(data$service_agreement,data)

# a maioria dos colaboradores que possuem level 5
# nao saem da empresa e 51% dos colaboradores estão no level 1
analyses_categorical_data(data$job_level,data)

# ha uma dominica de colaboradores que nao tiverem variacao no ultimo ano 
# em relacao a performace. Alem disso estamos perdendo colaboradores com
# alta variacao de pefroamce e tambem baixa
analyses_categorical_data(data$var_rating,data)

# A maioria dos colaboradores nao receberam algum promocao e colaboradores que
# receberam 0,3,4,6 promocoes possuem mais chance de sair da empresa
analyses_categorical_data(data$no_of_promotions,data)

# A maioria dos colaboradores no qual deixam a compania posuem um 
# risco de atracao igual a 3 e 55% do colaboradores que possuem risco
# igual 1 sairam da empresa
analyses_categorical_data(data$risk_of_attrition,data)

# colaboradores que possuem poterial rating igual 1  a  55% sairam da empresa
# e a maioria possui um rating igual a 3-5
analyses_categorical_data(data$potential_rating,data)

# 84% dos colaboradores que sairam, nao receberam um premio. Alem disso, alguns que
# receberam sairam da empresa
analyses_categorical_data(data$awards,data)


# analise estatistica para ver signficancia daquela variavel 
# em relacao ao desligamento
p_value_categorical_data   <- function(attribute, data) {
  tab <- table(attribute,data$status)
  print(chisq.test(tab))
}

p_value_categorical_data(data$gender, data)

# Status eh dependente de service_agreement, com p-value < 0.05
p_value_categorical_data(data$service_agreement, data)

p_value_categorical_data(data$job_level, data)

# Status eh dependente de no_of_promotions, com p-value < 0.05
p_value_categorical_data(data$no_of_promotions, data)

# # Status eh dependente de risk_of_attrition, com p-value < 0.05
p_value_categorical_data(data$risk_of_attrition, data)

# Status eh de potential_rating, como p-value < 0.05
p_value_categorical_data(data$potential_rating, data)

# Status eh dependente de awards, como p-value < 0.05
p_value_categorical_data(data$awards, data)

p_value_categorical_data(data$signon, data)

# Status eh dependente de var_rating, como p-value < 0.05
p_value_categorical_data(data$var_rating, data)

# Pode-se concluir que ha uma dependencia com a escolha do funcionario em
# se manter na empresa ou sair, atraves dos seguintes atributos: numero de promocoes, potetial rating
# awards, signon e var rating. Onde a maior dependencia dos atributos potetial rating e numero de promocoes

# funcao que realiza estatsitica descritiva em relacao 
# ao status do colaborador
analyses_numerical_data   <- function(attribute, data) {
  stats_functions <- function(x) {
    c(min = min(x), max = max(x), 
      mean = mean(x), median = median(x),
      desv = sd(x))
  }
  by(attribute, data$status, stats_functions)
}


analyses_numerical_data(data$age, data)

# pessoas com bonus maiores tendem sair da empresa
analyses_numerical_data(data$bonus, data)

# Nota-se que colaboradores que moram mais longe tendem a sair da empresa 
analyses_numerical_data(data$distance_from_home, data)

# Nota-se que coloboradores que possuem satifacao entre 79-86 estao saindo da empresa
analyses_numerical_data(data$employee_sat, data)

p <- ggplot(data, aes(x=status, y=employee_sat)) + geom_violin()
p + geom_violin(trim=FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

# percebe-se que coloboradores que estao saindo, possuem menos cursos realizados
# do que colabodores que ainda estao na empresa
analyses_numerical_data(data$no_courses_taken, data)

p <- ggplot(data, aes(x=status, y=no_courses_taken)) + geom_violin()
p + geom_violin(trim=FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

# percebe-se que pessoas que ficam muito tempo na mesma posicao tendem a sair a da empresa
analyses_numerical_data(data$time_in_position, data)

p <- ggplot(data, aes(x=status, y=time_in_position)) + geom_violin()
p + geom_violin(trim=FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

# Nota-se que coloboradores que nao tiveram uma mudanca significativa de salario, sairam da empresa
analyses_numerical_data(data$percent_salary_change, data)

p <- ggplot(data, aes(x=status, y=percent_salary_change)) + geom_violin()
p + geom_violin(trim=FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

t.test(age ~ status, data=data)

# percebe-se que distancia de casa ate o servico impacta a saida do colaborador
t.test(distance_from_home ~ status, data=data)

# Nota-se o status do colaborador eh impactado pela satifacao respectivamente
t.test(employee_sat ~ status, data=data)

# Nota-se o status do colaborador eh impactado pelo numero de cursos realizados
t.test(no_courses_taken ~ status, data=data)

t.test(time_in_position ~ status, data=data)

# Percebe-se que a decisao de saida do colaborador, tambem eh depedente
# do percentual de mudanca do salario entre os anos
t.test(percent_salary_change ~ status, data=data)

# Conclui-se entao, que colaboradores que tiveram poucas mudacas de salario ou nao
# realizaram tantos cursos ou moram longe da empresa, sairam da empresa


# remocao das variaveis que nao sao estatisticamente relacionadas com desligamento
data[ ,c('gender','job_level','signon','age','manager_changes','bonus','time_in_position')] <- list(NULL)

names(data)

# transformacao das colunas para dummys
results_dummy <- dummy_cols(data, remove_most_frequent_dummy = T)

names(results_dummy)

# remocao das colunas que estao no formato original e não dummys
results_dummy[ ,c("status","service_agreement","no_of_promotions","risk_of_attrion","potential_rating","awards")] <- list(NULL)

colnames(results_dummy)[29] <-"var_rating_minus3"
colnames(results_dummy)[30] <-"var_rating_minus1"
results_dummy$status_1 <-  as.factor(results_dummy$status_1)
names(results_dummy)

results_dummy <- results_dummy[sample(nrow(results_dummy)), ]
trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list=FALSE)

X_train <- results_dummy[trainIndex, ]
X_test <- results_dummy[-trainIndex, ]

model <- randomForest(status_1 ~., data=X_train)
plot(model)
# percebe-se que satisfacao do gestor, variacao do salario, numero de cursos realizados, data de contratacao
# e distancia de casa possuem uma importancia maior em relacao a outras variaveis
varImpPlot(model)
predictions <- predict(model, X_test)
matrix_conf <- table(X_test$status_1, predictions)

# percebe-se que o modelo possui uma acuracia de 95.60%
acc <-(matrix_conf[1,1] + matrix_conf[2,2]) / nrow(X_test) * 100
acc

# nota-se que o modelo possui uma AUC de 0.93 que indica que o modelo esta ajustado
# aos dados corretamente
lrROC <- roc(as.numeric(X_test$status_1),as.numeric(predictions),plot=TRUE,print.auc=TRUE,legacy.axes=TRUE,main="ROC Curves")
