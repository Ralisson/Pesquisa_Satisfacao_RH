# Mini-Projeto 3 - Data Science Aplicada ao RH
# Análise Estatística em Avaliações Anuais de Desempenho e Previsão de Comportamento dos Funcionários

# Todos os detalhes para este Mini-Projeto estão nos manuais em pdf.

# Diretório de trabalho
getwd()
setwd("C:/Users/ralis/Google Drive/Ciencia_dados/DSA/FAECD/Mini_projetos/Mini_projeto_RH")

# Pacotes
library(ggplot2)
library(dplyr)

########## Carregando os Dados ########## 

# Carregando os dados
dados_aval = read.csv("dados/dataset.csv", sep = ';', stringsAsFactors = FALSE)


# Tipos de dados
class(dados_aval)
View(dados_aval)

str(dados_aval)

# Nomes das colunas 
names(dados_aval)

names(dados_aval)[2] <- c('Pesquisa_atual')
names(dados_aval)[8] <- c('Funcionario_ja_saiu?')


########## Limpeza dos Dados ########## 


# Observe que as colunas "nivel_satisfacao" e "ultima_avaliacao" são colunas numéricas, mas estão com vírgula.
# A Linguagem R não vai processar como valor numérico e vai gerar erros.
# Vamos então substituir vírgulas por pontos.
dados_aval$Pesquisa_atual <- as.numeric(gsub(",", ".", dados_aval$Pesquisa_atual))
dados_aval$ultima_avaliacao <- as.numeric(gsub(",", ".", dados_aval$ultima_avaliacao))

# Há uma coluna X (a primeira) que parece ser um índice ou ID. Podemos removê-la.
dados_aval$X = NULL

# Conferindo os dados
View(dados_aval)

# Ok, vamos fazer uma análise exploratória e compreender os dados.

########## Análise Exploratória ########## 

# Vamos visualizar mais alguns detalhes nos dados

# Dimensões dos dados
dim(dados_aval)

# Sumário estatístico
summary(dados_aval)

# Classes dos dados
sapply(dados_aval, typeof)
sapply(dados_aval, class)


# Os histogramas ajudam a compreender a distribuição das variáveis. Vamos criar alguns.
par(mfrow = c(2,2))
hist(dados_aval$Pesquisa_atual, main = "Histograma da Pesquisa Atual", xlab = "Satisfação")
hist(dados_aval$ultima_avaliacao, main = "Histograma da Última Avaliação", xlab = "Última Avaliação")
hist(dados_aval$media_mensal_horas, main = "Histograma da Média Mensal de Horas Trabalhadas", xlab = "Média Mensal de Horas Trabalhadas")
hist(dados_aval$tempo_empresa, main = "Histograma do Tempo de Empresa", xlab = "Tempo de Empresa")

# Chama atenção apenas o histograma do tempo de empresa. Parece haver outliers. Vamos checar.

# Boxplots ajudam a visualizar possíveis outliers
par(mfrow = c(2,2))
boxplot(dados_aval$nivel_satisfacao, main = "Satisfação")
boxplot(dados_aval$ultima_avaliacao, main = "Última Avaliação")
boxplot(dados_aval$media_mensal_horas, main = "Média Mensal de Horas Trabalhadas")
boxplot(dados_aval$tempo_empresa, main = "Tempo de Empresa")

# Confirmado. Temos alguns outliers apenas na variável "tempo_empresa".

# Criaremos agora um gráfico de barras mostrando a relação entre nível de satisfação e saída do colaborador da empresa.
# Mas como a variável saida está como valo binário (0 ou 1), teremos que criar uma variável auxiliar com valores em string.
dados_aval$saidaFlag[dados_aval$saida ==  1] = 'Saiu'
dados_aval$saidaFlag[dados_aval$saida ==  0] = 'Nao Saiu'

# Agora criamos o gráfico
par(mfrow = c(1,1))
tabela_saistf_saida <- table(dados_aval$saidaFlag, dados_aval$nivel_satisfacao)
barplot(tabela_saistf_saida, 
        main = "Nível de Satisfação x Saída da Empresa",
        xlab = "Nível de Satisfação", col = c("blue", "orange"),
        legend = rownames(tabela_saistf_saida), 
        beside = TRUE)

# Parece que pessoas com baixa avaliação de desempenho estão mais propensas a deixar a empresa, 
# ou porque foram demitidas ou porque foram buscar novas oportunidades. Nenhuma novidade.

# Será que o número de projetos nos quais o colaborador trabalhou teve influência para deixar a empresa? Vamos checar.
tabela_projetos_saida <- table(dados_aval$saidaFlag, dados_aval$num_projetos)
barplot(tabela_projetos_saida, 
        main = "Saída da Empresa x Número de Projetos",
        xlab = "Número de Projetos", 
        col = c("green", "red"),
        legend = rownames(tabela_projetos_saida), 
        beside = TRUE)

# Colaboradores trabalhando com 2, 6 e 7 projetos, deixaram a empresa em maior número. 
# Mas não parece haver uma relação direta entre número de projetos e saída da empresa.

# Vamos analisar a proporção de pessoas que deixaram a empresa em comparação com o nível de salário
p1 = ggplot(subset(dados_aval, saida == 1), aes(x = factor('Salario'), fill = factor(salario))) +
  geom_bar(width = 1, position = "fill", color = "black") + coord_polar(theta = "y") + theme_bw()+
  ggtitle("Saída da Empresa Por Nível de Salário") + xlab("") + ylab("") + scale_fill_discrete(name = "Salario")

p1 = p1 + theme(
  plot.title = element_text(color = "Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")
)
print(p1)

# De acordo com o gráfico, pessoas com maior nível salarial, deixam a empresa em menor número.

# Nossa próxima análise será sobre a relação entre o nível de satisfação e o nível de salário.
# Mas teremos que preparar os dados para isso.

# Será necessário criar uma nova coluna numérica para representar o nível de salário, 
# ou será complicado criar o gráfico.

# E vamos criar outra coluna para categoriar o nível de satisfação. Vamos ao trabalho.

# Criamos então uma nova coluna numérica para representar o salário
dados_aval$nivel_salario[which(dados_aval$salario == "baixo")] = 1
dados_aval$nivel_salario[which(dados_aval$salario == "medio")] = 2
dados_aval$nivel_salario[which(dados_aval$salario == "alto")] = 3

# Criamos uma nova coluna para o nível de satisfação.
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao >= 0.9] = '1.Maximo'
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao >= 0.8 & dados_aval$nivel_satisfacao < 0.9 ] = '2.Alto'
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao >= 0.6 & dados_aval$nivel_satisfacao < 0.8 ] = '3.Bom'
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao >= 0.4 & dados_aval$nivel_satisfacao < 0.6 ] = '4.Medio'
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao >= 0.2 & dados_aval$nivel_satisfacao < 0.4 ] = '5.Baixo'
dados_aval$nivel_satisf_colaborador[dados_aval$nivel_satisfacao <  0.2] = '6.Minimo'

# Convertemos a coluna para o tipo fator, pois o ggplot espera coluna com muitas categorias como sendo fator.
dados_aval$nivel_satisf_colaborador = as.factor(dados_aval$nivel_satisf_colaborador)

# Vamos tabular os dados para calcular as frequências
tabela_nivel_sal_satisf <- table(dados_aval$nivel_salario, dados_aval$nivel_satisf_colaborador)
View(tabela_nivel_sal_satisf)
class(tabela_nivel_sal_satisf)

# Observe que o objeto anterior é do tipo "table".
# Convertemos a tabela para dataframe para facilitar a manipulação.
tabela_nivel_sal_satisf_df <- as.data.frame(tabela_nivel_sal_satisf)

# Criamos duas colunas com nomes representativos e atribuímos os valores.
tabela_nivel_sal_satisf_df$nivel_salario = tabela_nivel_sal_satisf_df$Var1
tabela_nivel_sal_satisf_df$nivel_satisf_colaborador = tabela_nivel_sal_satisf_df$Var2

# E então zeramos as colunas antigas.
tabela_nivel_sal_satisf_df$Var1 = NULL
tabela_nivel_sal_satisf_df$Var2 = NULL

# E conferimos o resultado.
View(tabela_nivel_sal_satisf_df)

# Perfeito. Vamos plotar.

p2 <- ggplot(tabela_nivel_sal_satisf_df, aes(x = nivel_salario, y = Freq, fill = nivel_satisf_colaborador)) +
  geom_bar(position = "dodge", stat = 'identity') +
  ggtitle("Nível Salário x Nível Satisfação") + 
  xlab("Nível Salário") + 
  ylab("Frequência")

p2 = p2 + theme(
  plot.title = element_text(color = "Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")  
)
print(p2)

# Observe que interessante: O nível de satisfação "bom" é o maior em todos os níveis salariais.

# Vamos checar o número de colaboradores que deixaram a empreda, por departamento.

# Criamos a tabela de frequência convertemos para dataframe e ajustamos as colunas
tabela_dept <- table(dados_aval$departamento, dados_aval$saida)
tabela_dept_df <- as.data.frame(tabela_dept)
tabela_dept_df$departamento = tabela_dept_df$Var1
tabela_dept_df$saida = tabela_dept_df$Var2
tabela_dept_df$Var1 = NULL
tabela_dept_df$Var2 = NULL

# Extraímos um subset com os colaboradores que deixaram a empresa
tabela_dept_saiu_df <- subset(tabela_dept_df, saida == 1)
View(tabela_dept_saiu_df)

# Convertemos a variável para o tipo fator ordenando os níveis.
tabela_dept_saiu_df$departamento <- factor(tabela_dept_saiu_df$departamento, 
                                           levels = tabela_dept_saiu_df$departamento[order(-tabela_dept_saiu_df$Freq)])

# Criamos o plot
p3 <- ggplot(tabela_dept_saiu_df, aes(x = departamento, y = Freq, fill = "orange3")) +
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = FALSE) +
  ggtitle("Número de Colaboradores Que Deixaram a Empresa Por Departamento") + xlab("Departmento")

p3 = p3 + theme(
  plot.title = element_text(color = "Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")
)
print(p3)

# A rotatividade está alta na área Comercial, em é a menor na área da Finanças.

# Podemos então analisar a satisfação dos colaboradores por departamento. Vamos calcular o nível médio de satisfação.

# Começamos agrupando os dados.
groupedByDept = dados_aval %>%
  group_by(departamento) %>%
  summarise(mean = mean(nivel_satisfacao), sd = sd(nivel_satisfacao), count = n())

# Convertemos o resultado para dataframe (está percebendo o padrão, não está?)
groupedByDept_df = data.frame(groupedByDept)

# Ordenamos pela média
groupedByDept_df = groupedByDept_df[order(groupedByDept_df$mean),]

# Criamos o plot
p4 <- ggplot(groupedByDept_df, aes(x = reorder(departamento, -mean), y = mean, fill = "skyblue")) +
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = FALSE) + coord_cartesian(ylim = c(0.55, 0.63)) +
  ggtitle("Nível de Satisfação Por Departamento") +
  xlab("Departmento") +
  ylab("Média")

p4 = p4 + theme(
  plot.title = element_text(color = "Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")
)
print(p4)

# O pessoal de Finanças tem a maior média de satisfação e até por isso, a menor rotatividade.

# Será que o número de projetos nos quais os colaboradores está trabalhando tem influência no nível de satisfação?

p5 <- ggplot(dados_aval, aes(x = factor(num_projetos), y = nivel_satisfacao, fill = factor(saida))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Número de Projetos x Nível de Satisfação") +
  xlab("Número de Projetos") +
  ylab("Nível de Satisfação")

p5 = p5 + theme(
  plot.title = element_text(color = "Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")
)
print(p5)

# Interessante. Observando apenas os colaboradores que deixaram a empresa vemos um nível de satisfação mais alto
# (acima da média), quando trabalhando em 2 projetos, mas o nível de satisfação cai com 4 e 5 projetos.

# Com 6 e 7 projetos, a média de satisfação é baixa, mas temos muitos ouliers.

# Vamos confirmar ou não esse padrão analisando o nível de satisfação com relação à última avaliação de desempenho.

p6 <-ggplot(dados_aval, aes(x = factor(num_projetos), y = ultima_avaliacao, fill = factor(saida))) +
  geom_boxplot() + scale_fill_manual(values = c("orange", "purple"))+
  ggtitle("Número de Projetos x Última Avaliação") +
  xlab("Número de Projetos") +
  ylab("Última Avaliação")

p6 = p6 + theme(
  plot.title = element_text(color  ="Black", size = 14, face = "bold.italic", hjust = 0.5),
  axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "Black", size = 14, face = "bold")
)
print(p6)

# O padrão é bastante similar. Talvez a empresa pudesse fazer uma divisão melhor dos projetos entre os colaboradores.

# Com isso finalizamos a análise exploratória. 

# Na parte 2 vamos formular algumas hipóteses, aplicar testes estatísticos para confirmá-las ou não e prever 
# o comportamento dos colaboradores de acordo com suas avaliações de desempenho e outros fatores.


