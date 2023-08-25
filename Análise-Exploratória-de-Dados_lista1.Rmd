---
title: "Análise Exploratória de Dados"
autor: "Luciana Sousa MArtins"
date: "`r Sys.Date()`"
output:
  rmdformats::downcute:
    self_contained: true
    default_style: "light"
    downcute_theme: "default"
editor_options: 
  markdown: 
    wrap: 72
---

```{r}
#Pacotes 
library(ggplot2)
library(tidyverse)
library(skimr)
library(readr)
library(knitr)
library(gmodels)
library(tidymodels)
library(dplyr)
library(kableExtra)
```

## Questão 1

```{r}
#A-Razão
#B-Ordinal
#C-Razão
#D-Intervalar
#E-Razão
#F-Nominal
#G-Intervalar
```

## Questão 2

```{r}
#carregar o banco de dados:
load(url(description = "https://www.ime.usp.br/~pam/dados.RData"))
view(tab2_1)
```

### Estado civil

```{r}

frequencia1<-table(tab2_1$estado_civil) 
proporcao1<-prop.table(frequencia1) 
Porcentagem1 <-100*prop.table(frequencia1) 

frequencia1<-c(frequencia1,sum(frequencia1)) 
proporcao1<-c(proporcao1,sum(proporcao1))
Porcentagem1 <-c(Porcentagem1,sum(Porcentagem1 ))
names(frequencia1)[3]<-"Total"
tab2_2<-cbind(frequencia1,proporcao1=round(proporcao1,digits=3),Porcentagem1=round
              (Porcentagem1,digits=2))
tab2_2

```

### Região de procedência

```{r}

frequencia2<-table(tab2_1$reg_procedencia) 
proporcao2<-prop.table(frequencia2)
Porcentagem2<-100*prop.table(frequencia2) 

frequencia2<-c(frequencia2,sum(frequencia2)) 
proporcao2<-c(proporcao2,sum(proporcao2))
Porcentagem2<-c(Porcentagem2,sum(Porcentagem2))
names(frequencia2)[4]<-"Total"
tab2_3<-cbind(frequencia2,proporcao2=round(proporcao2,digits=2),Porcentagem2=round
              (Porcentagem2,digits=2))
tab2_3

```

### Número de filhos dos empregados casados

```{r}

frequencia3<-table(cut(tab2_1$n_filhos,breaks = seq(0,6,l=7),right=FALSE ))
proporcao3<-prop.table(frequencia3)
Porcentagem3<-100*prop.table(frequencia3) 

frequencia3<-c(frequencia3,sum(frequencia3))
proporcao3<-c(proporcao3,sum(proporcao3))
Porcentagem3<-c(Porcentagem3,sum(Porcentagem3))
names(frequencia3)[7]<-"Total"
tab2_3<-cbind(frequencia3,proporcao3=round(proporcao3,digits=4),Porcentagem3=round
              (Porcentagem3,digits=5))
tab2_3

```

### Idade.

```{r}

frequencia4<-table(cut(tab2_1$idade_anos,breaks = seq(20,49,l=7),right=FALSE ))
proporcao4<-prop.table(frequencia4)
Porcentagem4<-100*prop.table(frequencia4) 

frequencia4<-c(frequencia4,sum(frequencia4))
proporcao4<-c(proporcao4,sum(proporcao4))
Porcentagem4<-c(Porcentagem4,sum(Porcentagem4))
names(frequencia4)[7]<-"Total"
tab2_4<-cbind(frequencia4,proporcao4=round(proporcao4,digits=4),Porcentagem4=round
              (Porcentagem4, digits = 2))
tab2_4

```

## Questão 3

```{r}
#carregar o banco de dados:
load(url(description = "https://www.ime.usp.br/~pam/dados.RData"))
view(cd_brasil)
```

### População Urbana

```{r}

frequencia1.1<-table(cut(cd_brasil$pop_urbana,breaks = seq(175000, 32000000, l=5),right=FALSE))
proporcao1.1<-prop.table(frequencia1.1)
Porcentagem1.1<-100*prop.table(frequencia1.1) 

frequencia1.1<-c(frequencia1.1,sum(frequencia1.1))
proporcao1.1<-c(proporcao1.1,sum(proporcao1.1))
Porcentagem1.1<-c(Porcentagem1.1,sum(Porcentagem1.1))
names(frequencia1.1)[5]<-"Total"
cd_brasil1.1<-cbind(frequencia1.1,proporcao1.1=round(proporcao1.1,digits=4),Porcentagem1.1=round
                 (Porcentagem1.1, digits = 2))
cd_brasil1.1

```

### Densidade populacional

```{r}

cd_brasil$densidade<-as.numeric(cd_brasil$densidade)
frequencia1.2<-table(cut(cd_brasil$densidade,breaks = seq(5,100,l=7),right=FALSE ))
proporcao1.2<-prop.table(frequencia1.2)
Porcentagem1.2<-100*prop.table(frequencia1.2) 
frequencia1.2<-c(frequencia1.2,sum(frequencia1.2))
proporcao1.2<-c(proporcao1.2,sum(proporcao1.2))
Porcentagem1.2<-c(Porcentagem1.2,sum(Porcentagem1.2))
names(frequencia1.2)[7]<-"Total"
cd_brasil1.2<-cbind(frequencia1.2,proporcao1.2=round(proporcao1.2,digits=4),Porcentagem1.2=round
                 (Porcentagem1.2, digits = 4))
cd_brasil1.2

```

## Questão 9

```{r}
#Banco de Dados:
dadoquest <- read_csv("~/R Studio/dado9.csv")
View(dadoquest)
```

### Item A

```{r}
#Variáveis da Tabela:

#Seção-       Qualitativa Nominal
#Administr.-  Quntitativa Contínua
#Direito-     Quntitativa Contínua
#Redação-     Quntitativa Contínua
#Estatíst.-   Quntitativa Contínua
#Inglês-      Qualitativa Ordinal
#Metodologia- Qualitativa Ordinal
#Política-    Quntitativa Contínua
#Economia-    Quntitativa Contínua

```

### Item B

```{r}
direito<-table(cut(dadoquest$Direito, breaks = seq(1,10,l=2), right = FALSE))
direito

politica<-table(cut(dadoquest$Politica, breaks = seq(6,10,l=5), right = FALSE))
politica

estatistica<-table(cut(dadoquest$Estatist, breaks = seq(4,10,l=5), right = FALSE))
estatistica

```

### Item C

```{r item, echo=FALSE}
frequencia2.1<-table(cut(dadoquest$Redacao, breaks = seq(6,10,l=5), right = FALSE))
proporcao2.1<-prop.table(frequencia2.1)
Porcentagem2.1<-100*prop.table(frequencia2.1) 

frequencia2.1<-c(frequencia2.1,sum(frequencia2.1))
proporcao2.1<-c(proporcao2.1,sum(proporcao2.1))
Porcentagem2.1<-c(Porcentagem2.1,sum(Porcentagem2.1))
names(frequencia2.1)[5]<-"Total"
dadoquestc<-cbind(frequencia2.1,proporcao2.1=round(proporcao2.1,digits=4),Porcentagem2.1=round
                 (Porcentagem2.1, digits = 4))
dadoquestc

hist(dadoquest$Redacao, col="darkblue", border="black")

```

### Item D

```{r}

dadoquest$Redacao<-as.numeric(dadoquest$Redacao)
frequencia2.2<-table(cut(dadoquest$Redacao,breaks = seq(6,9.5,l=5),right=FALSE ))
proporcao2.2<-prop.table(frequencia2.2)
Porcentagem2.2<-100*prop.table(frequencia2.2) 
frequencia2.2<-c(frequencia2.1,sum(frequencia2.1))
proporcao2.2<-c(proporcao2.1,sum(proporcao2.1))
Porcentagem2.2<-c(Porcentagem2.1,sum(Porcentagem2.1))
names(frequencia2.2)[5]<-"Total"
dadoquestd<-cbind(frequencia1.2,proporcao2.1=round(proporcao2.1,digits=4),Porcentagem2.1=round
                 (Porcentagem2.1, digits = 4))
dadoquestd

```
