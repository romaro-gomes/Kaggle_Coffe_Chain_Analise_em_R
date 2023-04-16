library(tidyverse)
library(here)

# Definições ----------
# Budget Profit: https://smallbusiness.chron.com/calculate-budgeted-profit-36513.html
# Budget Margin: https://www.ramseysolutions.com/budgeting/7-ways-to-create-financial-margin
# Budget COGS: https://www.thebalancesmb.com/prepare-a-cost-of-goods-sold-budget-an-example-393035
# Budget COGS: https://squareup.com/us/en/townsquare/what-is-cost-of-goods-sold
# Budget_Sales: https://www.thebalancesmb.com/the-sales-budget-an-example-393024
# Dataset= https://www.kaggle.com/datasets/qusaybtoush1990/coffee-chain 
# Total expensives= https://www.profitwell.com/recur/all/total-expenses
# Dados ----
coffee_raw=read_csv(here('data','raw_data',"Coffee Chain.csv"))
coffee=coffee_raw
coffee
str(coffee)
summary(coffee)

coffee$Ddate = as.Date(coffee$Ddate)
coffee$Ddate

colnames(coffee)=str_replace_all(colnames(coffee)," ",'_')
colnames(coffee)

coffee %>% group_by(Market_Size) %>% summarise(Total=n())

coffee %>% group_by(Type) %>% summarise(Total=n())

coffee %>% group_by(Market_Size,Type) %>% summarise(Total=n())

coffee %>% group_by(State,Market_Size,Type) %>% summarise(Total=n())

coffee %>% group_by(State,Market_Size) %>% summarise(Total=n())

coffee %>% group_by(State,Market_Size,Product) %>% summarise(Total=n())

coffee %>% group_by(Product) %>% summarise(Total=n()) %>% arrange(desc(Total))
 

coffee %>% group_by(x=Product) %>% summarise(Total=n()) %>% arrange(desc(Total)) %>%
 ggplot(aes(x=reorder(x,Total), y= Total)) +
 geom_col() +
 coord_flip()

coffee %>% group_by(State,Market_Size,Product) %>% summarise(Total=n()) %>%
 ggplot(aes(x=State,y=Total)) +
 geom_col()+
 facet_grid(Market_Size~.,scales='free') +
 coord_flip()

coffee$Total_Expenses
str(coffee)
# LOcalidade ------
library(zipcodeR)
zipcodeR::geocode_zip()
zipcodeR::zip_to_cd

zip=zipcodeR::zip_code_db
zip
zona=zip %>% select(zipcode,timezone)


colnames(coffee)
coffee$State
zona$zipcode

area_code=read_delim(here('data','raw_data',"area_code.txt"),delim = '\t')
area_code
 
area_code= area_code %>% select(!'Overlay_complex') %>% rename(Area_Code =Area_code)


head(coffee$State,3)
head(zona,3)
head(zip,3)
head(area_code,3)
head(coffee$Area_Code,3)

coffee= left_join(coffee,area_code)

colnames(coffee)

coffee %>% group_by(State,Market_Size,Product,Timezone) %>% summarise(Total=n()) %>%
 ggplot(aes(x=State,y=Total)) +
 geom_col(aes(fill=Timezone))+
 facet_wrap(Market_Size~.,scales='free') +
 coord_flip()

coffee %>% group_by(Market_Size,Product,Timezone) %>% summarise(Total=n()) %>%
 ggplot(aes(x=Product,y=Total)) +
 geom_col(aes(fill=Timezone))+
 facet_wrap(Market_Size~.,scales='free') +
 coord_flip()

coffee %>% group_by(Market_Size,Product_Type,Timezone) %>% summarise(Total=n()) %>%
 ggplot(aes(x=Product_Type,y=Total)) +
 geom_col(aes(fill=Timezone),color='black')+
 facet_grid(Market_Size~.,scales='free') +
 coord_flip()

coffee %>% group_by(Product,Product_Type,Timezone) %>% summarise(Total=n()) %>%
 ggplot(aes(x=Product,y=Total)) +
 geom_col(aes(fill=Timezone),color='black')+
 facet_wrap(Product_Type~.,scales='free') +
 coord_flip()

coffee %>%
        group_by(Product,Product_Type,Timezone) %>%
        summarise(Total=n()) %>%
        mutate(porcentagem=(Total/sum(Total))*100) %>%
        ggplot(aes(x=Product,y=porcentagem)) +
        geom_col(aes(fill=Timezone),color='black')+
        facet_wrap(Product_Type~.,scales='free') +
        coord_flip()


# Contas -----
coffee$Cogs_diff=coffee$Budget_Cogs-coffee$Cogs
colnames(coffee)
coffee$Margin_diff=coffee$Budget_Margin-coffee$Margin
coffee$Profit_diff=coffee$Budget_Profit-coffee$Profit
coffee$Sales_diff=coffee$Budget_Sales-coffee$Coffee_Sales

coffee %>%
        group_by(Type) %>%
        summarise(Inventario_Total=sum(Inventory),
                  Gasto=sum(Cogs),
                  Vendas=sum(Coffee_Sales),
                  Despesas=sum(Total_Expenses),
                  Lucro=sum(Profit),
                  Margem=sum(Margin),
                  Marketing=sum(Marketing)) %>%
        mutate(Total=Vendas-Gasto)

coffee %>%
        group_by(Product) %>%
        summarise(Inventario_Total=sum(Inventory),
                  Gasto=sum(Cogs),
                  Vendas=sum(Coffee_Sales),
                  Despesas=sum(Total_Expenses),
                  Lucro=sum(Profit),
                  Margem=sum(Margin),
                  Marketing=sum(Marketing)) %>%
        mutate(Total=Vendas-Gasto)

coffee %>%
        group_by(Product_Type) %>%
        summarise(Inventario_Total=sum(Inventory),
                  Gasto=sum(Cogs),
                  Vendas=sum(Coffee_Sales),
                  Despesas=sum(Total_Expenses),
                  Lucro=sum(Profit),
                  Margem=sum(Margin),
                  Marketing=sum(Marketing)) %>%
        mutate(Total=Vendas-Gasto)
coffee %>%
        group_by(Type,Product_Type) %>%
        summarise(Inventario_Total=sum(Inventory),
                  Gasto=sum(Cogs),
                  Vendas=sum(Coffee_Sales),
                  Despesas=sum(Total_Expenses),
                  Lucro=sum(Profit),
                  Margem=sum(Margin),
                  Marketing=sum(Marketing)) %>%
        mutate(Total=Vendas-Gasto)

coffee %>%
        group_by(Type,Product_Type) %>%
        summarise(Inventario_Total=sum(Inventory),
                  Gasto=sum(Cogs),
                  Vendas=sum(Coffee_Sales),
                  Despesas=sum(Total_Expenses),
                  Lucro=sum(Profit),
                  Margem=sum(Margin),
                  Marketing=sum(Marketing)) %>%
        mutate(Total=Vendas-Gasto)



unique(coffee$Timezone)
coffee

library(RVAideMemoire)
#install.packages("RVAideMemoire")
library(car)

# Teste de shapiro
## H0 = dist.normal
## H1 = n dist.normal
RVAideMemoire::byf.shapiro(Profit~Type,data=coffee) #Distribioção não é noraml
t.test(Profit~Type,data=coffee) # não posso usar

# Tste de Levene
## As variações são homogenas, h0
car::leveneTest(Profit~Type,data=coffee)
car::leveneTest(Profit~Type,data=coffee, center=mean)

car::leveneTest(Profit~Type,data=coffee)
# Não posso usar o teste T
# H0 mediana é igual
# h1 mediana é diferente
wilcox.test(Profit~Type,data=coffee)

#install.packages('rstatix')
library(rstatix)

coffee %>% group_by(Type) %>%
        get_summary_stats(Profit, type = 'median_iqr')

coffee %>% group_by(Type) %>%
        get_summary_stats(Profit, type = 'mean_sd')

hist(coffee$Profit[coffee$Type=='Decaf'])
hist(coffee$Profit[coffee$Type=='Regular'])

car::leveneTest(Profit~State,data=coffee)
RVAideMemoire::byf.shapiro(Profit~State,data=coffee)

#Ho, medianas são iguais

kruskal.test(Profit~State,data=coffee)
kruskal_test(Profit~State,data=coffee)

#poshoc,

#Teste de Dunn com ajuste do valor p, ele ajusta o valor de p para varias analises e informa se a difrença é significativa
dun=rstatix::dunn_test(Profit~State,data=coffee, p.adjust.method = 'bonferroni')
as.data.frame(dun)

# Análises DEscritiva os Dados
coffee %>% group_by(State) %>%
        rstatix::get_summary_stats(Profit, type='median_iqr')

boxplot(Profit~State,data=coffee, horizontal = T)

ggplot(data=coffee) +
        geom_boxplot(aes(x=State,y=Profit,fill=Timezone))


#Analise de distribuição
coffee %>% 
        ggplot() +
        geom_histogram(aes(Profit,fill=Timezone))  +
        facet_wrap(.~State)
        
 
        
        

colnames(coffee)

# Teste correlações -----

plot(Profit ~ Marketing, data= coffee)

coffee$Type
dplyr::filter(coffee,Type=='Decaf') %>%
        ggplot() + geom_point(aes(x=Marketing,y=Profit))

dplyr::filter(coffee,Type=='Regular') %>%
        ggplot() + geom_point(aes(x=Marketing,y=Profit))

ggplot(data=coffee)+
        geom_point(aes(x=Marketing,y=Profit)) +
        facet_wrap(.~Type)

ggplot(data=coffee)+
        geom_point(aes(x=Marketing,y=Profit)) +
        facet_wrap(.~State)

 plot(Profit ~ Inventory, data= coffee)       
 
 ggplot(data=coffee)+
        geom_point(aes(x=Inventory,y=Profit)) +
        facet_wrap(.~Type)
 
 ggplot(data=coffee)+
        geom_point(aes(x=Inventory,y=Profit)) +
        facet_wrap(.~State)

 plot(Profit ~ Coffee_Sales, data= coffee)
  plot(Coffee_Sales~Inventory, data= coffee)

  ggplot(data=coffee)+
        geom_point(aes(x=Inventory,y=Coffee_Sales)) +
        facet_wrap(.~State)
  
 #saveRDS(coffee,'coffee')
 #save.image(here('code','codigo_base.RDATA'))
 