# Libraries ----
#pacotes
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(quadprog)
library(highcharter)
library(readxl)
require(xts)
library(plotly)
library(magrittr)
library(timeSeries)
library(corrplot)
library(tbl2xts)
library(bizdays)
library(PerformanceAnalytics)
library(rsconnect)
library(shiny)
library(ggplot2)



#carrega base de dados
dados <- readRDS("input//dados.RDS")
retornos <- readRDS("input//retornos.RDS")


retornos_xts <- retornos
colnames(retornos_xts) <- c("AZ_QUEST","WESTERN_ASSET","ALASK_BLACK","TREND_INFLAÇÃO","TREND_IBOVESPA")

#separacao periodo de risco

j_D21 <- 21
j_D63 <- 63
j_D252 <- 252


#tabela de retornos
tabela_21D  <- table.AnnualizedReturns(retornos_xts,scale=j_D21, digits=6)
tabela_63D  <- table.AnnualizedReturns(retornos_xts,scale=j_D63, digits=6)
tabela_252D  <- table.AnnualizedReturns(retornos_xts,scale=j_D252, digits=6)

#transforma em ts
retornos_tb <- xts_tbl(retornos_xts)

#datas das janelas
D21 <- offset(as.Date(max(retornos_tb$date)), -(j_D21-1), cal = 'Brazil/ANBIMA')
D63 <- offset(as.Date(max(retornos_tb$date)), -(j_D63-1), cal = 'Brazil/ANBIMA')
D252 <- offset(as.Date(max(retornos_tb$date)), -(j_D252-1), cal = 'Brazil/ANBIMA')

# Selecionando amostra nos fundos

fund_return_xts_D21 <- retornos_xts %>% xts_tbl() %>% 
  dplyr::filter(between(date,D21,max(retornos_tb$date))) %>% tbl_xts()
fund_return_xts_D63 <- retornos_xts %>% xts_tbl() %>% 
  dplyr::filter(between(date,D63,max(retornos_tb$date))) %>% tbl_xts()
fund_return_xts_D252 <- retornos_xts %>% xts_tbl() %>% 
  dplyr::filter(between(date,D252,max(retornos_tb$date))) %>% tbl_xts()


#estatisticas dos fundos
stat_21D  <- table.Stats(fund_return_xts_D21,ci = 0.95, digits = 4)
stat_63D  <- table.Stats(fund_return_xts_D63,ci = 0.95, digits = 4)
stat_252D  <- table.Stats(fund_return_xts_D252,ci = 0.95, digits = 4)

##output limpo para visulazacao
outout_21d <- rbind(tabela_21D,stat_21D[c(1,15,16),])
outout_63d <- rbind(tabela_63D,stat_63D[c(1,15,16),])
outout_252d <- rbind(tabela_252D,stat_252D[c(1,15,16),])



# tabela de kpi
kpis_riscos_21d<- t(outout_21d)
nomes<- rownames(kpis_riscos_21d)
kpis_riscos_21d <- as.data.frame(kpis_riscos_21d) 

kpis_riscos_21d <- as.tibble(cbind(kpis_riscos_21d,nomes)) 

kpis_riscos_21d$nomes <- as.character(kpis_riscos_21d$nomes )
kpis_riscos_21d %<>% select(Fundo= nomes, `Retorno a.a` =`Annualized Return`,`Vol a.a` =`Annualized Std Dev`,
                            `Sharpe a.a`= 3, Observacoes = Observations, Assimetria =Skewness,
                            Curtose = Kurtosis) %>% 
  arrange(desc(`Retorno a.a`))

kpis_riscos_63d<- t(outout_63d)
nomes<- rownames(kpis_riscos_63d)

kpis_riscos_63d <- as.data.frame(kpis_riscos_63d) 

kpis_riscos_63d <- as.tibble(cbind(kpis_riscos_63d,nomes)) 

kpis_riscos_63d$nomes <- as.character(kpis_riscos_63d$nomes )
kpis_riscos_63d %<>% select(Fundo= nomes, `Retorno a.a` =`Annualized Return`,`Vol a.a` =`Annualized Std Dev`,
                             `Sharpe a.a`= 3, Observacoes = Observations, Assimetria =Skewness,
                             Curtose = Kurtosis) %>% 
  arrange(desc(`Retorno a.a`))


kpis_riscos_252d<- t(outout_252d)
nomes<- rownames(kpis_riscos_252d)

kpis_riscos_252d <- as.data.frame(kpis_riscos_252d) 

kpis_riscos_252d <- as.tibble(cbind(kpis_riscos_252d,nomes)) 

kpis_riscos_252d$nomes <- as.character(kpis_riscos_252d$nomes )
kpis_riscos_252d %<>% select(Fundo= nomes, `Retorno a.a` =`Annualized Return`,`Vol a.a` =`Annualized Std Dev`,
                             `Sharpe a.a`= 3, Observacoes = Observations, Assimetria =Skewness,
                             Curtose = Kurtosis) %>% 
  arrange(desc(`Retorno a.a`))



##retorno acumlado dos periodos de entrada
ret_cum <- tibble()
for(i in 1:nrow(dados)){

  df_filtro <- retornos_tb %>% 
    select(date,str_trim(str_replace_all(dados$NOME[i]," ","_"))) %>% 
    dplyr::filter(date>dados$date[i])
  
  df_filtro_xts <-  df_filtro%>% tbl_xts()
  df_filtro_xts <-   (cumprod(1 + df_filtro_xts)-1) %>% xts_tbl()

  df_filtro_xts  %<>%  
    mutate_if(is.numeric,list( ~dados$VL_APLICADO[i]+(.)*dados$VL_APLICADO[i])) %>% 
    mutate(FUNDO = dados$NOME[i]) %>% 
    rename(RET_APP = str_trim(str_replace_all(dados$NOME[i]," ","_")))
  
  ret_cum <- bind_rows(ret_cum,df_filtro_xts)
}


#soma do patrimonio
sum_pat <- ret_cum %>% 
  group_by(date) %>% 
  summarise(Patrimonio = sum(RET_APP)) %>% 
  tbl_xts()
#valor aplicado
vl_aplicado <- dados %>% 
  select(date,VL_APLICADO) %>% 
  group_by(date) %>% 
  summarise(Aplicado = sum(VL_APLICADO))

sum_cum_aplicado <- cumsum(vl_aplicado$Aplicado)
tablela_valor <- cbind(vl_aplicado,sum_cum_aplicado)  
colnames(tablela_valor)[ncol(tablela_valor)] <-"Aplicado Acm" 

#transforma em ts
tablela_valor <- tablela_valor %>%tbl_xts()







## ui visualizacao do usuario

ui <- dashboardPage(
  
  # Application title
  skin = "black",
  
  #cabeçalho
  dashboardHeader(
    
    title = "Dashboard Fundos",titleWidth = 400,
                    tags$li(a(href = 'https://games.crossfit.com/sanctionals',
                              img(src = 'bull.jpg',height='60',width='150'),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown")
  ),
  
  # barra lateral
  dashboardSidebar(
    width = 400,
    h3('Fundos da Lívia', align = 'center'),
    sidebarMenu(menuItem(text = 'Geral', icon = icon('donate'), 
                         tabName = 'geral_fundos')),
    
    sidebarMenu(menuItem(text = 'Risco', icon = icon('line-chart'), 
                         tabName = 'risco')),
    hr()
    
  ),
  # corpo do aplicatico
  dashboardBody(
    tabItems(
      #primeira aba
      tabItem(tabName = "geral_fundos",tabsetPanel(
        id = "ABAS",
        tabPanel("Nivel 1 - Carteira",
                 fluidRow(
                   fluidRow(h1(' Rentabilidade', align = 'center')),
                   br(),br(),br(),
                   column(8, highchartOutput('desempenho')),
                    column(4,
                          box(title="Rentabilidade por Fundos ",
                              status = "primary",solidHeader = T, width = 12,
                              div(DTOutput('df_resumo'),style = "font-size: 80%")
                              ))
                   
                 )
        ),
        #segunda aba
        tabPanel("Nivel 2 - Fundos",
            fluidRow(h1('Rentabilidade dos Fundos', align = 'center')),
            br(),
            br(),
             radioButtons(inputId = "fundos",label = "",choices = unique((dados$NOME)),inline=T,
                                         selected = unique((dados$NOME))[1]),
            column(9, highchartOutput('desempenho_fundos'))

                 ),
        tabPanel("Simula Fundos",
                 fluidRow(h1('Simulador dos Fundos', align = 'center')),
                 br(),
                 br(),
                 radioButtons(inputId = "fundo_simula",label = " Escolha o Fundo",choices = unique((dados$NOME)),inline=T,
                              selected = unique((dados$NOME))[1]),
                 
                 dateRangeInput("simula_datas", "Data Entrada", start = min(retornos_tb$date), end = max(retornos_tb$date), min = min(retornos_tb$date),
                                max = max(retornos_tb$date), format = "yyyy-mm-dd"),
                 h3("Clica que calcula"),
                 actionButton("simula_botao", "Click here!"),
                 fluidRow(column(9, highchartOutput('compara_fundos'))
                          ),
                 fluidRow(valueBoxOutput("rentabilidade_simulacao"),
                                                           valueBoxOutput("rentabilidade_livia"))

        )
        

      
      
    )
    
    
  ),
  #aba de risco
  tabItem(tabName = "risco",tabsetPanel(
    id = "ABAS_2",
    tabPanel("Nivel 1 - Carteira",
             fluidRow(
               box(title=" 21 Dias",
                   status = "primary",solidHeader = T,width = 12,
                   div(DT::dataTableOutput("risco_21d"),style = "font-size: 100%"))),
             fluidRow(
               box(title=" 63 Dias",
                   status = "primary",solidHeader = T,width = 12,
                   div(DT::dataTableOutput("risco_63d"),style = "font-size: 100%"))),
             fluidRow(
               box(title=" 252 Dias",
                   status = "primary",solidHeader = T,width = 12,
                   div(DT::dataTableOutput("risco_252d"),style = "font-size: 100%")))
             
    )
  )
  )
)
)
)

# retorno ao servidor para o usuario ----
server <- function(input, output) {
  
  # grafico de desempenho
  output$desempenho <- renderHighchart({

    highchart(type = "stock") %>% 
      hc_title(text = "Gráfico de Rentabilidade", style = list(color = "black", fontWeight = "bold")) %>% 
      hc_add_series(sum_pat[,1],
                    name = names(sum_pat)[1], id= "fundo") %>%
      hc_scrollbar(enabled = FALSE) %>% 
      hc_yAxis(opposite = FALSE)
    
    
    
  })
  
  # grafico de desempenho de cada fundo
  output$desempenho_fundos<- renderHighchart({
    
    
    fundos_in <- input$fundos
    df_dados <- dados %>% dplyr::filter(NOME ==  fundos_in) %>% arrange(date) %>% slice((1))
# melhor debug de shiny
print(head(df_dados))    
    # dados$NOME
    df_filtro <- retornos_tb %>% 
      select(date,str_trim(str_replace_all(fundos_in," ","_"))) %>% 
      dplyr::filter(date >= df_dados$date[1])
    
    
    df_filtro_ts <-  df_filtro%>% tbl_xts()
    df_filtro_ts <-   (cumprod(1 + df_filtro_ts)-1)
    
    highchart(type = "stock") %>% 
      hc_title(text = "Gráfico de Rentabilidade", style = list(color = "black", fontWeight = "bold")) %>% 
      hc_add_series(100*df_filtro_ts[,1],
                    name = names(df_filtro_ts)[1], id= "fundo") %>%
      hc_legend(enabled = TRUE) %>% 
      hc_scrollbar(enabled = FALSE) %>% 
      hc_yAxis(opposite = FALSE, labels = list(format = "{value}%"))
    
  })
  

  
## base de dados reativa  
  df_final_tt <- reactive({  
  df_final_tt <- as_tibble()
  
  for( i in 1: nrow(dados)){
    
    df_filtro <- retornos_tb %>%
      select(date,str_trim(str_replace_all(dados$NOME[i]," ","_"))) %>%
      dplyr::filter(date>dados$date[i])
    
    df_filtro_ts <-  df_filtro%>% tbl_xts()
    df_filtro_ts <-   (cumprod(1 + df_filtro_ts)-1) %>% xts_tbl()
    df_final <- df_filtro_ts%>% slice(n()) %>%
      mutate(Fundo = str_trim(str_replace_all(dados$NOME[i]," ","_"))) %>%
      rename('Retorno a.p ' = str_trim(str_replace_all(dados$NOME[i]," ","_")))
    
    df_final_tt <- bind_rows(df_final_tt,df_final)
    
    
  }
  df_final_tt
  })  
  ##output da tabela 
  
  output$df_resumo <-  renderDT({
   
    
  datatable(df_final_tt(), rownames = F, options = list(pageLength = 20, dom = 'pd')) %>%
    formatPercentage(columns = 2, 2)
})
  
  
  ##evento reativo ao click do botao para simular os retornos
  
  retornos_simula_app <-  eventReactive(input$simula_botao, {
     
    retornos_tb %>% 
      select(date,str_trim(str_replace_all(input$fundo_simula," ","_"))) %>% 
      dplyr::filter(between(date,input$simula_datas[1],input$simula_datas[2]))
    

  })
  
  
  ##evento reativo ao click do botao para simular os retornos
  retornos_simulados_output <-  eventReactive(input$simula_botao, {
    

    
    
   datas_sim <- dados %>% mutate(NOME = as.character(NOME)) %>% dplyr::filter(NOME==input$fundo_simula) %>% select(date) %>% arrange(date)
      
   
   output_retornos_sim <- retornos_tb %>%  select(date,str_trim(str_replace_all(input$fundo_simula," ","_"))) %>%
     dplyr::filter(date>datas_sim[1,1])
   
   output_retornos_sim 
      
    
  })
  
  # acumular o retorno da simulacao no periodo
  
simulador_acum_ret <- reactive({
  
  simulador_acum_ret <-  retornos_simula_app()%>% tbl_xts()
  simulador_acum_ret <-   (cumprod(1 + simulador_acum_ret)-1)
  simulador_acum_ret

  
  
})  

# acumular o retorno da livia no periodo comparar no box de output
simulador_acum_ret_output <- reactive({

  simulador_acum_ret_output <-  retornos_simulados_output()%>% tbl_xts()
  simulador_acum_ret_output <-   (cumprod(1 + simulador_acum_ret_output)-1)
  simulador_acum_ret_output



})

#box com o valor do retorno simulado

output$rentabilidade_simulacao <-  renderValueBox({
  

  

  valueBox( 
    
    paste0(round(as.vector(simulador_acum_ret()[nrow(simulador_acum_ret())])*100,2), "%", sep="")
    ,'Retorno Simulacao'
    ,icon = icon("star",lib='glyphicon')
    ,color = "red")
  
  
})
#box com o valor do retorno da livia

   output$rentabilidade_livia <-  renderValueBox({
     valueBox( 
       
       paste0(round(as.vector(simulador_acum_ret_output()[nrow(simulador_acum_ret_output())])*100,2), "%", sep="")
              ,'Retorno  Livia'
              ,icon = icon("star",lib='glyphicon')
              ,color = "blue")
     
     
   })
 
   # output com o retorno do fundo no periodo  
   
   output$compara_fundos<- renderHighchart({
     
    df_cum_ret <-   simulador_acum_ret()
     
     highchart(type = "stock") %>% 
       hc_title(text = "Gráfico de Rentabilidade", style = list(color = "black", fontWeight = "bold")) %>% 
       hc_add_series(100*df_cum_ret[,1],
                     name = names(df_cum_ret)[1], id= "fundo") %>%
       hc_legend(enabled = TRUE) %>% 
       hc_scrollbar(enabled = FALSE) %>% 
       hc_yAxis(opposite = FALSE, labels = list(format = "{value}%"))
     
   })

   # tabela com medidas de risco de 21 dias   
   
   output$risco_21d <- DT::renderDataTable({datatable(kpis_riscos_21d, rownames = F
   ) %>% 
       formatPercentage(columns = c(2,3,9),digits = 2) %>%  
       formatCurrency(c(4,5,6,7,8), currency = "", interval = 3, mark = ",", 
                      digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
   })
   # tabela com medidas de risco de 63 dias
      output$risco_63d <- DT::renderDataTable({datatable(kpis_riscos_63d, rownames = F
   ) %>% 
       formatPercentage(columns = c(2,3,9),digits = 2) %>%  
       formatCurrency(c(4,5,6,7,8), currency = "", interval = 3, mark = ",", 
                      digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
   })
      
      # tabela com medidas de risco de 252 dias
   output$risco_252d <- DT::renderDataTable({datatable(kpis_riscos_252d, rownames = F
   ) %>% 
       formatPercentage(columns = c(2,3,9),digits = 2) %>%  
       formatCurrency(c(4,5,6,7,8), currency = "", interval = 3, mark = ",", 
                      digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
   })

}
# Run the application 
shinyApp(ui = ui, server = server)
