
# 1) Função ---------------------------------------------------------------

forecastSelection <- function(st, start_date,
                              tt_s_r=0.75,freq='month'){ #st: série temporal
                                                         #start_date: data de início da série. Se os valores são mensais, por exemplo, pode ser uma data fictícia tipo o primeiro dia.
                                                         #tt_s_r: train and test split ratio
                                                         #freq: frequência da série. Por hora, só week e month
  ifelse(require(prophet),library(prophet),{install.packages('prophet');library(prophet)})
  ifelse(require(forecast),library(forecast),{install.packages('forecast');library(forecast)})
  ifelse(require(tscount),library(tscount),{install.packages('tscount');library(tscount)})
  ifelse(require(tseries),library(tseries),{install.packages('tseries');library(tseries)})
  ifelse(require(MLmetrics),library(MLmetrics),{install.packages('MLmetrics');library(MLmetrics)})
  ifelse(require(strucchange),library(strucchange),{install.packages('strucchange');library(strucchange)})
  ifelse(require(segmented),library(segmented),{install.packages('segmented');library(segmented)})
  ifelse(require(lubridate),library(lubridate),{install.packages('lubridate');library(lubridate)})

  if(st|>length()<20){ # 20 por conta do parametro "h" do piecewise regression
    return("FALSE")
  }
  
  st0=st
  st=st[1:ceiling(length(st0)*tt_s_r)]
  st_test=st0[(ceiling(length(st0)*tt_s_r)+1):length(st0)]
  
  Periodo0=1:length(st0) # Acredite... precisa disso
  Periodo=Periodo0[1:ceiling(length(Periodo0)*tt_s_r)]
  Periodo_test=Periodo0[(ceiling(length(Periodo0)*tt_s_r)+1):length(Periodo0)]
  
# 1.1) Modelos ------------------------------------------------------------

  
  # m0: regressão linear simples
  m0 <- lm(st~Periodo,data={dados_para_lm=data.frame("st"=st,"Periodo"=Periodo)})
  
  # m1: autoarima
  
  m1 <- auto.arima(ts(data = st,
                      frequency = ifelse(freq=="month",12,
                                         ifelse(freq=="week",365.25/7,1)),
                      start=ifelse(freq=="month",start_date,
                                   decimal_date(ymd(start_date)))))
  
  # m2: fbProphet flat
  m2 <- prophet(data.frame(ds=seq(start_date, 
                                  by = ifelse(freq=="month","month","week"),
                                  length = length(st)),
                           y=st),
                interval.width = 0.95,growth="flat")
  
  # m3: fbProphet Linear
  m3 <- prophet(data.frame(ds=seq(start_date, 
                                  by = ifelse(freq=="month","month","week"),
                                  length = length(st)),
                           y=st),
                interval.width = 0.95,growth="linear")
  # m4: tscount Poisson
  m4 <- tsglm(st, distr = "poisson")
  
  # m5: tscount NBinom
  m5 <- tsglm(st, distr = "nbinom")
  
  # m6: Suavização exponencial (Holt-Winters)
  if(freq=="month"|freq=="week"){
    m6 <- HoltWinters(ts(data = st,
                         frequency = ifelse(freq=="month",12,365.25/7),
                         start=ifelse(freq=="month",start_date,
                                      decimal_date(ymd(start_date)))),gamma = FALSE)
  }
  
  # m7: Strucchange
  if(breakpoints(st~Periodo)$breakpoints|>is.na()){
    m7=m0
  }else{
    m7 <- segmented(m0,
                    seg.Z = ~Periodo,
                    psi=breakpoints(st~Periodo)$breakpoints)  
  }

# 1.2) Testes -------------------------------------------------------------
  
  # m0: regressão linear simples
  result_m0 <- predict(m0,newdata={dados_para_lm=data.frame("st"=st_test,"Periodo"=Periodo_test)})|>as.numeric()
  
  # m1: autoarima
  result_m1 <- forecast(m1,h=length(st_test))$mean|>as.numeric()
  
  # m2: fbProphet flat
  result_m2 <- tail(predict(m2,
                            make_future_dataframe(m2,
                                                  freq = ifelse(freq=="month","month","week"),
                                                  periods = length(st_test))),length(st_test))[,16]
  
  # m3: fbProphet Linear
  result_m3 <- tail(predict(m3,
                            make_future_dataframe(m3,
                                                  freq = ifelse(freq=="month","month","week"),
                                                  periods = length(st_test))),length(st_test))[,16]  
  # m4: tscount Poisson
  result_m4 <-   as.vector(predict(m4,n.ahead=length(st_test))$pred)
  
  # m5: tscount NBinom
  result_m5 <-   as.vector(predict(m5,n.ahead=length(st_test))$pred)
  
  # m6: Suavização exponencial (Holt-Winters)
  result_m6 <- predict(m6, n.ahead=length(st_test))|>as.numeric()
  
  # m7: Strucchange
  result_m7 <- predict(m7,newdata={dados_para_lm=data.frame("st"=st_test,"Periodo"=Periodo_test)})|>as.numeric()

# 1.3) Resultados ---------------------------------------------------------
  
  resultados<- data.frame("Real" = st_test,
                          "SLR" = result_m0,
                          "Autoarima"=result_m1,
                          "fbProphet Flat"=result_m2,
                          "fbProphet Linear"=result_m3,
                          "tsCount Poisson"=result_m4,
                          "tsCount NBinomial"=result_m5,
                          "ETS HoltWinters"=result_m6,
                          "Piecewise"=result_m7)
  RMSE_resultado<- data.frame("SLR"=RMSE(resultados$SLR,resultados$Real),
                              "Autoarima"=RMSE(resultados$Autoarima,resultados$Real),
                              "fbProphet Flat"=RMSE(resultados$fbProphet.Flat,resultados$Real),
                              "fbProphet Linear"=RMSE(resultados$fbProphet.Linear,resultados$Real),
                              "tsCount Poisson"=RMSE(resultados$tsCount.Poisson,resultados$Real),
                              "tsCount NBinomial"=RMSE(resultados$tsCount.NBinomial,resultados$Real),
                              "ETS HoltWinters"=RMSE(resultados$ETS.HoltWinters,resultados$Real),
                              "Piecewise"=RMSE(resultados$Piecewise,resultados$Real))
  return(names(RMSE_resultado)[RMSE_resultado==min(RMSE_resultado)])

  
}

forecastMaking <- function(st, start_date,periods_ahead,
                           result=FALSE,freq='month'){#st: série temporal
                                                #start_date: data de início da série. Se os valores são mensais, por exemplo, pode ser uma data fictícia tipo o primeiro dia.
                                                #periods_ahead: número de períodos a frente
                                                #result: aqui coloca o nome do modelo, saída da função 'forecastSelection'
                                                #freq: frequência da série. Por hora, só week e month
  ifelse(require(prophet),library(prophet),{install.packages('prophet');library(prophet)})
  ifelse(require(forecast),library(forecast),{install.packages('forecast');library(forecast)})
  ifelse(require(tscount),library(tscount),{install.packages('tscount');library(tscount)})
  ifelse(require(tseries),library(tseries),{install.packages('tseries');library(tseries)})
  ifelse(require(MLmetrics),library(MLmetrics),{install.packages('MLmetrics');library(MLmetrics)})
  ifelse(require(strucchange),library(strucchange),{install.packages('strucchange');library(strucchange)})
  ifelse(require(segmented),library(segmented),{install.packages('segmented');library(segmented)})
  ifelse(require(lubridate),library(lubridate),{install.packages('lubridate');library(lubridate)})
  options(warn=-1)
  
  if(result=="SLR"){
    Periodo=1:length(st) # Acredite... precisa disso
    modelo <- lm(st~Periodo,data={dados_para_lm=data.frame("st"=st,
                                                           "Periodo"=Periodo)})
    predicao <- predict(modelo,newdata={dados_para_lm=data.frame("st"=rnorm(periods_ahead), #qualquer valor...
                                                                 "Periodo"=seq(max(Periodo)+1,
                                                                               max(Periodo)+periods_ahead,
                                                                               by=1))})|>as.numeric()
    return(predicao)
    
  }else if(result=="Autoarima"){
    modelo <- auto.arima(ts(data = st,
                        frequency = ifelse(freq=="month",12,
                                           ifelse(freq=="week",365.25/7,1)),
                        start=ifelse(freq=="month",start_date,
                                     decimal_date(ymd(start_date)))))
    predicao <- forecast(modelo,h=periods_ahead)$mean|>as.numeric()
    return(predicao)
    
  }else if(result=="fbProphet Flat"){
    modelo <- prophet(data.frame(ds=seq(start_date, 
                                    by = ifelse(freq=="month","month","week"),
                                    length = length(st)),
                             y=st),
                  interval.width = 0.95,growth="flat")
    predicao <- tail(predict(modelo,
                             make_future_dataframe(modelo,
                                                   freq = ifelse(freq=="month","month","week"),
                                                   periods = periods_ahead)),periods_ahead)[,16]
    return(predicao)
    
  }else if(result=="fbProphet Linear"){
    modelo <- prophet(data.frame(ds=seq(start_date, 
                                        by = ifelse(freq=="month","month","week"),
                                        length = length(st)),
                                 y=st),
                      interval.width = 0.95,growth="linear")
    predicao <- tail(predict(modelo,
                             make_future_dataframe(modelo,
                                                   freq = ifelse(freq=="month","month","week"),
                                                   periods = periods_ahead)),periods_ahead)[,16]
    return(predicao)
    
  }else if(result=="tsCount Poisson"){
    modelo <- tsglm(st, distr = "poisson")
    predicao <-   as.vector(predict(modelo,n.ahead=periods_ahead)$pred)
    return(predicao)
    
  }else if(result=="tsCount NBinomial"){
    modelo <- tsglm(st, distr = "nbinom")
    predicao <-   as.vector(predict(modelo,n.ahead=periods_ahead)$pred)
    return(predicao)
    
  }else if(result=="ETS HoltWinters"){
    modelo <- HoltWinters(ts(data = st,
                         frequency = ifelse(freq=="month",12,365.25/7),
                         start=ifelse(freq=="month",start_date,
                                      decimal_date(ymd(start_date)))),gamma = FALSE)
    predicao <- predict(modelo, n.ahead=periods_ahead)|>as.numeric()
    return(predicao)
    
  }else if(result=="Piecewise"){
    Periodo=1:length(st) # Acredite... precisa disso
    modelo0 <- lm(st~Periodo,data={dados_para_lm=data.frame("st"=st,"Periodo"=Periodo)})
    modelo <- segmented(modelo0,
                        seg.Z = ~Periodo,
                        psi=breakpoints(st~Periodo)$breakpoints)
    predicao <- predict(modelo,newdata={dados_para_lm=data.frame("st"=rnorm(periods_ahead), #qualquer valor...
                                                                 "Periodo"=seq(max(Periodo)+1,
                                                                               max(Periodo)+periods_ahead,
                                                                               by=1))})|>as.numeric()
    return(predicao)
    
  }else{
    # só a média
    return(rep(mean(st),times=periods_ahead))
    
  }
  
}

# 2) Teste ----------------------------------------------------------------

forecastSelection(rpois(100,10),as.Date("1994-11-08"))

forecastSelection(rpois(100,15),as.Date("1994-11-08"))

forecastSelection(rpois(10,15),as.Date("1994-11-08"))


forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"SLR")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"Autoarima")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"fbProphet Flat")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"fbProphet Linear")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"tsCount Poisson")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"tsCount NBinomial")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"ETS HoltWinters")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6,"Piecewise")
forecastMaking(rpois(100,10),as.Date("1994-11-08"),6)
forecastMaking(rpois(10,10),as.Date("1994-11-08"),6,forecastSelection(rpois(10,15),as.Date("1994-11-08")))


# 3) Salvando as funções criadas ------------------------------------------

setwd("//vcn.ds.volvo.net/parts-cta/PartsPROJ02/004998/DIP/19. Machine Learning/Dealer Spot Demand")
saveRDS(forecastSelection,"forecastSelectionFUN.RData")
saveRDS(forecastMaking,"forecastMakingFUN.RData")





