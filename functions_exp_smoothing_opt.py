import numpy as np
import pandas as pd

def simple_exp_smooth(d, extra_periods=1, alpha=0.4): # f_{t}=a*d_{t-1}+(1-a)*f_{t-1}
    # Quantidade de observações passadas
    cols=len(d)

    # Coloque valores "NA" (com np.nan) no final do vetor, para cobrir os períodos futuros
    d=np.append(d,[np.nan]*extra_periods)

    # Definindo o array/vetor de forecast
    f=np.full(cols+extra_periods,np.nan)
    # Inicialização do primeiro forecast
    f[1]=d[0]

    # Criar todas as t+1 forecast até o fim do período histórico
    for t in range(2,cols+1):
        f[t]=alpha*d[t-1]+(1-alpha)*f[t-1]
    
    # Forecast para todos os períodos extras
    for t in range(cols+1,cols+extra_periods):
        # Atualizar o forecast como o forecast anterior
        f[t]=f[t-1]



    # Retorne um data frame com a Demand, o forecast e o Error
    df=pd.DataFrame.from_dict({'Demand':d,"Forecast":f,"Error":d-f})

    return df

def double_exp_smooth(d, extra_periods=1, alpha=0.4, beta=0.4):
    # Historical period length
    cols = len(d)
    # Append np.nan into the demand array to cover future periods
    d = np.append(d,[np.nan]*extra_periods)
    # Creation of the level, trend and forecast arrays
    f,a,b = np.full((3,cols+extra_periods),np.nan) # aqui criamos 3 arrays vazios para f,a e b. 
    # Level & Trend initialization
    a[0] = d[0]
    b[0] = d[1] - d[0]
    # Create all the t+1 forecast
    for t in range(1,cols):
        f[t] = a[t-1] + b[t-1]
        a[t] = alpha*d[t] + (1-alpha)*(a[t-1]+b[t-1])
        b[t] = beta*(a[t]-a[t-1]) + (1-beta)*b[t-1]
    # Forecast for all extra periods
    for t in range(cols,cols+extra_periods):
        f[t] = a[t-1] + b[t-1]
        a[t] = f[t]
        b[t] = b[t-1]
    
    df = pd.DataFrame.from_dict({'Demand':d,'Forecast':f,'Level':a,'Trend':b, 'Error':d-f})

    return df

def double_exp_smooth_mult(d, extra_periods=1, alpha=0.4, beta=0.4):
    # Historical period length
    cols = len(d)
    # Append np.nan into the demand array to cover future periods
    d = np.append(d,[np.nan]*extra_periods)
    # Creation of the level, trend and forecast arrays
    f,a,b = np.full((3,cols+extra_periods),np.nan) # aqui criamos 3 arrays vazios para f,a e b. 
    # Level & Trend initialization
    a[0] = d[0]
    b[0] = d[1] - d[0]
    # Create all the t+1 forecast
    for t in range(1,cols):
        if t==1:
            f[t]=d[1]
        else:
            f[t] = a[t-1]*b[t-1]
        a[t] = alpha*d[t] + (1-alpha)*(a[t-1]+b[t-1])
        b[t] = beta*(a[t]/a[t-1]) + (1-beta)*b[t-1]
    # Forecast for all extra periods
    for t in range(cols,cols+extra_periods):
        f[t] = a[t-1]*b[t-1]
        a[t] = f[t]
        b[t] = b[t-1]
    
    df = pd.DataFrame.from_dict({'Demand':d,'Forecast':f,'Level':a,'Trend':b, 'Error':d-f})

    return df

def double_exp_smooth_damped(d, extra_periods=1,alpha=0.4,beta=0.4,phi=0.9):
    cols=len(d) # quantidade de observações
    d=np.append(d,[np.nan]*extra_periods) # junta 'np.nan' com as observações para cobrir os períodos futuros. Meio que já deixa o espaço deles reservado rsrsrsrs

    # Criando os arrays vazios para: Level (a), Trend (b) e o forecast (f)
    f,a,b=np.full((3,cols+extra_periods),np.nan)

    # Fazendo a inicialização do a e b:
    a[0]=d[0] # No 'a' vai a primeira observação mesmo,
    b[0]=d[1]-d[0] # E aqui no 'b' vai a primeira diferença das observações

    for t in range(1,cols): # aqui, vai até o período t+1
        f[t]=a[t-1]+phi*b[t-1]
        a[t]=alpha*d[t]+(1-alpha)*(a[t-1]+phi*b[t-1])
        b[t]=beta*(a[t]-a[t-1])+(1-beta)*phi*b[t-1]

    # E aqui, fazemos o forecast para os períodos extras.
    for t in range(cols, cols+extra_periods):
        f[t]=a[t-1]+phi*b[t-1]
        a[t]=f[t]
        b[t]=phi*b[t-1]
    
    df=pd.DataFrame.from_dict({'Demand':d,'Forecast':f,'Level':a,'Trend':b,'Error':d-f})

    return df

def seasonal_factors_mul(s,d,slen,cols):
    for i in range(slen):
        s[i]=np.mean(d[i:cols:slen]) # Pegando a média da temporada. Esse esquema do array é assim: array[começo:fim:tamanho do passo (se de 2 em 2, 3 em 3... e etc)]
    s/=np.mean(s[:slen]) # o sinal '/=' faz o mesmo que '+=' só que divide daí.

    return s

def triple_exp_smooth_mul(d, slen=12, extra_periods=1, alpha=0.4,beta=0.4, phi=0.9, gamma=0.3):
    cols = len(d) # número de observações
    d=np.append(d,[np.nan]*extra_periods) # Coloca os espaços para os períodos futuro

    # componentes de inicialização e inicialização da sazonalidade
    f,a,b,s=np.full((4,cols+extra_periods),np.nan)
    s=seasonal_factors_mul(s,d,slen,cols)

    # Inicialização do Level e Trend
    a[0]=d[0]/s[0]
    b[0]=d[1]/s[1]-d[0]/s[0]

    # Criando os forecasts para a primeira temporada (first season)
    for t in range(1,slen):
        f[t]=(a[t-1]+phi*b[t-1])*s[t]
        a[t]=alpha*d[t]/s[t]+(1-alpha)*(a[t-1]+phi*b[t-1])
        b[t]=beta*(a[t]-a[t-1])+(1-beta)*phi*b[t-1]

    # Forecast para todos os períodos, antes de chegar nos períodos extras
    for t in range(slen,cols):
        f[t]=(a[t-1]+phi*b[t-1])*s[t-slen]
        a[t]=alpha*d[t]/s[t-slen]+(1-alpha)*(a[t-1]+phi*b[t-1])
        b[t]=beta*(a[t]-a[t-1])+(1-beta)*phi*b[t-1]
        s[t]=gamma*d[t]/a[t]+(1-gamma)*s[t-slen]        

    # Forecast para todos os períodos extras
    for t in range(cols,cols+extra_periods):
        f[t]=(a[t-1]+phi*b[t-1])*s[t-slen]
        a[t]=f[t]/s[t-slen]
        b[t]=phi*b[t-1]
        s[t]=s[t-slen]

    df=pd.DataFrame.from_dict({'Demand':d,'Forecast':f,'Level':a,'Trend':b,'Temporadas (seasons)':s,'Error':d-f})

    return df

def seasonal_factors_add(s,d,slen,cols):
    for i in range(slen):
        s[i]=np.mean(d[i:cols:slen]) # Pegando a média da temporada. Esse esquema do array é assim: array[começo:fim:tamanho do passo (se de 2 em 2, 3 em 3... e etc)]
    s-=np.mean(s[:slen]) # o sinal '-=' faz o mesmo que '+=' só que subtrai daí.
    return s

def triple_exp_smooth_add(d, slen=12, extra_periods=1, alpha=0.4,beta=0.4, phi=0.9, gamma=0.3):
    cols=len(d) # número de observações
    d=np.append(d,[np.nan]*extra_periods) # coloca os espaços vazios para os próximos períodos

    # componentes de inicialização
    f,a,b,s=np.full((4,cols+extra_periods),np.nan)
    s=seasonal_factors_add(s,d,slen,cols)

    # Inicialização do Level e Trend
    a[0]=d[0]-s[0]
    b[0]=(d[1]-s[1])-(d[0]-s[0])

    # criando o forecast para a primeira Season
    for t in range(1,slen):
        f[t]=a[t-1]+phi*b[t-1]+s[t]
        a[t]=alpha*(d[t]-s[t])+(1-alpha)*(a[t-1]+phi*b[t-1])
        b[t]=beta*(a[t]-a[t-1])+(1-beta)*phi*b[t-1]

    # criando todos os forecast antes dos extra periods
    for t in range(slen, cols):
        f[t]=a[t-1]+phi*b[t-1]+s[t-slen]
        a[t]=alpha*(d[t]-s[t-slen])+(1-alpha)*(a[t-1]+phi*b[t-1])
        b[t]=beta*(a[t]-a[t-1])+(1-beta)*phi*b[t-1]
        s[t] = gamma*(d[t]-a[t]) + (1-gamma)*s[t-slen]

    # criando os forecasts para os períodos futuros
    for t in range(cols, cols+extra_periods):
        f[t]=a[t-1]+phi*b[t-1]+s[t-slen]
        a[t]=f[t]-s[t-slen]
        b[t]=phi*b[t-1]
        s[t]=s[t-slen]

    df=pd.DataFrame.from_dict({'Demand':d,'Forecast':f,'Level':a,'Trend':b,'Season':s,'Error':d-f})
    return df

def ultimate_exp_smooth_opt(d, extra_periods=6,kpi="MAE",alpha_range=np.arange(0.05,0.61,0.01),
                                                         beta_range=np.arange(0.05,0.41,0.01),
                                                         phi_range=np.arange(0,1.1,0.1),
                                                         gamma_range_mult=np.arange(0.05,0.31,0.01),
                                                         gamma_range_add=np.arange(0.05,0.31,0.01)):
    params=[] # Aqui entra todas as combinações de parâmetros
    KPIs=[] # Aqui entra os resultados de cada modelo
    dfs=[] # Aqui entra todos os dataframes retornados pelos modelos ajustados

    for alpha in alpha_range:
        df=simple_exp_smooth(d,extra_periods=extra_periods,alpha=alpha)
        params.append(f'Simple Exp Smoothing, alpha: {alpha}.')
        dfs.append(df)
        if kpi=="MAE":    
            MAE = df['Error'].abs().mean()
            KPIs.append(MAE)
        else:
            RMSE = np.sqrt((df['Error']**2).mean())
            KPIs.append(RMSE)
        
        for beta in beta_range:
            df=double_exp_smooth(d,extra_periods=extra_periods,alpha=alpha,beta=beta)
            params.append(f'Double Exp Smoothing additive, alpha: {alpha} and beta: {beta}.')
            dfs.append(df)
            if kpi=="MAE":    
                MAE = df['Error'].abs().mean()
                KPIs.append(MAE)
            else:
                RMSE = np.sqrt((df['Error']**2).mean())
                KPIs.append(RMSE)

            df=double_exp_smooth_mult(d,extra_periods=extra_periods,alpha=alpha,beta=beta)
            params.append(f'Double Exp Smoothing multiplicative, alpha: {alpha} and beta: {beta}.')
            dfs.append(df)
            if kpi=="MAE":    
                MAE = df['Error'].abs().mean()
                KPIs.append(MAE)
            else:
                RMSE = np.sqrt((df['Error']**2).mean())
                KPIs.append(RMSE)

            for phi in phi_range:
                df=double_exp_smooth_damped(d,extra_periods=extra_periods,alpha=alpha,beta=beta,phi=phi)
                params.append(f'Double Exp Smoothing with Damped Trend, alpha: {alpha}, beta: {beta} and phi: {phi}.')
                dfs.append(df)
                if kpi=="MAE":    
                    MAE = df['Error'].abs().mean()
                    KPIs.append(MAE)
                else:
                    RMSE = np.sqrt((df['Error']**2).mean())
                    KPIs.append(RMSE)
                
                for gamma in gamma_range_mult:
                    df=triple_exp_smooth_mul(d,extra_periods=extra_periods,alpha=alpha,beta=beta,phi=phi,gamma=gamma)
                    params.append(f'Triple Exp Smoothing Multiplicative, alpha: {alpha}, beta: {beta}, phi: {phi} and gamma: {gamma}.')
                    dfs.append(df)
                    if kpi=="MAE":    
                        MAE = df['Error'].abs().mean()
                        KPIs.append(MAE)
                    else:
                        RMSE = np.sqrt((df['Error']**2).mean())
                        KPIs.append(RMSE)

                for gamma in gamma_range_add:
                    df=triple_exp_smooth_add(d,extra_periods=extra_periods,alpha=alpha,beta=beta,phi=phi,gamma=gamma)
                    params.append(f'Triple Exp Smoothing Additive, alpha: {alpha}, beta: {beta}, phi: {phi} and gamma: {gamma}.')
                    dfs.append(df)
                    if kpi=="MAE":    
                        MAE = df['Error'].abs().mean()
                        KPIs.append(MAE)
                    else:
                        RMSE = np.sqrt((df['Error']**2).mean())
                        KPIs.append(RMSE)
            

    mini=np.argmin(KPIs)
    if kpi=="MAE":    
        print(f'The best solution is the model: {params[mini]} w/ a MAE of ',round(KPIs[mini],2))
    else:
        print(f'The best solution is the model: {params[mini]} w/ a RMSE of',round(KPIs[mini],2))

    return dfs[mini]
