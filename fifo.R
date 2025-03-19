################################################################################
# Funcion de fifo 
# Objetivo: Calcular el precio de compra ponderado por cada venta 
# Obtener la utilidad por operacion siguiendo FIFO
# Pasos 
# Por cada venta  calcular 
# 1. Encontrar el vector de compras que sucedieron antes de esta venta 
# 2. Determinar el vector de compras que se estan vendiendo  (e.g. q_v = q_c)
# 3. Estimar el precio de compra para esas compras 
# 4. Calcular el remanente de la ultima operacion de compra usada (q_c - q_v_used)
# 5. Actualizar el inventario como [p_c, q_c] = c([p1_c, q1_c], [p_c, q_c | trade_date> trade_date_sell_i])

# Input: stocks table with all the purchases of the stocks 
# Output: dataframe with all the sells of the year, p_c and profit 
################################################################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RCT)
library(lubridate)
library(quantmod)


fifo<-function(stock_db, year, symbols) {

stock_db<-
  stock_db %>% 
  arrange(symbol, trade_date)

  
# Filtering on symbol 
stock_db <-
  stock_db %>% 
  filter(symbol == symbols)

### Paso 0: Divide buy and sell in different tables
stocks_buy <-
  stock_db %>% 
  filter(activity_type == 'buy')

stocks_sell<-
  stock_db %>% 
  filter(activity_type == 'sell', year(trade_date) == year)




### 1. Encontrar el vector de compras que sucedieron antes de esta venta (BY SYMBOL) 
# POR VENTA  

# starting empty outputs 
precio_compra<-numeric(nrow(stocks_sell))

# Iterate per sell
for (i in seq_along(stocks_sell$trade_date)) {
stocks_buy_i <-
  stocks_buy %>% 
  filter(trade_date< stocks_sell$trade_date[i],
         quantity >0)

### 2. Determinar el vector de compras que se estan vendiendo  (e.g. q_v = q_c)
# Estimo la cantidad comprada acumulada; me quedo con las tabla de transacciones que excede a la venta 
stocks_buy_i <-
  stocks_buy_i %>% 
  mutate(aux_q_c = cumsum(quantity)) 


# La tabla puede exceder y tener filas que no necesito. Encuentro la primera transaccion que paso el volumen de venta  
id_filtrar<-which.min(stocks_buy_i$id[stocks_buy_i$aux_q_c>=stocks_sell$quantity[i]])
stocks_buy_i$aux_q_c<-NULL


# Tabla final para el calculo de p_c
stocks_buy_i<-stocks_buy_i[1:id_filtrar, ]

# Left join de la cantidad vendida
stocks_buy_i$q_v<-stocks_sell$quantity[i]

### 3. Estimar el precio de compra para esas compras 
### 4. Calcular el remanente de la ultima operacion de compra usada (q_c - q_v_used)

# q_used, q_remanent
# FOR EACH sell
q_v<-abs(stocks_sell$quantity[i])

# Initialize variables
q_remanent <- numeric(length(stocks_buy_i$quantity))
q_used <- numeric(length(stocks_buy_i$quantity))

# Distribute sold quantity among purchase transactions
for (j in seq_along(stocks_buy_i$quantity)) {
  if (q_v > 0) {
    q_used[j] <- min(q_v, stocks_buy_i$quantity[j])  # Use as much as possible from this batch
    q_remanent[j] <- stocks_buy_i$quantity[j] - q_used[j]  # Remaining stock from this batch
    q_v <- q_v - q_used[j]  # Reduce the quantity to be fulfilled
  } else {
    q_used[j] <- 0
    q_remanent[j] <- stocks_buy_i$quantity[j]  # No more to use, keep full stock
    
  }
}

# Create data frame
df <- data.frame(q_remanent = q_remanent, q_used = q_used)
stocks_buy_i<-bind_cols(stocks_buy_i, df)


rm(df, q_remanent, q_used, q_v)

## 3. Precio de compra: Llevar el precio al valor de la venta 
# INPC de Fecha de venta 
stocks_buy_i$inpc_sell<-stocks_sell$inpc[i]

# precios nuevos 
stocks_buy_i<-
  stocks_buy_i %>% 
  mutate(price_mxn_fv = price_mxn*inpc_sell/inpc)

# Poniendo el precio de compra ponderado en la tabla de ventas
precio_compra[i]<-weighted.mean(stocks_buy_i$price_mxn_fv, stocks_buy_i$q_used)


# 5. Actualizar el inventario como [p_c, q_c] = c([p1_c, q1_c], [p_c, q_c | trade_date> trade_date_sell_i])
stocks_buy<-
  stocks_buy %>% 
  left_join(stocks_buy_i %>% select(id, q_remanent), by = 'id') %>% 
  mutate(quantity = if_else(is.na(q_remanent), quantity, q_remanent)) %>% 
  select(-q_remanent)



}


# Paso final
stocks_sell$price_buy_mxn<-precio_compra

stocks_f<-bind_rows(stocks_buy, stocks_sell)

stocks_f<-
  stocks_f %>% 
  mutate(profit_mxn = abs(quantity)*(price_mxn-price_buy_mxn),
         profit_usd = abs(quantity)*(price - (price_buy_mxn)/usdmxn_final))

return(stocks_f)
}

