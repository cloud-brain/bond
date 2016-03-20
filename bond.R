## present_value 现值计算公式（第一期未收到钱）
## argument:
##  @@coupon: vector, 现金流，向量长度等于1认为固定现金流
##  @@market_rate: vector, 市场利率(按期),向量长度等于1认为固定利率，小数
##  @@times:int,期数
present_value <- function(coupon, market_rate, times)
{
  coupon_len <- length(coupon)
  market_len <- length(market_rate)
  if(coupon_len != times & coupon_len != 1) stop('length must be equal')
  if(market_len != times & market_len != 1) stop('length must be equal')
  if(coupon_len != 1 | market_len != 1)
  {
    if(coupon_len == 1) coupon <- rep(coupon, times)
    return(sum(coupon / (1+market_rate) ^ (1:times)))
  }else{
    return(coupon / market_rate * (1 - 1 / (1 + market_rate) ^ times))
  }
}

## future_value 终值计算公式
## argument:
##  @@coupon: vector, 现金流，向量长度等于1认为固定现金流
##  @@market_rate: vector, 市场利率(按期),向量长度等于1认为固定利率，小数
##  @@times:int,期数
future_value <- function(coupon, market_rate, times)
{
  coupon_len <- length(coupon)
  market_len <- length(market_rate)
  if(coupon_len != 1 | market_len != 1)
  {
    if(coupon_len != times & coupon_len != 1) stop('length must be equal')
    if(market_len != times & market_len != 1) stop('length must be equal')
    if(coupon_len == 1) coupon <- rep(coupon, times)
    return(sum(coupon * (1 + market_rate) ^ (times:1)))
  }else{
    return(coupon / market_rate * (1 + market_rate) * ((1 + market_rate) ^ times - 1))
  }
}

## bond_price 100元面面值债券价格计算函数
## argument:
##  @@start_date: Date, 当前时间
##  @@end_date: Data, 债券到期时间
##  @@coupon_rate: num, 票面利率，假设不超过1
##  @@market_rate: num, 市场利率，假设不超过1
##  @@times:int, 每年付息次数,预设为1次
##  @@dirty_price: logi, 返回类型，T为全价，F为全价
bond_price <- function(start_date, end_date, coupon_rate, market_rate, times = 1, dirty_price = T)
{
  start_date <- as.POSIXlt(start_date)
  end_date <- as.POSIXlt(end_date)
  # 计算买入日和到期日之间相差的月份数据
  period <- 12 * (end_date$year - start_date$year) + (end_date$mon - start_date$mon)
  period <- floor(ifelse(end_date$mday >= start_date$mday, period, period -1) / 12 * times)
  
  # 计算到下一个付息日前的现金流现值
  market_rate <- 1+market_rate / ifelse(market_rate>1, 100, 1) / times
  coupon <- coupon_rate * ifelse(coupon_rate > 1, 1, 100) / times
  temp_price <- present_value(coupon_rate/times, market_rate - 1, period) + 100 / market_rate ^ period
 
  # 累加下一个付息日的利息后调整到买入日
  next_date <- end_date
  next_date$mon <- next_date$mon - period * 12 / times
  if(next_date != start_date)
  {
    first_date <- next_date
    first_date$mon <- first_date$mon - 12 / times
    diff_time <- as.numeric(next_date - start_date) / as.numeric(next_date - first_date)
    temp_price <- (temp_price + coupon) / market_rate ^ diff_time
  }else{ diff_time <- 1 }
  if(dirty_price)
  {
    return(temp_price)
  }else{
    return(temp_price - coupon  * (1 - diff_time))
  }
}

## bond_ytm 计算到期收益率
## argument:
##  @@price: num， 100元面值债券的买入价格
##  @@start_date: Date, 当前时间
##  @@end_date: Data, 债券到期时间
##  @@coupon_rate: num, 票面利率，假设不超过1
##  @@times:int, 每年付息次数,预设为1次
##  @@dirty_price: logi, 返回类型，T为全价，F为全价
bond_ytm <- function(price, start_date, end_date, coupon_rate, times = 1, dirty_price = T)
{
  return(optimize(function(x) (bond_price(start_date, end_date, coupon_rate, x, times, dirty_price) - price)^2, interval = c(0,1))$minimum)
}

## bond_totol_return 全收益率计算
## argument:
##  @@price: num, 债券市场价格
##  @@start_date: Date, 买入时间
##  @@sold_date: Date, 债券售出时间
##  @@end_date: Data, 债券到期时间
##  @@coupon_rate: num, 票面利率，假设不超过1
##  @@sold_rate: num, 售出时市场收益率
##  @@reinvest_rate: num, 再投资利率
##  @@times:int, 每年付息次数,预设为1次
bond_totol_return <- function(price, start_date, sold_date, end_date, coupon_rate, sold_rate, reinvest_rate, times = 1)
{
  require(dplyr)
  count_period <- function(date_start, date_end, times)
  {
    ((12 * (date_end$year - date_start$year) + (date_end$mon - date_start$mon) - ifelse(date_end$mday >= date_start$mday, 0, 1)) / times) %>% floor %>% return
  }
  time_ratio <- function(date_start, date_end, next_period, times)
  {
    next_date <- end_date$mon - next_period * 12 / times
    first_date <- next_date
    first_date$mon <- first_date$mon - 12 / times
    return(as.numeric(next_date - start_date) / as.numeric(next_date - start_date))
  }
  ##计算卖出债券时的价格
  sold_price <- bond_price(start_date, end_date, coupon_rate, sold_rate, times)
  
  ##计算利息及对应的再投资收益
  start_date <- as.POSIXlt(start_date)
  sold_date <- as.POSIXlt(sold_date)
  end_date <- as.POSIXlt(end_date)
  reinvest_rate <- 1 + reinvest_rate / ifelse(reinvest_rate > 1, 100, 1) / times
  coupon <- coupon_rate * ifelse(coupon_rate > 1, 1, 100) / times
  period_sell <- count_period(sold_date, end_date, 12 / times)
  period_buy <- count_period(start_date, end_date, 12 / times)
  
  coupon_coupon <- future_value(coupon, reinvest_rate, period_sell - period_buy)
  coupon_coupon <- conpon_coupon * reinvest_rate ^ time_ratio(sold_date, end_date, period_sell, times)
  
  ##计算全收益率
  return((ln((coupon_coupon + sold_price) / price) / (period_sell - period_buy + time_ratio(sold_date, end_date, period_sell, times) + time_ratio(buy_date, end_date, period_buy, times)) - 1) %>%exp -1)
  
}

## conversion_factor, 债券转化因子
## argument:
##  @@start_date: Date,
##  @@end_date: Data, 债券到期时间
##  @@coupon_rate: num, 票面利率，假设不超过1
##  @@market_rate: num, 市场净价利率，假设不超过1
##  @@times:int, 每年付息次数
##  @@return_type: logi, 返回类型，T为净价，F为全价
conversion_factor <- function(start_date, end_date, coupon_rate, market_rate, times, return_type = T)
{
  
  
}