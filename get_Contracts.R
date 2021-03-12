require("RobinHood");require("httr");require("jsonlite");require("data.table");require("rvest")
username = "username"
password = "password"
# *************************************************************************************************************
#                                           Wrappers
# *************************************************************************************************************
mod_json <- function(x, type) {
  
  if (type == "toJSON") {
    x <- x %>% jsonlite::toJSON()
    x <- substr(x, 2, nchar(x) - 1)
    return(x)
  }
  
  if (type == "fromJSON") {
    x <- jsonlite::fromJSON(rawToChar(x$content))
    return(x)
  }
  
}

get_expirations = function(RH, chain_symbol)
{

# get all expirations
url = paste0("https://api.robinhood.com/options/instruments/",
             "?state=active",
             "&chain_symbol=", chain_symbol)
token <- paste("Bearer", RH$tokens.access_token)

# GET data
exp <- GET(url,
           add_headers("Accept" = "application/json",
                       "Content-Type" = "application/json",
                       "Authorization" = token))

# format 
exp <- mod_json(exp, "fromJSON")
exp <- as.data.frame(exp$results)
# extract unique expirations
exp <- unique(exp$expiration_date)
# order expirations
exp[order(exp)]
}

get_rh_options = function(RH, chain_symbol,expiration,type){

  # URL and token
  url = paste0("https://api.robinhood.com/options/instruments/",
               "?state=active",
               "&type=", type,
               "&chain_symbol=", chain_symbol,
               "&expiration_dates=", expiration)
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET data
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # retrives all options for that expiration
  ops = lapply(as.list(1:nrow(dta)), function(ii){
  tmpURL = paste0("https://api.robinhood.com/marketdata/options/",dta$id[ii],"/")
  # GET 
  dta2 <- GET(tmpURL,
              add_headers("Accept" = "application/json",
                          "Content-Type" = "application/json",
                          "Authorization" = token))
  # format return
  dta2 <- try(mod_json(dta2, "fromJSON"), silent = TRUE)
  # if no errors - format
  if(!inherits(dta2,'try-error'))
  {
  dta2 = as.data.frame(t(do.call(rbind, lapply(dta2, as.data.frame))))
  dta2$strike_price <- as.numeric(dta$strike_price[ii])
  dta2$type <- dta$type[ii]
  }else{
    dta2 <- NULL
  }
  dta2
  })
  # row bind all options
  ops = rbindlist(ops,use.names = TRUE, fill=TRUE)
  ops$expiration_date = expiration
  ops$access_date = as.character(Sys.Date())
  
  # convert to numeric columns
  ops$adjusted_mark_price       = ops$adjusted_mark_price %>% as.numeric 
  ops$ask_price                 = ops$ask_price %>% as.numeric 
  ops$ask_size                  = ops$ask_size %>% as.numeric 
  ops$bid_price                 = ops$bid_price %>% as.numeric 
  ops$bid_size                  = ops$bid_size %>% as.numeric 
  ops$break_even_price          = ops$break_even_price %>% as.numeric 
  ops$last_trade_price          = ops$last_trade_price %>% as.numeric 
  ops$last_trade_size           = ops$last_trade_size %>% as.numeric 
  ops$mark_price                = ops$mark_price %>% as.numeric 
  ops$open_interest             = ops$open_interest %>% as.numeric 
  ops$previous_close_price      = ops$previous_close_price %>% as.numeric 
  ops$volume                    = ops$volume %>% as.numeric 
  ops$chance_of_profit_long     = ops$chance_of_profit_long %>% as.numeric 
  ops$chance_of_profit_short    = ops$chance_of_profit_short %>% as.numeric 
  ops$delta                     = ops$delta %>% as.numeric 
  ops$gamma                     = ops$gamma %>% as.numeric 
  ops$implied_volatility        = ops$implied_volatility %>% as.numeric 
  ops$rho                       = ops$rho %>% as.numeric 
  ops$theta                     = ops$theta %>% as.numeric 
  ops$vega                      = ops$vega %>% as.numeric 
  ops$high_fill_rate_buy_price  = ops$high_fill_rate_buy_price %>% as.numeric 
  ops$high_fill_rate_sell_price = ops$high_fill_rate_sell_price %>% as.numeric 
  ops$low_fill_rate_buy_price   = ops$low_fill_rate_buy_price %>% as.numeric 
  ops$low_fill_rate_sell_price  = ops$low_fill_rate_sell_price %>% as.numeric 
  ops$strike_price              = ops$strike_price %>% as.numeric 
  ops$high_price                = ops$high_price %>% as.numeric 
  ops$low_price                 = ops$low_price %>% as.numeric 

  # return data
  ops
}
# *************************************************************************************************************
#                                           Examples
# *************************************************************************************************************
# establish RH connection
RH = RobinHood(username = username, password = password)
chain_symbol = "EEM"
exp = get_expirations(RH, chain_symbol = chain_symbol)

ops = pblapply(as.list(exp), function(expiration){
calls = get_rh_options(RH, chain_symbol = chain_symbol, expiration = expiration, type ="call")
puts  = get_rh_options(RH, chain_symbol = chain_symbol, expiration = expiration, type ="put")
rbind(calls,puts)
})
ops = rbindlist(ops,use.names = TRUE,fill = TRUE)
