library(rsconnect)

rsconnect::setAccountInfo(name='10b92c-romario-gomes',
			  token='E587A387D3E33A7B38E74A637274D4AA',
			  secret='eARh2shT0MuIWaOK7CSjz9InhUsnwSaKaxxK2pGw')

rm(list=ls())
deployApp(
 account  = '10b92c-romario-gomes'
)
