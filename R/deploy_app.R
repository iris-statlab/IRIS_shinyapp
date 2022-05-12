#deploy app
library(rsconnect)
rsconnect::setAccountInfo(name='mpviz',
			  token='5FFB126B97FB30495E6736D5E06416F7',
			  secret='<SECRET>')
rsconnect::deployApp(appName="individual_ri")