# Mapa interactivo de la cifra negra en delitos del fuero común a nivel estatal en 
# 2010, 2011 y 2012
# Basado en: http://rmaps.github.io/blog/posts/animated-choropleths/index.html y
# http://bl.ocks.org/diegovalle/8967565

# Instalar librerías en Mac
#####
require(reshape2)
require(devtools)

install_github(repo='rCharts',username='ramnathv',ref="dev")
install_github(repo='rMaps',username='ramnathv',ref="master")

install_github('ramnathv/rCharts')
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
# Cálculo de la cifra negra a nivel estatal

# La cifra negra se calculará como la diferencia en la tasa de incidencia delictiva en 2012
# y la diferencia en la tasa de delitos del fuero común (por cada 100 mil habitantes). La
# primera se toma de los tabulados básicos de la ENVIPE 2013 del INEGI y la segunda se
# calcula con las estadísticas de SEGOB y CONEVAL

# Carga datos
#####
envipe  <- read.csv("data/envipe2011-2014.csv", encoding= "utf8",stringsAsFactors=F)

# Mantener solo datos para los estados
envipe  <- subset(envipe, envipe$codigo != 0)
# Subset prevalencia delictiva
names(envipe)
inc  <- envipe[,c(1:3,5,8,11,14)] 
names(inc)

# Agregar datos de averiguaciones previas
ave  <-  read.csv("data/fuero-comun-estados.csv", as.is=T)
head(ave)

# Mantener solo el año de interes para las averiguaciones previas en 2010, 2011 y 2012
table(ave$year)
ave  <- subset(ave, ave$year == 2010 | ave$year == 2011 | ave$year == 2012 | ave$year == 2013) 

# Colapsar todas las averiguaciones previas en una sola
names(ave)
require(plyr)
head(ave)
ave <- ddply(ave, c("state_code","year"), summarize,
             averiguaciones = sum(count,na.rm=T),
             population = max(population))
head(ave)

# Tasa de delitos del fuero común por cada 100 mil habiantes
ave$rate  <- (ave$averiguaciones / ave$population) * 100000
ave  <- ave[,c(1,2,5)]

# Cambiar de averiguaciones de formato long a wide
temp  <- melt(ave, id=c("state_code","year"),
              measured=c("rate"))
temp$value  <- as.numeric(format(round(as.numeric(temp$value),2), nsmall = 2))
head(temp)
tasas_ave  <- dcast(temp, state_code ~ variable + year)
head(tasas_ave)

# Unir datos envipe con averiguaciones
head(inc)
head(tasas_ave)
data  <-  merge(inc, tasas_ave, by.x="codigo", by.y="state_code")
head(data)

# Calcular cifra negra: incidencia delictiva reportadap por la envipe como porcentaje del
# número de averiguaciones previas reportado por el secretariado de seguridad pública
data  <- transform(data, cf2010 = ((inc2010 -rate_2010)/inc2010)*100 )
data  <- transform(data, cf2011 = ((inc2011 -rate_2011)/inc2011)*100 )
data  <- transform(data, cf2012 = ((inc2012 -rate_2012)/inc2012)*100 )
data  <- transform(data, cf2013 = ((inc2013 -rate_2013)/inc2013)*100 )

# Mantener solo las columnas de cifra negra
data  <- data[,-4:-11]
# Cambiar formato a dos decimales
names(data)
cols  <- c(4:7)
data[,cols]  <-  apply(data[,cols], 2, function(x) as.numeric(format(round(as.numeric(x),2), nsmall = 2)))
summary(data[,cols])

# Melt dataframe para hacer el mapa interactivo
data  <- melt(data=data,id.vars=c(1:3))
head(data); tail(data)
table(data$variable)

# Cambiar nombres
head(data)
names(data)  <- c("state_code","name","state_name","year","cifra_negra")

# Elimar "cf" 
data$year  <- gsub("cf","", data$year)
data$year  <- as.numeric(data$year)
table(data$year) # 32 obs 
head(data); tail(data)

# Esta parte del script produce un mapa interactivo .html
require(rMaps)
require(rCharts)
# Requiere un servidor local, en la terminal de Mac usar: python -m SimpleHTTPServer 8888
##### 

# Separar prevalencia en intervalos
options(scipen=999)
d <- 3 
dat <- transform(data,
                 fillKey = cut(cifra_negra, breaks=c(quantile(cifra_negra, probs = seq(0, 1, by = 0.20))), dig.lab = d, include.lowest=T, right=T)
                 )
str(dat$fillKey)
levels(dat$fillKey)
names(dat)
dat$fillKey
#dat$fillKey  <- gsub("1e\\+02","99.5", dat$fillKey)
#dat$fillKey  <- as.factor(dat$fillKey)
keyNames <- levels(dat$fillKey)
keyNames

# Colores
fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), '#BD0026'),
  c(levels(dat$fillKey), 'defaultFill')
)
str(fills)

dat2 <- plyr::dlply(na.omit(dat), "year", function(x){
  y = rCharts::toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'name')
  return(y)
})

# Existe un bug en la función ichoropleth, por lo tanto, se utiliza el formato propuesto por Diego Valle-Jones 

d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "shapefiles/mx_states.json",
    popupTemplate =  "#! function(geography, data) { //this function should just return a string
    return '<div class=hoverinfo><strong>' + geography.properties.name + '</strong></div>';
    }  !#"
  ),
  dom = 'chart_1',
  scope = 'states',
  labels = TRUE,
  bodyattrs = "ng-app ng-controller='rChartsCtrl'",
  setProjection = '#! function( element, options ) {
  
  var projection, path;
  
  projection = d3.geo.mercator()
  .center([-90, 24])
  .scale(element.offsetWidth)
  .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  
  path = d3.geo.path()
  .projection( projection );
  
  return {path: path, projection: projection};
  } !#',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
d1$save("cifra_negra.html", cdn = TRUE)

#####
### Map con botones
#####

d1$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)
d1$setTemplate(chartDiv = " 
               
               <div class='container'>
               <div class=\"btn-group\" >
               <button ng-repeat=\"value in years\" ng-click='updateYear(value)'
               class=\"btn btn-default\" type=\"button\" 
               ng-model=\"value\" btn-radio=\"value\">
{{ value }}
               </button>
               </div>
               <div id='chart_1' class='rChart datamaps'></div>  
               </div>
               <script>
               function rChartsCtrl($scope){
               $scope.years = [2010, 2011, 2012, 2013]
               $scope.year = $scope.years[0]
               $scope.updateYear = function(x){
               $scope.year = x
               }
               $scope.$watch('year', function(newYear){
               mapchart_1.updateChoropleth(chartParams.newData[newYear]);
               })
               }
               </script>  "
)
d1$set(newData = dat2)
d1$save("cifra_negra.html", cdn = TRUE)
# Despues de ejecutar el codigo es necesario agregar la referncia a bootstra en el html
#<link href="http://netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">
# Tambien hay que modificar el <style> con los siguientes parámetros
#.container { margin-top: 20px; }
#.datamaps-legend dt{font-weight: normal; }
# Agregar manualmente {{value}} en el html