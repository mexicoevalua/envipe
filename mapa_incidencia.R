# Mapa interactivo incidencia delictiva ENVIPE 2011, 2012 y 2013
# Basado en: http://rmaps.github.io/blog/posts/animated-choropleths/index.html y
# http://bl.ocks.org/diegovalle/8967565

# Instalar librerías en Mac
#####
require(reshape2)
require(devtools)
require(plyr)
require(rCharts)
require(rMaps)

#install_github(repo='rCharts',username='ramnathv',ref="dev")
#install_github(repo='rMaps',username='ramnathv',ref="master")

# Carga datos
#####
envipe  <- read.csv("data/envipe2011-2014.csv", encoding= "utf8",stringsAsFactors=F)
# Subset states
envipe  <- subset(envipe, envipe$codigo != 0)
# Subset incidencia delictiva
names(envipe)
inc  <- envipe[,c(1:3,5,8,11,14)] 
names(inc)
# Melt dataframe
minc  <- melt(data=inc,id.vars=c(1:3))
head(minc); tail(minc)

# Cambiar nombres
names(minc)  <- c("state_code","name","state_name","year","incidencia")

# Elimar "pre" 
minc$year  <- gsub("inc","", minc$year)
minc$year  <- as.numeric(minc$year)
table(minc$year) # 32 obs 
head(minc); tail(minc)

# Esta parte del script produce un mapa interactivo .html
# Requiere un servidor local, en la terminal de Mac usar: python -m SimpleHTTPServer 8888
##### 

# Separar prevalencia en intervalos
dat <- transform(minc,
                 fillKey = cut(incidencia, breaks=c(quantile(incidencia, probs = seq(0, 1, by = 0.20))), dig.lab = 5, include.lowest=T, right=T)
)
table(dat$fillKey)
keyNames <- levels(dat$fillKey)
dat$incidencia  <- format(dat$incidencia, big.mark=",", digits=1)
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

# Existe un bug en la función ichoropleth, utilizar el formato propuesto por Diego Valle-Jones 

d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "shapefiles/mx_states.json",
    popupTemplate =  "#! function(geography, data) { //this function should just return a string
    return '<div class=hoverinfo>' + geography.properties.name + ': ' + data.incidencia + '</div>';
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
d1$save("incidencia.html", cdn = TRUE)

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
d1$save("incidencia.html", cdn = TRUE)
# Despues de ejecutar el codigo es necesario agregar la referncia a bootstra en el html
#<link href="http://netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css" rel="stylesheet">
# Tambien hay que modificar el <style> con los siguientes parámetros
#.container { margin-top: 20px; }
# .datamaps-legend dt{font-weight: normal; }
# Agregar manualmente {{value}} en el html