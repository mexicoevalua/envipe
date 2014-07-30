# Interactive map for ENVIPE 2011, 2012, 2013
# Based on: http://rmaps.github.io/blog/posts/animated-choropleths/index.html and
# http://bl.ocks.org/diegovalle/8967565

# Load data
#####
require(reshape2)
require(devtools)

#install_github('ramnathv/rCharts@dev')
install_github(repo='rCharts',username='ramnathv',ref="dev")
install_github(repo='rMaps',username='ramnathv',ref="master")

envipe  <- read.csv("data/envipe2011-2013.csv", encoding= "utf8",stringsAsFactors=F)
tail(envipe)
# Subset states
envipe  <- subset(envipe, envipe$codigo != 0)
# Subset prevalencia delictiva
names(envipe)
prev  <- envipe[,c(1:4,7,10)] 
names(prev)
# Melt dataframe
mprev  <- melt(data=prev,id.vars=c(1:3))
head(mprev); tail(mprev)

# Change names
names(mprev)  <- c("state_code","short_name","name","year","prevalencia")

# Delete "pre" from year
mprev$year  <- gsub("pre","", mprev$year)
mprev$year  <- as.numeric(mprev$year)
table(mprev$year) # 32 obs for each variable
head(mprev); tail(mprev)

# Interactve map: produces an index.html file
# Requries alocal server to be visualized. In Mac use python -m SimpleHTTPServer 8888
##### 

# Separate prevalencia in five intervals
dat <- transform(mprev,
                 fillKey = cut(prevalencia, breaks = 5, dig.lab = 5, include.lowest=T, right=T)
)
dat
keyNames <- levels(dat$fillKey)

fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), '#BD0026'),
  c(levels(dat$fillKey), 'defaultFill')
)
str(fills)
# Set payload for rMaps
dat2 <- plyr::dlply(na.omit(dat), "year", function(x){
  y = rCharts::toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'name')
  return(y)
})

# Since rMaps ichoropleth funciton has a bug on the interval's maximum values, use 
#boiler plate provided by Diego Valle-Jones

d1 <- Datamaps$new()
d1$set(
  geographyConfig = list(
    dataUrl = "data/mx_states.json",
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
d1$save("index.html", cdn = TRUE)

#####
#### Map a with slider
#####
d1$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)
d1$setTemplate(chartDiv = "
  <div id = 'chart_1' class = 'rChart datamaps'>
  <input id='slider' type='range' min=2010 max=2012 ng-model='year' width=200>
  <span ng-bind='year'></span>
    
  <script>
    function rChartsCtrl($scope){
      $scope.year = '2012';
      $scope.$watch('year', function(newYear){
        mapchart_1.updateChoropleth(chartParams.newData[newYear]);
      })
    }
  </script>
  </div>   "
)
d1$set(newData = dat2)
d1$save("index.html", cdn = TRUE)

#####
### Map with drop down menu and default year to 2012
#####
d1$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)
d1$setTemplate(chartDiv = "
               <div class='container'>
        <div class='col-md-2'>
          <select ng-model='year' class='form-control'
             ng-options='year for year in [2010, 2011, 2012]'>
            {{ year }}
          </select>
      </div>
      <div id = 'chart_1' class = 'rChart datamaps'></div>
    </div> 
               
               <script>
               function rChartsCtrl($scope){
               $scope.year = 2012;
               $scope.$watch('year', function(newYear){
               mapchart_1.updateChoropleth(chartParams.newData[newYear]);
               })
               }
               </script>
               </div>   "
)
d1$set(newData = dat2)
d1$save("index2.html", cdn = TRUE)

#####
### Map with button groups
#####

d1$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)
d1$setTemplate(chartDiv = " 

      <div class='container'>
        <div class= 'btn-group' >
          <button ng-repeat="value in years" ng-click='updateYear(value)'
            class="btn btn-default" type="button" 
            ng-model="value" btn-radio="value">
            {{ value }}
          </button>
        </div>
        <div id='chart_1' class='rChart datamaps'></div>  
      </div>
      <script>
        function rChartsCtrl($scope){
          $scope.years = [1960, 1980, 2000]
          $scope.year = $scope.years[0]
          $scope.updateYear = function(x){
            $scope.year = x
          }
          $scope.$watch('year', function(newYear){
            mapchart_1.updateChoropleth(chartParams.newData[newYear]);
          })
        }
      </script>"
)
d1$set(newData = dat2)
d1$save("victimizacion.html", cdn = TRUE)


### Further adjustments have to be done directly in the HTML check, mapa_incidencia.R