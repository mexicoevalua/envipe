.PHONY: all clean
all: estados.zip mx_states.json clean
estados.zip: 
	curl -o estados.zip http://mapserver.inegi.org.mx/MGN/mge2010v5_0.zip

mx_states.json: estados.zip
	unzip -o estados.zip 
	ogr2ogr states.shp Entidades_2010_5.shp -t_srs "+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0"
	## id-property needed so that DataMaps knows how to color the map
	topojson -o mx_states.json -s 1e-7 -q 1e5 states.shp -p state_code=+CVE_ENT,name=NOM_ENT --id-property NOM_ENT

clean:
	-rm -f *.zip *.csv.gz Estados*
