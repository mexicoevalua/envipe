# Gráficas  ENVIPE 2013

require(rCharts)
require(ggplot2)
require(scales)
require(wordcloud)

# Cargar datos
data  <- read.csv("envipe2011-2013.csv", as.is=T, encoding="utf8")

# Incidencia y prevalencia delictiva

pdf("images/text_plot.pdf", width=7.7, height=7.7)
textplot(data$pre2012,data$inc2012, words=data$abreviatura,xlim=c(min(data$pre2012),
  max(data$pre2012)), ylim=c(min(data$inc2012),max(data$inc2012)), show.lines=T, cex=1) + 
  title("Tasas de incidencia y prevalencia delictiva \n (por cada 100 mil habitantes)")
grid(NULL, lty =2, lwd =2)
dev.off()

# Percepción inseguridad
p  <- ggplot(data, aes(x=ins2013, y= reorder(abreviatura, ins2013))) + geom_point(size=3) 

mex_eval_theme  <- theme_bw() + theme(text = element_text(family="Helvetica"), 
  plot.title = element_text(face="bold",hjust=3), panel.grid.major = element_line(size =.8),
  legend.position=c(1,0))

png("images/percepcion2013.png", width=567, height=567)
p + ggtitle("Percepción de inseguridad en la entidad federativa \ (marzo y abril de 2013)") + 
  xlab("Porcentaje de la población de 18 años y más") + ylab("Entidad") + mex_eval_theme
p + xlab("Porcentaje de la población de 18 años y más") + ylab("Entidad") + mex_eval_theme
dev.off()

# Incidencia, prevalencia y percepción inseguridad (scatter plot)
names(data)

q  <- ggplot(data, aes(x = inc2012, y = pre2012, size=ins2013)) + geom_text(aes(label=abreviatura)) +
  scale_y_continuous(lim=c(0,50000),labels=comma) + scale_x_continuous(lim=c(0,60000),labels=comma) +mex_eval_theme
q  <- q + scale_size(range=c(1,6)) + xlab("Incidencia delictiva en 2012") + ylab("Pevalencia delictiva en 2012") +
  labs(size="% Pob 18 y más\n percibe inseguridad")
q
# png
mex_eval_theme  <- theme_bw() + theme(text = element_text(family="Helvetica"), 
  plot.title = element_text(face="bold", hjust=0), 
  panel.grid.major = element_line(size =.8),
  legend.position=c(1,0), legend.justification=c(1,0),
  legend.background=element_rect(fill="white",colour="black"))
q  <- ggplot(data, aes(x = inc2012, y = pre2012, size=ins2013)) + 
  geom_point(aes(label=abreviatura), shape=1, postion="jitter") +
  geom_text(aes(x=inc2012 +600, label=abreviatura),size=3, hjust=0, vjust=1) +
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma) +mex_eval_theme
q <- q + xlab("Incidencia delictiva en 2012") + ylab("Pevalencia delictiva en 2012") +
  labs(size="% Pob 18 años y\n más que percibe\ninseguridad en la \nentidad") +
  ggtitle("Percepción de inseguridad en la entidad federativa \n (marzo y abril de 2013) y tasas de incidencia y prevalencia delictivas\npor cada 100 mil habitanes en 2012")
q
# Exportar imagen como PDF
png("images/prevalencia_vs_incidencia.png", width=567, height=490)
q 
dev.off()

pdf("images/prevalencia_vs_incidencia.pdf", width=6.35, height=5.34)
q 
dev.off()
