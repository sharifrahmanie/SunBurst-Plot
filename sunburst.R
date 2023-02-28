require(ggplot2)
require(readxl)
# By @biomedical_informatics Edris Sharif Rahmani. Feb 28 2023
df <- data.frame(read_excel("sunburst_plot.xlsx"))
colnames(df) <- c("Gene", "Substrate", "Percentage")
df$id <- seq(1, nrow(df))
sample_number <- nrow(df)
angle <- 90 - 360 * (df$id-0.5) /sample_number 
df$hjust <- ifelse( angle < -90, 1, 0)
df$angle <- ifelse(angle < -90, angle+180, angle)
p <- ggplot(df, aes(x=as.factor(id), y=Percentage, fill = Substrate)) +   
  geom_bar(stat="identity",alpha = 1) + ylim(-100,120) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0,4), "cm")) +
  coord_polar(start = 0) + 
  geom_text(data=df, aes(x=id, y=Percentage+10, label=Gene, hjust=hjust),
            color="black", fontface="bold",alpha=0.6, size=2.5,
            angle= df$angle, inherit.aes = FALSE ) +
  scale_fill_discrete(labels=c('Amikacin','Gentamicin','Tobramycin',
                               'Ciprofloxacin', 'Levofloxacin','sulfanimide',
                               'Trimethoprim'))
pdf("Sunburst.pdf", width = 10, height = 8)
print(p)
dev.off() 
