.left.colour <- "#B793C2"
.right.colour <- "#e08e5a"

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param x named or ordered (respect to y) vector with character labels that will be plotted as colored stacked bar plots.
#' @param y named or ordered (respect to x) vector with character labels that will be plotted in the y axis. Will be used to compute the proportions.
#' @param labs character vector of length 2 indicating the label name of x and y. Defaults to "class" and "layer".
#' @param clr Color vector for x. Defaults to rainbow(nlevels(contrast))
#' @param title Character indicating the title for the plot, normally the cells analysed. Defaults to "5HT3aR-INs".
#' @keywords plot
#' @import stats methods ggplot2
#' @importFrom scales percent
#' @importFrom cowplot get_legend

layer.class.distribution <- function(labels, clr){
  if(is.null(names(labels))) names(labels) <- c("cluster","layer","subclass")
  lnames <- names(labels)
  df <- as.data.frame(prop.table(table(labels[2:3]),1))

  ggplot(data=df, aes_string(x=lnames[2], y="Freq",group=lnames[3])) +
    geom_bar(aes_string(fill=lnames[3]),stat="identity",position = position_stack()) +
    geom_text(aes(label=ifelse(Freq > 0.10, scales::percent(Freq,1), "")),
              position=position_stack(vjust = 0.5), color="white", size=6) +
    coord_flip() + xlab("") + ylab(paste("% of each",lnames[3], "of", "in a", lnames[2])) +
    scale_y_continuous(labels = scales::percent) + scale_fill_manual(values=clr) +
    theme(axis.text = element_text(size=14), axis.text.y = element_text(face="bold"),
          axis.title.x = element_text(face = "bold", size=18))
}


#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param x named or ordered (respect to y) vector with character labels that will be plotted as colored stacked bar plots.
#' @param y named or ordered (respect to x) vector with character labels that will be plotted in the y axis. Will be used to compute the proportions.
#' @param labs character vector of length 2 indicating the label name of x and y. Defaults to "class" and "layer".
#' @param clr Color vector for x. Defaults to rainbow(nlevels(contrast))
#' @param title Character indicating the title for the plot, normally the cells analysed. Defaults to "5HT3aR-INs".
#' @keywords plot
#' @import stats methods ggplot2
#' @importFrom scales percent
#' @importFrom cowplot get_legend

layer.cluster.distribution <- function(labels){
  # if(is.null(names(labels))) names(labels) <- c("cluster","layer","subclass")
  lnames <- colnames(labels)
  df <- as.data.frame(prop.table(table(labels[1:2]),2))

  ggplot(data=df, aes_string(x=lnames[2], y="Freq",group=lnames[1])) +
    geom_bar(aes_string(fill=lnames[1]),stat="identity",position = position_stack()) +
    geom_text(aes(label=ifelse(Freq > 0.10, scales::percent(Freq,1), "")),
              position=position_stack(vjust = 0.5), color="black", size=6) +
    coord_flip() + xlab("") + ylab(paste("% of selected subtypes in Tasic's 2018 database")) +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text = element_text(size=14), axis.text.y = element_text(face="bold"),
          axis.title.x = element_text(face = "bold", size=18))
}



#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param dg dendrogram object created with as.dendrogram(hclust(dist(t(m)))), where m is a numerical matrix with the labels as columns.
#' @param labels a named list of 3 character vectors, in which 1st and 2nd components are plotted on x and y axis, respectively, while 3rd component is used to colour axis.x.text.
#' 2nd component is also used to compute the proportions. Example: cluster, layer and subclass identities.
#' If no name is provided for the list components, their name defaults to c("cluster","layer","subclass")
#' @param clr named character vector, with names corresponding to the levels of labels' 3rd component and values as colour codes.
#' Example: c(subclass1="red",subclass2="blue")
#' @keywords plot
#' @import stats methods ggplot2 ggdendro
#' @importFrom dplyr left_join
#' @importFrom cowplot plot_grid get_legend

layer.cell.distribution <- function(dg, labels, clr)
{
  if(is.null(names(labels))) names(labels) <- c("cluster","layer","subclass")
  lnames <- names(labels)
  # Cell frequency across layers
  freq.x <- as.data.frame(prop.table(table(labels[1:2]),1))
  freq.x <- dplyr::left_join(as.data.frame(labels), freq.x, by=lnames[1:2])
  # Colour from labels' 3rd component
  clr.df <- data.frame(unique(freq.x[,lnames[c(1,3)]]),row.names=1)[labels(dg),lnames[3],drop=TRUE]
  clr <- clr[clr.df]

  p.hc <- ggplot(segment(dendro_data(dg, type = "rectangle"))) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_x_continuous(breaks = 1:max(order.dendrogram(dg)), labels = labels(dg), expand = c(0,.5)) +
    theme(axis.text.x = element_text(angle = 45,hjust=1)) + ylab("") + xlab("") + theme_void()

  nY <- length(unique(freq.x[,lnames[2]]))
  p.gj <- ggplot(freq.x, aes_string(x=lnames[1], y=lnames[2], color=if(nY>1) "Freq" else lnames[2])) +
    geom_jitter(alpha=0.80, size=1.25,stroke=0.25) + theme_light() + scale_x_discrete(expand=c(0,.5)) +
    # geom_vline(xintercept=seq(1.5, length(unique(df$contrast))-0.5, 1), colour="grey70", size=rel(0.5)) +
    theme(axis.text.x=element_text(angle=45,hjust=1,colour=clr,size=14,face="bold"), axis.text.y=element_text(size=18,face="bold"),
          legend.text = element_text(size = 11), panel.grid = element_blank()) + ylab("") + xlab("") + labs(colour="")
  if(nY>1) p.gj <- p.gj + scale_color_continuous(low="#d9d9d9", high="#252525", limits=c(0,1), labels=scales::percent)
  # geom_hline(yintercept=seq(1.5, length(unique(df$layer))-0.5, 1), colour="grey70", size=rel(0.5))

  plot_grid(p.hc, p.gj+theme(legend.position = "none"), ncol=1, nrow=2, align="v",rel_heights = c(0.4,0.8))
  # p2 <- plot_grid(ggplot()+geom_blank(), get_legend(p.gj)+theme(legend.position = "top"), ncol=1, nrow=2, rel_heights = c(0.4, 0.8))
  #
  # plot_grid(p1,p2, ncol=2, nrow=1, rel_widths = c(0.9,0.1))
}




#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param dg dendrogram object created with as.dendrogram(hclust(dist(t(m)))), where m is a numerical matrix with the labels as columns.
#' @param labels a named list of 3 character vectors, in which 1st and 2nd components are plotted on x and y axis, respectively, while 3rd component is used to colour axis.x.text.
#' 2nd component is also used to compute the proportions. Example: cluster, layer and subclass identities.
#' If no name is provided for the list components, their name defaults to c("cluster","layer","subclass")
#' @param clr named character vector, with names corresponding to the levels of labels' 3rd component and values as colour codes.
#' Example: c(subclass1="red",subclass2="blue")
#' @keywords plot
#' @import stats methods ggplot2 ggdendro
#' @importFrom dplyr left_join
#' @importFrom cowplot plot_grid get_legend

auc.plot <- function(res)
{
  # r <- svm.predict(w, x.tst, y.tst)

  ggplot(res) + geom_line(aes_string(x="FPR",y="TPR"), alpha=1, size=1.5) + coord_fixed() +
    annotate("text",x=0.75,y=0.25, colour="#b2182b",label=paste0("AUC\n",round(attr(res,"AUC"),2)), size=8) +
    theme(axis.title = element_text(size=14, face="bold"))
  # labs(title = names(geneFam.roc[which(gf==max(gf))])) + coord_equal()
}



#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param dg dendrogram object created with as.dendrogram(hclust(dist(t(m)))), where m is a numerical matrix with the labels as columns.
#' @param labels a named list of 3 character vectors, in which 1st and 2nd components are plotted on x and y axis, respectively, while 3rd component is used to colour axis.x.text.
#' 2nd component is also used to compute the proportions. Example: cluster, layer and subclass identities.
#' If no name is provided for the list components, their name defaults to c("cluster","layer","subclass")
#' @param clr named character vector, with names corresponding to the levels of labels' 3rd component and values as colour codes.
#' Example: c(subclass1="red",subclass2="blue")
#' @keywords plot
#' @import stats methods ggplot2 ggdendro
#' @importFrom dplyr left_join
#' @importFrom cowplot plot_grid get_legend

# genesPlot.log2FC_and_weight <- function(res){
#   ggplot(res, aes(x=log2FC,y=weight,color=ifelse(selected,"yes","no"))) + geom_point() + scale_color_manual(values=c("black","#CD1F72")) +
#     labs(colour="Is selected?", y="predictive power (SVM weight)",x="fold change (log2 Dock5/Lsp1)") + geom_text_repel(aes(label=res$label))
# }
# 
genesPlot.xy <- function(res, selectedGenes){
  # g.down <- res[rank(res$weight, ties.method = "first") %in% 1:input.n,]$genes
  # g.up <- rev(res[rank(-res$weight, ties.method = "first") %in% 1:input.n,]$genes)
  res$selected <- ifelse(res$genes %in% selectedGenes, "Chosen","Not chosen")
  
  plot_ly(res, x=~log2FC, y=~weight, key=~key, showlegend=T,
          text = ~paste("Gene symbol: ", genes, '<br>Protein name:'),
          color=~selected, type="scatter", mode = "markers" ,colors=c("#CD1F72","black"),
          marker=list(opacity=0.50)) %>%
    layout(dragmode="box", legend=list(orientation="h", x=0, y=1.02))
  # p <- ggplot(res, aes(x=log2FC,y=weight,color=ifelse(selected,"Chosen","Not chosen", key=key))) + geom_point() + scale_color_manual(values=c("#CD1F72","black")) +
  #   labs(colour="", y="predictive power (SVM weight)",x="fold change (log2 Dock5/Lsp1)") + geom_text_repel(aes(label=res$label))
  # ggplotly(p) %>% layout(dragmode = "lasso")
}

#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param dg dendrogram object created with as.dendrogram(hclust(dist(t(m)))), where m is a numerical matrix with the labels as columns.
#' @param labels a named list of 3 character vectors, in which 1st and 2nd components are plotted on x and y axis, respectively, while 3rd component is used to colour axis.x.text.
#' 2nd component is also used to compute the proportions. Example: cluster, layer and subclass identities.
#' If no name is provided for the list components, their name defaults to c("cluster","layer","subclass")
#' @param clr named character vector, with names corresponding to the levels of labels' 3rd component and values as colour codes.
#' Example: c(subclass1="red",subclass2="blue")
#' @keywords plot
#' @import stats methods ggplot2 ggdendro
#' @importFrom dplyr left_join
#' @importFrom cowplot plot_grid get_legend

genesPlot.topViolins <- function(res, m, labs, g.up, g.down){
  # r <- res[if(sum(res$selected) >= 6) res$selected else pmin(rank(+res$weight), rank(-res$weight)) %in% 1:5, ]
  # g.down <- res[rank(res$weight, ties.method = "first") %in% 1:input.n,]$genes
  # g.up <- rev(res[rank(-res$weight, ties.method = "first") %in% 1:input.n,]$genes)
  
  input.n <- length(g.up)
  df <- reshape2::melt(m[c(g.up, g.down),,drop=FALSE])
  colnames(df) <- c("gene_symbol","cell_id","value")
  df$gene_symbol <- factor(df$gene_symbol, levels=unique(c(g.up, g.down)))
  # df <- dplyr::left_join(df, data.frame("Var2"=colnames(INs),"cluster"=INs$cluster,stringsAsFactors = F), by="Var2")
  df <- dplyr::left_join(df, labs, by="cell_id")
  
  ggplot(df) + geom_violin(aes(x=selected,y=value, fill=selected, color=selected),scale="width", alpha=0.5) + 
    facet_wrap(~gene_symbol, nrow = if(input.n > 5) 4 else 2, ncol=if(input.n > 5) input.n/2 else input.n) +
    # facet_wrap(~gene_symbol, nrow = 2, ncol= input.n) +
    theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none") + labs(x="", y="log10 uTPM") +
    scale_fill_manual(values=c("Htr3a type(s) 1"=.left.colour,"Htr3a type(s) 2"=.right.colour)) +
    scale_color_manual(values=c("Htr3a type(s) 1"=.left.colour,"Htr3a type(s) 2"=.right.colour)) + 
    theme_bw() + theme(legend.position = "none", axis.text.x=element_text(angle=45, hjust=1, face="bold",size=11,color=c(.left.colour,.right.colour)),
                       panel.grid = element_blank(), strip.text.x = element_text(size=12, face="bold"))
}



#' Returns a ggplot object that you can modify with the grammar of graphics.
#' @param dg dendrogram object created with as.dendrogram(hclust(dist(t(m)))), where m is a numerical matrix with the labels as columns.
#' @param labels a named list of 3 character vectors, in which 1st and 2nd components are plotted on x and y axis, respectively, while 3rd component is used to colour axis.x.text.
#' 2nd component is also used to compute the proportions. Example: cluster, layer and subclass identities.
#' If no name is provided for the list components, their name defaults to c("cluster","layer","subclass")
#' @param clr named character vector, with names corresponding to the levels of labels' 3rd component and values as colour codes.
#' Example: c(subclass1="red",subclass2="blue")
#' @keywords plot
#' @import stats methods ggplot2 ggdendro
#' @importFrom dplyr left_join
#' @importFrom cowplot plot_grid get_legend

genesPlot.frequencies <- function(res){
  ggplot(res[res$selected,], aes(x=freq.y, y=freq.x)) + coord_fixed() + theme_minimal() +    
    geom_rect(data=data.frame(xmin=0,xmax=0.1,ymin=0.50,ymax=1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="lightblue",alpha=0.75,inherit.aes=FALSE) +
    geom_rect(data=data.frame(xmin=0.50,xmax=1,ymin=0,ymax=0.1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="lightgoldenrod",alpha=0.75,inherit.aes=FALSE) +
    # geom_rect(data=data.frame(xmin=0.70,xmax=1,ymin=0.70,ymax=1),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="grey80",alpha=0.75,color="grey20",inherit.aes=FALSE) +
    geom_point(color="#CD1F72") + geom_text_repel(aes(label=genes), colour="#CD1F72") + scale_x_continuous(labels=scales::percent) + scale_y_continuous(labels=scales::percent) +
    labs(y=paste("% of cells in selection 1 expressing the gene"), x=paste("% of cells in selection 2 expressing the gene"))
}





genesPlot.customViolins <- function(m, labs){
  df <- reshape2::melt(m)
  df <- dplyr::left_join(df, labs, by="Var2")
  df$Var1 <- factor(df$Var1, levels=rownames(m))

  ggplot(df) + geom_violin(aes(x=cluster,y=value, fill=cluster),scale="width") + facet_wrap(~Var1, ncol=1,scales = "free_y") + theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none") + labs(x="", y="log10 uTPM")

}

# L1 <- inOI[,inOI$layer== "L1"]
# L1 <- L1[,L1$cluster %in% names(which(table(L1$cluster) > 10))]
# L1$cluster <- factor(L1$cluster, c("Lamp5 Fam19a1 Pax6","Lamp5 Krt73","Lamp5 Fam19a1 Tmem182","Lamp5 Ntn1 Npy2r","Lamp5 Plch2 Dock5","Lamp5 Lsp1",
#                                    "Vip Igfbp6 Car10","Vip Igfbp6 Pltp","Vip Rspo4 Rxfp1 Chat",
#                                    "Vip Chat Htr1f","Vip Pygm C1ql1","Vip Crispld2 Htr2c","Vip Col15a1 Pde1a","Sncg Vip Nptx2","Sncg Gpr50",
#                                    "Vip Rspo1 Itga4","Vip Lect1 Oxtr"))
# 
# L23 <- inOI[,inOI$layer== "L2/3"]
# L23 <- L23[,L23$cluster %in% names(which(table(L23$cluster) > 10))]
# 
# # Which are alpha7 from Schuman paper?
# 
# genes <- c("Htr3a","Vip","Lamp5","Ndnf","Npy","Reln","Chrna7","Id2")
# 
# pdf(file = "data/Chrna7-L1.pdf",width = 15, height=20, useDingbats = F)
# genesPlot.customViolins(genes, L1)
# dev.off()
# 
# pdf(file = "data/Chrna7-L23.pdf",width = 15, height=20, useDingbats = F)
# genesPlot.customViolins(genes, L23)
# dev.off()
# 
# genes <- c("Vip","Lamp5","Ndnf","Npy")
# pdf(file = "data/class-markers-L1.pdf",width = 12, height=9, useDingbats = F)
# genesPlot.customViolins(genes, L1)
# dev.off()
# 
# genes <- c("Id2")
# pdf(file = "data/Id2-L1.pdf",width = 12, height=4, useDingbats = F)
# genesPlot.customViolins(genes, L1)
# dev.off()
# 
# genes <- c("Id2","Cck","Calb2","Bcl11b","Npas3","Rfxp1")
# pdf(file = "data/TFs-and-markers-L1.pdf",width = 12, height=7, useDingbats = F)
# genesPlot.customViolins(genes, L1)
# dev.off()
# 
# #
# L1$Lamp5 <- ifelse(L1$subclass == "Lamp5", as.character(L1$cluster), "other L1 cells")
# df <- as.data.frame(round(table(L1[,L1$subclass == "Lamp5"]$Lamp5) / table(inOI$layer)["L1"],2))
# ggplot(df, aes(x="layer", y=Freq)) + geom_bar(aes(fill=Var1),stat="identity",position=position_stack()) + coord_flip()
# 
# ggplot(data=df, aes_string(x=lnames[2], y="Freq",group=lnames[1])) +
#   geom_bar(aes_string(fill=lnames[1]),stat="identity",position = position_stack()) +
#   geom_text(aes(label=ifelse(Freq > 0.10, scales::percent(Freq,1), "")),
#             position=position_stack(vjust = 0.5), color="black", size=6) +
#   coord_flip() + xlab("") + ylab(paste("% of selected subtypes in Tasic's 2018 database")) +
#   scale_y_continuous(labels = scales::percent) +
#   theme(axis.text = element_text(size=14), axis.text.y = element_text(face="bold"),
#         axis.title.x = element_text(face = "bold", size=18))
# 
# 
# layer.cluster.distribution(labels=list("cluster"=L1$cluster, "layer"=L1$layer))
