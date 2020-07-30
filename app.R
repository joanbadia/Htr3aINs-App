library(shinyWidgets)
library(shinyBS)
library(shinydashboard)
library(shiny)
library(bmrm)
library(ggplot2)
library(ggdendro)
library(ggrepel)
library(cowplot)
library(plotly)
library(ggpubr)
library(eulerr)
library(DT)
library(dipsaus)


source("plots.R")
source("analysis.R")

load(file = "Data/input-dataset.rData")
M <- readRDS("Data/input-matrix.rds")
ListOfGenes <- rownames(M)
dg <- readRDS("Data/dendrogram.rds")

.left.colour <- "#B793C2"
.right.colour <- "#e08e5a"

clusters <- local({
  cl.colors <- c(
    "#DDACC9","#FF88AD","#FF88AD", # SBC
    "#FFB8CE","#D0A380","#A25D34", # NGC
    "#353634", # Chandelier?
    "#abd9e9","#abd9e9","#abd9e9","#abd9e9",  #SNCG
    "#fdae61","#fee090", # SERPINF1
    "#B09FFF","#B09FFF","#B09FFF","#B09FFF", # VIP-CCK
    "#756FB3","#756FB3","#756FB3", # VIP-ISC
    "#B3128A", "#B3128A","#353634","#B3128A", # VIP-DL
    "#B09FFF","#B09FFF", # ?
    "#FF4DC1","#FF4DC1","#FF4DC1" # ?
  )
  cl.names <- c(
    "Lamp5 Krt73","Lamp5 Fam19a1 Pax6","Lamp5 Fam19a1 Tmem182",   # SBC
    "Lamp5 Ntn1 Npy2r","Lamp5 Plch2 Dock5","Lamp5 Lsp1",   # NGC
    "Lamp5 Lhx6", # Chandelier?
    "Sncg Slc17a8","Sncg Gpr50","Sncg Vip Nptx2","Sncg Vip Itih5",   # SNCG
    "Serpinf1 Clrn1","Serpinf1 Aqp5 Vip",   # SERPINF1
    "Vip Crispld2 Htr2c","Vip Crispld2 Kcne4","Vip Col15a1 Pde1a", # VIP-CCK
    "Vip Rspo4 Rxfp1 Chat","Vip Ptprt Pkp2","Vip Lmo1 Myl1", # VIP-ISC
    "Vip Arhgap36 Hmcn1","Vip Gpc3 Slc18a3","Vip Lmo1 Fam159b","Vip Pygm C1ql1", # VIP-DL
    "Vip Chat Htr1f", "Vip Lect1 Oxtr","Vip Rspo1 Itga4", # ?
    "Vip Igfbp6 Car10","Vip Igfbp6 Pltp","Vip Igfbp4 Mab21l1" # ?
  )
  cl.types <- c(
    rep("SBC",3), # SBC
    rep("NGC",3), # NGC
    rep("Other",1), 
    rep("Other",4), # SNCG
    rep("Other",2), # SERPINF1
    rep("VIPCCK",3), # VIP-CCK
    rep("VIPCR",3), # VIP-CR
    rep("VIPDL",4), # VIP-DL
    rep("Other",3), # ?
    rep("Other",3) # ?
  )
  data.frame(colour=cl.colors, name=cl.names, type=cl.types,stringsAsFactors = F)
})


# === # == # === # == # === # == # === # == # === # == # === # == # === # == # 
# UI 
# === # == # === # == # === # == # === # == # === # == # === # == # === # == #


ui <- fluidPage(
  
  # htmltools::tags$style(HTML("
  # .row[id='first'] {background-color: #fae4d1; color: #b26623}
  # .row[id='second'] {background-color: #d6e8f6; color: #36688d}
  # 
  # ")),
  
  dashboardHeader(disable = TRUE),
  dashboardBody(
    tags$style(HTML("
                    
                    :root {
                    --left-bg-color: #B793C2;
                    --left-text-color: #B793C2;
                    --right-bg-color: #e08e5a;
                    --right-text-color: #e08e5a;
                    }
                    
                    body{
                      min-height: 611px;
                      height: auto;
                      max-width: 900px;
                      margin: auto;
                    }
                    
                    #RowSelection {
                      display: grid;
                      width: 1fr;
                      grid-template-columns: repeat(2, 1fr);
                      grid-auto-flow: row;
                      grid-gap: 5px;
                      background-color: #ffffff;
                    }
                    
                    #FirstSelection {
                      padding: 5px;
                      margin: 6px 0px 6px 20px;
                      background-color: var(--left-bg-color);
                      border-radius: .1em;
                      color: white;
                    }
                    
                    #SecondSelection {
                      padding: 5px;
                      margin: 6px 6px 6px 6px;
                      background-color: var(--right-bg-color);
                      border-radius: .1em;
                      color: white;
                    }
                    ")),
    
    h4("Select the desired Htr3a types to be compared, according to Tasic et al., 2018 taxonomy"),
    br(),
    div(id="RowSelection", class="wrapper",
        
        # BOX FOR CELL SELECTION 1
        div(id="FirstSelection", class="box",
            div(class="col-sm-4",
                pickerInput(inputId = "VIPCR",label = "VIP bipolar", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "VIPCR"],multiple = T, selected = c()),
                pickerInput(inputId = "NGC",label = "Neurogliaform", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "NGC"], multiple = T, selected = c("Lamp5 Plch2 Dock5","Lamp5 Lsp1"))
            ),
            div(class="col-sm-4",
                pickerInput(inputId = "VIPCCK",label = "VIP basket cell", width = "100%", multiple = T, options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "VIPCCK"], selected = c()),
                pickerInput(inputId = "Other",label = "Other(s)", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type %in% c("Other","VIPDL","SBC")], multiple = T, selected = c())
            ),
            div(class="col-sm-4",
                awesomeCheckboxGroup(inputId = "layer", label = "Layer(s)", choices = c("L1","L2/3","L4","L5","L6"),selected = c("L1"))
            )
        ),
        
        # BOX FOR CELL SELECTION 2
        div(id="SecondSelection", class="box",
            div(class="col-sm-4",
                pickerInput(inputId = "contrastVIPCR",label = "VIP bipolar", width = "100%", multiple = T, options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "VIPCR"], selected = c("Vip Rspo4 Rxfp1 Chat","Vip Ptprt Pkp2","Vip Lmo1 Myl1")),        
                pickerInput(inputId = "contrastNGC",label = "Neurogliaform", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "NGC"], multiple = T, selected = c())
            ),
            div(class="col-sm-4",
                pickerInput(inputId = "contrastVIPCCK",label = "VIP basket cell", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type == "VIPCCK"], multiple = T, selected = c()),
                pickerInput(inputId = "contrastOther",label = "Other(s)", width = "100%", options = list(`actions-box` = TRUE),
                            choices = clusters$name[clusters$type %in% c("Other","VIPDL","SBC")], multiple = T, selected = c())
            ),
            div(class="col-sm-4",
                awesomeCheckboxGroup(inputId = "contrastLayer", label = "Layer(s)",choices = c("L1","L2/3","L4","L5","L6"),selected = c("L2/3"))
            )
        )
    ),
                
      # box(title="Select your set of cells (Set1)", width = 6, solidHeader = T,background = "light-blue",
      # box(title="Select your set of cells (Set2)", width = 6, solidHeader = T, background = "olive",

    plotOutput(outputId = "tSNE", height="338px", width="100%"),
    
    hr(),
    h4("Display the expression of genetic markers in your selected Htr3a types"),
    br(),
  
    fluidRow(id="third",
             column(4,
                    fluidRow(
                      column(9, selectizeInput(inputId = "ListOfMarkers", label="Select gene(s)",width="100%", options = list(maxOptions = 5),
                                               choices=ListOfGenes,selected=c("Vip","Cck","Calb2"),multiple=T)),
                      column(3, div(style="margin: 25px 0px 0px 0px;text-align: center;",
                                    bsButton("MarkerButton", label = "Plot", width="100%", block = F, style="danger")))),
                    fluidRow(plotOutput(outputId = "MarkersViolins", height="400px"))),
             column(8, div(style="margin: 15px 10px 10px 10px",fluidRow(plotOutput(outputId = "layerDistribution", height = "500px"))))
    ),
    
    hr(),
    h4("Perform a binary classification (SVM) to extract the best genes distinguishing the two selection of cells"),
    fluidRow(id="fourth",
             column(3,
                    selectizeInput(inputId = "genes", label= "Gene family", width = "100%", 
                                   choices = c(
                                     "Transcription factors","Endogenous ligands","G protein-coupled receptors",
                                     "Ion channels","Cell adhesion molecules"
                                   ))),
             column(3,
                    numericInput(inputId = "NumberTopGenes", label = "Number of selected genes", width = "100%", 
                                 value=4, min = 4, max = 12, step = 2)
                    
                    ),
             column(2, div(style="margin: 25px 0px 0px 0px;text-align: center;",
                           bsButton("RunSVM", label = "Run SVM", width="100%", block = F, style="danger"))),
             column(2, div(style="margin: 25px 0px 0px 0px;text-align: center;",
                           bsButton("slot1", label = "Save genes", width="100%", block = F, style="info"))),
                    # actionBttn(inputId = "slot1", label="Save results")),
             column(2, div(style="margin: 25px 0px 0px 0px;text-align: center;",
                           bsButton("slot2", label = "Save genes", width="100%", block = F, style="info"))) 
                    # actionBttn(inputId = "slot2", label="Save results"))
    ),
    
    fluidRow(
      column(6, plotlyOutput(outputId = "topGenes", height="338px")),
      column(6, plotOutput(outputId = "geneViolins", height="338px"))
    ),
    
    hr(),
    fluidRow(
      column(6, DT::dataTableOutput("DT1", width="425px", height="500px")),
      column(6, DT::dataTableOutput("DT2", width="425px", height="500px"))
    ),
    fluidRow(plotOutput(outputId = "venn"))
  )
)


# === # == # === # == # === # == # === # == # === # == # === # == # === # == # 
# SERVER 
# === # == # === # == # === # == # === # == # === # == # === # == # === # == #


server <- function(input,output){
  
  # Reactive Panel 1 and 2: cell selection by type and layer
  r.metadata <- reactive({
    m1.ChoicesValues <- c(input$NGC, input$SBC, input$Other, input$VIPCCK, input$VIPCR, input$VIPDL)
    m2.ChoicesValues <- c(input$contrastNGC, input$contrastSBC, input$contrastOther, input$contrastVIPCCK, input$contrastVIPCR, input$contrastVIPDL)

    L$selected <- ifelse(L$cluster %in% m1.ChoicesValues & L$layer %in% input$layer, "Htr3a type(s) 1",
                         ifelse(L$cluster %in% m2.ChoicesValues & L$layer %in% input$contrastLayer,"Htr3a type(s) 2","not selected"))
    L$selected <- factor(L$selected, levels=c("Htr3a type(s) 1","Htr3a type(s) 2","not selected"))
    L
  })
  
  # Figure 1: tSNE
  output$tSNE <- renderPlot({
    ggplot(r.metadata(), aes(x=tSNE1, tSNE2, colour=selected)) + geom_point() + theme_bw() +
      scale_colour_manual(values=c("Htr3a type(s) 1"=.left.colour, "Htr3a type(s) 2"=.right.colour, "not selected"="grey80")) + 
      theme(legend.position = "none", panel.grid = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
  })
  
  # Figure 2: marker violin by merged types
  output$MarkersViolins <- renderPlot({
    
    # Remember that match uses as first argument the filtered version, and in the 2nd the full, so no need to filter.
    l <- r.metadata()
    m.markers <- reshape2::melt(M[input$ListOfMarkers, l$selected %in% c("Htr3a type(s) 1","Htr3a type(s) 2")])
    m.markers$names <- l$selected[match(m.markers$Var2, l$cell_id)]
    
    ggplot(m.markers,aes(x=names,y=value,fill=names,colour=names)) + geom_violin(scale="width",alpha=0.50) + ylab("uTPM") + xlab("") +
      scale_colour_manual(values=c("Htr3a type(s) 1"=.left.colour, "Htr3a type(s) 2"=.right.colour)) + 
      scale_fill_manual(values=c("Htr3a type(s) 1"=.left.colour, "Htr3a type(s) 2"=.right.colour)) + 
      facet_wrap(~Var1,ncol=1,strip.position = "left") + scale_x_discrete(expand=c(0,.5)) + theme_bw() + 
      theme(panel.grid = element_blank(), axis.text.x=element_text(angle=45,hjust=1,size=11,face="bold", color=c(.left.colour,.right.colour)),
            strip.text = element_text(size=14), legend.position = "none", ) 
  })
  
  
  # Figure 3: drendrogram + marker violin by individual types
  
  output$layerDistribution <- renderPlot({
    p.hc <- ggplot(segment(dendro_data(dg, type = "rectangle"))) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
      scale_x_continuous(breaks = 1:max(order.dendrogram(dg)), labels = labels(dg), expand = c(0,.5)) +
      theme(axis.text.x = element_text(angle = 45,hjust=1)) + ylab("") + xlab("") + theme_void()
    set.seed(9); p.gj <- ggplot(r.metadata(), aes_string(x="cluster.dg", y="layer",colour="selected")) + ylab("") + xlab("") +
      geom_jitter(alpha=0.80, size=1,stroke=0.25) + theme_minimal() + scale_x_discrete(expand=c(0,.5)) +
      scale_colour_manual(values=c("Htr3a type(s) 1"=.left.colour, "Htr3a type(s) 2"=.right.colour, "not selected"="grey80")) +
      theme(axis.text.x=element_text(angle=90,hjust=1,size=8,face="bold"), axis.text.y=element_text(size=18,face="bold"),
            legend.text = element_text(size = 11), panel.grid = element_blank(), plot.margin = margin(t=-0.2, unit="cm")) 
    if(length(input$ListOfMarkers>0)){
      
      # Drendrogram cluster order and colour
      dg.names <- levels(r.metadata()$cluster.dg)
      dg.cols <- setNames(
        ifelse(dg.names %in% c(input$NGC, input$SBC, input$Other, input$VIPCCK, input$VIPCR, input$VIPDL), .left.colour, 
               ifelse(dg.names %in% c(input$contrastNGC, input$contrastSBC, input$contrastOther, input$contrastVIPCCK, input$contrastVIPCR, input$contrastVIPDL), .right.colour,"grey80")),dg.names)    
      
      # Full gene expression matrix filtered by gene marker
      M.markers <- reshape2::melt(M[input$ListOfMarkers,])
      M.markers$cluster <- L$cluster.dg[match(M.markers$Var2, r.metadata()$cell_id)]
      
      # Gene violins below drendrogram and jitter
      p.violin <- ggplot(M.markers,aes(x=cluster,y=value)) + geom_violin(scale="width",fill="grey80") + ylab("uTPM") + xlab("") +
        facet_wrap(~Var1,ncol=1,strip.position = "left") + scale_x_discrete(expand=c(0,.5)) + theme_bw() + 
        theme(panel.grid = element_blank(), axis.text.x=element_text(angle=45,hjust=1,size=11,face="bold",color=dg.cols), 
              plot.margin = margin(t=-0.6, unit="cm")) 
      plot_grid(p.hc, p.gj+theme(legend.position = "none",axis.text.x = element_blank()),p.violin+theme(legend.position = "none"), 
                ncol=1, nrow=3, align="v",rel_heights = c(0.1,0.35,0.55),axis = "l")
    }
    else plot_grid(p.hc, p.gj+theme(legend.position = "none"), ncol=1, nrow=2, align="v",rel_heights = c(0.2,0.8))
  })
  
  # RUN SVM for Figure 4 and 5. Reactive panels 4, 5, 6, 7 and 8 depend on it. Table 1 and 2 and Figure 6 depend on it.

  svm <- eventReactive(input$RunSVM, {
    
    # First filter
    genes <- rownames(M) %in% gms[gms$gene_family == input$genes,]$gene_symbol
    cells <- r.metadata()$selected %in% c("Htr3a type(s) 1","Htr3a type(s) 2")
    
    # Filtered matrix and metadata, to the chosen Htr3a types
    mm <- M[genes, cells]
    ll <- r.metadata()[cells,]
    ll$contrast <- ll$selected == "Htr3a type(s) 1"
    
    # Split cells for train and test sets
    samples <- split.samples(ll$cell_id, ll$cluster)
    x.trn = t(mm[, samples$train])
    y.trn = ll[samples$train,]$contrast
    w <- svm.weights(x.trn = x.trn, y.trn = y.trn, MAX_ITER=1000)
    r <- svm.predict(w, x.tst = t(mm[,samples$test]), y.tst = ll[samples$test,]$contrast)
    g <- genes.stats(mm, ll$contrast, w, expr.threshold = 2, weight.threshold = 0.001, foldchange.threshold = 0.5)

    list(roc.stats=r, w=w, gene.stats=g, filter.genes=genes, filter.cells=cells)
  })
  
  # Top genes for Figure 5, first row
  currentUpGenes <- reactive({
    res <- svm()$gene.stats
    res$genes[rank(res$weight, ties.method = "first") %in% 1:input$NumberTopGenes]
  })
  
  # Top genes for Figure 5, second row
  currentDownGenes <- reactive({
    res <- svm()$gene.stats
    rev(res$genes[rank(-res$weight, ties.method = "first") %in% 1:input$NumberTopGenes])
  })
  
  output$topGenes <- renderPlotly({
    genesPlot.xy(svm()$gene.stats, c(currentUpGenes(), currentDownGenes()))
  })
  
  output$geneViolins <- renderPlot({
    genesPlot.topViolins(svm()$gene.stats, M[c(currentUpGenes(), currentDownGenes()), svm()$filter.cells], 
                         r.metadata()[svm()$filter.cells,],
                         currentUpGenes(), currentDownGenes())
  })
  
  
  slot1Genes <- eventReactive(input$slot1, {
    c(currentUpGenes(),currentDownGenes())
  })
  output$DT1 <- renderDataTable({svm()$gene.stats[svm()$gene.stats$genes %in% slot1Genes(),]},
                                extensions= c("Buttons","ColReorder","Scroller"), 
                                caption= 'Table 1: list1 of gene parameters.',
                                options= list(dom = 'citlB',buttons = c('copy', 'csv', 'excel'),colReorder = TRUE, deferRender = TRUE, scrollY = 200, scroller = TRUE)
  )
  
  slot2Genes <- eventReactive(input$slot2, {
    c(currentUpGenes(),currentDownGenes())
  })
  output$DT2 <- renderDataTable({svm()$gene.stats[svm()$gene.stats$genes %in% slot2Genes(),]}, 
                                extensions= c("Buttons","ColReorder","Scroller"), 
                                caption = 'Table 2: list 2 of gene parameters.',
                                options= list(dom = 'citlB',buttons = c('copy', 'csv', 'excel'), colReorder = TRUE, deferRender = TRUE, scrollY = 200, scroller = TRUE)
  )
  
  output$venn <- renderPlot({
    eu <- .util.aggregate(slot1Genes(),slot2Genes(), group=c("Gene slot 1","Gene slot 2"))[,2]
    eu2 <- .util.aggregate(slot1Genes(),slot2Genes(), group=c("Gene slot 1","Gene slot 2"))
    plot(euler(eu), quantities = c(sum((eu[,1] - eu[,2]) == 1), sum((eu[,1] - eu[,2]) == 0), sum((eu[,1] - eu[,2]) == -1)))
    plot(euler(eu))
  })
  
}

shinyApp(ui=ui, server=server)