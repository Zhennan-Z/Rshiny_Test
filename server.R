server <- function(input, output, session) {
  
  path <- reactiveValues(
    pth=NULL
  )
  observeEvent(input$filechoose,{
    fullpath <- file.choose()
    path$pth <- unlist(strsplit(fullpath,"[.]"))[1]
    print(path$pth)
  })
  
  infer_df <- eventReactive(input$EnterFID, {
    #req(input$fileinfer)
    req(path$pth)
    req(input$FID)
    #fileinfer <- input$fileinfer
    
    if (input$FID == "All"){
      infer_all <- read.table(paste0(path$pth, ".seg"), header = TRUE, stringsAsFactors = FALSE) } else {
        infer_all <- read.table(paste0(path$pth, ".seg"), header = TRUE, stringsAsFactors = FALSE)
        infer_all <- infer_all[infer_all$FID1 == input$FID | infer_all$FID2 == input$FID, ]
      }
    validate(
      need(nrow(infer_all) > 0, "Please type a valid FID")
    )
    updateSliderInput(session, "IBD1Seg",label = "IBD1Seg_Range",
                      min = min(infer_all$IBD1Seg), max = max(infer_all$IBD1Seg), 
                      value = c(min(infer_all$IBD1Seg),max(infer_all$IBD1Seg)))
    updateSliderInput(session, "IBD2Seg",label = "IBD2Seg_Range",
                      min = min(infer_all$IBD2Seg), max = max(infer_all$IBD2Seg), 
                      value = c(min(infer_all$IBD2Seg),max(infer_all$IBD2Seg)))
    return(infer_all)
  })
  
  segments_df <- reactive({
    #req(input$fileibdseg)
    #fileibdseg <- input$fileibdseg
    ibdseg <- read.table(paste0(path$pth, ".segments.gz"), header = TRUE, stringsAsFactors = FALSE)
    ibdseg <- ibdseg[, c("ID1", "ID2", "IBDType", "Chr", "StartMB", "StopMB")]
    return(ibdseg)
  })
  
  all_seg_df <- reactive({
    #req(input$fileallseg)
    #fileallseg <- input$fileallseg
    #allseg <- read.table(fileallseg$datapath, header = TRUE)
    allseg <- read.table(paste0(path$pth, "allsegs.txt"), header = TRUE)
    allseg <- allseg[, c("Chr", "StartMB","StopMB")]
    return(allseg)
  })
  
  filename <- reactive({
    #req(input$fileinfer)
    #fileinfer <- input$fileinfer
    length.ind <- length(strsplit(path$pth, "/"))
    file_prefix <- unlist(strsplit(path$pth, "/"))[length.ind]
    return(file_prefix)
  })
  
  
  
  observeEvent(input$EnterFID, {
    #req(infer_df)
    #req(input$FID)
    infer_df_FID <- infer_df()
    infer_df_FID <- infer_df_FID[infer_df_FID$FID1 == input$FID | infer_df_FID$FID2 == input$FID, ]
    related_pairs <- paste(infer_df_FID$ID1, infer_df_FID$ID2, sep=" & ")
    updateSelectizeInput(session, "IDs", label = "IDs", choices = c(Choose='', related_pairs), selected = NULL)
  })
  
  output$plot1 <- renderPlot({
    req(infer_df())
    req(input$EnterFID)
    prefix <- filename()
    individuals_all <- infer_df()
    individuals_all <- individuals_all[individuals_all$IBD1Seg >= input$IBD1Seg[1] & individuals_all$IBD1Seg <= input$IBD1Seg[2] & 
                                         individuals_all$IBD2Seg >= input$IBD2Seg[1] & individuals_all$IBD2Seg <= input$IBD2Seg[2],]
    validate(
      need(nrow(individuals_all) > 0, "Please adjust the IBD1 Seg and IBD2 Seg")
    )
    
    d0 <- individuals_all$IBD2Seg>0.7
    d1.PO <- (!d0) & individuals_all$IBD1Seg+individuals_all$IBD2Seg>0.96 | (individuals_all$IBD1Seg+individuals_all$IBD2Seg>0.9 & individuals_all$IBD2Seg<0.08)
    d1.FS <- (!d0) & (!d1.PO) & individuals_all$PropIBD>0.35355 & individuals_all$IBD2Seg>=0.08
    d2 <- individuals_all$PropIBD>0.17678 & individuals_all$IBD1Seg+individuals_all$IBD2Seg<=0.9 & (!d1.FS)
    d3 <- individuals_all$PropIBD>0.08839 & individuals_all$PropIBD<=0.17678
    d4 <- individuals_all$PropIBD>0.04419 & individuals_all$PropIBD<=0.08839
    dU <- individuals_all$PropIBD>0 & individuals_all$PropIBD<=0.04419
    plot(individuals_all$IBD1Seg[dU], individuals_all$IBD2Seg[dU], type="p", col = "black", cex.lab=1.2,
         xlim=c(min(individuals_all$IBD1Seg), max(individuals_all$IBD1Seg)),
         ylim=c(min(individuals_all$IBD2Seg), max(individuals_all$IBD2Seg)),
         main = paste0("IBD Segments In Inferred ", prefix, " Relatives"),
         xlab=expression(paste("Length Proportion of IBD1 Segments (", pi[1], ")",sep="")),
         ylab=expression(paste("Length Proportion of IBD2 Segments (", pi[2], ")",sep="")))
    points(individuals_all$IBD1Seg[d0], individuals_all$IBD2Seg[d0], col="purple")
    points(individuals_all$IBD1Seg[d1.PO], individuals_all$IBD2Seg[d1.PO], col="red")
    points(individuals_all$IBD1Seg[d1.FS], individuals_all$IBD2Seg[d1.FS], col="green")
    points(individuals_all$IBD1Seg[d2], individuals_all$IBD2Seg[d2], col="blue")
    points(individuals_all$IBD1Seg[d3], individuals_all$IBD2Seg[d3], col="magenta")
    points(individuals_all$IBD1Seg[d4], individuals_all$IBD2Seg[d4], col="gold")
    abline(h = 0.08, col = "green", lty = 3, lwd = 2)
    abline(a = 0.96, b = -1, col = "red", lty = 3, lwd = 2)
    abline(a = 0.3535534, b = -0.5, col = "green", lty = 3, lwd = 2)
    abline(a = 0.1767767, b = -0.5, col = "blue", lty = 3, lwd = 2)
    abline(a = 0.08838835, b = -0.5, col = "magenta", lty = 3, lwd = 2)
    abline(a = 0.04419, b = -0.5, col = "gold", lty = 3, lwd = 2)
    allcolors <- c("purple", "red", "green", "blue", "magenta", "gold", "black")
    legend("topright", c("Inferred MZ", "Inferred PO", "Inferred FS", "Inferred 2nd", "Inferred 3rd", "Inferred 4th", "Inferred UN"),
           col=allcolors, text.col = allcolors, pch = 19, cex = 1.2)
  })
  
  output$click_info <- renderPrint({
    req(input$plot_click)
    req(infer_df())
    req(segments_df())
    req(all_seg_df())
    individuals_all <- infer_df()
    individuals_all <- individuals_all[individuals_all$IBD1Seg >= input$IBD1Seg[1] & individuals_all$IBD1Seg <= input$IBD1Seg[2] & 
                                         individuals_all$IBD2Seg >= input$IBD2Seg[1] & individuals_all$IBD2Seg <= input$IBD2Seg[2],]
    segments <- segments_df()
    all_seg <- all_seg_df()
    
    chr.num <- ifelse (max(segments$Chr)==23, 23, max(segments$Chr))
    all_seg <- all_seg[all_seg$Chr <=chr.num, ]
    point.index <- which.min((individuals_all$IBD1Seg-input$plot_click$x)^2+(individuals_all$IBD2Seg-input$plot_click$y)^2) 
    if (!(abs(individuals_all[point.index, "IBD1Seg"]-input$plot_click$x) <= 0.01 & abs(individuals_all[point.index,"IBD2Seg"]-input$plot_click$y) <= 0.01)) {
      target.data <- NULL } else {
        segments$IBDType <- factor(segments$IBDType, levels = c("IBD0", "IBD1", "IBD2"))
        target.data <- segments[segments$ID1==individuals_all[point.index,"ID1"] & segments$ID2==individuals_all[point.index,"ID2"], ]
      }
    output$plot2 <- renderPlot({
      validate(
        need(nrow(target.data) > 0, "Please select a related pair")
      )
      Prop.IBD1 <- formatC(individuals_all[point.index, "IBD1Seg"], digits = 3, format = "f")
      Prop.IBD2 <- formatC(individuals_all[point.index, "IBD2Seg"], digits = 3, format = "f")
      #
      theme_set(theme_bw(base_size = 14))
      g <- ggplot() +
        geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
        geom_rect(data = target.data , aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9, fill = IBDType)) + 
        geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
        scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
        facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) +
        labs(x = "Position (MB)", y = "", title=substitute(paste("IBD Segments between ", ID1," and ", ID2, " (", pi[1], "=", PropIBD1, ";", pi[2], "=", PropIBD2, ")"),
                                                           list(ID1 = target.data$ID1, ID2 = target.data$ID2, PropIBD1 = Prop.IBD1, PropIBD2 = Prop.IBD2)))+
        theme(
          legend.position = "bottom", legend.key = element_rect(color = "black"),
          panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank()
        )
      print(g)
    })
    output$dt1 <- renderDataTable({
      validate(
        need(nrow(target.data) > 0, "Please select a related pair")
      )
      target.data
    })
  })
  
  output$plot3 <- renderPlot({
    req(input$FID)
    req(input$IDs)
    req(infer_df())
    req(all_seg_df())
    individuals_all <- infer_df()
    all_seg <- all_seg_df()
    all_seg_gz <- segments_df()
    pairs <- input$IDs
    pair1 <- unlist(strsplit(pairs, " & " ))[1]
    pair2 <- unlist(strsplit(pairs, " & "))[2]
    #target.data <- input_target()
    target.data <- all_seg_gz[(all_seg_gz$ID1==pair1 & all_seg_gz$ID2==pair2) | (all_seg_gz$ID2==pair1 & all_seg_gz$ID1==pair2), ]
    validate(
      need(nrow(target.data) > 0, "Please select a related pair")
    )
    
    chr.num <- ifelse (max(target.data$Chr)==23, 23, max(target.data$Chr))
    all_seg <- all_seg[all_seg$Chr <= chr.num, ]
    
    ID1 <- unique(target.data$ID1)
    ID2 <- unique(target.data$ID2)
    
    Prop.IBD1 <- formatC(individuals_all[individuals_all$ID1==ID1 & individuals_all$ID2==ID2, "IBD1Seg"], digits = 3, format = "f")
    Prop.IBD2 <- formatC(individuals_all[individuals_all$ID1==ID1 & individuals_all$ID2==ID2, "IBD2Seg"], digits = 3, format = "f")
    
    theme_set(theme_bw(base_size = 16))
    g <- ggplot() +
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), fill = 'white', color = "black", size = 0.85) + 
      geom_rect(data = target.data , aes(xmin = StartMB, xmax = StopMB, ymin = 0, ymax = 0.9, fill = IBDType)) + 
      geom_rect(data = all_seg, aes(xmin = StartMB, xmax = StopMB, ymin = 0, max = 0.9), color = "black", alpha = 0, size = 0.85) + 
      scale_fill_manual(values = c("IBD0" = "white", "IBD1" = "dodgerblue2", "IBD2" = "firebrick2"), drop = FALSE) + 
      facet_grid(Chr ~ .) + scale_x_continuous(expand  = c(0, 0), limits = c(0, NA)) + 
      labs(x = "Position (Mb)", y = "", title=substitute(paste("IBD Segments between ", ID1," and ", ID2, " (", pi[1], "=", PropIBD1, ";", pi[2], "=", PropIBD2, ")"),
                                                         list(ID1 = target.data$ID1, ID2 = target.data$ID2, PropIBD1 = Prop.IBD1, PropIBD2 = Prop.IBD2))) + 
      theme(
        legend.position = "bottom", legend.key = element_rect(color = "black"),
        panel.background = element_rect(fill = 'grey80', color = 'grey80'), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()
      )
    print(g)
    
  })
  
  output$dt2 <- renderDataTable({
    #req(input$fileibdseg)
    req(input$IDs)
    pairs <- input$IDs
    pair1 <- unlist(strsplit(pairs, " & "))[1]
    pair2 <- unlist(strsplit(pairs, " & "))[2]
    all_seg_df <- segments_df()
    select_df <- all_seg_df[(all_seg_df$ID1 == pair1 & all_seg_df$ID2 == pair2) |(all_seg_df$ID2 == pair1 & all_seg_df$ID1 == pair2), ]
    validate(
      need(nrow(select_df) > 0, "Please select a related pair")
    )
    select_df
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
