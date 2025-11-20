#' Plots a sankey diagram displaying the flow between first, second and third regimen eras
#' @param processedEras An output dataframe created by calculateEras
#' @param regGroups A dataframe indicating how to group regimens
#' @param saveLocation A file directory location where files may be saved
#' @param fileName A filename prefix for saved files
#' @export
plotSankey_local <- function(processedEras, regGroups, saveLocation = NA, fileName = "Network"){
  
  if(is.na(saveLocation)){
    saveLocation <- here::here()
  }
  
  firstLine <- processedEras_combined[processedEras_combined$First_Line==1,]
  firstLine_Tab <- as.data.frame(table(firstLine$component))
  
  secondLine <- processedEras_combined[processedEras_combined$Second_Line==1,]
  secondLine_Tab <- as.data.frame(table(secondLine$component))
  
  thirdLine <- processedEras_combined[processedEras_combined$Other==1,]
  thirdLine_Tab <- as.data.frame(table(thirdLine$component))
  
  sankey_first <- firstLine[,c(3,1)]
  sankey_sec <- secondLine[,c(3,1)]
  sankey_third <- thirdLine[,c(3,1)]
  
  colnames(sankey_first) <- c("personID","Var1")
  colnames(sankey_sec) <- c("personID","Var1")
  colnames(sankey_third) <- c("personID","Var1")
  
  colnames(regGroups) <- c("Var1","regGroup")
  #regimens_empirical2 <- regimens_empirical
  #colnames(regimens_empirical2) <- c("Var1","regGroup")
  
  sankey_first <- merge(sankey_first,regGroups,by="Var1")[,c(2,3)]
  sankey_sec <- merge(sankey_sec,regGroups,by="Var1")[,c(2,3)]
  sankey_third <- merge(sankey_third,regGroups,by="Var1")[,c(2,3)]
  
  colnames(sankey_first) <- c("personID","First Line")
  colnames(sankey_sec) <- c("personID","Second Line")
  colnames(sankey_third) <- c("personID","Subsequent Lines")
  
  sankey_all <- merge(merge(sankey_first,sankey_sec,all = T),sankey_third,all=T)
  sankey_all <- sankey_all[!duplicated(sankey_all$personID),]
  
  sankey_all[is.na(sankey_all$`Second Line`),]$`Second Line` <- ""
  sankey_all[is.na(sankey_all$`Subsequent Lines`),]$`Subsequent Lines` <- ""
  
  tt1 <- as.data.frame(table(reshape2::melt(sankey_all[,c(2,3)],
                                            id.vars = c("First Line","Second Line"), na.rm = F)))
  
  tt2 <- as.data.frame(table(reshape2::melt(sankey_all[,c(3,4)],
                                            id.vars = c("Second Line","Subsequent Lines"), na.rm = F)))
  
  
  tt1$First.Line <- as.character(tt1$First.Line)
  tt1$Second.Line <- as.character(tt1$Second.Line)
  tt2$Second.Line <- as.character(tt2$Second.Line)
  tt2$Subsequent.Lines <- as.character(tt2$Subsequent.Lines)
  
  tt1 <- tt1[!tt1$First.Line==tt1$Second.Line,]
  tt2 <- tt2[!tt2$Second.Line==tt2$Subsequent.Lines,]
  
  tt1$First.Line <- paste(tt1$First.Line,"(1st)",sep=" ")
  tt1$Second.Line <- paste(tt1$Second.Line,"(2nd)",sep=" ")
  
  tt2$Second.Line <- paste(tt2$Second.Line,"(2nd)",sep=" ")
  tt2$Subsequent.Lines <- paste(tt2$Subsequent.Lines,"(3rd)",sep=" ")
  
  colnames(tt1) <- c("source","target","value")
  colnames(tt2) <- c("source","target","value")
  
  links <- rbind(tt1,tt2)
  
  links <- links[!links$target %in% c(" (2nd)"," (3rd)"),]
  links <- links[!links$source %in% c(" (2nd)"," (3rd)"),]
  
  nodes <- data.frame(
    name=c(as.character(links$source),
           as.character(links$target)) %>% unique()
  )
  
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1
  
  p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                Source = "IDsource", Target = "IDtarget",
                                Value = "value", NodeID = "name",
                                sinksRight=FALSE, width = 2200, height = 1000,
                                fontSize = 28, fontFamily = "calibri")
  
  #networkFile <- paste(saveLocation,"/",fileName,".html",sep="")
  
  #networkD3::saveNetwork(p, file = networkFile)
  
  #webshot::webshot(url = networkFile, file = paste(saveLocation,"/",fileName,".png",sep=""), vwidth = 2200, vheight = 1000)
  
  
}