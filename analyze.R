library(nombre)
library(rjson)

library(ggplot2)
library(effectsize)

##################################################################################################
# WARN  copied&pasted from util.R to make it self-contained
##################################################################################################

### Compute Vargha-Delaney A12 statistics
measureA <- function(a, b) {

  if (length(a) == 0 & length(b) == 0) {
    return(0.5)
  } else if (length(a) == 0) {
    ## motivation is that we have no data for "a" but we do for "b".
    ## maybe the process generating "a" always fail (eg out of memory)
    return(0)
  } else if (length(b) == 0) {
    return(1)
  }

  r = rank(c(a, b))
  r1 = sum(r[seq_along(a)])

  m = length(a)
  n = length(b)
  A = (r1 / m - (m + 1) / 2) / n

  return(A)
}


formatPvalue <- function(p){
  pv=""
  if(is.nan(p)){
    pv="NaN"
  }else{
    pv = formatFloat(p, 3)
    
    if (p < 0.001) {
      pv = "$\\le $0.001"
    } 
  }
  return(pv)
}

formatFloat <- function(value, digits = 1){
  if(is.null(value)) return("NULL")
  if(is.na(value)) return("Na")
  if(is.nan(value)) return("NaN")
  
  return(paste(formatC(value, digits = digits, format = "f"), sep = ""))
  
}

boldValue <- function(content, cond = TRUE, secCond = NULL){
  if(!cond) return(content)
  if(is.null(secCond) || secCond)
    c <- paste("\\textbf{", content, "}", sep = "")
  else{
    c <- paste("\\textcolor{red}{", content,"}", sep="")
  }
  return(c)
}


##################################################################################################

DATA_VERSION = "v2_0"

SEEDING_LABEL = "_seeding"

# Folder where files like tables and graphs should be saved to
GENERATED_FILES = "generated_files"

DATA_DIR = "data"

# Zip file of the aggregated data
# statistics
COMBINE_ZIP_FILE = paste(DATA_DIR, "/cy_compressedCombinedData","_",DATA_VERSION, ".zip",sep = "")
# snapshots
SNAP_ZIP_FILE=paste(DATA_DIR,"/cy_compressedSnapshotData","_",DATA_VERSION,"_final", ".zip",sep = "")
# statistics for seeding
COMBINE_ZIP_FILE_SEEDING = paste(DATA_DIR, "/cy_compressedCombinedData","_",DATA_VERSION, SEEDING_LABEL, ".zip", sep = "")


# # statistics
# COMBINE_ZIP_FILE = paste(DATA_DIR, "/compressedCombinedData","_",DATA_VERSION, ".zip",sep = "")
# # snapshots
# SNAP_ZIP_FILE=paste(DATA_DIR,"/compressedSnapshotData","_",DATA_VERSION,"_final", ".zip",sep = "")
# # statistics for seeding
# COMBINE_ZIP_FILE_SEEDING = paste(DATA_DIR, "/compressedCombinedData","_",DATA_VERSION, SEEDING_LABEL, ".zip", sep = "")


################ handle time budget #############

SKIPPED_TIME_BUDGETS <- c("1m")

tbFilter <- function(budgets){
  return(budgets[!budgets %in% SKIPPED_TIME_BUDGETS])
}

################ handle sut #############

SUTINFO_DATA <- "data/sutinfo.csv"

SUT_IDS <- c()
SUT_CMDS <- c()
SUT_CR_LINES <- c()
SUT_LINES <- c()

COVERAGEINFO_JSON = "data/v2_0_coverage_json"

orderSUT <- function(ByCategory=FALSE){
  sutdt <- read.table(SUTINFO_DATA, header = T)
  ordered <- NULL
  if(ByCategory)
    ordered <- sutdt[order(sutdt$Category,sutdt$Files),]
  else
    ordered <- sutdt[order(sutdt$Files),]
  projects = unique(ordered$Id)
  #projects = sort(unique(sutdt$Id))
  return(projects)
}

generateSutCmd <- function(){
  
  SUT_IDS <<- c()
  SUT_CMDS <<- c()
  SUT_CR_LINES <<- c()
  SUT_LINES <<- c()
  
  SUT_NAMES <- c()
  
  RESULTS_CMDS = paste(GENERATED_FILES, "/sutId.tex", sep = "")
  unlink(RESULTS_CMDS)
  sink(RESULTS_CMDS, append = TRUE, split = TRUE)
  
  all <- read.table(gzfile(COMBINE_ZIP_FILE), header = T)
  allseeding <- read.table(gzfile(COMBINE_ZIP_FILE_SEEDING), header = T)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  projects = orderSUT()
  
  for (index in 1:length(projects)) {
    
    project <- projects[[index]]
    projectCmd <- paste("\\cs",formatString(nom_ord(strtoi(index))), sep = "")
    SUT_IDS[index] <<- project
    SUT_CMDS[index] <<- projectCmd
    SUT_NAMES[index] <- paste("cs",sprintf("%02d",index), sep = "")
    id <- paste("\\newcommand{",projectCmd, "}{\\emph{",SUT_NAMES[index],"}\\xspace}", sep="")
    cat(id, "\n",sep = "")
    
    SUT_CR_LINES[index] <<- max(c(all$lineAndtotal[all$id == project],allseeding$lineAndtotal[allseeding$id == project]))
    SUT_LINES[index] <<- max(c(all$numberOfLines[all$id == project], allseeding$numberOfLines[allseeding$id == project]))
  }
  
  sink()
  
}

formatBigNumber <- function(x){
  
  return(format(x, big.mark = ",", scientific = FALSE))
}

generateSutStatistics <- function(){
  
  SUTINFO = paste(GENERATED_FILES, "/sutstatisticsinfo.tex", sep = "")
  unlink(SUTINFO)
  sink(SUTINFO, append = TRUE, split = TRUE)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  projects = orderSUT()

  cat("\\begin{tabular}{ l r rr r rr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT", " & \\#Files & LOC & LOC\\textsubscript{\\emph{b}} & \\#Interfaces & \\#Dependent &\\#Tables \\\\ \n", sep ="")
  cat("", " &  & & & (\\#Functions) & Services &(\\#Rows) \\\\ \n", sep ="")
  
  
  cat("\\midrule \n")


  for (i in 1:length(projects)) {
    p <- projects[[i]]
    sutdtIdMastk = sutdt$Id == p
    
    cat(formatSUT(p), 
        " & ", formatBigNumber(sutdt$Files[sutdtIdMastk]), 
        " & ", formatBigNumber(sutdt$Lines[sutdtIdMastk]), 
        " & ", formatBigNumber(sutdt$ByteLines[sutdtIdMastk]), 
        " & ", formatBigNumber(sutdt$Interfaces[sutdtIdMastk]),"(",sutdt$Functions[sutdtIdMastk],")", 
        " & ", formatBigNumber(sutdt$Down[sutdtIdMastk]),
        " & ", formatBigNumber(sutdt$Tables[sutdtIdMastk]),"(",formatBigNumber(sutdt$Rows[sutdtIdMastk]),")", 
        "\\\\ \n", sep = "")
  }

  cat("\\midrule \n")
  cat("Sum",  
      " & ", formatBigNumber(sum(sutdt$Files)),  
      " & ", formatBigNumber(sum(sutdt$Lines)), 
      " & ", formatBigNumber(sum(sutdt$ByteLines)), 
      " & ", formatBigNumber(sum(sutdt$Interfaces)),"(",sum(sutdt$Functions),")", 
      " & ", formatBigNumber(sum(sutdt$Down)),
      " & ", formatBigNumber(sum(sutdt$Tables)),"(",formatBigNumber(sum(sutdt$Rows)),")", 
      "\\\\ \n", sep = "")

  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
  
  ## statistics cmd
  
  SUTST = paste(GENERATED_FILES, "/sutstatisticscmd.tex", sep = "")
  unlink(SUTST)
  sink(SUTST, append = TRUE, split = TRUE)
  cat("\\newcommand{\\sumFiles}{",formatBigNumber(sum(sutdt$Files)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumLines}{",formatBigNumber(sum(sutdt$Lines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumByteLines}{",formatBigNumber(sum(sutdt$ByteLines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumInterfaces}{",formatBigNumber(sum(sutdt$Interfaces)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumFunctions}{",formatBigNumber(sum(sutdt$Functions)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumDirectServices}{",formatBigNumber(sum(sutdt$Down)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumTables}{",formatBigNumber(sum(sutdt$Tables)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\sumRows}{",formatBigNumber(sum(sutdt$Rows)),"\\xspace} \n", sep="")
  
  cat("\\newcommand{\\minFiles}{",formatBigNumber(min(sutdt$Files)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minLines}{",formatBigNumber(min(sutdt$Lines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minByteLines}{",formatBigNumber(min(sutdt$ByteLines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minInterfaces}{",formatBigNumber(min(sutdt$Interfaces)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minFunctions}{",formatBigNumber(min(sutdt$Functions)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minDirectServices}{",formatBigNumber(min(sutdt$Down)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minTables}{",formatBigNumber(min(sutdt$Tables)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\minRows}{",formatBigNumber(min(sutdt$Rows)),"\\xspace} \n", sep="")
  
  cat("\\newcommand{\\maxFiles}{",formatBigNumber(max(sutdt$Files)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxLines}{",formatBigNumber(max(sutdt$Lines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxByteLines}{",formatBigNumber(max(sutdt$ByteLines)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxInterfaces}{",formatBigNumber(max(sutdt$Interfaces)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxFunctions}{",formatBigNumber(max(sutdt$Functions)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxDirectServices}{",formatBigNumber(max(sutdt$Down)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxTables}{",formatBigNumber(max(sutdt$Tables)),"\\xspace} \n", sep="")
  cat("\\newcommand{\\maxRows}{",formatBigNumber(max(sutdt$Rows)),"\\xspace} \n", sep="")
  
  sink()
}

formatString <- function(title){
  t<- gsub("[^A-Za-z]","" , title ,ignore.case = TRUE)
  return(t)
}


formatSUT <- function(sut, anonymized = TRUE){
  if(!anonymized)
    return(sut)
  
  index <- match(sut, SUT_IDS)
  if(is.na(index))
    return(sut)
  return(SUT_CMDS[index])
}

formatSUTName <- function(sut, anonymized = TRUE){
  if(!anonymized)
    return(sut)
  
  index <- match(sut, SUT_IDS)
  if(is.na(index))
    return(sut)
  return(SUT_NAMES[index])
}


getTotalCRLines <- function(sut){
  index <- match(sut, SUT_IDS)
  if(is.na(index))
    stop(paste("cannot find total cr lines for sut:",sut, sep = ""))
  return(SUT_CR_LINES[index])
}

getTotalLines <- function(sut){
  index <- match(sut, SUT_IDS)
  if(is.na(index))
    stop(paste("cannot find total lines for sut:",sut, sep = ""))
  return(SUT_LINES[index])
}

notNumSetZero <- function(value){
  if(is.numeric(value))
    return(value)
  else
    0
}

#############################


coverageGraph <- function(sbudget = NULL, dt, prefix = paste("plot_",DATA_VERSION, "_coverage", sep = ""), sprojects = NULL, anonymized = TRUE){
  
  projects = sort(unique(dt$id))
  if(!is.null(sprojects))
    projects <- projects[projects %in% sprojects]

  budgets <- sort(unique(dt$maxTime))
  if(!is.null(sbudget))
    budgets <- c(sbudget)

  
  all_plot_colors = c("blue", "red", "green")
  plot_colors <- all_plot_colors[1:length(budgets)]
  
  for (proj in projects) {
    
    bindex <- 0
    
    fp <- formatSUT(proj, anonymized)
    if(anonymized)
      fp <- substring(fp, 2)
    pdf(paste(GENERATED_FILES, "/", prefix,"_",fp,".pdf", sep = ""))
    targets = sort(unique(dt$interval))
    z = length(targets)
    all <- matrix(0, nrow = length(budgets), ncol = z)
    
    for(budget in budgets){
      bindex <- bindex + 1

      baseMask = dt$id == proj

      tenMask = baseMask & dt$maxTime == budget

      ten = rep(0, times = z)

      for (i in 1 : z) {
        targetMask = dt$interval == targets[[i]]
        all[bindex,i] = mean(dt$coveredTargets[targetMask & tenMask])
      }

    }
    
    
    line_width = 2
    
    yMin = min(all)
    yMax = max(all)
    
    for (j in 1: length(budgets)) {
      ten <- all[j,]
      if(j == 1){
        plot(ten, ylim = c(yMin, yMax), type = "o", col = plot_colors[[j]], pch = 20+j, lty = j, lwd = line_width, ylab = "Covered Testing Targets", xlab = "Budget Percentage", xaxt = "n")
        axis(side = 1, labels = targets, at = 1 : z)
      }else{
        lines(one, type = "o", col = plot_colors[[j]], pch = 20+j, lty = j, lwd = line_width)
      }
    }
    
    
    
    lx = 15
    ly = yMin + 0.5 * (yMax - yMin)
    
    legend(lx, ly, budgets
           , cex = 1.5, col = plot_colors
           , pch = 21 : (20+length(budgets))
           , lty = 1 : length(budgets))
    
    dev.off()
    
  }
}


linecoverage_cellcolor <- function(mean_percent){
  if(mean_percent < 5)
    return("\\cellcolor{red!30!white}")
  else if(mean_percent < 10)
    return("\\cellcolor{red!10!white}")
  else if(mean_percent < 20)
    return("\\cellcolor{green!10!white}")
  else
    return("\\cellcolor{green!30!white}")
}

linecoverage_index <- function(mean_percent){
  if(mean_percent < 5)
    return(1)
  else if(mean_percent < 10)
    return(2)
  else if(mean_percent < 20)
    return(3)
  else
    return(4)
}


boolean_cellcolor <- function(bvalue, level){
  if(is.null(bvalue))
    return("")
  if(is.null(level))
    level <- 10
  
  setting <- ""
  if(bvalue){
    setting <- paste("\\cellcolor{green!",level,"!white}", sep = "")
  }else
    setting <- paste("\\cellcolor{red!",level,"!white}", sep = "")
  return(setting)
}

coverageTableWithMask <- function(dt, maskId, showTB= TRUE, showLabel=FALSE, anonymized, st_metrics = c(TRUE, TRUE, TRUE, TRUE, TRUE), showDistribution = FALSE) {


  TABLE = paste(GENERATED_FILES, "/coverage_",DATA_VERSION, "_" ,maskId,".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)

  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  tb_row_name <- ""
  tb_row_setting <- ""
  if(showTB){
    tb_row_name <- "& TB"
    tb_row_setting <- "l |"
  }
  
  label_row_name <- ""
  label_row_setting <- ""
  if(showLabel){
    label_row_name <- "& Type"
    label_row_setting <- " l "
  }
    
  
  cat("\\begin{tabular}{ l", tb_row_setting,  label_row_setting, " rrr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT", tb_row_name,label_row_name,"&Line\\%  &Critical Line\\% & \\#Detected Faults  \\\\ \n", sep ="")
  cat("\\midrule \n")

  existingprojects = sort(unique(dt$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  projectsByCategory <- orderSUT(ByCategory = FALSE)
  projects <- projects[order(match(projects, projectsByCategory))]
  
  labels = sort(unique(sutdt$Category)) # row
  performanceLevels = c("<5","[5,10)", "[10, 20)", "[20,100]") # column
  
  nprojects = length(projects)

  budgets = tbFilter(sort(unique(dt$maxTime)))
  nbudgets = length(budgets)

  # avgLines = rep(0, nprojects * nbudgets)
  # medianLines = rep(0, nprojects * nbudgets)
  # Mean, Min, Median, Max
  stLines = matrix(0, nrow = 4, ncol = nprojects * nbudgets)
  avgLinesByType = matrix(0, nrow = length(labels), ncol = length(performanceLevels))
  
  # avgCrLines = rep(0, nprojects * nbudgets)
  # medianCrLines = rep(0, nprojects * nbudgets)
  stCrLines = matrix(0, nrow = 4, ncol = nprojects * nbudgets)
  avgCrLinesByType = matrix(0, nrow = length(labels), ncol = length(performanceLevels))
  
  avgBranches = rep(0, nprojects * nbudgets)
  #avgFaults = rep(0, nprojects * nbudgets)
  #medianFaults = rep(0, nprojects * nbudgets)
  stFaults = matrix(0, nrow = 4, ncol = nprojects * nbudgets)
  
  labels_indexes <- c()
  
  lclevels <- c()
  crlclevels <- c()
  
  for (i in 1:length(projects)) {
    proj = projects[[i]]
    
    projectMask =  dt$id == proj
    
    totLines = getTotalLines(proj)
    totBranches = max(dt$numberOfBranches[projectMask])
    
    totCrLines = getTotalCRLines(proj)
    
    for (j in 1:length(budgets)) {
      
      BINDEX = (i-1)*nbudgets + j
      
      budget = budgets[[j]]
      
      if(j == 1){
        #cat("\\emph{", formatSUT(proj), "}", sep = "")
        cat(formatSUT(proj, anonymized))
      }
      
      if(showTB)
        cat(" &", budget, sep = "")
      
      sutprojectMask = sutdt$Id == proj
      label <- sutdt$Category[sutprojectMask]
      
      if(showLabel){
        cat(" &", label, sep = "")
      }
      labels_indexes <- append(labels_indexes, label)
      
      row <- match(label, labels)
      
      budgetMask = projectMask & dt$maxTime == budget
      lines = dt$coveredLines[budgetMask]
      
      
      percent = 100 * (lines / totLines)
      stLines[1, BINDEX] = mean(percent)
      stLines[2, BINDEX] = min(percent)
      stLines[3, BINDEX] = median(percent)
      stLines[4, BINDEX] = max(percent)
      
      cat(" & ")
      mean_percent <- mean(percent)
      cat(linecoverage_cellcolor(mean_percent))
      
      plIndex <- linecoverage_index(mean_percent)
      avgLinesByType[row, plIndex] <- avgLinesByType[row, plIndex]+1
      
      if(st_metrics[1])
        cat(formatC(mean_percent, digits = 1, format = "f"),"", sep = "")

      if(st_metrics[2])
        cat(" [", formatC(min(percent), digits = 1, format = "f"),", ", sep = "")
      if(st_metrics[3])
        cat(formatC(median(percent), digits = 1, format = "f"),", ", sep = "")
      if(st_metrics[4])
        cat(formatC(max(percent), digits = 1, format = "f"),"]", sep = "")
      
      if(st_metrics[5])
        cat(" (",formatC(sd(percent), digits = 2, format = "f"),")", sep = "")
      
      crlines = dt$lineAndcovered[budgetMask]
      
      
      crpercent = 100 * (crlines / totCrLines)
      stCrLines[1, BINDEX] = mean(crpercent)
      stCrLines[2, BINDEX] = min(crpercent)
      stCrLines[3, BINDEX] = median(crpercent)
      stCrLines[4, BINDEX] = max(crpercent)
        
      cat(" & ")
      
      mean_crpercent = mean(crpercent)
      cat(linecoverage_cellcolor(mean_crpercent))
      plclIndex <- linecoverage_index(mean_crpercent)
      avgCrLinesByType[row, plclIndex]  <- avgCrLinesByType[row, plclIndex] + 1
      
      if(st_metrics[1])
        cat(formatC(mean_crpercent, digits = 1, format = "f"),"", sep = "")
      
      if(st_metrics[2])
        cat(" [", formatC(min(crpercent), digits = 1, format = "f"),", ", sep = "")
      if(st_metrics[3])
        cat(formatC(median(crpercent), digits = 1, format = "f"),", ", sep = "")
      if(st_metrics[4])
        cat(formatC(max(crpercent), digits = 1, format = "f"),"]", sep = "")
      if(st_metrics[5])
        cat(" (",formatC(sd(crpercent), digits = 1, format = "f"),")", sep = "")
      
      
      faults = dt$potentialFaults[budgetMask]
      stFaults[1, BINDEX] = mean(faults)
      stFaults[2, BINDEX] = min(faults)
      stFaults[3, BINDEX] = median(faults)
      stFaults[4, BINDEX] = max(faults)
      
      cat(" & ")
      if(st_metrics[1])
        cat(formatC(mean(faults), digits = 1, format = "f"), sep = "")
      
      if(st_metrics[2])
        cat(" [", min(faults),", ", sep = "")
      if(st_metrics[3])
        cat(median(faults),", ", sep = "")
      if(st_metrics[4])
        cat(max(faults),"]", sep = "")
      if(st_metrics[5])
        cat(" (",formatC(sd(faults), digits = 1, format = "f"),")", sep = "")
      
      cat(" \\\\ \n")
    }
  }
  
  colCount <- 1
  
  if(showTB)
    colCount <- colCount + 1
  
  if(showLabel)
    colCount <- colCount + 1
  
  cat("\\midrule \n")
  cat("\\multicolumn{",colCount,"} {l} {Mean of Mean} &", formatFloat(mean(stLines[1,])), "&", formatFloat(mean(stCrLines[1,])), "&", formatFloat(mean(stFaults[1,])), "\\\\ \n", sep = "")
  cat("\\multicolumn{",colCount,"} {l} {$\\sigma$ of Mean} &", formatFloat(sd(stLines[1,])), "&", formatFloat(sd(stCrLines[1,])), "&", formatFloat(sd(stFaults[1,])), "\\\\ \n", sep = "")
  
  if(showDistribution){
    stLinesMean <- stLines[1,]
    stCrLinesMean <- stCrLines[1,]
    cat("\\multicolumn{",colCount,"} {l} {Mean $\\in$ (0, 5\\%]}", 
        " & ", linecoverage_cellcolor(3), length(stLinesMean[stLinesMean <= 5]), 
        " & ", linecoverage_cellcolor(3), length(stCrLinesMean[stCrLinesMean <= 5]), " & \\\\ \n", sep = "")
    
    cat("\\multicolumn{",colCount,"} {l} {Mean $\\in$ (5, 10\\%]}", 
        " & ", linecoverage_cellcolor(8), length(stLinesMean[stLinesMean < 10 & stLinesMean > 5]), 
        " & ", linecoverage_cellcolor(8), length(stCrLinesMean[stCrLinesMean < 10 & stCrLinesMean < 5]), " & \\\\ \n", sep = "")
    
    cat("\\multicolumn{",colCount,"} {l} {Mean $\\in$ (10, 20\\%]}", 
        " & ", linecoverage_cellcolor(15), length(stLinesMean[stLinesMean < 20 & stLinesMean > 10]), 
        " & ", linecoverage_cellcolor(15), length(stCrLinesMean[stCrLinesMean < 20 & stCrLinesMean > 10]), " & \\\\ \n", sep = "")
    
    cat("\\multicolumn{",colCount,"} {l} {Mean $\\in$ (20, 100\\%]}", 
        " & ", linecoverage_cellcolor(25), length(stLinesMean[stLinesMean > 20]), 
        " & ", linecoverage_cellcolor(25), length(stCrLinesMean[stCrLinesMean > 20]), " & \\\\ \n", sep = "")
    
  }
  

  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")

  sink()
  
  TYPETABLE = paste(GENERATED_FILES, "/coverage_st_byType_",DATA_VERSION, "_" ,maskId,".tex", sep = "")
  unlink(TYPETABLE)
  sink(TYPETABLE, append = TRUE, split = TRUE)

  cat("\\begin{tabular}{ ll rr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("Type & & Line\\%  &Critical Line\\%  \\\\ \n", sep ="")
  

  cat("\\midrule \n")
  summarizeStatistics(labels, labels_indexes ,stLines, stCrLines, NULL, 1, length(labels), 1, 1, showTB, showLabel, TRUE)

  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")

  sink()
  
}

summarizeStatistics <- function(labels, labels_indexes ,stLines, stCrLines, stFaults, startLabel, endLabel, startK=1, endK=nrow(stLines), showTB, showLabel, highlight = FALSE, showMetric = FALSE){
  
  st_count <- 5 # Mean, Min, Median, Max, sd
  summaryLine <- matrix(0, nrow = endLabel, ncol = endK * st_count)
  summaryCrLine <- matrix(0, nrow = endLabel, ncol = endK * st_count)
  summaryFault <- matrix(0, nrow = endLabel, ncol = endK * st_count)
  highlight <- highlight && (endLabel <=length(labels)) 
  
  if(highlight){
    for(t in startLabel:endLabel){
      
      stdt <- stLines
      stCrdt <-stCrLines
      stFaultsdt <- stFaults
      if(t <= length(labels)){
        indexes <- which(labels_indexes == labels[t])
        stdt <- stLines[,indexes]
        stCrdt <- stCrLines[,indexes]
        if(!is.null(stFaults))
          stFaultsdt <- stFaults[,indexes]
      }
      
      for(k in startK:endK){
        summaryLine[t, (k-1)*st_count + 1] = mean(stdt[k,])
        summaryLine[t, (k-1)*st_count + 2] = min(stdt[k,])
        summaryLine[t, (k-1)*st_count + 3] = median(stdt[k,])
        summaryLine[t, (k-1)*st_count + 4] = max(stdt[k,])
        summaryLine[t, (k-1)*st_count + 5] = sd(stdt[k,])
        
        summaryCrLine[t, (k-1)*st_count + 1] = mean(stCrdt[k,])
        summaryCrLine[t, (k-1)*st_count + 2] = min(stCrdt[k,])
        summaryCrLine[t, (k-1)*st_count + 3] = median(stCrdt[k,])
        summaryCrLine[t, (k-1)*st_count + 4] = max(stCrdt[k,])
        summaryCrLine[t, (k-1)*st_count + 5] = sd(stCrdt[k,])
        
        if(!is.null(stFaults)){
          summaryFault[t, (k-1)*st_count + 1] = mean(stFaultsdt[k,])
          summaryFault[t, (k-1)*st_count + 2] = min(stFaultsdt[k,])
          summaryFault[t, (k-1)*st_count + 3] = median(stFaultsdt[k,])
          summaryFault[t, (k-1)*st_count + 4] = max(stFaultsdt[k,])
          summaryFault[t, (k-1)*st_count + 5] = sd(stFaultsdt[k,])
        }
        
      }
    }
  }
  
  
  for (c in 1:ncol(summaryLine)) {
    summaryLine[,c] = rank(-summaryLine[,c])
    summaryCrLine[,c] = rank(-summaryCrLine[,c])
    summaryFault[,c] = rank(-summaryFault[,c])
  }
      
  for(t in startLabel:endLabel){
    
    if(startK < endK)
      cat("\\midrule \n")
    
    labelName <- "All"
    stdt <- stLines
    stCrdt <-stCrLines
    stFaultsdt <- stFaults
    if(t <= length(labels)){
      labelName <- labels[t]
      indexes <- which(labels_indexes == labels[t])
      stdt <- stLines[,indexes]
      stCrdt <- stCrLines[,indexes]
      if(!is.null(stFaults))
        stFaultsdt <- stFaults[,indexes]
    }
    
    for(k in startK:endK){
      
      allName <- labelName
      
      
      if(k == 1)
        stName <- "Mean"
      else if (k == 2)
        stName <- "Minimum"
      else if (k == 3)
        stName <- "Median"
      else if (k == 4)
        stName <- "Maximum"
      if(showMetric)
        allName <- paste(allName, " (", stName ,")", sep = "")
      
      colCount <- 1
      
      if(showTB)
        colCount <- colCount + 1
      
      if(showLabel)
        colCount <- colCount + 1
      
      cat("\\multicolumn{",colCount,"} {l} {",allName,"} &", sep = "")
      
      
      cat(boldValue(formatC(mean(stdt[k,]), digits = 1, format = "f"), highlight && summaryLine[t, (k-1)*st_count + 1] == 1)," ", sep = "")
      cat(" [")
      cat(boldValue(formatC(min(stdt[k,]), digits = 1, format = "f"), highlight &&summaryLine[t, (k-1)*st_count + 2] == 1),", ", sep = "")
      cat(boldValue(formatC(median(stdt[k,]), digits = 1, format = "f"), highlight &&summaryLine[t, (k-1)*st_count + 3] == 1),", ", sep = "")
      cat(boldValue(formatC(max(stdt[k,]), digits = 1, format = "f"), highlight &&summaryLine[t, (k-1)*st_count + 4] == 1),"]", sep = "")
      cat(" (", boldValue(formatC(sd(stdt[k,]), digits = 1, format = "f"), FALSE),")", sep = "")
      
      cat(" & ")
      cat(boldValue(formatC(mean(stCrdt[k,]), digits = 1, format = "f"), highlight && summaryCrLine[t, (k-1)*st_count + 1] == 1)," ", sep = "")
      cat(" [")
      cat(boldValue(formatC(min(stCrdt[k,]), digits = 1, format = "f"), highlight && summaryCrLine[t, (k-1)*st_count + 2] == 1),", ", sep = "")
      cat(boldValue(formatC(median(stCrdt[k,]), digits = 1, format = "f"), highlight && summaryCrLine[t, (k-1)*st_count + 3] == 1),", ", sep = "")
      cat(boldValue(formatC(max(stCrdt[k,]), digits = 1, format = "f"), highlight && summaryCrLine[t, (k-1)*st_count + 4] == 1),"]", sep = "")
      cat(" (", boldValue(formatC(sd(stCrdt[k,]), digits = 1, format = "f"), FALSE),")", sep = "")
      
      
      if(!is.null(stFaults)){
        cat(" & ")
        cat(boldValue(formatC(mean(stFaultsdt[k,]), digits = 1, format = "f"), highlight && summaryFault[t, (k-1)*st_count + 1] == 1)," ", sep = "")
        cat(" [")
        cat(boldValue(formatC(min(stFaultsdt[k,]), digits = 1, format = "f"), highlight && summaryFault[t, (k-1)*st_count + 2] == 1),", ", sep = "")
        cat(boldValue(formatC(median(stFaultsdt[k,]), digits = 1, format = "f"), highlight && summaryFault[t, (k-1)*st_count + 3] == 1),", ", sep = "")
        cat(boldValue(formatC(max(stFaultsdt[k,]), digits = 1, format = "f"), highlight && summaryFault[t, (k-1)*st_count + 4] == 1),"]", sep = "")
        cat(" (", boldValue(formatC(sd(stFaultsdt[k,]), digits = 1, format = "f"), FALSE),")", sep = "")
      }
      
      
      cat(" \\\\ \n")
    }
  }
  
}


spearmanWithSutStatistics <- function(dt, maskId){
  
  SPEARMANTABLE = paste(GENERATED_FILES, "/spearman_",DATA_VERSION, "_" ,maskId,".tex", sep = "")
  unlink(SPEARMANTABLE)
  sink(SPEARMANTABLE, append = TRUE, split = TRUE)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  projects <- sort(unique(dt$id))
  budgets <- sort(unique(dt$maxTime))
  categories <- sort(unique(sutdt$Category))
  
  results_index = 2
  properties <- c("\\#Files", "LOC", "LOC\\textsubscript{\\emph{b}}", "\\#Interfaces", "\\#Functions", "\\#Dependent Services", "\\#Tables", "\\#Rows")
  
  
  results_colnames <- c("LineCoverage","CrLineCoverage",properties, "Category")
  results <- matrix(0, nrow = nrow(dt), ncol = length(results_colnames))
  category_index = length(results_colnames) # last column
  
  
  
  rowindex <- 1
  for (i in 1:length(projects)) {
    proj = projects[[i]]
    projectMask =  dt$id == proj
    
    totLines = getTotalLines(proj)
    totCrLines = getTotalCRLines(proj)
    
    
    sutdtIdMastk = sutdt$Id == proj
    numFiles = sutdt$Files[sutdtIdMastk]
    numLines = sutdt$Lines[sutdtIdMastk]
    numInterfaces = sutdt$Interfaces[sutdtIdMastk]
    numFunctions = sutdt$Functions[sutdtIdMastk]
    numDependency = sutdt$Down[sutdtIdMastk]
    numTables = sutdt$Tables[sutdtIdMastk]
    numRows = sutdt$Rows[sutdtIdMastk]
    category = match(sutdt$Category[sutdtIdMastk], categories)
    
    for (j in 1:length(budgets)) {
      budget = budgets[[j]]
      
      budgetMask = projectMask & dt$maxTime == budget
      
      lines = dt$coveredLines[budgetMask]
      linecov = 100 * (lines / totLines)
      
      crlines = dt$lineAndcovered[budgetMask]
      crlinecov = 100 * (crlines / totCrLines)
      
      n <- length(lines)
      
      results[rowindex:(rowindex+n-1),1] = linecov
      results[rowindex:(rowindex+n-1),2] = crlinecov
      results[rowindex:(rowindex+n-1),3] = rep(numFiles, n)
      results[rowindex:(rowindex+n-1),4] = rep(numLines, n)
      results[rowindex:(rowindex+n-1),5] = rep(totLines, n)
      #results[rowindex:(rowindex+n-1),6] = rep(totCrLines, n) 
      results[rowindex:(rowindex+n-1),6] = rep(numInterfaces, n)
      results[rowindex:(rowindex+n-1),7] = rep(numFunctions, n)
      results[rowindex:(rowindex+n-1),8] = rep(numDependency, n)
      results[rowindex:(rowindex+n-1),9] = rep(numTables, n)
      results[rowindex:(rowindex+n-1),10] = rep(numRows, n)
      results[rowindex:(rowindex+n-1),category_index] = rep(category, n)
      
      rowindex <- rowindex + n
    }
  }
  
  cat("\\begin{tabular}{ l rrrr c}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("Size & \\multicolumn{2}{c}{Line\\%} & \\multicolumn{2}{c}{Critical Line\\%}  & $R_{mean}=$\\\\ \n", sep ="")
  cat("Properties & $\\rho$ ($R_l$) &  $p$-value  & $\\rho$ ($R_{cl}$) & $p$-value  & $\\frac{R_l + R_{cl}}{2}$\\\\ \n", sep ="")
  cat("\\midrule \n")
  
  values <- matrix(0, nrow = length(properties), ncol = results_index * 2)
  for (j in 1:length(properties)) {
    for (i in 1:results_index) {
      # if(i ==  1)
      #   row_content <- paste(row_content, properties[j], " & ", sep = "")
      # else
      #   row_content <- paste(row_content, " & ", sep = "")
      
      spearst <- cor.test(results[,i], results[,j+results_index],  method = "spearman")
      values[j,i*2-1]= spearst$estimate
      values[j,i*2] = spearst$p.value
      #row_content <- paste()
      
    }
    #cat("\\\\ \n")
    #rows <- append(rows, row_content)
  }
  
  
  ranks <- matrix(0, nrow = length(properties), ncol = results_index)
  for(i in 1:results_index){
    ranks[,i] = rank(-abs(values[,i*2-1]))
  }
  
  for(j in 1:nrow(values)){
    cat(properties[j], " & ", sep = "")
    for (i in 1:results_index) {
      cat(boldValue(paste(formatFloat(values[j,i*2-1], 3),"(",ranks[j,i],")", sep = ""), values[j,i*2] < 0.05), " & ", boldValue(formatPvalue(values[j,i*2]), values[j,i*2] < 0.05), " & ", sep="")
    }
    cat(formatFloat(mean(ranks[j,]), 1), "\\\\ \n", sep = "")
  }
  
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
  colnames(results) <- results_colnames
  data_results <- as.data.frame(results)
  data_results$Category <- as.factor(data_results$Category)
  
  
  ### vioplot
  ## http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
  
  lcvp <- ggplot(data_results, aes(x=Category, y=LineCoverage)) + 
    geom_violin(fill="gray") + 
    geom_boxplot(width=0.1) + 
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    scale_x_discrete(labels = categories) +
    labs(y = "%Line") +
    theme_classic(base_size = 25)
  
  pdf(paste(GENERATED_FILES, "/vioplot_linecoverage_",maskId,".pdf", sep = ""))
  print(lcvp)
  dev.off()
  
  crlcvp <- ggplot(data_results, aes(x=Category, y=CrLineCoverage)) + 
    geom_violin(fill="gray") + 
    geom_boxplot(width=0.1)  + 
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    scale_x_discrete(labels = categories) +
    labs(y = "%Critical Line") +
    theme_classic(base_size = 25)
  
  pdf(paste(GENERATED_FILES, "/vioplot_crlincoverage_",maskId,".pdf", sep = ""))
  print(crlcvp)
  dev.off()
  
  
}

coverageTableShowingSeededLine <- function(dt, maskId, anonymized, evodt, showInvalid) {
  
  
  TABLE = paste(GENERATED_FILES, "/coverage_manual_vs_evo",DATA_VERSION, "_" ,maskId,".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  cat("\\begin{tabular}{ ll rrr r rrr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("", "& &\\multicolumn{2}{c}{EETests} &   \\multicolumn{4}{c}{Base vs. EETests}   \\\\ \n", sep ="")
  cat("SUT & Type &\\#Tests & Critical  & Relative\\% & \\multicolumn{3}{c}{Ratio of lines covered by} \\\\ \n")
  cat("& &  &Line\\%  &  & EETests & Both  & Base  \\\\ \n", sep ="")
  cat("\\midrule \n")
  
  existingprojects = sort(unique(dt$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  
  nprojects = length(projects)
  
  budgets = tbFilter(sort(unique(dt$maxTime)))
  nbudgets = length(budgets)
  
  avgSeededLines = rep(0, nprojects * nbudgets)
  avgLines = rep(0, nprojects * nbudgets)
  avgCrLines = rep(0, nprojects * nbudgets)
  
  avgCrLinesByEvo = rep(0, nprojects * nbudgets)
  
  avgBranches = rep(0, nprojects * nbudgets)
  avgFaults = rep(0, nprojects * nbudgets)
  
  onlyA <- rep(0, nprojects * nbudgets)
  both <- rep(0, nprojects * nbudgets)
  onlyB <- rep(0, nprojects * nbudgets)
  
  sumtests <- 0
  
  HighestLevel <- 50
  MedianLevel <- 30
  LowLevel <- 10
  
  for (i in 1:length(projects)) {
    proj = projects[[i]]
    
    projectMask =  dt$id == proj
    evo_projectMask = evodt$id == proj
    
    totLines = getTotalLines(proj)
    totBranches = max(dt$numberOfBranches[projectMask])
    
    totCrLines = getTotalCRLines(proj)
    
    
    for (j in 1:nbudgets) {
      
      BINDEX = (i-1)*nbudgets + j
      
      budget = budgets[[j]]
      
      if(j == 1){
        cat(formatSUT(proj, anonymized))
        
        sutprojectMask = sutdt$Id == proj
        label <- sutdt$Category[sutprojectMask]
        cat(" &", label, sep = "")
      }
      
      
      
      budgetMask = projectMask & dt$maxTime == budget
      evo_budgetMask = evo_projectMask & evodt$maxTime == budget
      
      numtests = unique(dt$numberOfRPCSeededTests[budgetMask])
      if(length(numtests)){
        cat(" & ", numtests, sep = "")
        sumtests <- sumtests + numtests 
      }else{
        stop(paste("mismatched number of seeded tests ",proj, sep = ""))
      }
      
      seeddlines = dt$seedingTimeCoveredLines[budgetMask]
      percent = 100 * (seeddlines / totLines)
      avgSeededLines[[BINDEX]] = mean(percent)
      #cat(" & ")
      #cat(formatC(mean(percent), digits = 1, format = "f"),"", sep = "")
      
      
      
      crlines = dt$lineAndcovered[budgetMask]
      percent = 100 * (crlines / totCrLines)
      avgCrLines[[BINDEX]] = mean(percent)
      cat(" & ")
      cat(formatC(mean(percent), digits = 1, format = "f"),"", sep = "")

      
      crlinesByEvo = evodt$lineAndcovered[evo_budgetMask]
      percentByEvo = 100 * (crlinesByEvo / totCrLines)
      additionalcrlines = (mean(percentByEvo) - mean(percent))/mean(percent) * 100
      avgCrLinesByEvo[[BINDEX]] = additionalcrlines
      prefixadditional <- ""
      if(additionalcrlines > 0){
        prefixadditional <- "+"
      }
      cat(" & ")
      cat(prefixadditional,formatC(additionalcrlines, digits = 1, format = "f"),"", sep = "")
      
      
      
      # test during seeding pattern
      patternA <- "coverageInfo_seeding_"
      seedsA <- dt$seed[budgetMask]

      patternB <- "coverageInfo_"
      seedsB <- evodt$seed[evo_budgetMask]
      
      compareresults <- compareCoveredLines(patternA, seedsA, crlines, patternB, seedsB,crlinesByEvo, proj, showInvalid = showInvalid)
      
      
      for (ri in 1:length(compareresults)) {
        color_sign <- NULL
        level <- NULL
        r <- compareresults[ri]
        if(ri == 1){
          onlyA[[BINDEX]] = as.numeric(r) 
          if(as.numeric(r) > as.numeric(compareresults[3])){
            color_sign <- FALSE
          }
        }
        if(ri == 2){
          both[[BINDEX]] = as.numeric(r)
        }
        if(ri == 3){
          onlyB[[BINDEX]] = as.numeric(r)
          if(as.numeric(r) > as.numeric(compareresults[1]))
            color_sign <- TRUE
        }
        
        if(!is.null(color_sign)){
          if(as.numeric(r) > 50)
            level <- HighestLevel
          else if(as.numeric(r) > 33.3)
            level <- MedianLevel
          else
            level <- LowLevel
        }
        
        cat(" & ", boolean_cellcolor(color_sign, level),r , "\\%", sep = "")
      }
      
      
      cat(" \\\\ \n")
      
    }
    
    
  }
  
  cat("\\midrule \n")
  
  cat("Sum & &", sumtests, "& & & & & \\\\ \n", sep = "")
  cat("Mean & &")
  
  # cat(" & ")
  # cat(formatC(mean(avgSeededLines), digits = 1, format = "f"),"", sep = "")
  cat(" & ")
  cat(formatC(mean(avgCrLines), digits = 1, format = "f"),"", sep = "")

  cat(" & ")
  meanadditional <- mean(avgCrLinesByEvo)
  prefixadditinoal <- ""
  if(meanadditional > 0)
    prefixadditinoal <- "+"
  cat(prefixadditinoal, formatC(meanadditional, digits = 1, format = "f"),"", sep = "")
  
  cat(" & ")
  cat(formatC(mean(onlyA), digits = 1, format = "f"),"\\%", sep = "")
  
  cat(" & ")
  cat(formatC(mean(both), digits = 1, format = "f"),"\\%", sep = "")
  
  cat(" & ")
  cat(formatC(mean(onlyB), digits = 1, format = "f"),"\\%", sep = "")
  
  cat(" \\\\ \n")
  
  cat("\\midrule \n")
  cat("\\multicolumn{4}{r}{\\# Relative > 0}",
      " & ", length(avgCrLinesByEvo[avgCrLinesByEvo > 0]) , " &  &  &  \\\\ \n", sep = "")
  
  
  cat("\\midrule \n")
  
  cat("\\multicolumn{4}{r}{Analysis with Ratio}",
      " & Total & (0, 33.3\\%] & (33.3\\%, 50\\%] & (50, 100\\%] \\\\ \n", sep = "")
  
  cat("\\multicolumn{4}{r}{\\# Ratio\\textsubscript{EETests} $>$ Ratio\\textsubscript{Base}}",
      " & ", length(onlyA[onlyA > onlyB]), 
      " & ", boolean_cellcolor(FALSE, LowLevel),length(onlyA[onlyA > onlyB & onlyA <= 33.3]), 
      " & ",boolean_cellcolor(FALSE, MedianLevel),length(onlyA[onlyA > onlyB & onlyA > 33.3 & onlyA <= 50]),
      " & ",boolean_cellcolor(FALSE, HighestLevel),length(onlyA[onlyA > onlyB & onlyA > 50]),
      "\\\\ \n", sep = "")
  
  cat("\\multicolumn{4}{r}{\\# Ratio\\textsubscript{Base} $>$ Ratio\\textsubscript{EETests}}",
      " & ",  length(onlyB[onlyB > onlyA]), 
      " & ",boolean_cellcolor(TRUE, LowLevel), length(onlyB[onlyB > onlyA & onlyB <= 33.3]), 
      " & ",boolean_cellcolor(TRUE, MedianLevel),length(onlyB[onlyB > onlyA & onlyB > 33.3 & onlyB <= 50]),
      " & ",boolean_cellcolor(TRUE, HighestLevel),length(onlyB[onlyB > onlyA & onlyB > 50]),
      "\\\\ \n", sep = "")
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
}


# compare dta vs dtb using U test and A12
compareMetrics <- function(dta, maskIda, dtb, maskIdb, anonymized){
  
  TABLE = paste(GENERATED_FILES, "/coverage_",DATA_VERSION, "_" ,maskIda, "_vs_",maskIdb,".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  cat("\\begin{tabular}{ lrr rr rr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT & \\multicolumn{2}{c}{Line\\%}  & \\multicolumn{2}{c}{Critical Line\\%}  & \\multicolumn{2}{c}{\\#Detected Faults} \\\\ \n", sep ="")
  cat(" & $\\hat{A}_{12}$  & \\emph{p}-value  & $\\hat{A}_{12}$  & \\emph{p}-value & $\\hat{A}_{12}$  & \\emph{p}-value \\\\ \n", sep ="")
  cat("\\midrule \n")
  
  existingprojects = sort(unique(dta$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  projectsByCategory <- orderSUT(ByCategory = FALSE)
  projects <- projects[order(match(projects, projectsByCategory))]
  
  nprojects = length(projects)
  
  for (i in 1:length(projects)) {
    proj = projects[[i]]
    aprojectMask =  dta$id == proj
    bprojectMask =  dtb$id == proj
    cat(formatSUT(proj, anonymized))
    
    
    alines <- dta$coveredLines[aprojectMask]
    acrlines <- dta$lineAndcovered[aprojectMask]
    afaults <- dta$potentialFaults[aprojectMask]
    
    
    blines <- dtb$coveredLines[bprojectMask]
    bcrlines <- dtb$lineAndcovered[bprojectMask]
    bfaults <- dtb$potentialFaults[bprojectMask]
    
    
    lines_a12 = measureA(alines, blines)
    lines_w = wilcox.test(alines, blines)
    lines_p = lines_w$p.value
    
    cat(" & ", boldValue(formatFloat(lines_a12, 2), lines_p < 0.05, lines_a12 > 0.5), 
        "&", boldValue(formatPvalue(lines_p), lines_p < 0.05, lines_a12 > 0.5), sep = "")
    
    
    crlines_a12 = measureA(acrlines, bcrlines)
    crlines_w = wilcox.test(acrlines, bcrlines)
    crlines_p = crlines_w$p.value
    
    cat(" & ", boldValue(formatFloat(crlines_a12, 2), crlines_p < 0.05, crlines_a12 > 0.5), 
        "&", boldValue(formatPvalue(crlines_p), crlines_p < 0.05, crlines_a12 > 0.5), sep = "")
    
    faults_a12 = measureA(afaults, bfaults)
    faults_w = wilcox.test(afaults, bfaults)
    faults_p = faults_w$p.value
    
    
    
    cat(" & ", boldValue(formatFloat(faults_a12, 2), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), 
        "&", boldValue(formatPvalue(faults_p), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), sep = "")
    
    cat(" \\\\ \n")
     
  }
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
}

compareMetricsShowRelativeAndA12WithAMean <- function(dta, maskIda, maskIdaName1st, maskIdaName2nd, dtb, maskIdb, anonymized, compareIdName, 
                                                      reverseCompare = FALSE, 
                                                      showAdditionalSummary = FALSE, showComparsionColumn = FALSE, showType = FALSE, showSeedingBudget = FALSE){
  
  TABLE = paste(GENERATED_FILES, "/coverage_",DATA_VERSION, "_" ,maskIda, "_vs_",maskIdb,"_with_amean_using_relative_a12.tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  col_num <- 1
  col_type <- ""
  col_budget <- ""
  if(showType){
    col_num <- col_num + 1
    col_type <- "l"
  }
    
  if(showSeedingBudget){
    col_num <- col_num + 1
    col_budget <- "l"
  }
    
  
  cat("\\begin{tabular}{ l ",col_type,col_budget,"  rr  rr  rr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT & ")
  if(showType)
    cat("Type &")
  if(showSeedingBudget)
    cat("Budgets &")
  
  cat(" \\multicolumn{2}{c}{Line\\%}  & \\multicolumn{2}{c}{Critical Line\\%}  & \\multicolumn{2}{c}{\\#Detected Faults} \\\\ \n", sep ="")
  if(showComparsionColumn){
    cat(" & ")
    if(showType)
      cat(" & ")
    if(showSeedingBudget)
      cat("by seeds &")
    cat("\\multicolumn{1}{r}{",maskIdaName1st,"}  & \\multicolumn{2}{c}{",compareIdName,"}"," & \\multicolumn{1}{r}{",maskIdaName1st,"} & \\multicolumn{2}{c}{",compareIdName,"}"," & ",maskIdaName1st," & \\multicolumn{2}{c}{",compareIdName,"}"," \\\\ \n", sep ="")
  }
  cat(" & ")
  if(showType)
    cat(" & ")
  if(!showComparsionColumn && showSeedingBudget)
    cat("by seeds &")
  cat("\\multicolumn{1}{r}{",maskIdaName2nd,"}  & Relative\\%($\\hat{A}_{12}$) "," & \\multicolumn{1}{r}{",maskIdaName2nd,"} & Relative\\%($\\hat{A}_{12}$) "," & ",maskIdaName2nd," & Relative\\%($\\hat{A}_{12}$)  \\\\ \n", sep ="")
  cat("\\midrule \n")
  
  existingprojects = sort(unique(dta$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  projectsByCategory <- orderSUT(ByCategory = FALSE)
  projects <- projects[order(match(projects, projectsByCategory))]
  
  nprojects = length(projects)
  
  all <- matrix(0, nrow = nprojects, ncol = 4 * 3)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  for (i in 1:nprojects) {
    proj = projects[[i]]
    aprojectMask =  dta$id == proj
    bprojectMask =  dtb$id == proj
    
    
    totLines = getTotalLines(proj)
    totCrLines = getTotalCRLines(proj)
    
    cat(formatSUT(proj, anonymized))
    
    sutdt_label = sutdt$Id == proj
    label <- sutdt$Category[sutdt_label]
    
    if(showType)
      cat(" &", label, sep = "")
    
    
    if(showSeedingBudget){
      seedingDone = aprojectMask & dta$notExecutedSeededTests == 0
      aNumSeeds = dta$numberOfRPCSeededTests[seedingDone]
      aNumActions = dta$evaluatedActions[seedingDone]
      seedingNotDone = aprojectMask & dta$notExecutedSeededTests > 0
      aNotDoneActions = dta$evaluatedActions[seedingNotDone]
      aSeedsRatio = c(aNumSeeds / aNumActions, aNotDoneActions/aNotDoneActions)
      cat(" & ", formatFloat(mean(aSeedsRatio) * 100), "\\%", sep = "")
    }
    
    alines <- dta$coveredLines[aprojectMask]
    acrlines <- dta$lineAndcovered[aprojectMask]
    afaults <- dta$potentialFaults[aprojectMask]
    
    
    blines <- dtb$coveredLines[bprojectMask]
    bcrlines <- dtb$lineAndcovered[bprojectMask]
    bfaults <- dtb$potentialFaults[bprojectMask]
    
    
    lines_a12 = measureA(alines, blines)
    if(reverseCompare)
      lines_a12 = 1.0 - lines_a12
    lines_w = wilcox.test(alines, blines)
    lines_p = lines_w$p.value
    
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(alines)/totLines * 100),formatFloat(mean(alines)/totLines * 100), "}",sep = "")
    #cat(" & ", linecoverage_cellcolor(mean(alines)/totLines * 100),formatFloat(mean(alines)/totLines * 100), sep ="")
    relativelines = (mean(alines)-mean(blines))/mean(blines) * 100
    if(reverseCompare)
      relativelines = (mean(blines)-mean(alines))/mean(alines) * 100
    sign <- "+"
    if(relativelines < 0)
      sign <- ""
    cat(" & \\databar{", formatFloat(relativelines), "}{", sign ,boldValue(formatFloat(relativelines), lines_p < 0.05, lines_a12 > 0.5), sep = "")
    cat(" ( ", boldValue(formatFloat(lines_a12, 2), lines_p < 0.05, lines_a12 > 0.5), ")","}", sep ="")
    #cat("&", boldValue(formatPvalue(lines_p), lines_p < 0.05, lines_a12 > 0.5), sep = "")
    all[i, 1] = mean(alines)/totLines * 100 
    all[i, 2] = relativelines
    all[i, 3] = lines_a12
    all[i, 4] = lines_p
    
    crlines_a12 = measureA(acrlines, bcrlines)
    if(reverseCompare)
      crlines_a12 = 1.0 - crlines_a12
    crlines_w = wilcox.test(acrlines, bcrlines)
    crlines_p = crlines_w$p.value
    
    
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(acrlines)/totCrLines * 100),formatFloat(mean(acrlines)/totCrLines * 100), "}",sep = "")
    relativecrlines = (mean(acrlines)-mean(bcrlines))/mean(bcrlines) * 100
    if(reverseCompare)
      relativecrlines = (mean(bcrlines)-mean(acrlines))/mean(acrlines) * 100
    sign <- "+"
    if(relativecrlines < 0)
      sign <- ""
    cat(" & \\databar{", formatFloat(relativecrlines), "}{",sign, boldValue(formatFloat(relativecrlines), crlines_p < 0.05, crlines_a12 > 0.5),  sep = "")
    cat(" (", boldValue(formatFloat(crlines_a12, 2), crlines_p < 0.05, crlines_a12 > 0.5), ")","}",sep = "")
    all[i, 5] = mean(acrlines)/totCrLines * 100
    all[i, 6] = relativecrlines
    all[i, 7] = crlines_a12
    all[i, 8] = crlines_p
    
    faults_a12 = measureA(afaults, bfaults)
    if(reverseCompare)
      faults_a12 = 1 - faults_a12
    faults_w = wilcox.test(afaults, bfaults)
    faults_p = faults_w$p.value
    
    cat(" &", formatFloat(mean(afaults)), sep = "")
    relativefaults = (mean(afaults)-mean(bfaults))/mean(bfaults) * 100
    if(reverseCompare)
      relativefaults = (mean(bfaults)-mean(afaults))/mean(afaults) * 100
    sign <- "+"
    if(relativefaults < 0)
      sign <- ""
    cat(" & \\databar{", formatFloat(relativefaults), "}{",sign, boldValue(formatFloat(relativefaults), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), sep = "")
    cat(" (", boldValue(formatFloat(faults_a12, 2), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5),")", "}", sep = "")
    all[i, 9] = mean(afaults)
    all[i, 10] = relativefaults
    all[i, 11] = faults_a12
    all[i, 12] = faults_p
    
    cat(" \\\\ \n")
    
  }
  
  cat("\\midrule \n")
  
  cat("Mean")
  
  if(showSeedingBudget)
    cat("&")
  
  for (j in 1:ncol(all)) {
    if(j %% 4 != 0){
      sign <- ""
      if((j %% 4 == 2) && mean(all[,j]) > 0)
        sign <- "+"
      
      if(j %% 4 == 3){
        cat(" (", sign, formatFloat(mean(all[,j])), ")",sep = "")
      }
      else{
        if(j == 5 || j == 1){
          cat(" & \\multicolumn{1}{r}{", sign, formatFloat(mean(all[,j])), "}",sep = "")
        }else{
          cat(" & ", sign, formatFloat(mean(all[,j])), sep = "")
        }
      }
    }
  }
  
  cat(" \\\\ \n")
  
  if(showAdditionalSummary){
    cat("\\midrule \n")
    relativeline <- all[,2]
    a12line <- all[,3]
    pline <- all[,4]
    
    relativeCrline <- all[,6]
    a12Crline <- all[,7]
    pCrline <- all[,8]
    
    relativefault <- all[,10]
    a12fault <- all[,11]
    pfault <- all[,12]
    
    
    cat("\\multicolumn{",col_num+1,"}{l}{\\#Relative $> 0$}",
        " & ", length(relativeline[relativeline > 0]),"\\phantom{\\textbf{1.00}}",
        #" & ",
        " & ",
        " & ", length(relativeCrline[relativeCrline > 0]),"\\phantom{\\textbf{1.00}}",
        # " & ",
        " & ",
        " & ", length(relativefault[relativefault > 0]),"\\phantom{\\textbf{1.00}}",
        #" & ",
        " \\\\ \n",sep = "")
    
    cat("\\multicolumn{",col_num+1,"}{l}{\\#\\emph{Seeded} $\\succ$ \\emph{Base}}",
        # " & ",
        " & ", length(a12line[a12line > 0.5 & pline < 0.05]) ,
        " & ",
        #" & ",
        " & ", length(a12Crline[a12Crline > 0.5 & pCrline < 0.05]) ,
        " & ",
        #" & ",
        " & ", length(a12fault[a12fault > 0.5 & pfault < 0.05]) ,
        " \\\\ \n",sep = "")
    
    cat("\\multicolumn{",col_num+1,"}{l}{\\#\\emph{Base} $\\succ$ \\emph{Seeded}}",
        #" & ",
        " & ", length(a12line[a12line < 0.5 & pline < 0.05]) ,
        " & ",
        #" & ",
        " & ", length(a12Crline[a12Crline < 0.5 & pCrline < 0.05]) ,
        " & ",
        #" & ",
        " & ", length(a12fault[a12fault < 0.5 & pfault < 0.05]) ,
        " \\\\ \n",sep = "")
    
    
  }
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
}



compareAvsBCMetricsShowRelativeAndA12WithAMean <- function(dta, maskIda, maskIdaName1st, maskIdaName2nd, 
                                                           dtb, maskIdb, 
                                                           dtc, maskIdc, 
                                                           anonymized, 
                                                           compareAvsBIdName,
                                                           compareAvsCIdName,
                                                           showAdditionalSummary = FALSE, showComparsionColumn = FALSE, showType = FALSE, showSeedingBudget = FALSE){
  
  TABLE = paste(GENERATED_FILES, "/coverage_",DATA_VERSION, "_" ,maskIda, "_vs_",maskIdb,"_vs_",maskIdc,"_with_amean_using_relative_a12.tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  col_num <- 1
  col_type <- ""
  col_budget <- ""
  if(showType){
    col_num <- col_num + 1
    col_type <- "l"
  }
  
  if(showSeedingBudget){
    col_num <- col_num + 1
    col_budget <- "l"
  }
  
  
  cat("\\begin{tabular}{ l ",col_type,col_budget,"  rrr  rrr  rrr}\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT & ")
  if(showType)
    cat("Type &")
  if(showSeedingBudget)
    cat("Budgets &")
  
  cat(" \\multicolumn{3}{c}{Line\\%}  & \\multicolumn{3}{c}{Critical Line\\%}  & \\multicolumn{3}{c}{\\#Detected Faults} \\\\ \n", sep ="")
  if(showComparsionColumn){
    cat(" & ")
    if(showType)
      cat(" & ")
    if(showSeedingBudget)
      cat("by seeds &")
    cat("\\multicolumn{1}{r}{",maskIdaName1st,"}  & ",compareAvsBIdName,"&",compareAvsCIdName,"&",
        "\\multicolumn{1}{r}{",maskIdaName1st,"}  & ",compareAvsBIdName,"&",compareAvsCIdName,"&",
        "\\multicolumn{1}{r}{",maskIdaName1st,"}  & ",compareAvsBIdName,"&",compareAvsCIdName,"  \\\\ \n", sep ="")
  }
  cat(" & ")
  if(showType)
    cat(" & ")
  if(!showComparsionColumn && showSeedingBudget)
    cat("by seeds &")
  cat("\\multicolumn{1}{r}{",maskIdaName2nd,"}  & Relative\\%($\\hat{A}_{12}$) & Relative\\%($\\hat{A}_{12}$)"," & \\multicolumn{1}{r}{",maskIdaName2nd,"} & Relative\\%($\\hat{A}_{12}$) & Relative\\%($\\hat{A}_{12}$)"," & ",maskIdaName2nd," & Relative\\%($\\hat{A}_{12}$) & Relative\\%($\\hat{A}_{12}$) \\\\ \n", sep ="")
  cat("\\midrule \n")
  
  existingprojects = sort(unique(dta$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  projectsByCategory <- orderSUT(ByCategory = FALSE)
  projects <- projects[order(match(projects, projectsByCategory))]
  
  nprojects = length(projects)
  
  all <- matrix(0, nrow = nprojects, ncol = (1+ 3*2)*3)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  for (i in 1:nprojects) {
    proj = projects[[i]]
    aprojectMask =  dta$id == proj
    bprojectMask =  dtb$id == proj
    cprojectMask =  dtc$id == proj
    
    
    totLines = getTotalLines(proj)
    totCrLines = getTotalCRLines(proj)
    
    cat(formatSUT(proj, anonymized))
    
    sutdt_label = sutdt$Id == proj
    label <- sutdt$Category[sutdt_label]
    
    if(showType)
      cat(" &", label, sep = "")
    
    
    if(showSeedingBudget){
      seedingDone = aprojectMask & dta$notExecutedSeededTests == 0
      aNumSeeds = dta$numberOfRPCSeededTests[seedingDone]
      aNumActions = dta$evaluatedActions[seedingDone]
      seedingNotDone = aprojectMask & dta$notExecutedSeededTests > 0
      aNotDoneActions = dta$evaluatedActions[seedingNotDone]
      aSeedsRatio = c(aNumSeeds / aNumActions, aNotDoneActions/aNotDoneActions)
      cat(" & ", formatFloat(mean(aSeedsRatio) * 100), "\\%", sep = "")
    }
    
    alines <- dta$coveredLines[aprojectMask]
    acrlines <- dta$lineAndcovered[aprojectMask]
    afaults <- dta$potentialFaults[aprojectMask]
    
    
    blines <- dtb$coveredLines[bprojectMask]
    bcrlines <- dtb$lineAndcovered[bprojectMask]
    bfaults <- dtb$potentialFaults[bprojectMask]
    
    
    clines <- dtc$coveredLines[cprojectMask]
    ccrlines <- dtc$lineAndcovered[cprojectMask]
    cfaults <- dtc$potentialFaults[cprojectMask]
    
    
    ## Mean
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(alines)/totLines * 100),formatFloat(mean(alines)/totLines * 100), "}",sep = "")
    all[i, 1] = mean(alines)/totLines * 100 
    
    ## avsb
    lines_a12 = measureA(alines, blines)
    lines_w = wilcox.test(alines, blines)
    lines_p = lines_w$p.value
    
    
    relativelines = (mean(alines)-mean(blines))/mean(blines) * 100
    all[i, 2] = relativelines
    all[i, 3] = lines_a12
    all[i, 4] = lines_p
    
    sign <- "+"
    if(relativelines < 0)
      sign <- ""
    cat(" &", sign ,boldValue(formatFloat(relativelines), lines_p < 0.05, lines_a12 > 0.5), sep = "")
    cat(" ( ", boldValue(formatFloat(lines_a12, 2), lines_p < 0.05, lines_a12 > 0.5), ")", sep ="")

    
    ## avsc
    lines_a12 = measureA(alines, clines)
    lines_w = wilcox.test(alines, clines)
    lines_p = lines_w$p.value
    
    relativelines = (mean(alines)-mean(clines))/mean(clines) * 100
    all[i, 5] = relativelines
    all[i, 6] = lines_a12
    all[i, 7] = lines_p
    
    sign <- "+"
    if(relativelines < 0)
      sign <- ""
    cat(" &", sign ,boldValue(formatFloat(relativelines), lines_p < 0.05, lines_a12 > 0.5), sep = "")
    cat(" ( ", boldValue(formatFloat(lines_a12, 2), lines_p < 0.05, lines_a12 > 0.5), ")", sep ="")
    
    
    # Mean
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(acrlines)/totCrLines * 100),formatFloat(mean(acrlines)/totCrLines * 100), "}",sep = "")
    all[i, 8] = mean(acrlines)/totCrLines * 100
    
    # a vs b
    crlines_a12 = measureA(acrlines, bcrlines)
    crlines_w = wilcox.test(acrlines, bcrlines)
    crlines_p = crlines_w$p.value
    
    
    relativecrlines = (mean(acrlines)-mean(bcrlines))/mean(bcrlines) * 100
    all[i, 9] = relativecrlines
    all[i, 10] = crlines_a12
    all[i, 11] = crlines_p
    sign <- "+"
    if(relativecrlines < 0)
      sign <- ""
    cat(" &", sign, boldValue(formatFloat(relativecrlines), crlines_p < 0.05, crlines_a12 > 0.5), sep = "")
    cat(" (", boldValue(formatFloat(crlines_a12, 2), crlines_p < 0.05, crlines_a12 > 0.5), ")",sep = "")
    
    
    # a vs c
    crlines_a12 = measureA(acrlines, ccrlines)
    crlines_w = wilcox.test(acrlines, ccrlines)
    crlines_p = crlines_w$p.value
    
    
    relativecrlines = (mean(acrlines)-mean(ccrlines))/mean(ccrlines) * 100
    all[i, 12] = relativecrlines
    all[i, 13] = crlines_a12
    all[i, 14] = crlines_p
    sign <- "+"
    if(relativecrlines < 0)
      sign <- ""
    cat(" &", sign, boldValue(formatFloat(relativecrlines), crlines_p < 0.05, crlines_a12 > 0.5), sep = "")
    cat(" (", boldValue(formatFloat(crlines_a12, 2), crlines_p < 0.05, crlines_a12 > 0.5), ")",sep = "")
    
    
    cat(" &", formatFloat(mean(afaults)), sep = "")
    all[i, 15] = mean(afaults)
    
    # a vs b
    faults_a12 = measureA(afaults, bfaults)
    faults_w = wilcox.test(afaults, bfaults)
    faults_p = faults_w$p.value
    
    relativefaults = (mean(afaults)-mean(bfaults))/mean(bfaults) * 100
    all[i, 16] = relativefaults
    all[i, 17] = faults_a12
    all[i, 18] = faults_p
    
    sign <- "+"
    if(relativefaults < 0)
      sign <- ""
    cat(" &", sign, boldValue(formatFloat(relativefaults), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), sep = "")
    cat(" (", boldValue(formatFloat(faults_a12, 2), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5),")", sep = "")

    
    
    # a vs c
    faults_a12 = measureA(afaults, cfaults)
    faults_w = wilcox.test(afaults, cfaults)
    faults_p = faults_w$p.value
    
    relativefaults = (mean(afaults)-mean(cfaults))/mean(cfaults) * 100
    all[i, 19] = relativefaults
    all[i, 20] = faults_a12
    all[i, 21] = faults_p
    
    sign <- "+"
    if(relativefaults < 0)
      sign <- ""
    cat(" &", sign, boldValue(formatFloat(relativefaults), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), sep = "")
    cat(" (", boldValue(formatFloat(faults_a12, 2), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5),")", sep = "")
    
    cat(" \\\\ \n")
    
  }
  
  cat("\\midrule \n")
  
  cat("Mean")
  
  if(showSeedingBudget)
    cat("&")
  
  for (j in 1:ncol(all)) {
    x <- j %% 7
    y <- (x-1) %% 3
    if((y != 0 && x != 0) ||  x == 1){
      sign <- ""
      if((y == 1) && mean(all[,j]) > 0)
        sign <- "+"
      if(y == 2)
        cat(" (", sign, formatFloat(mean(all[,j])), ")",sep = "")
      else
        cat(" & ", sign, formatFloat(mean(all[,j])), "",sep = "")
    }
  }
  
  cat(" \\\\ \n")
  
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
}


compareAvsBAndBvsCAndAvsB <- function(dta, maskIda, maskIdAName1st,maskIdAName2nd, showAMean, # seed 10h
                                      dtb, maskIdb, maskIdBName1st,maskIdBName2nd, showBMean, # base 10h
                                      dtc, maskIdc,  # seed 1h
                                      dtd, maskIdd, # base 1h
                                      compareIdAvsC,
                                      compareIdBvsD,
                                      compareIdAvsB,
                                      anonymized){
  
  TABLE = paste(GENERATED_FILES, "/coverage_",DATA_VERSION, "_" ,maskIda, "_vs_",maskIdb,"_vs_",maskIdc,"_with_amean_using_relative_a12.tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  a_col_settings <- "r"
  a_col_num <- 1
  a_col_name1st <- ""
  a_col_name2nd <- ""
  b_col_settings <- "r"
  b_col_num <- 1
  b_col_name1st <- ""
  b_col_name2nd <- ""
  if(showAMean){
    a_col_settings <- "rr"
    a_col_num <- 2
    a_col_name1st <- paste(maskIdAName1st, " & ", sep = "")
    a_col_name2nd <- paste(maskIdAName2nd, " & ", sep = "")
  }
  if(showBMean){
    b_col_settings <- "rr"
    b_col_num <- 2
    b_col_name1st <- paste(maskIdBName1st, " & ", sep = "")
    b_col_name2nd <- paste(maskIdBName2nd, " & ", sep = "")
  }
    
  cat("\\begin{tabular}{ l l  ",a_col_settings,"",a_col_settings,"", a_col_settings, "",b_col_settings,"", b_col_settings,"", b_col_settings,"  r r r }\\\\ \n", sep = "")
  cat("\\toprule \n")
  cat("SUT & Type &  \\multicolumn{",a_col_num*3,"}{c}{",compareIdAvsC,"}  & \\multicolumn{",b_col_num*3,"}{c}{",compareIdBvsD,"}  & \\multicolumn{",3,"}{c}{",compareIdAvsB,"} \\\\ \n", sep ="")
  
  cat(" &  &  ",a_col_name1st,"\\multicolumn{1}{r}{Line\\%}  & ",a_col_name1st,"\\multicolumn{1}{r}{Critical Line\\%}  & ",a_col_name1st,"\\multicolumn{1}{r}{\\#Detected Faults} ", sep ="")
  cat("&  ",b_col_name1st,"\\multicolumn{1}{r}{Line\\%}  & ",b_col_name1st,"\\multicolumn{1}{r}{Critical Line\\%}  & ",b_col_name1st,"\\multicolumn{1}{r}{\\#Detected Faults}", sep ="")
  cat("&  \\multicolumn{1}{r}{Line\\%}  & \\multicolumn{1}{r}{Critical Line\\%}  & \\multicolumn{1}{r}{\\#Detected Faults} \\\\ \n", sep ="")
  
  
  cat(" & & ",a_col_name2nd," Relative\\%($\\hat{A}_{12}$)  & ",a_col_name2nd," Relative\\%($\\hat{A}_{12}$) & ",a_col_name2nd," Relative\\%($\\hat{A}_{12}$) ", sep ="")
  cat(" & ",b_col_name2nd," Relative\\%($\\hat{A}_{12}$) & ",b_col_name2nd," Relative\\%($\\hat{A}_{12}$) & ",b_col_name2nd," Relative\\%($\\hat{A}_{12}$) ", sep ="")
  cat(" &  Relative\\%($\\hat{A}_{12}$) & Relative\\%($\\hat{A}_{12}$) &  Relative\\%($\\hat{A}_{12}$) "," \\\\ \n", sep ="")
  cat("\\midrule \n")
  
  existingprojects = sort(unique(dta$id))
  projects = SUT_IDS[SUT_IDS %in% existingprojects]
  projectsByCategory <- orderSUT(ByCategory = FALSE)
  projects <- projects[order(match(projects, projectsByCategory))]
  
  nprojects = length(projects)
  
  # AvsC: Mean, Relative, A12, p; BvsD: Mean, Relative, A12, p; AvsB: Mean, Relative, A12, p
  all <- matrix(0, nrow = nprojects, ncol = 4 * 3 * 3)
  
  sutdt <- read.table(SUTINFO_DATA, header = T)
  
  for (i in 1:nprojects) {
    proj = projects[[i]]
    aprojectMask =  dta$id == proj
    bprojectMask =  dtb$id == proj
    cprojectMask =  dtc$id == proj
    dprojectMask =  dtd$id == proj
    
    totLines = getTotalLines(proj)
    totCrLines = getTotalCRLines(proj)
    
    cat(formatSUT(proj, anonymized))
    
    sutdt_label = sutdt$Id == proj
    label <- sutdt$Category[sutdt_label]
    cat(" &", label, sep = "")
    
    alines <- dta$coveredLines[aprojectMask]
    acrlines <- dta$lineAndcovered[aprojectMask]
    afaults <- dta$potentialFaults[aprojectMask]
    
    
    blines <- dtb$coveredLines[bprojectMask]
    bcrlines <- dtb$lineAndcovered[bprojectMask]
    bfaults <- dtb$potentialFaults[bprojectMask]
    
    
    clines <- dtc$coveredLines[cprojectMask]
    ccrlines <- dtc$lineAndcovered[cprojectMask]
    cfaults <- dtc$potentialFaults[cprojectMask]
    
    dlines <- dtd$coveredLines[dprojectMask]
    dcrlines <- dtd$lineAndcovered[dprojectMask]
    dfaults <- dtd$potentialFaults[dprojectMask]
    
    j <- 0
    
    ## a vs c
    all <- compareAvsBWithMRA(all, 
                         alines, clines, 
                         acrlines, ccrlines,
                         afaults, cfaults,
                         totLines, totCrLines,
                         showAMean, showAMean, i, j,FALSE)
    
    ## b vs d
    j <- 4 * 3
    all <- compareAvsBWithMRA(all, 
                         blines, dlines, 
                         bcrlines, dcrlines,
                         bfaults, dfaults,
                         totLines, totCrLines,
                         showAMean, showAMean, i, j,FALSE)
    
    j <- 4 * 3 *2
    ## a vs b
    all <- compareAvsBWithMRA(all, 
                         alines, blines, 
                         acrlines, bcrlines,
                         afaults, bfaults,
                         totLines, totCrLines,
                         FALSE, TRUE, i, j,FALSE)
    
    # cat("&",formatSUT(proj, anonymized))
    cat(" \\\\ \n")
  }
  
  cat("\\midrule \n")
  
  cat("Mean &")
  
  for (j in 1:ncol(all)) {
    if(j %% 4 != 0 && (j <= 4*3*2 || (j-4*3*2)%%4 != 1)){
      sign <- ""
      if((j %% 4 == 2) && mean(all[,j]) > 0)
        sign <- "+"
      if(j %% 4 == 3)
        cat(" (", sign, formatFloat(mean(all[,j])), ")", sep = "")
      else
        cat(" & ", sign, formatFloat(mean(all[,j])), sep = "")
      
    }
  }
  cat("\\\\ \n")
  
  cat("\\midrule \n")
  cat("\\multicolumn{2}{c}{\\#(10\\%, 20\\%]}")
  for (j in 1:ncol(all)) {
    if(j %% 4 != 0 && (j <= 4*3*2 || (j-4*3*2)%%4 != 1)){
      if(j %% 4 != 3){
        cat(" & ")
        if(j %% 4 == 1 && j%%12 != 9){
          m <- all[,j]
          cat(" ",length(m[m > 10 & m <= 20]), sep = "")
        }
      }
      
    }
  }
  cat("\\\\ \n")
 
  cat("\\multicolumn{2}{c}{\\#(20\\%, 100\\%]}")
  for (j in 1:ncol(all)) {
    if(j %% 4 != 0 && (j <= 4*3*2 || (j-4*3*2)%%4 != 1)){
      if(j %% 4 !=3){
        cat(" & ")
        if(j %% 4 == 1 && j%%12 != 9){
          m <- all[,j]
          cat(" ",length(m[m > 20]), sep = "")
        }
      }
    }
  }
  cat("\\\\ \n")
  
  
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
  
}


compareAvsBWithMRA<- function(all, 
                       alines, blines, 
                       acrlines, bcrlines,
                       afaults, bfaults,
                       totLines, totCrLines,
                       showMean, calculateMean, i, j,reverseCompare = FALSE){
  
  index <- 1
  
  lines_a12 = measureA(alines, blines)
  if(reverseCompare)
    lines_a12 = 1.0 - lines_a12
  lines_w = wilcox.test(alines, blines)
  lines_p = lines_w$p.value
  
  if(showMean)
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(alines)/totLines * 100),formatFloat(mean(alines)/totLines * 100), "}",sep = "")
  relativelines = (mean(alines)-mean(blines))/mean(blines) * 100
  if(reverseCompare)
    relativelines = (mean(blines)-mean(alines))/mean(alines) * 100
  sign <- "+"
  if(relativelines < 0)
    sign <- ""
  cat(" & \\databar{", formatFloat(relativelines), "}{",sign ,boldValue(formatFloat(relativelines), lines_p < 0.05, lines_a12 > 0.5), sep = "")
  cat(" (", boldValue(formatFloat(lines_a12, 2), lines_p < 0.05, lines_a12 > 0.5),")}",sep ="")
  if(calculateMean){
    all[i, j+index] = mean(alines)/totLines * 100 
    index <- index + 1
  }
  all[i, j+index] = relativelines
  index <- index + 1
  all[i, j+index] = lines_a12
  index <- index + 1
  all[i, j+index] = lines_p
  index <- index + 1
  
  crlines_a12 = measureA(acrlines, bcrlines)
  if(reverseCompare)
    crlines_a12 = 1.0 - crlines_a12
  crlines_w = wilcox.test(acrlines, bcrlines)
  crlines_p = crlines_w$p.value
  
  if(showMean)
    cat(" &", "\\multicolumn{1}{r}{", linecoverage_cellcolor(mean(acrlines)/totCrLines * 100),formatFloat(mean(acrlines)/totCrLines * 100), "}",sep = "")
  relativecrlines = (mean(acrlines)-mean(bcrlines))/mean(bcrlines) * 100
  if(reverseCompare)
    relativecrlines = (mean(bcrlines)-mean(acrlines))/mean(acrlines) * 100
  sign <- "+"
  if(relativecrlines < 0)
    sign <- ""
  cat(" & \\databar{",formatFloat(relativecrlines),"}{", sign, boldValue(formatFloat(relativecrlines), crlines_p < 0.05, crlines_a12 > 0.5), sep = "")
  cat(" (", boldValue(formatFloat(crlines_a12, 2), crlines_p < 0.05, crlines_a12 > 0.5), ")}", sep = "")
  if(calculateMean){
    all[i, j+index] = mean(acrlines)/totCrLines * 100
    index <- index + 1
  }
  all[i, j+index] = relativecrlines
  index <- index + 1
  all[i, j+index] = crlines_a12
  index <- index + 1
  all[i, j+index] = crlines_p
  index <- index + 1
  
  faults_a12 = measureA(afaults, bfaults)
  if(reverseCompare)
    faults_a12 = 1 - faults_a12
  faults_w = wilcox.test(afaults, bfaults)
  faults_p = faults_w$p.value
  
  if(showMean)
    cat(" &", formatFloat(mean(afaults)), sep = "")
  relativefaults = (mean(afaults)-mean(bfaults))/mean(bfaults) * 100
  if(reverseCompare)
    relativefaults = (mean(bfaults)-mean(afaults))/mean(afaults) * 100
  sign <- "+"
  if(relativefaults < 0)
    sign <- ""
  cat(" & \\databar{", formatFloat(relativefaults),"}{",sign, boldValue(formatFloat(relativefaults), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5), sep = "")
  cat(" (", boldValue(formatFloat(faults_a12, 2), is.numeric(faults_p) && (!is.nan(faults_p)) && faults_p < 0.05, faults_a12 > 0.5),")}", sep = "")
  
  if(calculateMean){
    all[i, j+index] = mean(afaults)
    index <- index + 1
  }
  
  all[i, j+index] = relativefaults
  index <- index + 1
  all[i, j+index] = faults_a12
  index <- index + 1
  all[i, j+index] = faults_p
  
  return(all)
}


##################################################################################################

compareCoveredLines <- function(patternA, seedsA, expectedNumA, patternB, seedsB, expectedNumB, project, showInvalid){
  
  results <- matrix(nrow=length(seedsA) * length(seedsB), ncol = 3)
  
  i <- 1
  for (ia in 1:length(seedsA)) {
    a <- seedsA[ia]
    ea <- expectedNumA[ia]
    for (ib in 1:length(seedsB)) {
      
      b <- seedsB[ib]
      eb <- expectedNumB[ib]
      
      setA <- collectAll(patternA, c(a), ea, project, showInvalid = showInvalid)
      setB <- collectAll(patternB, c(b), eb, project, showInvalid = showInvalid)
      
      
      if((!is.null(setA)) && (!is.null(setB))){
        all <- unique(c(setA, setB))
        
        both <- intersect(setA, setB)
        # only A
        onlyAPercentage <- (length(setA) - length(both))/length(all) * 100 
        bothPercentage <-  length(both)/length(all) * 100
        onlyBPercentage <-(length(setB) - length(both))/length(all) * 100
        
        results[i, 1] <- onlyAPercentage
        results[i, 2] <- bothPercentage
        results[i, 3] <- onlyBPercentage
        i <- i+1
      }
    }
  }
  
  
  return(c(formatC(mean(results[,1]),  digits = 1, format = "f"), formatC(mean(results[,2]),  digits = 1, format = "f"),formatC(mean(results[,3]),  digits = 1, format = "f")))
  
}


collectAll <- function(pattern, seeds, expectedNum, project, des="", showInvalid = TRUE){
  tolerror <- 10
  set <- NULL
  for (s in seeds) {
    concrete_pattern = paste(pattern,s,".*\\.json", sep = "")
    jsonFiles <- list.files(COVERAGEINFO_JSON, pattern = concrete_pattern, all.files = TRUE, full.names=TRUE, recursive = TRUE)
    if(length(jsonFiles) == 0){
      if(showInvalid)
        cat(paste("Regarding ", project, " for ",des,", cannot find a coverage json file with ", pattern, " for seedid ", s, "\n",sep = ""))
       
    }else{
      invalid <- c()
      for (jsonFile in jsonFiles){
        #tmp <- extractCoveredLines(jsonFile)
        tmp <- extractXml2JsonCoveredLines(jsonFile)
        invalid <- append(invalid, length(tmp))
        if((length(tmp) <= expectedNum + tolerror) & (length(tmp) >= expectedNum - tolerror)){
          set <- tmp
          break
        }
        
      }
      
      if(is.null(set)) {
        if(showInvalid){
          invalids <- paste(invalid, collapse=", ", sep = "")
          cat("Regarding ", project, " for ",des,", cannot find a json file (current info shows [", invalids,"] covered lines) which has ", expectedNum, "(+/-",tolerror,") covered lines for seedid ", s, " with ", pattern, "\n",sep = "")
        }
        return(set)
      }
      
    }
  }  
  
  return(unique(set))
}

extractXml2JsonCoveredLines <- function(jsonFile){
  set <- NULL
  jsonObject <- fromJSON(file=jsonFile)
  set <- jsonObject[["coveredLinesInfo"]]
  return(unique(set))
}


extractCoveredLines <- function(jsonFile,keysuffix=".java"){
  set <- NULL
  jsonObject <- fromJSON(file=jsonFile)
  content <- unlist(jsonObject)
  for (key in names(content)) {
    cur <- unlist(strsplit(key, split = keysuffix))
    for (v in content[[key]]) {
      element <- paste(cur[1],keysuffix,"_LINENO_", v, sep = "")
      if(is.null(set)){
        set <- c(element)
      }else{
        set <- append(set, element)
      }
    }
  }
  if(is.null(set)) return(set)
  
  return(unique(set))
}

formatMean <- function(data){
  return(formatC(mean(data), digits = 2, format = "f"))
}


##################################################################################################

analyzeAll <- function(anonymized = TRUE, showInvalid = TRUE){
  generateSutCmd()
  generateSutStatistics()
  
  
  dt <- read.table(gzfile(COMBINE_ZIP_FILE), header = T)
  seedingdt <- read.table(gzfile(COMBINE_ZIP_FILE_SEEDING), header = T)
  
  # RQ1
  disable_seed_mask = dt$seedTestCases == "false" & dt$maxTime == "1h"
  basesubdt <- subset(dt, disable_seed_mask)
  coverageTableWithMask(basesubdt, "without_seeds", FALSE, TRUE, anonymized)
  spearmanWithSutStatistics(basesubdt, "without_seeds")
  
  # RQ2
  enable_seed_mask_seedingdt = seedingdt$seedTestCases == "true"
  subseedingdt <- subset(seedingdt, enable_seed_mask_seedingdt)
  coverageTableShowingSeededLine(subseedingdt,"_duringseeding", anonymized, basesubdt, showInvalid= showInvalid)
  
  
  # RQ3
  enable_seed_mask = dt$seedTestCases == "true" & dt$maxTime == "1h"
  seededsubdt <- subset(dt, enable_seed_mask)
  coverageTableWithMask(seededsubdt, "enabled_seeds", FALSE, TRUE, anonymized)
  compareMetricsShowRelativeAndA12WithAMean(seededsubdt,"seeded_1h", "","Mean", basesubdt, "base_1h", anonymized, compareIdName = "Seeded vs. Base", 
                                            reverseCompare = FALSE,showAdditionalSummary = TRUE, showComparsionColumn= FALSE, showType = FALSE, showSeedingBudget = TRUE)
  
  # RQ4
  
  enable_seed_mask_10h = dt$seedTestCases == "true" & dt$maxTime == "10h"
  seeded10hsubdt <- subset(dt, enable_seed_mask_10h)
  
  
  snapshotdt <- read.table(gzfile(SNAP_ZIP_FILE), header = T)
  enabled_seed_10h_snapshotMask = snapshotdt$seedTestCases == "true" & snapshotdt$maxTime == "10h"
  enabled_seed_10h_snapshot <- subset(snapshotdt, enabled_seed_10h_snapshotMask)
  sprojects <- c("cs03","cs06","cs18","cs29")
  coverageGraph(sbudget = c("10h"), dt = enabled_seed_10h_snapshot, sprojects = sprojects)
  
  disable_seed_mask_10h = dt$seedTestCases == "false" & dt$maxTime == "10h"
  based10hsubdt <- subset(dt, disable_seed_mask_10h)
  
  compareAvsBAndBvsCAndAvsB(seeded10hsubdt, "seeded_10h", "","Mean", TRUE, # seed 10h
                            based10hsubdt, "base_10h", "","Mean", TRUE, # base 10h
                            seededsubdt, "seeded_1h",  # seed 1h
                            basesubdt, "base_1h", # base 1h
                            "\\textbf{Mean of Seeded 10h; Seeded 10h vs. Seeded 1h}",
                            "\\textbf{Mean of Base 10h; Base 10h vs. Base 1h}",
                            "\\textbf{Seeded 10h vs. Base 10h}",
                            anonymized)
  
}



