## RefWeb code written by Owen Petchey - 1/1/2005
## Please acknowledge appropriately

## Read in the references and cited references from the Web of Science text file
## This sometimes produces warnings. I haven't yet figured out why.
GetRefs <- function(file.name){
  data <- readLines(file.name)
  i <- 1
  ref.number <- 1
  while(i<length(data)){
    
    split.line <- strsplit(data[i], split=" ")[[1]]
    ## skip forward to first author line
    while(ifelse(length(split.line)==0, T, split.line[1]!="AU")){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    
    ## authors
    split.line <- strsplit(data[i], split=",")[[1]]
    author <- as.character(substr(split.line[1], start=4, stop=nchar(split.line[1])))
    while(split.line[1]!="TI"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    
    ## skip forward to cited references
    while(split.line[1]!="CR"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    cr.number <- 1
    first.used <- T

    if(split.line[2]!="None"){
      while(split.line[1]!="J9"){
        if(cr.number==1 & first.used){
          split.line <- strsplit(data[i], split=" ")[[1]]
          cr.author <- split.line[2]
          split.line <- strsplit(data[i], split=", ")[[1]]
          cr.year <- as.numeric(split.line[2])
          cr.journal <- split.line[3]
          if(length(split.line)==5){
            cr.volume <- as.numeric(substr(split.line[4], start=2, stop=nchar(split.line[4])))
            position <- 2
            while(is.na(as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5])))))
              position <- position + 1
            cr.page <-  as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5])))
          }
          if(length(split.line)!=5){
            cr.number <- cr.number - 1
            first.used <- F
          }
        }
        
        if(cr.number==1 & !first.used){
          split.line <- strsplit(data[i], split=", ")[[1]]
          if(length(split.line)==5){
            split.line <- strsplit(data[i], split=" ")[[1]]
            cr.author <- split.line[4]
            split.line <- strsplit(data[i], split=", ")[[1]]
            cr.year <- as.numeric(split.line[2])
            cr.journal <- split.line[3]
            cr.volume <- as.numeric(substr(split.line[4], start=2, stop=nchar(split.line[4])))
            position <- 2
            while(is.na(as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5])))))
              position <- position + 1
            cr.page <-  as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5])))
          }
          if(length(split.line)!=5)
            cr.number <- cr.number - 1
        }
        
        if(cr.number>1){
          split.line <- strsplit(data[i], split=", ")[[1]]
          if(length(split.line)==5){
            split.line <- strsplit(data[i], split=" ")[[1]]
            cr.author <- c(cr.author, split.line[4])
            split.line <- strsplit(data[i], split=", ")[[1]]
            cr.year <- c(cr.year, as.numeric(split.line[2]))
            cr.journal <- c(cr.journal, split.line[3])
            cr.volume <- c(cr.volume, as.numeric(substr(split.line[4], start=2, stop=nchar(split.line[4]))))
            position <- 2
            while(is.na(as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5])))))
              position <- position + 1        
            cr.page <- c(cr.page, as.numeric(substr(split.line[5], start=position, stop=nchar(split.line[5]))))
          }
          if(length(split.line)!=5)
            cr.number <- cr.number - 1
        }
        i <- i + 1
        cr.number <- cr.number + 1
        split.line <- strsplit(data[i], split=" ")[[1]]
      }
      
      if(ref.number==1)
        cited.refs <- data.frame(ref.number=rep(ref.number, length(cr.author)),
                                 author=cr.author,
                                 year=cr.year,
                                 journal=cr.journal,
                                 volume=cr.volume,
                                 page=cr.page)
      if(ref.number>1)
        cited.refs <- rbind(cited.refs,
                            data.frame(ref.number=rep(ref.number, length(cr.author)),
                                       author=cr.author,
                                       year=cr.year,
                                       journal=cr.journal,
                                       volume=cr.volume,
                                       page=cr.page))
    }

    ## journal
    if(split.line[2]=="None"){
      while(split.line[1]!="J9"){
        i <- i + 1
        split.line <- strsplit(data[i], split=" ")[[1]]
      }
    }
    journal <- paste(split.line[-1], collapse=" ")

    
    ## year
    while(split.line[1]!="PY"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    year <- as.numeric(split.line[2])

    
    ## volume
    while(split.line[1]!="VL"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    volume <- as.numeric(split.line[2])

    
    ## beginning page
    while(split.line[1]!="BP"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
    }
    position <- 1
    while(is.na(as.numeric(substr(split.line[2], start=position, stop=nchar(split.line[2])))))
      position <- position + 1        
    page <- as.numeric(substr(split.line[2], start=position, stop=nchar(split.line[2])))
    
    if(ref.number==1)
      refs <- data.frame(ref.number=ref.number,
                         author=author,
                         year=year,
                         journal=journal,
                         volume=volume,
                         page=page)
    if(ref.number>1)
      refs <- rbind(refs,
                    data.frame(ref.number=ref.number,
                               author=author,
                               year=year,
                               journal=journal,
                               volume=volume,
                               page=page))
    
    while(i<length(data) & split.line[1]!="AU"){
      i <- i + 1
      split.line <- strsplit(data[i], split=" ")[[1]]
      if(length(split.line)==0)
        split.line <- 9999
    }
    ref.number <- ref.number + 1
  }
  result <- list(refs=refs, cited.refs=cited.refs)
  result
}


## Add some of the most commonly cited refs
AddCommonlyCited <- function(references, frequency=5){

  cited.refs=references[[2]]
  
  more.than.itself <- cited.refs[duplicated(cited.refs[,-1], MARGIN=1),]
  cited.papers <- unique(more.than.itself[,-1], MARGIN=1)
  times.cited <- numeric(length(cited.papers[,1]))+1
  for(i in 1:length(cited.papers[,1])){
    author.match <- grep(as.character(cited.papers[i,1]),
                         as.character(cited.refs[,2]),
                         ignore.case=T)  
    for(j in 1:length(author.match)){
      this.one <- author.match[j]
      times.cited[i] <- times.cited[i] +
        ref.match(cited.papers[i,], cited.refs[this.one,-1])
    }
  }
  commonly.cited.refs <- cbind(ref.number=(max(references[[1]][,1])+1):(max(references[[1]][,1]+length(cited.papers[times.cited>frequency,1]))), cited.papers[times.cited>frequency,])
  references.plus <- list(rbind(references[[1]], commonly.cited.refs),
                          references[[2]])
}

 
## Produce a matrix of matches between references
GetRefWeb <- function(references){
  refs <- references[[1]]
  cited.refs <- references[[2]]

  ## remove * citations
  if(!is.na(-charmatch("*",as.character(references[[2]][,2]))))
    cited.refs <- cited.refs[-charmatch("*",as.character(references[[2]][,2])),]

  citation.matrix <- matrix(0, length(refs[,1]), length(refs[,1]))
  for(i in 1:max(refs[,1])){
    these.cr <- cited.refs[cited.refs[,1]==i,]
    if(length(these.cr[,1])>0){
      for(j in 1:length(these.cr[,1])){
        author.match <- grep(as.character(these.cr[j,2]),
                             as.character(refs[,2]),
                             ignore.case=T)
        if(length(author.match)>0 & sum(is.na(author.match))!=length(author.match)){
          result <- 0
          for(k in 1:length(author.match)){
            check.this <- author.match[k]
            if(citation.matrix[i,author.match[k]]!=1)
              citation.matrix[i,author.match[k]] <- ref.match(these.cr[j,-1], refs[check.this,-1])
          }
        }
      }
    }
  }

  ref.name <- character(max(refs[,1]))
  for(i in 1:max(refs[,1]))
    ref.name[i] <- paste("\"", paste(refs[i,2], refs[i,3], refs[i,1],  sep="."), "\"", sep="")
  dimnames(citation.matrix) <- list(ref.name, ref.name)
  citation.matrix
}


## check for a match between two references
ref.match <- function(ref1, ref2){
  result <- ifelse(length(grep(as.character(ref1[1,1]), as.character(ref2[1,1]), ignore.case=T))==1 &&
                   ref1[1,2]==ref2[1,2] &&
                   length(grep(as.character(ref1[1,3]), as.character(ref2[1,3]), ignore.case=T))==1 &&
                   ref1[1,4]==ref2[1,4] &&
                   ref1[1,5]==ref2[1,5], 1, 0)
  result
}


## writes a file that gviz can use to plot a graph
Makegvizdat <- function(mat, filename="gviz.web.dot", list.of.colours="No"){

  ## matrix.to.list <- function(web.matrix, predator.first=TRUE){
  dat <- Matrix.to.list(mat, predator.first=FALSE)

  ## Make output file in graphviz format
  cat("digraph web \{ \n", append=F, file=filename)
  cat("size=", paste("\"100,100\";"), " node [style=filled];\n", append=T, file=filename)
  for(i in 1:length(dat[,1]))
      cat(paste(dat[i,1], "->", dat[i,2], ";\n"), append=T, file=filename)

  if(length(list.of.colours)>1){
    list.of.species <- dimnames(mat)[[1]]
    for(i in 1:length(list.of.species))
      cat(paste(list.of.species[i], " [fillcolor=", list.of.colours[i], "];\n", sep=""), append=T, file=filename)
  }
  cat("}", append=T, file=filename)
  
}


## takes a web in matrix format and coverts it to list format
Matrix.to.list <- function(web.matrix, predator.first=TRUE){
  if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
    species.names <- dimnames(web.matrix)[[1]]
  else
    species.names <- 1:length(web.matrix[,1])
  web.list <- matrix(0, sum(web.matrix), 2)
  counter <- 1
  for(i in 1:length(web.matrix[,1]))
    for(j in 1:length(web.matrix[,1]))
      if(web.matrix[i,j]==1){
        web.list[counter,] <- c(species.names[i],species.names[j])
        counter <- counter + 1
      }
  if(!predator.first)
    web.list <- cbind(web.list[,2], web.list[,1])
  web.list
}
