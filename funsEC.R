rm(list = ls())

# Works
openReportFile <- function(filename) {
    # opens and reads files
    #df <- read.delim(filename, header = FALSE)
    fileHandle = file(filename, open = "r")
    fileInfo = readLines(fileHandle)
    close(fileHandle)
    return (fileInfo)
}

# Works
generateFileNames <- function(root, indexes) {
    # filename extraction (note: you may need to write your own!)
    out = vector()
    for (i in indexes) {
        out = append(out, paste0(toString(root), toString(i), ".report.txt"))
    }
    return (out)
}

# Works 
trim <- function(item) gsub("^\\s+|\\s+$", "", item) #leading and trailing whitespace

# Works
getNodes <- function(filename) {
    # get the nodes from a file with node names separated by "\n"
    fileHandle = file(filename, 'r')
    nodesx = readLines(fielHandle)
    nodes = vector()
    
    for (x in nodesx) {
        x = toString(x)
        y = trim(x)
        nodes = append(nodes, y)
    }
    close(fileHandle)
    return (nodes)
}

getNetworks <- function(fnames) {
    # function will return all networks with no parsing
    networks = vector()
    for (i in fnames) { # change this later to use indexes array
        fileHandle = file(i, open = 'r')
        network = readLines(fileHandle)
        close(fileHandle)
        # print network
        # print scores
        networks = append(networks, network)
    }
    return (networks)
}

getNetsAll <- function(files, outfile) {
    # Purpose: get all of the networks from the BANJO files
    # Banjo files should all be saved in the same directory 
    scores = list()
    file.out.array = vector()
    i = -1
    
    netcount = 0
    i = i + 1
    for (fileinx in files) {
        x = 0
        edges = vector()
        BanjoFile = openReportFile(fileinx)
        for (line in BanjoFile) {
            linex = toString(line)
            linex = trim(linex)
            if (x) {
                # currently in the list of top-scoring networks
                m = grep("Network .*, score: ([0-9\-\.]+)", linex, value = TRUE) 
                if (length(m) != 0) {
                    score = m[1]
                    if (length(edges) >= 1) {
                        file.out.array = append(file.out.array, paste0(";", paste(edges, sep = ","), "\n"))
                    }
                    file.out.array = append(file.out.array, score)
                    edges = vector()
                    if (!(toString(score) %in% scores)) {  
                        scores[[toString(score)]] = 1
                    }
                    else {
                        scores[[toString(score)]] = scores[[toString(score)]] + 1
                    }
                }
                else if (length(grep("Search Statistics", linex, value = TRUE)) != 0) {
                    if (len(edges) >= 1) {
                        file.out.array = append(file.out.array, paste0(";", paste(edges, sep = ","), "\n"))
                        x = 0
                        break
                    }
                }    
                else {
                    m = grep("([\d]+) ([\d]+)", linex, value = TRUE)
                    if (m && x) { ## & - boolean vector, && - not boolean vector
                        if (length(m) >= 2) { 
                            entries = strsplit(linex, " ")
                            child = entries[-1]
                            for (parent in entries) {
                                edges = append(edges, paste0(parent, "->", child))
                            }
                        }
                    }
                }
            }
            else {
                m = grep("These are the ([0-9]+) ", linex, value = TRUE)
                if (length(m) != 0)
                {
                    netcount = m[1]
                }
                else {
                    m = grep("Best ([\d]+) Structures", linex, value = TRUE)
                    if (m) {
                        netcount = m[1]
                        x = 1
                    }
                    else {
                        m = grep("\- Final report", linex, value = TRUE)
                        if (length(m) != 0) {
                            x = 1
                        }
                    }
                }
            }
        }
    }
    scorex = names(scores)
    fileOut = file(outputfile, "wb")
    write.table(file = fileOut, x = paste(file.out.array, sep = ""))
    close(fileOut)
    return (list(scorex, netcount, scores))
}

reorderNetworks <- function(infile, currS) {
    # pass in regex excpression... write each network only if it matches the score regex
    fileHandle = file(infile, open = "r")
    fileInfo = readLines(fileHandle)
    close(fileHandle)
    returnArray = vector()
    for (i in fileInfo) {
        if (length(grep(currS, i, value = TRUE))) { 
            returnArray = append(returnArray, i)
        }
    }
    return (returnArray)
}

rewriteScoresFile <- function(infile, outfile, scores) {
    # function to parse the network/score file... wrapper for reorderNetworks funtion
    scoresSorted = scores
    allReturnedInfo = vector()
    for (currS in scoresSorted) {
        currS = paste0(sub("[\.]","\.",currS), ";")
        R = reorderNetworks(infile, currS)
        allReturnedInfo = append(allReturnedInfo, paste(R, sep = ""))
    }
    file.out = file(outfile, open = "wb")
    write.table(file = fileOut, x = paste(allReturnedInfo, sep = ""))
    close(file.out)
    return (1)
}

grabScores <- function(infile) {
    # function gets the parsed score information and returns the scores
    scores = vector()
    netInfo = openReportFile(infile)
    for (i in netInfo) {
        i = strsplit(x = i, split = ";")
        scores = append(scores, i[1])
    }
    return (scores)
}

getMaxScore <- function(listx) {
    # function grabs the max score and returns it    
    scores = vector()
    for (i in listx) {
        scores = append(scores, as.numeric(i))
    }
    return (max(scores))
}


bvalCalculator <- function(infile, e) {
    # function calculates the b-values    
    # initialize variables 
    all.scores = list()
    uniq.scores = list()
    combined_probabilities = 0.0
    score.changes = list()
    bval = vector()
    bval_mapper = list()
    # executable portion
    # first get the unique scores and max value 
    scores = grabScores(infile)
    for (score in scores) {
        all.scores[[score]] = 1
    }
    uniq.scores = names(all.scores)
    uniq.scores = sort(uniq.scores)
    maxscore = getMaxScore(uniq.scores)
    # Step one of computation... compute score change
    for (key in 1:len(uniq.scores)) {
        score.changes[[key]] = e^(as.numeric(uniq.scores[[key]]) - maxscore)
        combined_probabilities = combined_probabilities + score.changes[[key]]
    }
    #step 2: generate right-tail density for each score cutoff
    for (key in 1:len(uniq.scores)) {
        bval = 0.0
        for (key2 in rev(1:len(uniq.scores))) {
            if (as.numeric(uniq.scores[[key]]) > as.numeric(uniq.scores[[key2]])) {
                bval = bval + (score.changes[[key2]] / combined_probabilities)
            }
            else {
                break
            }
        }
        bval_mapper[toString(uniq.scores[[key]])] = bval
    }
    return (bval_mapper)
}


avalCalculator <- function(infile, e) {
    return (1 - bvalCalculator(infile, e))
}


percent_appear <- function(networks = list(), count = 0, edgePercent = 1.0) {
    # function generates the decisions for whether an edge should be included or not
    # major assumption is that an edge must be present with freq = 1.0 to include it 
    sort.edges = list() # tuple
    final_edges = vector()
    count = edgePercent * count
    for (edge in names(networks)){ # Networks.Key() fix b/c Python
        nodes = strsplit(x = edge, split = "->")
        nodes = order(nodes)
        edger = paste0(toString(nodes[1]),"->",toString(nodes[2]))
        sort.edges[[edger]] = 1
    }
    for (edge in names(sort.edges)) { # sort.edges.keys() fix b/c Python
        num.occur = 0
        nodes = strsplit(edge, "->")
        edge2 = paste0(toString(nodes[2]),"->",toString(nodes[1]))
        if (edge %in% networks) {
            num.occur = num.occur + as.numeric(networks[[edge]])
        }
        if (edge2 %in% networks) {
            num.occur = num.occur + as.numeric(networks[[edge2]])
        }
        if (num.occur >= count) {
            final_edges = append(final_edges, edge)
        }
    }
    return (final_edges)
}

bvalConsensus <- function(score, infile, edgePercent) {
    # function generates the consensus network given the b-values
    # note: function is a wrapper for the percent_appear function 
    consensus.edges = list() # tuple
    y = 0
    include_edges = list() # tuple
    z = 0
    fileInfo = openReportFile(infile)
    for (strings in fileInfo) {
        y = y + 1
        scorei = 0
        edgestr = ""
        strings = toString(strings)
        strings = trim(strings)
        if (string != "") {
            info  = strsplit(strings, ";")
            scorei = info[1]
            edgestr = info[2]
            if (scorei >= score) {
                z = z + 1
                edges = strsplit(edgestr, ",")
                for (edge in edges) {
                    if (!(edge %in% names(include_edges))) {
                        include_edges[[edge]] = 0
                    }
                    include_edges[[edge]] = include_edges[[edge]] + 1
                }
            }
            else {
                consensus.edges[[score]] = ""
                consensus.edges[[score]] = percent_appear(include_edges, z, edgePercent) 
            }
        }
        else if (!(score %in% names(consensus.edges))) {
            consensus.edges[[score]] = percent_appear(include_edges, z, edgePercent)
        }
    }
    return (consensus.edges[[score]])
}

readEdgeInformation <- function(filename, nodes, writer) {
    # function generates the formatted information for the c-value output
    cutoff.array = list()
    include.edges = vector()
    lines = openReportFile(filename)
    strings = lines[2] # line 0 has the header, while line 1 has the top-level network
    strings = trim(strings)
    info = strsplit(strings, "    ")
    edgesx = info[3]
    edges = strsplit(edgesx, ",")
    for (edge in edges) {
        edger = strsplit(edge, "->")
        cutoff.array[[edge]] = c(info[2], nodes[numeric(edger[1])], nodes[numeric(edger[2])], 1, 1) 
        include.edges = append(include.edges, edge)
    }
    print (cutoff.array)
    for (i in 3:length(lines)) {
        strings = lines[i]
        strings = trim(strings)
        if (strings != "") {
            vals = strsplit(strings, "\t")
            if (length(vals) == 3) {
                bval = vals[2]
                current.edges = vals[3]
                print (paste0(bval, "\t", current.edges, "\n"))
                for (edge in include.edges) {
                    if (cutoff.array[edge][4] == 0 && !edge %in% current.edges) {
                        cutoff.array[edge][4] = 1
                        cutoff.array[edge][5] = bval
                    }
                }
            }
            else {
                break
            }
        }
    }
    return (cutoff.array) # Returns a list
}

bvalWrapper <- function(outputBvals, bvalFile, netfile2, edgePercent) {
    # main wrapper for b-value computation and output 
    ouput.array = vector()
    scores.n = vector()
    #scores.n <- outputBvals.keys()
    scores.n = names(outputBvals) # outputBvals --> list
    scores.n = sort(scores.n)
    for (score in scores.n) {
        edge.info = bval.consensus(toString(score), netfile2, edgePercent)
        stringx = paste0(toString(score), "\t", toString(outputBvals[[toString(score)]]), "\t",
                          paste(edge.info, sep = ","), "\n")
        output.array = append(output.array, stringx)
    }
    fout = file(bvalFile, open = "wb")
    write.table(file = fout, x = "Score\tBval\tEdges\n")
    write.table(file = fout, x = paste(outputArray, sep = ""), "\n")
    close(fout)
    return (1)
}

cvalWrapper <- function(bvalFile, cvalFile, nodes) {
    # main wrapper for c-value computation and output  
    ouput.array = vector()
    output.cvalues = readEdgeInformation(bvalFile, nodes) # list
    for (edge in names(output.cvalues)) {
        output.array = append(output.array, paste0(edge, "\t", paste(ouput.cvalues[[edge]], sep = "\t"), "\n"))
    }
    fHandleCval = file(cvalFile, open = "wb")
    write.table(file = fHandleCval, x = paste(outputArray, sep = ""), "\n")
    close(fileHandleCval)
    return (1)
}




