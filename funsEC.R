rm(list = ls())

# For EC-L Only

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
    scores = vector()
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
            linex = trimp(linex)
            if (x) {
                # currently in the list of top-scoring networks
                m = grep("Network .*, score: ([0-9\-\.]+)", linex, value = TRUE) 
                if (length(m) != 0) {
                    score = m[1]
                    if (len(edges) >= 1) {
                        file.out.array = append(file.out.array, paste0(";", paste(edges, sep = ","), "\n"))
                    }
                    file.out.array = append(file.out.array, score)
                    edges = vector()
                    if (!(toString(score) %in% scores)) {  
                        scores[toString(score)] = 1
                    } # Python specific
                    else {
                        scores[toString(score)] = scores[toString(score)] + 1
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
    #scorex = scores.keys() #Python esque
    scorex = scores
    fileOut = file(outputfile, "wb")
    write.table(file = fileOut, x = paste(file.out.array, sep = ""))
    close(fileOut)
    return (list(scorex, netcount, scores))
}

reorderNetworks <- function(infile, currS, re) {
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

rewriteScoresFile <- function(infile, outfile, scores, re) {
    # function to parse the network/score file... wrapper for reorderNetworks funtion
    scoresSorted = scores
    allReturnedInfo = vector()
    for (currS in scoresSorted) {
        currS = paste0(sub("[\.]","\.",currS), ";")
        R = reorderNetworks(infile, currS, re)
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
        strsplit(x = i, split = ";")
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


percent_appear <- function(networks = vector(), count = 0, edgePercent = 1.0) {
    # function generates the decisions for whether an edge should be included or not
    # major assumption is that an edge must be present with freq = 1.0 to include it 
    sort.edges = vector() # tuple
    final_edges = vector()
    count = edgePercent * count
    for (edge in networks){ # Networks.Key() fix b/c Python
        nodes = strsplit(x = edge, split = "->")
        order(nodes)
        edger = paste0(toString(nodes[1]),"->",toString(nodes[2]))
        sort.edges[edger] = 1
    }
    for (edge in sort.edges) { # sort.edges.keys() fix b/c Python
        num.occur = 0
        nodes = strsplit(edge, "->")
        edge2 = paste0(toString(nodes[2]),"->",toString(nodes[1]))
        if (edge %in% networks) {
            num.occur = num.occur + as.numeric(networks[edge])
        }
        if (edge2 %in% networks) {
            num.occur = num.occur + as.numeric(networks[edge2])
        }
        if (num.occur >= count) {
            final_edges = append(final_edges, edge)
        }
    }
    return (final_edges)
}



readEdgeInformation <- function(filename, nodes, writer) {
    # function generates the formatted information for the c-value output
    cutoff.array = list()
    include.edges = vector()
    lines = openReportFile(filename)
    strings = lines[2] # line 0 has the header, while line 1 has the top-level network
    strings = trim(strings)
    info = strsplit(strings, "\t")
    edgesx = info[3]
    edges = strsplit(edgesx, ",")
    for (edge in edges) {
        edger = strsplit(edge, "->")
        cutoff.array[edge] = c(info[2], nodes[int(edger[1])], nodes[int(edger[2])], 1, 1) 
        include.edges = append(include.edges, edge)
    }
    print (cutoff.array)
    for (i in 2:length(lines)) {
        strings = lines[i]
        strings = trim(strings)
        if (strings != "") {
            vals = strsplit(strings, "\t")
            if (length(vals) == 3) {
                bval = vals[2]
                current.edges = vals[3]
                print (paste0(bval, "\t", current.edges, "\n"))
                for (edge in include.edges) {
                    if (cutoff.array[[edge]][[4]] == 0 && !edge %in% current.edges) {
                        cutoff.array[[edge]][[4]] = 1
                        cutoff.array[[edge]][[5]] = bval
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


cvalWrapper <- function(bvalFile, cvalFile, nodes) {
    # main wrapper for c-value computation and output  
    ouput.array = vector()
    output.cvalues = readEdgeInformation(bvalFile, nodes)
    for (edge in output.cvalues.keys()) {
        output.array = append(output.array, paste0(edge, "\t", paste(ouput.cvalues[edge], sep = "\t"), "\n"))
    }
    fHandleCval = file(cvalFile, open = "wb")
    write.table(file = fHandleCval, x = paste(outputArray, sep = ""), "\n")
    close(fileHandleCval)
    return (1)
}




