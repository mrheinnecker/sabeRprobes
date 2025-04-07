## fncts for sabeRprobes package
#library(Biostrings)


align_yeast <- function(seq){
  
  pattern <- "AAAAAGCTCGTAGTTGGATTTCTGCTGAGGATGACCGGTCC"
  
  ### this is how we can do alignment inside R
  rRNA_s_cerevisiae="TATCTGGTTGATCCTGCCAGTAGTCATATGCTTGTCTCAAAGATTAAGCCATGCATGTCTAAGTATAAGCAATTTATACAGTGAAACTGCGAATGGCTCATTAAATCAGTTATCGTTTATTTGATAGTTCCTTTACTACATGGTATAACTGTGGTAATTCTAGAGCTAATACATGCTTAAAATCTCGACCCTTTGGAAGAGATGTATTTATTAGATAAAAAATCAATGTCTTCGGACTCTTTGATGATTCATAATAACTTTTCGAATCGCATGGCCTTGTGCTGGCGATGGTTCATTCAAATTTCTGCCCTATCAACTTTCGATGGTAGGATAGTGGCCTACCATGGTTTCAACGGGTAACGGGGAATAAGGGTTCGATTCCGGAGAGGGAGCCTGAGAAACGGCTACCACATCCAAGGAAGGCAGCAGGCGCGCAAATTACCCAATCCTAATTCAGGGAGGTAGTGACAATAAATAACGATACAGGGCCCATTCGGGTCTTGTAATTGGAATGAGTACAATGTAAATACCTTAACGAGGAACAATTGGAGGGCAAGTCTGGTGCCAGCAGCCGCGGTAATTCCAGCTCCAATAGCGTATATTAAAGTTGTTGCAGTTAAAAAGCTCGTAGTTGAACTTTGGGCCCGGTTGGCCGGTCCGATTTTTTCGTGTACTGGATTTCCAACGGGGCCTTTCCTTCTGGCTAACCTTGAGTCCTTGTGGCTCTTGGCGAACCAGGACTTTTACTTTGAAAAAATTAGAGTGTTCAAAGCAGGCGTATTGCTCGAATATATTAGCATGGAATAATAGAATAGGACGTTTGGTTCTATTTTGTTGGTTTCTAGGACCATCGTAATGATTAATAGGGACGGTCGGGGGCATCAGTATTCAATTGTCAGAGGTGAAATTCTTGGATTTATTGAAGACTAACTACTGCGAAAGCATTTGCCAAGGACGTTTTCATTAATCAAGAACGAAAGTTAGGGGATCGAAGATGATCAGATACCGTCGTAGTCTTAACCATAAACTATGCCGACTAGGGATCGGGTGGTGTTTTTTTAATGACCCACTCGGCACCTTACGAGAAATCAAAGTCTTTGGGTTCTGGGGGGAGTATGGTCGCAAGGCTGAAACTTAAAGGAATTGACGGAAGGGCACCACCAGGAGTGGAGCCTGCGGCTTAATTTGACTCAACACGGGGAAACTCACCAGGTCCAGACACAATAAGGATTGACAGATTGAGAGCTCTTTCTTGATTTTGTGGGTGGTGGTGCATGGCCGTTCTTAGTTGGTGGAGTGATTTGTCTGCTTAATTGCGATAACGAACGAGACCTTAACCTACTAAATAGTGGTGCTAGCATTTGCTGGTTATCCACTTCTTAGAGGGACTATCGGTTTCAAGCCGATGGAAGTTTGAGGCAATAACAGGTCTGTGATGCCCTTAGACGTTCTGGGCCGCACGCGCGCTACACTGACGGAGCCAGCGAGTCTAACCTTGGCCGAGAGGTCTTGGTAATCTTGTGAAACTCCGTCGTGCTGGGGATAGAGCATTGTAATTATTGCTCTTCAACGAGGAATTCCTAGTAAGCGCAAGTCATCAGCTTGCGTTGATTACGTCCCTGCCCTTTGTACACACCGCCCGTCGCTAGTACCGATTGAATGGCTTAGTGAGGCCTCAGGATCTGCTTAGAGAAGGGGGCAACTCCATCTCAGAGCGGAGAATTTGGACAAACTTGGTCATTTAGAGGAACTAAAAGTCGTAACAAGGTTTCCGTAGGTGAACCTGCGGAAGGATCATTA"
  
  SCER <- "TATCGTCAAGTTGTTTTTCTAGCTAGATCGATGATCGATAGCTAGAGAGGATATATGCGCGCTATAGTA"
  
  seq1 <-  rel_seq[[1]]
  seq2 <-  rel_seq[[2]]
  
  alignment <- pairwiseAlignment(seq1, seq2, type = "local")
  print(alignment)
  
}







# Load necessary libraries
#library(Biostrings)
#library(DECIPHER)

# Assuming your sequences are stored in a FASTA file
#fasta_file <- "your_sequences.fasta"

# Read the sequences
#seqs <- rel_seq

# Perform multiple sequence alignment (if not aligned)


make_consensus_sequence <- function(all_seq){
  
  aligned <- AlignSeqs(all_seq)
  cons_matrix <- consensusMatrix(aligned, baseOnly = FALSE)
  
  consensus_seq <- apply(cons_matrix[c("A", "C", "G", "T"),], 2, function(col) {
  
    max_pos <- which.max(col) %>% .[1]
    
    max_base <- names(max_pos)
    complement <- c(1:4) %>% .[which(.!=max_pos)]
    
    return(c(base=max_base, 
             prob=as.numeric(col[max_pos]/sum(col))))
    
  }, simplify=F) %>%
    bind_rows() %>%
    mutate(pos=seq(1,nrow(.)),
           prob=as.numeric(prob))
  
  return(consensus_seq)
}

mine_probes_from_sequence <- function(char_seq, probe_length, len, len_range){
  
  lapply(seq(1, nchar(char_seq)-probe_length), function(START){
    #cat(START, "\n")
    tibble(start=START,
           seq=lapply(seq(as.numeric(len)-as.numeric(len_range),
                          as.numeric(len)+as.numeric(len_range)),
                      function(x){
                        str_sub(char_seq, START, START+x-1)
                      }
                      
                      
           ) %>% unlist()
           
    ) %>%
      return()
    
  }) %>% bind_rows() %>%
    return()
  
}


group_sequences <- function(df, max_gap = 5) {
  # Sort by start position
  df <- df[order(df$start), ]
  
  # Initialize group assignment and tracking of last end positions
  df$group <- NA
  group_end <- numeric(0)  # Stores the latest `end` per group
  
  for (i in seq_len(nrow(df))) {
    assigned <- FALSE
    
    # Try to assign the sequence to an existing group
    for (g in seq_along(group_end)) {
      if (df$start[i] > group_end[g] + max_gap) {
        df$group[i] <- g
        group_end[g] <- df$end[i]  # Update group's latest end position
        assigned <- TRUE
        break
      }
    }
    
    # If no existing group is available, create a new group
    if (!assigned) {
      df$group[i] <- length(group_end) + 1
      group_end <- c(group_end, df$end[i])
    }
  }
  
  return(df)
}



