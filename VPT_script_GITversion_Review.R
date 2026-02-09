######################################################### VISUAL PERSPECTIVE TAKING SCRIPT ########################################################################

# this script uses R version 4.5.0
# script by Leonie Polzer
# latest changes on 09th February, 2026

## Description:
# data information: Visual perspective taking task (see Samson et al., 2010)
# conditions: Perspective (self/other) x Perspective Congruency (congruent/incongruent) x cue validity (correct/incorrect)
# this script compares Behavioral Data, ERPs (P200, P3, LFSW) and pupil data between autistic and non-autistic participants
# EEG data have been preprocessed in BrainVision Analyzer 2.3

## Session Preparation
# clear working space
  rm(list=ls())
# load packages
  require(tidyr)
  require(tidyverse)
  require(ggplot2)
  require(readxl)
  require(dplyr)
  require(lme4)
  require(lmerTest)
  require(emmeans)
  require(viridis) 
  require(ggpubr)
  require(zoo) 
  require(pbapply)
  require(officer)
  require(simr)
  library(purrr)
  library(tibble)

####################################################################################################################################################################
#### 0. DEMOGRAPHICS ##############################################################################################################################################
## Read & preprocess demographics data
  setwd("C:/Users/Leonie/PowerFolders/VPT/demographics")
  ADI<-read_xlsx("ADI-R.xlsx") # ADI assessed within STIPED
  ADOS2_M2<-read_xlsx("ADOS_2_M2_WPS.xlsx") 
  ADOS2_M3<-read_xlsx("ADOS_2_M3_G_WPS.xlsx") 
  ADOS2_M4<-read_xlsx("ADOS_2_M4_WPS.xlsx")
  ADOS1_M1<-read_xlsx("ADOS_M1_G_WPS.xlsx") 
  ADOS1_M2<-read_xlsx("ADOS_M2_WPS.xlsx") 
  ADOS1_M3<-read_xlsx("ADOS_M3_G_WPS.xlsx") 
  CBCL<-read_xlsx("CBCL_4_18.xlsx")
  SRS<-read_xlsx("SRS.xlsx")
  ADHS<-read_xlsx("FBB-HKS.xlsx")
  Stammdaten<-read_xlsx("Checkliste.xlsx")
  handedness<-read_xlsx("Haendigkeit.xlsx")
  HAWIK<-read_xlsx("HAWIK-III.xlsx")
  Wais<-read_xlsx("Wais_III.xlsx")
  Wie<-read_xlsx("WIE.xlsx")

## participant overview (Stammdaten) - rename ids
  names(Stammdaten)[names(Stammdaten)=="ID_Studie"]<-"id"
  Stammdaten$id<-toupper(Stammdaten$id)
  Stammdaten$id<-gsub("-", "_", Stammdaten$id)
  Stammdaten$id<-gsub("ASS", "AUT", Stammdaten$id)
  Stammdaten$id<-gsub("F_AUT", "AUT_F", Stammdaten$id)
  Stammdaten<-Stammdaten[,c("ID_Bado", "id", "Geburt_Index", "Geschlecht_Index")]

## ADI 
  ADI<-ADI[,c("ID_Bado", "ADI_alg_SOZ_INT", "ADI_alg_KOM", "ADI_alg_RITUALE", "ADI_alg_ABNORM_ENTW")]

## ADOS 
# create matching severity labels
# ADOS-1
  names(ADOS1_M1)[names(ADOS1_M1)=="Severity_Gesamt"]<-"Severity_Gesamt_ADOS1_M1"
  names(ADOS1_M2)[names(ADOS1_M2)=="Severity_Gesamt"]<-"Severity_Gesamt_ADOS1_M2"
  names(ADOS1_M3)[names(ADOS1_M3)=="Severity_Gesamt"]<-"Severity_Gesamt_ADOS1_M3"
# ADOS-2
  names(ADOS2_M2)[names(ADOS2_M2)=="ADOS_2_M2_comp_Score"]<-"Severity_Gesamt_ADOS2_M2"
  names(ADOS2_M3)[names(ADOS2_M3)=="Severity_Gesamt"]<-"Severity_Gesamt_ADOS2_M3"
  names(ADOS2_M4)[names(ADOS2_M4)=="Severity_Gesamt"]<-"Severity_Gesamt_ADOS2_M4"
# match date and age labels
# ADOS-1
  names(ADOS1_M1)[names(ADOS1_M1)=="DatumADOS_M1_G_WPS"]<-"date_ADOS1_M1"
  names(ADOS1_M1)[names(ADOS1_M1)=="ADOS_M1_G_WPS_alter"]<-"age_ADOS1_M1"
  names(ADOS1_M2)[names(ADOS1_M2)=="DatumADOS_M2"]<-"date_ADOS1_M2"
  names(ADOS1_M2)[names(ADOS1_M2)=="ADOS_M2alter"]<-"age_ADOS1_M2"
  names(ADOS1_M3)[names(ADOS1_M3)=="DatumADOS_M3"]<-"date_ADOS1_M3"
  names(ADOS1_M3)[names(ADOS1_M3)=="ADOS_M3alter"]<-"age_ADOS1_M3"
# ADOS-2
  names(ADOS2_M2)[names(ADOS2_M2)=="DatumADOS_2_M2"]<-"date_ADOS2_M2"
  names(ADOS2_M2)[names(ADOS2_M2)=="ADOS_2_M2alter"]<-"age_ADOS2_M2"
  names(ADOS2_M3)[names(ADOS2_M3)=="DatumADOS_2_M3"]<-"date_ADOS2_M3"
  names(ADOS2_M3)[names(ADOS2_M3)=="ADOS_2_M3alter"]<-"age_ADOS2_M3"
  names(ADOS2_M4)[names(ADOS2_M4)=="DatumADOS_2_M4"]<-"date_ADOS2_M4"
  names(ADOS2_M4)[names(ADOS2_M4)=="ADOS_2_M4alter"]<-"age_ADOS2_M4"
# exclude unnecessary variables
  ADOS1_M1<-ADOS1_M1[,c("ID_Bado", "date_ADOS1_M1", "age_ADOS1_M1", "Severity_Gesamt_ADOS1_M1")]
  ADOS1_M2<-ADOS1_M2[,c("ID_Bado", "date_ADOS1_M2", "age_ADOS1_M2", "Severity_Gesamt_ADOS1_M2")]
  ADOS1_M3<-ADOS1_M3[,c("ID_Bado", "date_ADOS1_M3", "age_ADOS1_M3", "Severity_Gesamt_ADOS1_M3")]
  ADOS2_M2<-ADOS2_M2[,c("ID_Bado", "date_ADOS2_M2", "age_ADOS2_M2", "Severity_Gesamt_ADOS2_M2")]
  ADOS2_M3<-ADOS2_M3[,c("ID_Bado", "date_ADOS2_M3", "age_ADOS2_M3", "Severity_Gesamt_ADOS2_M3")]
  ADOS2_M4<-ADOS2_M4[,c("ID_Bado", "date_ADOS2_M4", "age_ADOS2_M4", "Severity_Gesamt_ADOS2_M4")]

#exclude first ADOS by ID_Bado 11199-2015 (participant had a diagnostic ADOS-1, and a later study-ADOS-2)
  ADOS1_M2<-ADOS1_M2[!(ADOS1_M2$ID_Bado=="11199-2015"),]

# merge ADOS values
  df_list <- list(ADOS1_M1, ADOS1_M2, ADOS1_M3, ADOS2_M2, ADOS2_M3, ADOS2_M4)
  
#merge all data frames in list
  ADOS<- df_list %>% reduce(full_join, by='ID_Bado')

#get dates into long format
  ADOS<-as.data.frame(pivot_longer(ADOS, cols=c("date_ADOS1_M1", "date_ADOS1_M2",  "date_ADOS1_M3",  
                                                "date_ADOS2_M2", "date_ADOS2_M3", "date_ADOS2_M4"),
                                   names_to='version_module_date',values_to='date', values_drop_na = T))
#get age into long format
  ADOS<-as.data.frame(pivot_longer(ADOS, cols=c("age_ADOS1_M1", "age_ADOS1_M2",  "age_ADOS1_M3",  
                                                "age_ADOS2_M2", "age_ADOS2_M3", "age_ADOS2_M4"),
                                   names_to='version_module_age',values_to='age_ADOS', values_drop_na = T))
#get severity into long format
  ADOS<-as.data.frame(pivot_longer(ADOS, cols=c("Severity_Gesamt_ADOS1_M1", "Severity_Gesamt_ADOS1_M2",  "Severity_Gesamt_ADOS1_M3",  
                                                "Severity_Gesamt_ADOS2_M2", "Severity_Gesamt_ADOS2_M3", "Severity_Gesamt_ADOS2_M4"),
                                   names_to='version_module_severity',values_to='Severity', values_drop_na = T))

## IQ
# extract relevant variables
  HAWIK<-HAWIK[,c("ID_Bado", "Hawik_III_IQ_P", "HAWIK_III_alter")]
  Wais<-Wais[,c("ID_Bado", "Wais_III_IQ_P", "Wais_III_alter")]
  Wie<-Wie[,c("ID_Bado", "WIE_III_IQ_P", "WIE_alter")]
# exclude 14983-2019 in WAIS, had HAWIK (=WISC) done
  Wais<-Wais[!(Wais$ID_Bado=="14983-2019"),]
# merge HAWIK, Wais and Wie
  df_list <- list(HAWIK, Wais, Wie)
# merge all data frames in list
  IQ<-df_list %>% reduce(full_join, by='ID_Bado')
  IQ<-as.data.frame(pivot_longer(IQ, cols=c("Hawik_III_IQ_P", "Wais_III_IQ_P",  "WIE_III_IQ_P"),
                                 names_to='IQ_test',values_to='IQ', values_drop_na = T))
  IQ<-as.data.frame(pivot_longer(IQ, cols=c("HAWIK_III_alter", "Wais_III_alter",  "WIE_alter"),
                                 names_to='IQ_test_age',values_to='age', values_drop_na = T))
  IQ<-IQ[,c("ID_Bado", "IQ_test", "IQ", "age")]

## CBCL (general psychopathology)
  CBCL<-CBCL[!(CBCL$ID_Bado=="11014-2015" & (!CBCL$Meßzeit_CBCL=="T2")),]
  CBCL<-CBCL[,c("ID_Bado","CBCL_T_INT", "CBCL_T_EXT", "CBCL_T_GES")]

## handedness questionnaire
  handedness<-handedness[,c("ID_Bado","EHI_Score_L", "EHI_Score_R","LQ")]
  
### SRS ###
  SRS<-SRS[!(SRS$ID_Bado=="11014-2015" & (!SRS$Meßzeit_SRS=="T2")),] #exlude time points != T2 for 11014-2015
  SRS<-SRS[!(SRS$ID_Bado=="13706-2017" & (!SRS$Datum_SRS=="11.07.2019")),] #exlude time points != T2 for 11014-2015
  SRS<-SRS[,c("ID_Bado","Gesamtwert_N_k_RW","Gesamt_TW_AB")]
  
### ADHD ###
  ADHS<-ADHS[!(ADHS$ID_Bado=="11014-2015" & (!ADHS$Meßzeit_FBB_HKS=="T2")),]
  ADHS<-ADHS[!(ADHS$ID_Bado=="11425-2015" & (!ADHS$Meßzeit_FBB_HKS=="T2")),]
  ADHS<-ADHS[,c("ID_Bado", "ADHS_aufm", "ADHS_hyperak", "ADHS_impul","ADHS_Hinweis_Diag_ICD","ADHS_Hinweis_Diag_DSM","ADHs_ges")]

#merge datasets into demographics file
  df_list <- list(Stammdaten, ADI, ADOS, IQ, CBCL, handedness, SRS, ADHD)
#merge all data frames in list
  demogr<-df_list %>% reduce(full_join, by='ID_Bado')
#note: no ados severity for 13404-2017, but confirmed diagnosis through ADOS (calculation of CSS not possible in M3 for 18 year olds)

#check participants with ADOS score below 4 -> will be exlcuded later on
  ADOS$ID_Bado[ADOS$Severity<4]
  ADOS_excl<-demogr$id[demogr$Severity<4]

########################################################### Group matching ########################################################################

## use aggregated data for one ERP to get set of IDs for which EEG was recorded
  setwd(".../VPT/export")
  P2_mean<-read.table("Mean_p200.txt", header=T)
  #separately for F-AUT-001 because of export issues
  P2_mean_F_AUT_001<-read.table("Mean_p200_F_AUT_001.txt", header=T)
  #bind together
  P2_mean<-rbind(P2_mean, P2_mean_F_AUT_001)
# clean ID names: delete dates, testing times etc.
  colnames(P2_mean)[1]<-"id"
  P2_mean$id<-gsub("\\_VP.*","",P2_mean$id)
  P2_mean$id<-gsub("\\_Posner.*","",P2_mean$id)
  P2_mean$id<-gsub("\\_T2.*","",P2_mean$id)
  P2_mean$id<-toupper(P2_mean$id)
  P2_mean$id<-gsub("-", "_", P2_mean$id)
  P2_mean$id<-gsub("ASS", "AUT", P2_mean$id)
  P2_mean$id<-gsub("F_AUT", "AUT_F", P2_mean$id)
  P2_mean$id<-gsub("F_AUT", "AUT_F", P2_mean$id)
  P2_mean$id[P2_mean$id=="QS_A_001"]<-"SOKO_AUT_001"
# check if all EEG participants are in demographics file
  which(P2_mean$id %in% demogr$id == FALSE)

# exclusion of participants
  #exclude participants without EEG data from demographics file 
  demogr<-demogr[demogr$id %in% P2_mean$id,]
  #exclude participants with too low ADOS score
  demogr<-demogr[!demogr$id %in% ADOS_excl,]
  
# define group variable
  demogr$group<-"ASD"
  demogr$group[grep("HC",demogr$id)]<-"NT"

## Matching
require(MatchIt)
# apply full matching
  all.match<-matchit(as.factor(group)~IQ,
                     data=demogr,
                     method='full', caliper=0.9)
  all.match<-match.data(all.match)
  table(all.match$group)
  #check differences
  t.test(all.match$IQ[all.match$group=="NT"], all.match$IQ[all.match$group=="ASD"], paired=F, alternative = "two.sided")
# exclude non-matched participants from demographics
  demogr<-demogr[demogr$id %in% all.match$id,]

## Demographics: test group differences
# number participants
  table(demogr$group)
# sex
  table(demogr$group, demogr$Geschlecht_Index)
  chisq.test(table(demogr$group, demogr$Geschlecht_Index))
# ADHD
  table(demogr$ADHS_Hinweis_Diag_DSM, demogr$group)
  table(demogr$ADHS_Hinweis_Diag_ICD, demogr$group)
# get statistics for dimensional variables
  by(demogr, demogr$group, psych::describe)
# age
  t.test(demogr$age[demogr$group=="ASD"], demogr$age[demogr$group=="NT"], paired=F, alternative = "two.sided")
# IQ
  t.test(demogr$IQ[demogr$group=="ASD"], demogr$IQ[demogr$group=="NT"], paired=F, alternative = "two.sided")
# CBCL
  t.test(demogr$CBCL_T_INT[demogr$group=="ASD"], demogr$CBCL_T_INT[demogr$group=="NT"], paired=F, alternative = "two.sided")
  t.test(demogr$CBCL_T_EXT[demogr$group=="ASD"], demogr$CBCL_T_EXT[demogr$group=="NT"], paired=F, alternative = "two.sided")
  t.test(demogr$CBCL_T_GES[demogr$group=="ASD"], demogr$CBCL_T_GES[demogr$group=="NT"], paired=F, alternative = "two.sided")
# handedness
  t.test(demogr$LQ[demogr$group=="ASD"], demogr$LQ[demogr$group=="NT"], paired=F, alternative = "two.sided")
# SRS
  t.test(demogr$Gesamt_TW_AB[demogr$group=="ASD"], demogr$Gesamt_TW_AB[demogr$group=="NT"])
  

#### 1. EEG DATA ##################################################################################################################################################
### Figure 2: Plot time-course of ERPs #####
  # uses per-time-point output of aggregated activity across respective electrodes from BrainVision Analyzer
  
  rm(list = ls())
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(stringr)
    library(patchwork)
  })
  
  clean_id <- function(x) {
    x <- gsub("\\_VP.*",    "", x)
    x <- gsub("\\_Posner.*","", x)
    x <- gsub("\\_T2.*",    "", x)
    x <- toupper(x)
    x <- gsub("-", "_", x)
    x <- gsub("ASS", "AUT", x)
    x <- gsub("F_AUT", "AUT_F", x)
    x[x == "QS_A_001"] <- "SOKO_AUT_001"
    x
  }
  
  
  # =========================
  # PATHS
  # =========================
  in_dir  <- "S:/KJP_Neurophys/Studien/STIPED/WP5 Querschnitt (SoKoASS)/6_Projekte/2_VPT/Auswertung/1_Auswertung EEG Daten/export" 
  out_dir <- file.path(in_dir, "figs_R")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  load("S:/KJP_Neurophys/Studien/STIPED/WP5 Querschnitt (SoKoASS)/6_Projekte/2_VPT/Auswertung/4_Statistische Auswertung/saved_R_files/demogr")
  
  # Make sure we have an id column
  stopifnot(exists("demogr"))
  stopifnot("id" %in% names(demogr))
  
  demogr <- demogr %>%
    mutate(id = clean_id(as.character(id)))
  
  keep_ids <- unique(demogr$id)
  message("Demographics IDs (keep): ", length(keep_ids))
  
  
  # =========================
  # STYLE 
  # =========================
  cols_cong <- c("congruent" = "#11A57D", "incongruent" = "#D6A633")
  
  theme_erp <- theme_classic(base_size = 17) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "lightgrey", color = NA),
      strip.text = element_text(size = 14, face = "plain"),
      
      axis.title = element_text(size = 14, face = "plain"),  # <- NOT bold
      axis.text  = element_text(size = 12),
      
      plot.title = element_text(size = 16, face = "bold"),
      text = element_text(family = "sans"),
      
      # remove grey legend key boxes (linetype legend etc.)
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )
  
  
  # =========================
  # HELPERS: read BrainVision header (.vhdr from "Data Exchange")
  # =========================
  read_bv_header <- function(vhdr_path) {
    x <- readLines(vhdr_path, warn = FALSE, encoding = "UTF-8")
    x <- str_trim(x)
    x <- x[x != ""]
    # drop comment lines
    x <- x[!str_starts(x, ";")]
    
    kv <- x[str_detect(x, "=")]
    if (length(kv) == 0) return(tibble(key = character(), val = character()))
    
    tibble(
      key = str_trim(str_extract(kv, "^[^=]+")),
      val = str_trim(str_replace(kv, "^[^=]+=", ""))
    )
  }
  
  get_hdr_val <- function(hdr_tbl, key) {
    i <- match(key, hdr_tbl$key)
    if (is.na(i)) return(NA_character_)
    hdr_tbl$val[i]
  }
  
  # =========================
  # HELPERS: parse filename condition info
  # Example:
  # SoKo-HC-059_VPT_15122023_other_incongruent_valid_correct_P3_progression.dat
  # AUT-F-003_T2_VPT_other_incongruent_valid_correct_LFSW_progression.dat
  # =========================
  parse_from_filename <- function(fname_no_ext) {
    parts <- strsplit(fname_no_ext, "_", fixed = TRUE)[[1]]
    
    id <- parts[1]
    
    perspective <- parts[parts %in% c("self", "other")]
    congruency  <- parts[parts %in% c("congruent", "incongruent")]
    validity    <- parts[parts %in% c("valid", "invalid")]
    correctness <- parts[parts %in% c("correct", "incorrect")]
    
    erp <- parts[parts %in% c("P200", "P3", "LFSW")]
    erp <- if (length(erp) > 0) erp[1] else NA_character_
    
    group <- NA_character_
    if (str_detect(id, "AUT|ASD")) group <- "ASD"
    if (str_detect(id, "HC|NT"))   group <- "NT"
    # fallback: sometimes "AUT"/"HC" is elsewhere
    if (is.na(group)) {
      if (str_detect(fname_no_ext, "AUT|ASD")) group <- "ASD"
      if (str_detect(fname_no_ext, "HC|NT"))   group <- "NT"
    }
    
    tibble(
      id = id,
      group = factor(group, levels = c("ASD", "NT")),
      perspective = factor(if (length(perspective) > 0) perspective[1] else NA_character_,
                           levels = c("other","self")),
      congruency = factor(if (length(congruency) > 0) congruency[1] else NA_character_,
                          levels = c("congruent","incongruent")),
      validity = factor(if (length(validity) > 0) validity[1] else NA_character_,
                        levels = c("valid","invalid")),
      correctness = factor(if (length(correctness) > 0) correctness[1] else NA_character_),
      erp = erp
    )
  }
  
  # =========================
  # HELPERS: read .dat in a robust way
  # Uses VHDR:
  # - DecimalSymbol
  # - SkipColumns (here usually 1: first column is channel name)
  # - DataPoints
  # - NumberOfChannels
  # - SamplingInterval (microseconds)
  # - Channel Infos (Ch1=..., Ch2=...)
  # =========================
  extract_channel_names <- function(hdr_tbl) {
    # Pull lines like "Ch1=NAME,..."
    ch_rows <- hdr_tbl %>%
      filter(str_detect(key, "^Ch\\d+$")) %>%
      mutate(ch_idx = as.integer(str_remove(key, "^Ch")))
    
    if (nrow(ch_rows) == 0) return(character())
    
    # Channel info is "NAME,<ref>,<res>,<unit>..." -> we want NAME
    ch_names <- ch_rows %>%
      arrange(ch_idx) %>%
      mutate(name = str_trim(str_split_fixed(val, ",", n = 2)[,1])) %>%
      pull(name)
    
    ch_names
  }
  
  read_dat_from_header <- function(dat_path, hdr_tbl) {
    dp <- suppressWarnings(as.integer(get_hdr_val(hdr_tbl, "DataPoints")))
    nc <- suppressWarnings(as.integer(get_hdr_val(hdr_tbl, "NumberOfChannels")))
    decsym <- get_hdr_val(hdr_tbl, "DecimalSymbol")
    skip_cols <- suppressWarnings(as.integer(get_hdr_val(hdr_tbl, "SkipColumns")))
    if (is.na(skip_cols)) skip_cols <- 1
    
    if (is.na(dp) || dp <= 0) stop("Header missing/invalid DataPoints for: ", dat_path)
    if (is.na(nc) || nc <= 0) stop("Header missing/invalid NumberOfChannels for: ", dat_path)
    
    # Read all lines, one per channel (vectorized, each line: label + dp numbers)
    x <- readLines(dat_path, warn = FALSE, encoding = "UTF-8")
    x <- str_trim(x)
    x <- x[x != ""]
    if (length(x) == 0) return(tibble())
    
    # Some exports might include more than nc non-empty lines; keep first nc
    if (length(x) >= nc) x <- x[1:nc]
    
    # Channel names from header are authoritative; if missing, use first token in the line
    ch_names <- extract_channel_names(hdr_tbl)
    if (length(ch_names) != nc) ch_names <- rep(NA_character_, nc)
    
    parse_line <- function(line, ch_fallback) {
      parts <- str_split(line, "\\s+", simplify = TRUE)
      parts <- parts[parts != ""]
      if (length(parts) < (1 + dp)) {
        warning("Fewer values than DataPoints in: ", basename(dat_path))
      }
      
      # First token is label if SkipColumns==1
      label <- parts[1]
      nums  <- parts[(1 + skip_cols):length(parts)]
      if (length(nums) == 0) return(tibble())
      
      # decimal symbol handling
      if (!is.na(decsym) && decsym == ",") nums <- str_replace_all(nums, ",", ".")
      y <- suppressWarnings(as.numeric(nums))
      if (all(is.na(y))) return(tibble())
      
      # keep indices aligned even if a few values are NA
      tibble(
        channel = if (!is.na(ch_fallback)) ch_fallback else label,
        idx = seq_along(y),
        amp = y
      )
    }  
    
    out <- map2_dfr(x, ch_names, parse_line)
    
    # enforce dp if possible (trim or keep as-is)
    out <- out %>%
      filter(idx <= dp) %>%    # enforce header-defined length
      select(channel, idx, amp)
    
    
    out
  }
  
  make_time_vector <- function(hdr_tbl, n_points, t0_ms = -500) {
    si_us <- suppressWarnings(as.integer(get_hdr_val(hdr_tbl, "SamplingInterval")))
    if (is.na(si_us) || si_us <= 0) stop("Header missing/invalid SamplingInterval.")
    
    dt_ms <- si_us / 1000  # 2000 us -> 2 ms
    t0_ms + dt_ms * (0:(n_points - 1))
  }
  
  # =========================
  # 1) COLLECT FILES
  # =========================
  dat_files <- list.files(in_dir, pattern = "progression.*\\.(dat|DAT)$", full.names = TRUE)
  if (length(dat_files) == 0) stop("No progression .dat files found in: ", in_dir)
  
  vhdr_files <- str_replace(dat_files, "\\.(dat|DAT)$", ".vhdr")
  ok <- file.exists(vhdr_files)
  
  message("Found DAT files: ", length(dat_files))
  message("With matching VHDR: ", sum(ok))
  
  dat_files  <- dat_files[ok]
  vhdr_files <- vhdr_files[ok]
  
  ## print time summary
  time_summary <- map2_dfr(dat_files, vhdr_files, function(dpath, hpath) {
    hdr  <- read_bv_header(hpath)
    dp   <- suppressWarnings(as.integer(get_hdr_val(hdr, "DataPoints")))
    si   <- suppressWarnings(as.integer(get_hdr_val(hdr, "SamplingInterval")))  # us
    dtms <- si/1000
    tmin <- -500
    tmax <- tmin + dtms*(dp-1)
    
    tibble(
      file = basename(dpath),
      DataPoints = dp,
      SamplingInterval_us = si,
      dt_ms = dtms,
      tmin_ms = tmin,
      tmax_ms = tmax
    )
  })
  
  print(time_summary %>% distinct(DataPoints, SamplingInterval_us, dt_ms, tmin_ms, tmax_ms))
  
  
  # =========================
  # 2) LOAD ALL FILES -> LONG TABLE
  # =========================
  df_all <- map2_dfr(dat_files, vhdr_files, function(dpath, hpath) {
    fname <- basename(dpath)
    fname_no_ext <- str_remove(fname, "\\.[^.]+$")
    
    meta_fn <- parse_from_filename(fname_no_ext)
    
    hdr <- read_bv_header(hpath)
    
    # data + time
    dat_long <- read_dat_from_header(dpath, hdr)
    if (nrow(dat_long) == 0) return(NULL)
    
    # time from header (and check dp vs actual)
    n_points <- max(dat_long$idx, na.rm = TRUE)
    time_ms  <- make_time_vector(hdr, n_points = n_points, t0_ms = -500)
    
    # hemisphere from channel name if present
    dat_long %>%
      mutate(
        file = fname,
        time_ms = time_ms[idx],
        hemisphere = case_when(
          str_detect(channel, "_left$")  ~ "left",
          str_detect(channel, "_right$") ~ "right",
          TRUE ~ NA_character_
        )
      ) %>%
      bind_cols(meta_fn[rep(1, nrow(.)), ]) %>%
      # keep only useful columns
      select(file, id, group, perspective, congruency, validity, correctness, erp,
             channel, hemisphere, time_ms, amp)
  })
  
  if (nrow(df_all) == 0) stop("df_all is empty after loading. Check paths / matching headers.")
  
  # -------------------------
  # CLEAN IDs 
  # -------------------------
  df_all <- df_all %>%
    mutate(id = clean_id(as.character(id)))
  
  ids_before_filter <- unique(df_all$id)
  message("IDs in EEG export (after cleaning): ", length(ids_before_filter))
  
  # -------------------------
  # FILTER to matched demographics sample
  # -------------------------
  df_all <- df_all %>% filter(id %in% keep_ids)
  
  ids_after_filter <- unique(df_all$id)
  message("IDs after demogr filter: ", length(ids_after_filter))
  
  # -------------------------
  # REPORT DROPPED IDs (EEG present, but not in demogr)
  # -------------------------
  dropped_ids <- setdiff(ids_before_filter, keep_ids)
  
  if (length(dropped_ids) > 0) {
    message(
      "Dropped IDs (EEG export but not in demogr): ",
      paste(head(dropped_ids, 10), collapse = ", ")
    )
  } else {
    message("No dropped IDs (all EEG IDs are in demogr).")
  }
  
  
  # If demogr has group information, overwrite group in df_all from demogr
  if ("group" %in% names(demogr)) {
    demogr_small <- demogr %>% select(id, group) %>% distinct()
    
    df_all <- df_all %>%
      select(-group) %>%
      left_join(demogr_small, by = "id") %>%
      mutate(group = factor(group, levels = c("ASD","NT")))
  }
  
  
  # check if some files have more (or fewer) lines/channels than expected
  hdr_check <- map2_dfr(dat_files, vhdr_files, function(dpath, hpath) {
    hdr <- read_bv_header(hpath)
    nc  <- suppressWarnings(as.integer(get_hdr_val(hdr, "NumberOfChannels")))
    dp  <- suppressWarnings(as.integer(get_hdr_val(hdr, "DataPoints")))
    tibble(
      file = basename(dpath),
      NumberOfChannels = nc,
      DataPoints_hdr   = dp
    )
  })
  
  obs_check <- df_all %>%
    group_by(file) %>%
    summarise(
      channels_obs = n_distinct(channel),
      points_obs   = max(time_ms, na.rm = TRUE) - min(time_ms, na.rm = TRUE),
      n_obs_rows   = n(),
      .groups="drop"
    )
  
  check <- hdr_check %>%
    left_join(obs_check, by = "file") %>%
    mutate(
      rows_per_channel = n_obs_rows / channels_obs
    )
  
  print(check %>% arrange(desc(abs(channels_obs - NumberOfChannels))) %>% head(20))
  
  
  # =========================
  # 3) SANITY CHECKS
  # =========================
  message("ERP counts:")
  print(table(df_all$erp, useNA = "ifany"))
  
  message("Channel/hemisphere counts for LFSW:")
  print(df_all %>% filter(erp == "LFSW") %>% count(channel, hemisphere, sort = TRUE))
  
  # Check time range plausibility for one file
  tmp <- df_all %>% group_by(file) %>% summarise(tmin = min(time_ms), tmax = max(time_ms), .groups="drop")
  message("Example time range (first 5 files):")
  print(head(tmp, 5))
  
  # =========================
  # 4) SPLIT INTO ERP-SPECIFIC DATAFRAMES
  # If P200/P3 have multiple channels, average across channels per subject-condition-time.
  # LFSW keeps hemisphere (left/right).
  # =========================
  df_p200 <- df_all %>%
    filter(erp == "P200") %>%
    group_by(id, group, perspective, congruency, validity, correctness, time_ms) %>%
    summarise(amp = mean(amp, na.rm = TRUE), .groups = "drop")
  
  df_p3 <- df_all %>%
    filter(erp == "P3") %>%
    group_by(id, group, perspective, congruency, validity, correctness, time_ms) %>%
    summarise(amp = mean(amp, na.rm = TRUE), .groups = "drop")
  
  df_lfsw <- df_all %>%
    filter(erp == "LFSW") %>%
    mutate(hemisphere = factor(hemisphere, levels = c("left","right"))) %>%
    group_by(id, group, hemisphere, perspective, congruency, validity, correctness, time_ms) %>%
    summarise(amp = mean(amp, na.rm = TRUE), .groups = "drop")
  
  # =========================
  # 5) GRAND MEAN + SEM (SEM OVER SUBJECTS) AT EACH TIMEPOINT
  # =========================
  summarise_sem <- function(df, extra_group_vars = character()) {
    gvars <- c("group", extra_group_vars, "perspective", "congruency", "time_ms")
    
    df %>%
      group_by(across(all_of(c("id", gvars)))) %>%
      summarise(amp = mean(amp, na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(gvars))) %>%
      summarise(
        n = n_distinct(id),
        mean = mean(amp, na.rm = TRUE),
        sd = sd(amp, na.rm = TRUE),
        sem = sd / sqrt(n),
        .groups = "drop"
      )
  }
  
  sum_p200 <- summarise_sem(df_p200)
  sum_p3   <- summarise_sem(df_p3)
  sum_lfsw <- summarise_sem(df_lfsw, extra_group_vars = c("hemisphere"))
  
  # =========================
  # 6) PLOTS 
  # =========================
  
  plot_erp_faceted <- function(sum_df, title, facet_formula, filename_out,
                               win_ms = NULL,
                               width = 11, height = 4, dpi = 320) {
    
    p <- ggplot(
      sum_df,
      aes(
        x = time_ms, y = mean,
        color = congruency, fill = congruency,
        linetype = perspective,
        group = interaction(congruency, perspective)
      )
    )
    
    # grey ERP extraction window
    if (!is.null(win_ms) && length(win_ms) == 2) {
      p <- p +
        geom_rect(
          data = tibble(xmin = win_ms[1], xmax = win_ms[2]),
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE,
          fill = "grey80", alpha = 0.35
        )
    }
    
    p <- p +
      geom_ribbon(aes(ymin = mean - sem, ymax = mean + sem),
                  alpha = 0.20, color = NA) +
      geom_line(linewidth = 1.0) +
      geom_vline(xintercept = 0, linewidth = 0.6) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      facet_grid(facet_formula) +
      scale_colour_manual(values = cols_cong) +
      scale_fill_manual(values = cols_cong) +
      guides(linetype = guide_legend(override.aes = list(fill = NA))) +  
      labs(title = title, x = "Time (ms)", y = expression(paste("Amplitude (", mu, "V)"))) +
      coord_cartesian(xlim = c(-250, 1100)) +
      theme_erp
    
    ggsave(file.path(out_dir, filename_out), p, width = width, height = height, dpi = dpi)
    p
  }
  
  
  
  p_p200 <- plot_erp_faceted(
    sum_p200,
    title = "P200",
    facet_formula = . ~ group,
    filename_out = "ERP_P200_ASD_vs_NT_SEM.png",
    win_ms = c(175, 275)
  )
  
  p_p3 <- plot_erp_faceted(
    sum_p3,
    title = "P3",
    facet_formula = . ~ group,
    filename_out = "ERP_P3_ASD_vs_NT_SEM.png",
    win_ms = c(275, 450)
  )
  
  
  # LFSW: 2x2 (hemisphere rows, group cols)
  # LFSW (hemisphere x group) – add the same window rectangle directly
  p_lfsw <- ggplot(sum_lfsw, aes(x = time_ms, y = mean,
                                 color = congruency, fill = congruency,
                                 linetype = perspective,
                                 group = interaction(congruency, perspective))) +
    geom_rect(data = tibble(xmin = 400, xmax = 600),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey80", alpha = 0.35) +
    geom_ribbon(aes(ymin = mean - sem, ymax = mean + sem), alpha = 0.20, color = NA) +
    geom_line(linewidth = 1.0) +
    geom_vline(xintercept = 0, linewidth = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    facet_grid(hemisphere ~ group) +
    scale_colour_manual(values = cols_cong) +
    scale_fill_manual(values = cols_cong) +
    guides(linetype = guide_legend(override.aes = list(fill = NA))) +
    coord_cartesian(xlim = c(-250, 1100)) +   
    labs(title = "LFSW", x = "Time (ms)", y = expression(paste("Amplitude (", mu, "V)"))) +
    theme_erp
  
  ggsave(file.path(out_dir, "ERP_LFSW_ASD_vs_NT_byHemisphere_SEM.png"), p_lfsw, width = 11, height = 7, dpi = 320)
  
  message("Done. Figures written to: ", out_dir)
  
  
  fig_erp <- (p_p200 / p_p3 / p_lfsw) +
    plot_layout(
      heights = c(1, 1, 2),   # P200, P3, LFSW
      guides  = "collect"
    ) +
    plot_annotation(
      tag_levels = "A"
    ) &
    theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "bold", size = 16),
      plot.tag.position = c(0.01, 0.99)
    )
  
  fig_erp
  
  # same "style" export as behavioral figure
  out_file <- file.path(out_dir, "Figure_3.pdf")
  
  # PDF 
  ggsave(
    filename = file.path(out_dir, "Figure_3.pdf"),
    plot     = fig_erp,
    width    = 950,
    height   = 1524,
    units    = "px",
    dpi      = 600,
    scale    = 6
  )
  
  message("Saved: ", out_file)
  
  
#### 1.1 AMPLITUDE - SINGLE TRIAL ANALYSIS ########################################################################################################################
# load data 
  datapath<-".../VPT/export"
# define datapaths / file names
  data.files<-list.files(path=datapath,full.names=T)
  data.p200<-data.files[grep('cached_P200_singletrial_Mean',data.files)]
  data.p3<- data.files[grep('cached_P3_singletrial_Mean',data.files)]
  data.LFSW<- data.files[grep('cached_LFSW_singletrial_Mean',data.files)]
## read data
# p200
  test_list_p200<-list(0)
  for(i in 1:length(data.p200)){
    test_list_p200[[i]]<-read.table(data.p200[i], header=T, na.strings=c("-.----"))
    print(paste0('read: ',i))
  }
# P3
  test_list_p3<-list(0)
  for(i in 1:length(data.p3)){
    test_list_p3[[i]]<-read.table(data.p3[i], header=T, na.strings=c("-.----"))
    print(paste0('read: ',i))
  }
# LFSW
  test_list_LFSW<-list(0)
  for(i in 1:length(data.LFSW)){
    test_list_LFSW[[i]]<-read.table(data.LFSW[i], header=T, na.strings=c("-.----"))
    print(paste0('read: ',i))
  }

## Define id & general condition variable from file names
## P200
# id
  id.names.p200<-substr(data.p200,nchar(datapath)+2,nchar(datapath)+20) #delete data path, keep file names
  id.names.p200<-gsub("\\_VP.*","",id.names.p200) #clean names
  id.names.p200<-gsub("\\_Posner.*","",id.names.p200)
  id.names.p200<-gsub("\\_T2.*","",id.names.p200)
  names(test_list_p200)<-id.names.p200
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_p200<-mapply(fun_retrieve_id,x=test_list_p200,id=names(test_list_p200),SIMPLIFY=F)
# condition
  cond.names.p200<-substr(data.p200,nchar(datapath)+2,nchar(data.p200)) 
  cond.names.p200<-gsub("\\_correct.*","",cond.names.p200) 
  cond.names.p200<-gsub(".*self","self",cond.names.p200)
  cond.names.p200<-gsub(".*other","other",cond.names.p200)
  names(test_list_p200)<-cond.names.p200
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_p200<-mapply(fun_retrieve_cond,x=test_list_p200,condition=names(test_list_p200),SIMPLIFY=F)
# bind as data frame
  df.p200<-dplyr::bind_rows(test_list_p200)
  names(df.p200)<-c("filename", "trial.number", "mean_amp", "id", "condition")

## P3
# id
  id.names.p3<-substr(data.p3,nchar(datapath)+2,nchar(datapath)+20) 
  id.names.p3<-gsub("\\_VP.*","",id.names.p3) 
  id.names.p3<-gsub("\\_Posner.*","",id.names.p3)
  id.names.p3<-gsub("\\_T2.*","",id.names.p3)
  names(test_list_p3)<-id.names.p3
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_p3<-mapply(fun_retrieve_id,x=test_list_p3,id=names(test_list_p3),SIMPLIFY=F)
# condition
  cond.names.p3<-substr(data.p3,nchar(datapath)+2,nchar(data.p3)) 
  cond.names.p3<-gsub("\\_correct.*","",cond.names.p3) 
  cond.names.p3<-gsub(".*self","self",cond.names.p3)
  cond.names.p3<-gsub(".*other","other",cond.names.p3)
  names(test_list_p3)<-cond.names.p3
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_p3<-mapply(fun_retrieve_cond,x=test_list_p3,condition=names(test_list_p3),SIMPLIFY=F)
# bind as data frame
  df.p3<-dplyr::bind_rows(test_list_p3)
  names(df.p3)<-c("filename", "trial.number", "mean_amp", "id", "condition")

## LFSW
# id
  id.names.LFSW<-substr(data.LFSW,nchar(datapath)+2,nchar(datapath)+20) 
  id.names.LFSW<-gsub("\\_VP.*","",id.names.LFSW) 
  id.names.LFSW<-gsub("\\_Posner.*","",id.names.LFSW)
  id.names.LFSW<-gsub("\\_T2.*","",id.names.LFSW)
  names(test_list_LFSW)<-id.names.LFSW
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_LFSW<-mapply(fun_retrieve_id,x=test_list_LFSW,id=names(test_list_LFSW),SIMPLIFY=F)
# condition
  cond.names.LFSW<-substr(data.LFSW,nchar(datapath)+2,nchar(data.LFSW)) 
  cond.names.LFSW<-gsub("\\_correct.*","",cond.names.LFSW) 
  cond.names.LFSW<-gsub(".*self","self",cond.names.LFSW)
  cond.names.LFSW<-gsub(".*other","other",cond.names.LFSW)
  names(test_list_LFSW)<-cond.names.LFSW
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_LFSW<-mapply(fun_retrieve_cond,x=test_list_LFSW,condition=names(test_list_LFSW),SIMPLIFY=F)
# bind as data frame
  df.LFSW<-dplyr::bind_rows(test_list_LFSW)
  names(df.LFSW)<-c("filename", "trial.number", "left", "right", "id", "condition") 
# turn hemisphere info into long format (to later add in models)
  df.LFSW<-as.data.frame(pivot_longer(df.LFSW, cols=c("left", "right"),
                                      names_to='hemisphere',values_to='mean_amp', values_drop_na = T))

### Define separate variables for group and conditions ###
## P200 ##
  df.p200$group<-"ASD"
  df.p200$group[grep("HC", df.p200$id)]<-"NT"
  df.p200$perspective<-"self"
  df.p200$perspective[grep("other", df.p200$condition)]<-"other"
  df.p200$congruency<-"congruent"
  df.p200$congruency[grep("incongruent", df.p200$condition)]<-"incongruent"
  df.p200$validity<-"valid"
  df.p200$validity[grep("invalid", df.p200$condition)]<-"invalid"
## P3 ##
  df.p3$group<-"ASD"
  df.p3$group[grep("HC", df.p3$id)]<-"NT"
  df.p3$perspective<-"self"
  df.p3$perspective[grep("other", df.p3$condition)]<-"other"
  df.p3$congruency<-"congruent"
  df.p3$congruency[grep("incongruent", df.p3$condition)]<-"incongruent"
  df.p3$validity<-"valid"
  df.p3$validity[grep("invalid", df.p3$condition)]<-"invalid"
## LFSW ##
  df.LFSW$group<-"ASD"
  df.LFSW$group[grep("HC", df.LFSW$id)]<-"NT"
  df.LFSW$perspective<-"self"
  df.LFSW$perspective[grep("other", df.LFSW$condition)]<-"other"
  df.LFSW$congruency<-"congruent"
  df.LFSW$congruency[grep("incongruent", df.LFSW$condition)]<-"incongruent"
  df.LFSW$validity<-"valid"
  df.LFSW$validity[grep("invalid", df.LFSW$condition)]<-"invalid"

## Remove participants that were not matched
## correct id variables
# P200
  df.p200$id<-toupper(df.p200$id)
  df.p200$id<-gsub("-", "_", df.p200$id)
  df.p200$id<-gsub("ASS", "AUT", df.p200$id)
  df.p200$id<-gsub("F_AUT", "AUT_F", df.p200$id)
  df.p200$id<-gsub("F_AUT", "AUT_F", df.p200$id)
  df.p200$id[df.p200$id=="QS_A_001"]<-"SOKO_AUT_001"
# P3 
  df.p3$id<-toupper(df.p3$id)
  df.p3$id<-gsub("-", "_", df.p3$id)
  df.p3$id<-gsub("ASS", "AUT", df.p3$id)
  df.p3$id<-gsub("F_AUT", "AUT_F", df.p3$id)
  df.p3$id<-gsub("F_AUT", "AUT_F", df.p3$id)
  df.p3$id[df.p3$id=="QS_A_001"]<-"SOKO_AUT_001"
# LFSW 
  df.LFSW$id<-toupper(df.LFSW$id)
  df.LFSW$id<-gsub("-", "_", df.LFSW$id)
  df.LFSW$id<-gsub("ASS", "AUT", df.LFSW$id)
  df.LFSW$id<-gsub("F_AUT", "AUT_F", df.LFSW$id)
  df.LFSW$id<-gsub("F_AUT", "AUT_F", df.LFSW$id)
  df.LFSW$id[df.LFSW$id=="QS_A_001"]<-"SOKO_AUT_001"
# turn character-vectors into factors
  df.p200[sapply(df.p200, is.character)] <- lapply(df.p200[sapply(df.p200, is.character)], as.factor)
  df.p3[sapply(df.p3, is.character)] <- lapply(df.p3[sapply(df.p3, is.character)], as.factor)
  df.LFSW[sapply(df.LFSW, is.character)] <- lapply(df.LFSW[sapply(df.LFSW, is.character)], as.factor) 
# remove non-matched participants
  df.p200<-df.p200[df.p200$id %in% demogr$id,]
  df.p200$id<-droplevels(df.p200$id)
  df.p3<-df.p3[df.p3$id %in% demogr$id,]
  df.p3$id<-droplevels(df.p3$id)
  df.LFSW<-df.LFSW[df.LFSW$id %in% demogr$id,]
  df.LFSW$id<-droplevels(df.LFSW$id)
# merge together with demographics
  df.p200<-merge(df.p200, demogr[,c("id", "age", "IQ", "Geschlecht_Index",)], by="id", all.x=T, all.y=T)
  df.p3<-merge(df.p3, demogr[,c("id", "age", "IQ", "Geschlecht_Index")], by="id", all.x=T, all.y=T)
  df.LFSW<-merge(df.LFSW, demogr[,c("id", "age", "IQ", "Geschlecht_Index")], by="id", all.x=T, all.y=T)

## exclude invalid conditions (evokes substantially different neurophysiological responses)
  df.p200.valid.amp<-df.p200[df.p200$validity=="valid",]
  df.p3.valid.amp<-df.p3[df.p3$validity=="valid",]
  df.LFSW.valid.amp<-df.LFSW[df.LFSW$validity=="valid",]

  
#### 1.1.2  POWER ANALYSIS for trial-wise data in the full sample  ################################################################################################################################################
  ## Start from observed design 
  design <- df.p200.valid.amp %>%
    transmute(
      id = factor(id),
      group = factor(group),
      perspective = factor(perspective),
      congruency = factor(congruency),
    )
  
  ## Check trial counts 
  design %>% count(id) %>% summarise(median = median(n), min = min(n), max = max(n))
  
  # Set assumptions
  icc <- 0.3
  total_sd <- 1
  rand_sd  <- sqrt(icc) * total_sd # random intercept SD
  resid_sd <- sqrt(1 - icc) * total_sd  # residual SD
  
  # build the model matrix
  m_template <- makeLmer(
    y ~ group * perspective * congruency + (1|id),
    fixef = rep(0, 1 + (ncol(model.matrix(~ group * perspective * congruency, design)) - 1)),
    VarCorr = list(id = rand_sd^2),
    sigma = resid_sd,
    data = design
  )
  
  ## Power for two-way interaction effect (group x perspective)
  # set effect size 
  names(fixef(m_template))
  coef_name <- "groupNT:perspectiveself"  
  target_beta <- 0.15  
  fixef(m_template)[coef_name] <- target_beta
  
  # power for detecting a small effect 
  powerSim(
    m_template,
    fixed(coef_name, "t"),
    nsim = 500,
    progress = TRUE
  )
  
  ## Power for three-way interaction effect (group x perspective x congruency)
  # set effect size 
  names(fixef(m_template))
  coef_name <- "groupNT:perspectiveself:congruencyincongruent"  
  target_beta <- 0.20   # standardized effect size for the interaction effect
  fixef(m_template)[coef_name] <- target_beta
  
  # power for detecting a three-way interaction effect with small effect size
  powerSim(
    m_template,
    fixed(coef_name, "t"),
    nsim = 500,
    progress = TRUE
  )  

    
#### 1.1.3  GROUP DIFFERENCES IN ERP AMPLITUDES ################################################################################################################################################
# P200 
  m1.p200.amp<-lmer(scale(mean_amp)~group*perspective*congruency + (1|id),data=df.p200.valid.amp)
  anova(m1.p200.amp)
  emmeans(m1.p200.amp, list(pairwise ~congruency))
  # extract CI
  emm_p200_cong <- emmeans(m1.p200.amp, ~ congruency)
  p200_cong_pw  <- contrast(emm_p200_cong, method = "pairwise")
  p200_cong_tab <- as.data.frame(summary(
    p200_cong_pw,
    infer  = c(TRUE, TRUE),  # CIs + p
    level  = 0.95
  ))
  p200_cong_tab
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.p200.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p200_amp_table.docx")

# P3
  m2.p3.amp<-lmer(scale(mean_amp)~group*perspective*congruency + (1|id),data=df.p3.valid.amp)
  anova(m2.p3.amp)
  emmeans(m2.p3.amp, list(pairwise ~congruency))
  # CI
  emm_p3_cong <- emmeans(m2.p3.amp, ~ congruency)
  p3_cong_pw  <- contrast(emm_p3_cong, method = "pairwise")
  p3_cong_tab <- as.data.frame(summary(
    p3_cong_pw,
    infer = c(TRUE, TRUE),
    level = 0.95
  ))
  p3_cong_tab
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2.p3.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p3_amp_table.docx")

#### LFSW
  m3.LFSW.amp<-lmer(scale(mean_amp)~group*perspective*congruency*hemisphere + (1|id), data=df.LFSW.valid.amp)
  anova(m3.LFSW.amp)
  # hemisphere effect
  emmeans(m3.LFSW.amp, list(pairwise ~hemisphere))
  emm_hemi <- emmeans(m3.LFSW.amp, ~ hemisphere)
  pw_hemi  <- contrast(emm_hemi, method = "pairwise")
  hemi_tab <- as.data.frame(summary(
    pw_hemi,
    infer = c(TRUE, TRUE),   # adds CI + p
    level = 0.95
  ))
  hemi_tab
  # interaction effect
  m3.LFSW.emm <- emmeans(m3.LFSW.amp, ~ group * perspective * congruency)
  consec <- contrast(m3.LFSW.emm, "consec", simple = "each", combine = TRUE)
    # 1) Unadjusted 95% CIs
    consec_ci <- as.data.frame(confint(consec, level = 0.95, adjust = "none"))
    # 2) FDR-adjusted p-values
    consec_p  <- as.data.frame(test(consec, adjust = "fdr"))
    # Identify join columns (= all non-stat columns that appear in BOTH)
    stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                   "lower.CL","upper.CL","asymp.LCL","asymp.UCL",
                   "p.value","p.value.adj")
    join_cols <- intersect(
      setdiff(names(consec_ci), stat_cols),
      setdiff(names(consec_p),  stat_cols)
    )
    # Join
    consec_tab <- consec_ci %>%
      left_join(consec_p %>% select(all_of(join_cols), p.value),
                by = join_cols)
    consec_tab
  
  
  #contrast of contrasts
  diffs <- contrast(
    m3.LFSW.emm,
    method = "revpairwise",
    by     = c("group", "congruency")
  )
  interaction_contrasts <- contrast(
    diffs,
    method = "pairwise",
    by     = "group"
  )
  # 1) Unadjusted 95% CIs
  ic_ci <- as.data.frame(confint(interaction_contrasts, level = 0.95, adjust = "none"))
  # 2) FDR-adjusted p-values
  ic_p  <- as.data.frame(test(interaction_contrasts, adjust = "fdr"))
  # 3) Join (robustly)
  stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                 "lower.CL","upper.CL","asymp.LCL","asymp.UCL",
                 "p.value")
  join_cols <- intersect(
    setdiff(names(ic_ci), stat_cols),
    setdiff(names(ic_p),  stat_cols)
  )
  interaction_contrasts_tab <- ic_ci %>%
    left_join(ic_p %>% select(all_of(join_cols), p.value), by = join_cols)
  interaction_contrasts_tab
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m3.LFSW.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_amp_table.docx")
  
  
  ### Sensitivity analysis: LFSW without participants who fulfilled ADHD criteria according to FBB-ADHS
  ids_ASD_ADHD <- c(
    "AUT_F_007",
    "AUT_F_012",
    "SOKO_AUT_005",
    "SOKO_AUT_008",
    "SOKO_AUT_010",
    "SOKO_AUT_017",
    "SOKO_AUT_041"
  )
  
  df.LFSW.noADHD <- df.LFSW.valid.amp %>%
    filter(!id %in% ids_ASD_ADHD)
  
  m3.LFSW.amp<-lmer(scale(mean_amp)~group*perspective*congruency*hemisphere + (1|id), data=df.LFSW.noADHD)
  anova(m3.LFSW.amp)
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m3.LFSW.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_amp_table_noADHD.docx")
  
  
#### 1.2 LATENCY - subject-level (50% area measure) ########################################################################################################################
## load data 
  datapath<-"S:/KJP_Neurophys/Studien/STIPED/WP5 Querschnitt (SoKoASS)/6_Projekte/2_VPT/Auswertung/1_Auswertung EEG Daten/export"
  #define variable names and read in datapaths / file names
  data.files<-list.files(path=datapath,full.names=T)
  data.p200<-data.files[grep('P200_175_275_300_600',data.files)]
  data.p3<- data.files[grep('P3_275_450_300_600',data.files)]
  data.LFSW<- data.files[grep('LFSW_400_600_300_600',data.files)]
  #the 300_600 in the names were mistakes from how the name was saved during the export and can be ignored

## read in data: amplitudes per time point aggregated per condition (but only within pre-defined time windows) ###
# P200
  test_list_p200<-list(0)
  for(i in 1:length(data.p200)){
    #transpose before reading in, otherwise wide format but convert to data frame instead of vector
    test_list_p200[[i]]<-as.data.frame(t(read.table(data.p200[i], header=F,dec=",")[,-1])) #without first row (says p200)
    colnames(test_list_p200[[i]])<-"p200"
    print(paste0('read: ',i))
  }
# P3
  test_list_p3<-list(0)
  for(i in 1:length(data.p3)){
    test_list_p3[[i]]<-as.data.frame(t(read.table(data.p3[i], header=F,dec=",")[,-1]))
    colnames(test_list_p3[[i]])<-"p3"
    print(paste0('read: ',i))
  }
# LFSW
  test_list_LFSW<-list(0)
  for(i in 1:length(data.LFSW)){
    test_list_LFSW[[i]]<-as.data.frame(t(read.table(data.LFSW[i], header=F,dec=",")[,-1]))
    colnames(test_list_LFSW[[i]])<-c("LFSW.left","LFSW.right")
    print(paste0('read: ',i))
  }

### ID & condition from file names ###
## P200 ##
# ID
  id.names.p200<-substr(data.p200,nchar(datapath)+2,nchar(datapath)+20) 
  id.names.p200<-gsub("\\_VP.*","",id.names.p200)
  id.names.p200<-gsub("\\_Posner.*","",id.names.p200)
  id.names.p200<-gsub("\\_T2.*","",id.names.p200)
  names(test_list_p200)<-id.names.p200
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_p200<-mapply(fun_retrieve_id,x=test_list_p200,id=names(test_list_p200),SIMPLIFY=F)
# condition
  cond.names.p200<-substr(data.p200,nchar(datapath)+2,nchar(data.p200)) 
  cond.names.p200<-gsub("\\_correct.*","",cond.names.p200)
  cond.names.p200<-gsub(".*self","self",cond.names.p200)
  cond.names.p200<-gsub(".*other","other",cond.names.p200)
  names(test_list_p200)<-cond.names.p200
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_p200<-mapply(fun_retrieve_cond,x=test_list_p200,condition=names(test_list_p200),SIMPLIFY=F)

# create time variable (in ms)
  test_list_p200<-lapply(test_list_p200, function(x) {
    ms.factor<-1000/500 # data was downsampled to 500 Hz
    time<-seq(nrow(x))*ms.factor #get sequence as long as data and multiplicate it with ms.factor to get time in ms
    x[,"time"]<-time
    return(x)
  })

# Function: define 50% area latency by INTEGRAL (i.e. both positive and negative values go in without sign change)
  lat.func<-function(x) {
    #area: defined by mean*time
    total.area.p200<-mean(x$p200)*(max(x$time)-min(x$time)) #total area under the curve
    half.area<-total.area.p200/2 #50% area under the curve
    p200.cmlt<-cummean(x$p200)*(x$time-min(x$time)) #cumulated area values = sum of reached area to each time point
    p200.cmlt.diff<-p200.cmlt-half.area   # difference wave between cumulated area per time point and half area. change of sign means 50% crossed
    p200.cmlt.diff.sign<-sign(p200.cmlt.diff) # get vector of signs per value for diff wave (-1 for "-", 0 for "0", and 1 for "+")
    p200.cmlt.diff.sign.change<-c(0,diff(p200.cmlt.diff.sign)) # get vector with 0=no sign change, 2=sign change
    p200.cmlt.diff.sign.change.number<-length(which(p200.cmlt.diff.sign.change!=0)) # count number of sign changes
    latency<-x$time[which(p200.cmlt.diff.sign.change!=0)] # define latency based on where sign change occurs in diff wave
    latency<-ifelse(length(latency)>1, NA, latency) #set latency to NA if there is more than one solution for 50% integral
    x['half.area']<-rep(half.area,nrow(x))
    x[,'cmlt.area']<-p200.cmlt
    x[,'sign.change']<-p200.cmlt.diff.sign.change
    x['sign.change.number']<-rep(p200.cmlt.diff.sign.change.number, nrow(x))
    x[,'p200.lat']<-rep(latency,nrow(x))
    return(x)
  }
  #apply function 
  test_list_p200<-lapply(test_list_p200, lat.func)
  # bind as data frame
  df.p200.time<-dplyr::bind_rows(test_list_p200)
  table(df.p200.time$sign.change.number)/50 # check for how many conditions there was more than 1 sign change 
  df.p200.lat<-aggregate(p200.lat ~ id + condition, mean, data=df.p200.time)

## P3 ##
### ID & condition from file names ###
# ID
  id.names.p3<-substr(data.p3,nchar(datapath)+2,nchar(datapath)+20) 
  id.names.p3<-gsub("\\_VP.*","",id.names.p3) 
  id.names.p3<-gsub("\\_Posner.*","",id.names.p3)
  id.names.p3<-gsub("\\_T2.*","",id.names.p3)
  names(test_list_p3)<-id.names.p3
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_p3<-mapply(fun_retrieve_id,x=test_list_p3,id=names(test_list_p3),SIMPLIFY=F)
# condition
  cond.names.p3<-substr(data.p3,nchar(datapath)+2,nchar(data.p3))
  cond.names.p3<-gsub("\\_correct.*","",cond.names.p3)
  cond.names.p3<-gsub(".*self","self",cond.names.p3)
  cond.names.p3<-gsub(".*other","other",cond.names.p3)
  names(test_list_p3)<-cond.names.p3
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_p3<-mapply(fun_retrieve_cond,x=test_list_p3,condition=names(test_list_p3),SIMPLIFY=F)

# create time variable (in ms)
  test_list_p3<-lapply(test_list_p3, function(x) {
    ms.factor<-1000/500 # data was downsampled to 500 Hz
    time<-seq(nrow(x))*ms.factor #get sequence as long as data and multiplicate it with ms.factor to get time in ms
    x[,"time"]<-time
    return(x)
  })

# Function: define 50% area latency by INTEGRAL (i.e. both positive and negative values go in without sign change)
  lat.func<-function(x) {
    #area: defined by mean*time 
    total.area.p3<-mean(x$p3)*(max(x$time)-min(x$time)) #total area under the curve
    half.area<-total.area.p3/2 #50% area under the curve
    p3.cmlt<-cummean(x$p3)*(x$time-min(x$time)) #cumulated area values = sum of reached area to each time point
    p3.cmlt.diff<-p3.cmlt-half.area   # difference wave between cumulated area per time point and half area. change of sign means 50% crossed
    p3.cmlt.diff.sign<-sign(p3.cmlt.diff) # get vector of signs per value for diff wave (-1 for "-", 0 for "0", and 1 for "+")
    p3.cmlt.diff.sign.change<-c(0,diff(p3.cmlt.diff.sign)) # get vector with 0=no sign change, 2=sign change
    p3.cmlt.diff.sign.change.number<-length(which(p3.cmlt.diff.sign.change!=0)) # count number of sign changes
    latency<-x$time[which(p3.cmlt.diff.sign.change!=0)] # define latency based on where sign change occurs in diff wave
    latency<-ifelse(length(latency)>1, NA, latency) #set latency to NA if there is more than one solution for 50% integral
    x['half.area']<-rep(half.area,nrow(x))
    x[,'cmlt.area']<-p3.cmlt
    x[,'sign.change']<-p3.cmlt.diff.sign.change
    x['sign.change.number']<-rep(p3.cmlt.diff.sign.change.number, nrow(x))
    x[,'p3.lat']<-rep(latency,nrow(x))
    return(x)
  }
  # apply function
  test_list_p3<-lapply(test_list_p3, lat.func)
  # bind as data frame
  df.p3.time<-dplyr::bind_rows(test_list_p3)
  table(df.p3.time$sign.change.number)/88 #gives out how many conditions had to be dropped
  df.p3.lat<-aggregate(p3.lat ~ id + condition, mean, data=df.p3.time)

## LFSW ##
# ID
  id.names.LFSW<-substr(data.LFSW,nchar(datapath)+2,nchar(datapath)+20) 
  id.names.LFSW<-gsub("\\_VP.*","",id.names.LFSW) 
  id.names.LFSW<-gsub("\\_Posner.*","",id.names.LFSW)
  id.names.LFSW<-gsub("\\_T2.*","",id.names.LFSW)
  names(test_list_LFSW)<-id.names.LFSW
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list_LFSW<-mapply(fun_retrieve_id,x=test_list_LFSW,id=names(test_list_LFSW),SIMPLIFY=F)
# condition
  cond.names.LFSW<-substr(data.LFSW,nchar(datapath)+2,nchar(data.LFSW)) 
  cond.names.LFSW<-gsub("\\_correct.*","",cond.names.LFSW) 
  cond.names.LFSW<-gsub(".*self","self",cond.names.LFSW)
  cond.names.LFSW<-gsub(".*other","other",cond.names.LFSW)
  names(test_list_LFSW)<-cond.names.LFSW
  #as own variable
  fun_retrieve_cond<-function(x,condition){
    k<-nrow(x)
    id<-rep(condition,k)
    x[,'condition']<-condition
    return(x)
  }
  test_list_LFSW<-mapply(fun_retrieve_cond,x=test_list_LFSW,condition=names(test_list_LFSW),SIMPLIFY=F)

# create time variable (in ms)
  test_list_LFSW<-lapply(test_list_LFSW, function(x) {
    ms.factor<-1000/500 # data was downsampled to 500 Hz
    time<-seq(nrow(x))*ms.factor #get sequence as long as data and multiplicate it with ms.factor to get time in ms
    x[,"time"]<-time
    return(x)
  })

# bind as data frame
  df.LFSW<-dplyr::bind_rows(test_list_LFSW)
# turn hemisphere info into long format 
  df.LFSW<-as.data.frame(pivot_longer(df.LFSW, cols=c("LFSW.left", "LFSW.right"),
                                      names_to='hemisphere',values_to='LFSW', values_drop_na = T))
  df.LFSW[sapply(df.LFSW, is.character)] <- lapply(df.LFSW[sapply(df.LFSW, is.character)], as.factor) #turn character-vectors into factors

# Function: define 50% area latency by INTEGRAL (i.e. both positive and negative values go in without sign change)
  lat.func<-function(x) {
    #area: defined by mean*time (see Luck)
    total.area.LFSW<-mean(x$LFSW)*(max(x$time)-min(x$time)) #total area under the curve
    half.area<-total.area.LFSW/2 #50% area under the curve
    LFSW.cmlt<-cummean(x$LFSW)*(x$time-min(x$time)) #cumulated area values = sum of reached area to each time point
    LFSW.cmlt.diff<-LFSW.cmlt-half.area   # difference wave between cumulated area per time point and half area. change of sign means 50% crossed
    LFSW.cmlt.diff.sign<-sign(LFSW.cmlt.diff) # get vector of signs per value for diff wave (-1 for "-", 0 for "0", and 1 for "+")
    LFSW.cmlt.diff.sign.change<-c(0,diff(LFSW.cmlt.diff.sign)) # get vector with 0=no sign change, 2=sign change
    LFSW.cmlt.diff.sign.change.number<-length(which(LFSW.cmlt.diff.sign.change!=0)) # count number of sign changes
    latency<-x$time[which(LFSW.cmlt.diff.sign.change!=0)] # define latency based on where sign change occurs in diff wave
    latency<-ifelse(length(latency)>1, NA, latency) #set latency to NA if there is more than one solution for 50% integral
    x['half.area']<-rep(half.area,nrow(x))
    x[,'cmlt.area']<-LFSW.cmlt
    x[,'sign.change']<-LFSW.cmlt.diff.sign.change
    x['sign.change.number']<-rep(LFSW.cmlt.diff.sign.change.number, nrow(x))
    x[,'LFSW.lat']<-rep(latency,nrow(x))
    return(x)
  }
  # apply function
  df.LFSW_split<-split(df.LFSW, droplevels(interaction(df.LFSW$id, df.LFSW$condition, df.LFSW$hemisphere)))
  df.LFSW_split<-lapply(df.LFSW_split, lat.func)
  df.LFSW<-dplyr::bind_rows(df.LFSW_split)
  table(df.LFSW$sign.change.number)/200 #gives out number of conditions that had to be dropped due to more than one mathematical solution for 50% area latency
  # get aggregated data frame with only one row per condition
  df.LFSW.lat<-aggregate(LFSW.lat ~ id + condition + hemisphere, mean, data=df.LFSW)

## Define separate variables for conditions and groups
# P200
  df.p200.lat$group<-"ASD"
  df.p200.lat$group[grep("HC", df.p200.lat$id)]<-"NT"
  df.p200.lat$perspective<-"self"
  df.p200.lat$perspective[grep("other", df.p200.lat$condition)]<-"other"
  df.p200.lat$congruency<-"congruent"
  df.p200.lat$congruency[grep("incongruent", df.p200.lat$condition)]<-"incongruent"
  df.p200.lat$validity<-"valid"
  df.p200.lat$validity[grep("invalid", df.p200.lat$condition)]<-"invalid"
# P3
  df.p3.lat$group<-"ASD"
  df.p3.lat$group[grep("HC", df.p3.lat$id)]<-"NT"
  df.p3.lat$perspective<-"self"
  df.p3.lat$perspective[grep("other", df.p3.lat$condition)]<-"other"
  df.p3.lat$congruency<-"congruent"
  df.p3.lat$congruency[grep("incongruent", df.p3.lat$condition)]<-"incongruent"
  df.p3.lat$validity<-"valid"
  df.p3.lat$validity[grep("invalid", df.p3.lat$condition)]<-"invalid"
# LFSW 
  df.LFSW.lat$group<-"ASD"
  df.LFSW.lat$group[grep("HC", df.LFSW.lat$id)]<-"NT"
  df.LFSW.lat$perspective<-"self"
  df.LFSW.lat$perspective[grep("other", df.LFSW.lat$condition)]<-"other"
  df.LFSW.lat$congruency<-"congruent"
  df.LFSW.lat$congruency[grep("incongruent", df.LFSW.lat$condition)]<-"incongruent"
  df.LFSW.lat$validity<-"valid"
  df.LFSW.lat$validity[grep("invalid", df.LFSW.lat$condition)]<-"invalid"
# Remove invalid condition
  df.p200.lat<-df.p200.lat[df.p200.lat$validity=="valid",]
  df.p3.lat<-df.p3.lat[df.p3.lat$validity=="valid",]
  df.LFSW.lat<-df.LFSW.lat[df.LFSW.lat$validity=="valid",]

### Remove participants with too low autism severity (based on ADOS) ###
## correct id variables
# P200
  df.p200.lat$id<-toupper(df.p200.lat$id)
  df.p200.lat$id<-gsub("-", "_", df.p200.lat$id)
  df.p200.lat$id<-gsub("ASS", "AUT", df.p200.lat$id)
  df.p200.lat$id<-gsub("F_AUT", "AUT_F", df.p200.lat$id)
  df.p200.lat$id<-gsub("F_AUT", "AUT_F", df.p200.lat$id)
  df.p200.lat$id[df.p200.lat$id=="QS_A_001"]<-"SOKO_AUT_001"
# P3
  df.p3.lat$id<-toupper(df.p3.lat$id)
  df.p3.lat$id<-gsub("-", "_", df.p3.lat$id)
  df.p3.lat$id<-gsub("ASS", "AUT", df.p3.lat$id)
  df.p3.lat$id<-gsub("F_AUT", "AUT_F", df.p3.lat$id)
  df.p3.lat$id<-gsub("F_AUT", "AUT_F", df.p3.lat$id)
  df.p3.lat$id[df.p3.lat$id=="QS_A_001"]<-"SOKO_AUT_001"
# LFSW
  df.LFSW.lat$id<-toupper(df.LFSW.lat$id)
  df.LFSW.lat$id<-gsub("-", "_", df.LFSW.lat$id)
  df.LFSW.lat$id<-gsub("ASS", "AUT", df.LFSW.lat$id)
  df.LFSW.lat$id<-gsub("F_AUT", "AUT_F", df.LFSW.lat$id)
  df.LFSW.lat$id<-gsub("F_AUT", "AUT_F", df.LFSW.lat$id)
  df.LFSW.lat$id[df.LFSW.lat$id=="QS_A_001"]<-"SOKO_AUT_001"
## turn character-vectors into factors
  df.p200.lat[sapply(df.p200.lat, is.character)] <- lapply(df.p200.lat[sapply(df.p200.lat, is.character)], as.factor)
  df.p3.lat[sapply(df.p3.lat, is.character)] <- lapply(df.p3.lat[sapply(df.p3.lat, is.character)], as.factor)
  df.LFSW.lat[sapply(df.LFSW.lat, is.character)] <- lapply(df.LFSW.lat[sapply(df.LFSW.lat, is.character)], as.factor) 
## remove low ADOS scores
  df.p200.lat<-df.p200.lat[df.p200.lat$id %in% demogr$id,]
  df.p200.lat$id<-droplevels(df.p200.lat$id)
  df.p3.lat<-df.p3.lat[df.p3.lat$id %in% demogr$id,]
  df.p3.lat$id<-droplevels(df.p3.lat$id)
  df.LFSW.lat<-df.LFSW.lat[df.LFSW.lat$id %in% demogr$id,]
  df.LFSW.lat$id<-droplevels(df.LFSW.lat$id)

#### GROUP DIFFERENCES: ERP LATENCIES ####
# P200
  m1.p200<-lmer(scale(p200.lat) ~ group * perspective * congruency + (1|id), data=df.p200.lat)
  anova(m1.p200)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.p200)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p200_lat_table.docx")
  
# P3
  m2.p3<-lmer(scale(p3.lat) ~ group * perspective * congruency + (1|id), data=df.p3.lat)
  anova(m2.p3)
  emm_p3_persp <- emmeans(m2.p3, ~ perspective)
  p3_persp_pw  <- contrast(emm_p3_persp, method = "pairwise")
  # CI (unadjusted)
  p3_ci <- as.data.frame(confint(p3_persp_pw, level = 0.95, adjust = "none"))
  # p-values (FDR) - use "none" if it's only one contrast
  p3_p  <- as.data.frame(test(p3_persp_pw, adjust = "fdr"))
  # join
  stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                 "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
  join_cols <- intersect(
    setdiff(names(p3_ci), stat_cols),
    setdiff(names(p3_p),  stat_cols)
  )
  
  p3_persp_tab <- p3_ci %>%
    left_join(p3_p %>% select(all_of(join_cols), p.value), by = join_cols)
  p3_persp_tab
  # save anova table
  require(officer)
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2.p3)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p3_lat_table.docx")
  
# LFSW
  m3.LFSW<-lmer(scale(LFSW.lat) ~ group * perspective * congruency * hemisphere + (1|id), data=df.LFSW.lat)
  anova(m3.LFSW)
  m3.emm <- emmeans(m3.LFSW, ~ group * perspective * congruency * hemisphere)
  results <- contrast(m3.emm, "consec", simple = "each", combine = TRUE)
  # 1) Unadjusted 95% CIs (no simultaneous adjustment)
  res_ci <- as.data.frame(confint(results, level = 0.95, adjust = "none"))
  # 2) Raw p-values (no adjustment yet; FDR after filtering)
  res_p  <- as.data.frame(test(results, adjust = "none"))
  # 3) Join CI + raw p
  stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                 "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
  join_cols <- intersect(
    setdiff(names(res_ci), stat_cols),
    setdiff(names(res_p),  stat_cols)
  )
  res_tab <- res_ci %>%
    left_join(res_p %>% select(all_of(join_cols), p.value), by = join_cols)
  # 4) Filter out hemisphere comparisons 
  res_tab_filt <- res_tab %>%
    filter(!grepl("LFSW\\.right - LFSW\\.left", contrast))
  # 5) Apply FDR only to the remaining tests
  res_tab_filt <- res_tab_filt %>%
    mutate(p.value.corrected = p.adjust(p.value, method = "fdr"))
  res_tab_filt
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m3.LFSW)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_lat_table.docx")
  
  
  
#### 1.3 ERP - SUPPLEMENTARY ANALYSES ####
  
#### Trial counts per person × condition
  df.trialcounts <- df.LFSW.valid.amp %>%
    group_by(id, group, congruency, perspective) %>%
    summarise(
      n_trials = max(trial.number, na.rm = TRUE),
      .groups = "drop"
    )
  # per person
  trialcounts_subj <- df.trialcounts %>%
    group_by(id, group) %>%
    summarise(
      median_trials = median(n_trials),
      .groups = "drop"
    )
  t.test(trialcounts_subj$median_trials[trialcounts_subj$group=="ASD"], trialcounts_subj$median_trials[trialcounts_subj$group=="NT"])
  

#### Add trial count to data frames
# P200 amplitude
  df.p200.valid.amp_wCounts <- df.p200.valid.amp %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )
# P200 latency
  df.p200.lat_wCounts <- df.p200.lat %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )
  
# P3 amplitude
  df.p3.valid.amp_wCounts <- df.p3.valid.amp %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )
# P3 latency
  df.p3.lat_wCounts <- df.p3.lat %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )
  
# LFSW amplitude
  df.LFSW.valid.amp_wCounts <- df.LFSW.valid.amp %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )
# LFSW latency
  df.LFSW.lat_wCounts <- df.LFSW.lat %>%
    left_join(
      df.trialcounts,
      by = c("id", "group", "congruency", "perspective")
    )

## Exclude conditions with low trial counts from ERP models
# 1) P200
  # amplitude
  m1.p200.amp<-lmer(scale(mean_amp)~group*perspective*congruency + (1|id),data=df.p200.valid.amp_wCounts[df.p200.valid.amp_wCounts$n_trials>=15,])
  anova(m1.p200.amp)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.p200.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p200_amp_trialexcl.docx")
  
  
  # latency
  m1.p200<-lmer(scale(p200.lat) ~ group * perspective * congruency + (1|id), data=df.p200.lat_wCounts[df.p200.lat_wCounts$n_trials>=15,])
  anova(m1.p200)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.p200)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p200_lat_trialexcl.docx")
  

# 2) P3
  # aplitude
  m2.p3.amp<-lmer(scale(mean_amp)~group*perspective*congruency + (1|id), data=df.p3.valid.amp_wCounts[df.p3.valid.amp_wCounts$n_trials>=15,])
  anova(m2.p3.amp)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2.p3.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p3_amp_trialexcl.docx")
  # latency
  m2.p3<-lmer(scale(p3.lat) ~ group * perspective * congruency + (1|id), data=df.p3.lat_wCounts[df.p3.lat_wCounts$n_trials>=15,])
  anova(m2.p3)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2.p3)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../p3_lat_trialexcl.docx")

# 3) LFSW   
  m3.LFSW.amp<-lmer(scale(mean_amp)~group*perspective*congruency*hemisphere + (1|id), data=df.LFSW.valid.amp_wCounts[df.LFSW.valid.amp_wCounts$n_trials>=15,])
  anova(m3.LFSW.amp)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m3.LFSW.amp)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_amp_trialexcl.docx")
  # latency
  m3.LFSW<-lmer(scale(LFSW.lat) ~ group * perspective * congruency * hemisphere + (1|id), data=df.LFSW.lat_wCounts[df.LFSW.lat_wCounts$n_trials>=15,])
  anova(m3.LFSW)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m3.LFSW)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_lat_trialexcl.docx")


## 2) Look at relationships between aggregated ERP amplitudes and trial count
# P200
  df.p200.amp_agg <- df.p200.valid.amp %>%
    group_by(id, group, congruency, perspective) %>%
    summarise(
      mean_amp_cond = mean(mean_amp, na.rm = TRUE),
      n_trials       = max(trial.number, na.rm = TRUE),
      .groups = "drop"
    )
  # relationships
  # amplitude
  cor.test(df.p200.amp_agg$n_trials, df.p200.amp_agg$mean_amp_cond)
  # latency
  cor.test(df.p200.lat_wCounts$n_trials, df.p200.lat_wCounts$p200.lat)
  
# P3
  df.p3.amp_agg <- df.p3.valid.amp %>%
    group_by(id, group, congruency, perspective) %>%
    summarise(
      mean_amp_cond = mean(mean_amp, na.rm = TRUE),
      n_trials       = max(trial.number, na.rm = TRUE),
      .groups = "drop"
    )
  # relationships
  # amplitude
  cor.test(df.p3.amp_agg$n_trials, df.p3.amp_agg$mean_amp_cond)
  # latency
  cor.test(df.p3.lat_wCounts$n_trials, df.p3.lat_wCounts$p3.lat)
  
# LFSW
  df.LFSW.amp_agg <- df.LFSW.valid.amp %>%
    group_by(id, group, hemisphere, congruency, perspective) %>%
    summarise(
      mean_amp_cond = mean(mean_amp, na.rm = TRUE),
      n_trials       = max(trial.number, na.rm = TRUE),
      .groups = "drop"
    )
  # relationships
  # amplitude
  cor.test(df.LFSW.amp_agg$n_trials, df.LFSW.amp_agg$mean_amp_cond)
  # latency
  cor.test(df.LFSW.lat_wCounts$n_trials, df.LFSW.lat_wCounts$LFSW.lat)


  
#### 2. BEHAVIORAL DATA ############################################################################################################################################
#### 2.1 REACTION TIMES ############################################################################################################################################
## load data 
  datapath<-".../raw files"
  #define variable names and read in datapaths / file names
  colnames<-c("Type","Stimulus","time","x1","x0")
  data.files<-list.files(path=datapath,full.names=T)
  data.behav<-data.files[grep('.vmrk',data.files)]
  #read data
  test_list<-list(0)
  for(i in 1:length(data.behav)){
    test_list[[i]]<-read.table(data.behav[i],  sep=",", skip= 12, col.names = colnames, fill=T, dec=",",na.strings=c("NA", "-", "???"))
    print(paste0('read: ',i))
  }

## id names
  id.names<-substr(data.behav,nchar(datapath)+2,nchar(datapath)+20) 
  id.names<-gsub("\\_VP.*","",id.names) 
  id.names<-gsub("\\_Posner.*","",id.names)
  id.names<-gsub("\\_T2.*","",id.names)
  names(test_list)<-id.names
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  test_list<-mapply(fun_retrieve_id,x=test_list,id=names(test_list),SIMPLIFY=F)
  #bind to data frame
  df.bhv<-dplyr::bind_rows(test_list)
  #correct ID names
  df.bhv$id<-toupper(df.bhv$id)
  df.bhv$id<-gsub("-", "_", df.bhv$id)
  df.bhv$id<-gsub("ASS", "AUT", df.bhv$id)
  df.bhv$id<-gsub("F_AUT", "AUT_F", df.bhv$id)
  df.bhv$id<-gsub("F_AUT", "AUT_F", df.bhv$id)
  df.bhv$id[df.bhv$id=="QS_A_001"]<-"SOKO_AUT_001"
  #remove IDs that did not reach ADOS cutoff
  df.bhv<-df.bhv[df.bhv$id %in% demogr$id,]

## define stimulus events
  df.bhv$Stimulus<-gsub(" ", "", df.bhv$Stimulus)
  #stimulus event
  df.bhv$event[df.bhv$Stimulus=="S99"]<-"fix.cross"
  df.bhv$event[df.bhv$Stimulus=="S100" | df.bhv$Stimulus=="S4"]<-"cue_1"
  df.bhv$event[df.bhv$Stimulus=="S101" | df.bhv$Stimulus=="S102" | df.bhv$Stimulus=="S103" |
                 df.bhv$Stimulus=="S1" | df.bhv$Stimulus=="S2" | df.bhv$Stimulus=="S3"]<-"cue_2"
  df.bhv$event[df.bhv$Stimulus=="S10" | df.bhv$Stimulus== "S20" |
                 df.bhv$Stimulus=="S11" | df.bhv$Stimulus=="S21" | 
                 df.bhv$Stimulus=="S12" | df.bhv$Stimulus=="S22" |
                 df.bhv$Stimulus=="S13" | df.bhv$Stimulus=="S23" |
                 df.bhv$Stimulus=="S14" | df.bhv$Stimulus=="S24" |
                 df.bhv$Stimulus=="S15" | df.bhv$Stimulus=="S25" |
                 df.bhv$Stimulus=="S16" | df.bhv$Stimulus=="S26" |
                 df.bhv$Stimulus=="S17" | df.bhv$Stimulus=="S27" |
                 df.bhv$Stimulus=="S18" | df.bhv$Stimulus=="S28" ]<-"target"
  df.bhv$event[df.bhv$Stimulus=="S7" | df.bhv$Stimulus=="S8"]<-"response"
  ###unclear events all have to do with button presses that are not part of the response -> exclude
  df.bhv[which(is.na(df.bhv$event)),]
  df.bhv<-df.bhv[!is.na(df.bhv$event),]

## define trial
  df.bhv_split<-split(df.bhv, df.bhv$id)
  # Function: trial definition
  df.bhv_split<-lapply(df.bhv_split, function(x) {
    trial.number<-rep(1:length(x$event[x$event=="cue_1"]),times=diff(c(which(x$event=="cue_1"),length(x$event)+1)))
    trial.number<-c(rep(NA,times=which(x$event=="cue_1")[1]-1),trial.number)
    x[,"trial.number"]<-trial.number
    return(x)})
  # bind as data frame
  df.bhv <-as.data.frame(dplyr::bind_rows(df.bhv_split))
# Exclude errors in data (new segments e.g. due to breaks & NA stimulus trigger)
  df.bhv<-df.bhv[!is.na(df.bhv$trial.number),]
# remove unnecessary stimulus events
  df.bhv<-df.bhv[!df.bhv$event=="fix.cross",]

### remove duplicates 
# step 1: first duplicates due to system error (cues, targets) 
# step2: then response duplicates (response before target or more than one response after target)
df.bhv_split<-split(df.bhv, interaction(df.bhv$id, df.bhv$trial.number))
# problem 1: more than one target in some trials due to "New segments" (breaks)
  dbl<-lapply(df.bhv_split, function(x) {
    x$time[x$event=="target"]
  })
  which(lapply(dbl, length)>1)
  rmv<-names(which(lapply(dbl, length)>1))
  #--> concerns SoKo-HC-056: trial=1; SoKo-AUT-041, trial=73; SoKo-HC-058; trial=193 
  #exclude
  df.bhv<-df.bhv[!interaction(df.bhv$id, df.bhv$trial.number) %in% rmv,]
# problem 2: sometimes in presentation stream more than one cue_2 without cue 1
  dbl<-lapply(df.bhv_split, function(x) {
    x$time[x$event=="cue_2"]
  })
  which(lapply(dbl, length)>1)
  df.bhv[df.bhv$id=="SoKo-AUT-026" & df.bhv$trial.number=="28",] #example
  rmv<-names(which(lapply(dbl, length)>1))
  #remove trials
  df.bhv<-df.bhv[!interaction(df.bhv$id, df.bhv$trial.number) %in% rmv,]
# new split without removed trials
df.bhv_split<-split(df.bhv, interaction(df.bhv$id, df.bhv$trial.number))
# remove responses that occurred before target was shown
  df.bhv_split<-lapply(df.bhv_split, function(x) {
    x<-x[!is.na(x$trial.number),]
    x<-x[!(x$event=="response" & x$time<x$time[x$event=="target"]),]
    return(x)
  })
  #bind as data frame
  df.bhv <-as.data.frame(dplyr::bind_rows(df.bhv_split))
# keep only first response after target was shown
  df.bhv<-df.bhv %>% 
    group_by(id, trial.number) %>% 
    distinct(event, .keep_all = TRUE) #distinct() keeps only first observation if duplicates are detected
# wide format
  df.bhv.wide<-as.data.frame(pivot_wider(df.bhv, id_cols=c(id, trial.number), names_from=c(event,event), values_from=c(time, Stimulus)))
  df.bhv.wide$RT<-ifelse(is.na(df.bhv.wide$Stimulus_response)==T, NA, df.bhv.wide$time_response-df.bhv.wide$time_target)

## define conditions
attach(df.bhv.wide)
# perspective
  df.bhv.wide$perspective[Stimulus_cue_2=="S101" | Stimulus_cue_2=="S102" | Stimulus_cue_2=="S103"]<-"self"
  df.bhv.wide$perspective[Stimulus_cue_2=="S1" | Stimulus_cue_2=="S2" | Stimulus_cue_2=="S3"]<-"other"
# congruency (if perspectives self vs avatar are congruent)
  df.bhv.wide$congruency[Stimulus_target=="S10" | Stimulus_target=="S20" | 
                           Stimulus_target=="S11" | Stimulus_target=="S21" |
                           Stimulus_target=="S12" | Stimulus_target=="S22"] <- "congruent"
  df.bhv.wide$congruency[Stimulus_target=="S13" | Stimulus_target=="S23" | 
                           Stimulus_target=="S14" | Stimulus_target=="S24" |
                           Stimulus_target=="S15" | Stimulus_target=="S25" |
                           Stimulus_target=="S16" | Stimulus_target=="S26" |
                           Stimulus_target=="S17" | Stimulus_target=="S27" |
                           Stimulus_target=="S18" | Stimulus_target=="S28"] <- "incongruent"
# validity
  df.bhv.wide$validity<-"invalid" # set default to invalid and only fill in valid conditions
  df.bhv.wide$validity[Stimulus_cue_2=="S101" & (Stimulus_target=="S10" | Stimulus_target=="S20" | 
                                                   Stimulus_target=="S13"| Stimulus_target=="S23")]<-"valid"
  df.bhv.wide$validity[Stimulus_cue_2=="S102" & (Stimulus_target=="S11" | Stimulus_target=="S21" |
                                                   Stimulus_target=="S14" | Stimulus_target=="S24" |
                                                   Stimulus_target=="S16" | Stimulus_target=="S26")]<-"valid"
  df.bhv.wide$validity[Stimulus_cue_2=="S103" & (Stimulus_target=="S12" | Stimulus_target =="S22" |
                                                   Stimulus_target=="S15" | Stimulus_target=="S25" |
                                                   Stimulus_target=="S17" | Stimulus_target=="S27" |
                                                   Stimulus_target=="S18" | Stimulus_target=="S28")]<-"valid"
  df.bhv.wide$validity[Stimulus_cue_2=="S1" & (Stimulus_target=="S10" | Stimulus_target=="S20" |
                                                 Stimulus_target=="S16" | Stimulus_target=="S26" |
                                                 Stimulus_target=="S17" |Stimulus_target=="S27")]<-"valid"
  df.bhv.wide$validity[Stimulus_cue_2=="S2" & (Stimulus_target=="S11" | Stimulus_target=="S21" |
                                                 Stimulus_target=="S18" | Stimulus_target=="S28")]<-"valid"
  df.bhv.wide$validity[Stimulus_cue_2=="S3" & (Stimulus_target=="S12" | Stimulus_target=="S22")]<-"valid"
# correct responses
  df.bhv.wide$correct_response<-"incorrect"
  df.bhv.wide$correct_response[df.bhv.wide$validity=="valid" & df.bhv.wide$Stimulus_response=="S7"]<-"correct"
  df.bhv.wide$correct_response[df.bhv.wide$validity=="invalid" & df.bhv.wide$Stimulus_response=="S8"]<-"correct"
  df.bhv.wide$correct_response[is.na(df.bhv.wide$RT)]<-"omitted"
  detach(df.bhv.wide)

# define group variable
  df.bhv.wide$group<-"ASD"
  df.bhv.wide$group[grep("HC", df.bhv.wide$id)]<-"NT"
# set factor variables
  df.bhv.wide[sapply(df.bhv.wide, is.character)] <- lapply(df.bhv.wide[sapply(df.bhv.wide, is.character)], as.factor)
  str(df.bhv.wide)

# remove RT outliers
  #get number of omitted responses
  num.om.asd<-nrow(df.bhv.wide[df.bhv.wide$correct_response=="omitted" & df.bhv.wide$group=="ASD",])
  num.om.nt<-nrow(df.bhv.wide[df.bhv.wide$correct_response=="omitted" & df.bhv.wide$group=="NT",])
    #percentage
    num.om.asd/nrow(df.bhv.wide[df.bhv.wide$group=="ASD",])*100
    num.om.nt/nrow(df.bhv.wide[df.bhv.wide$group=="NT",])*100
  #get number of outliers
  num.out.asd<-nrow(df.bhv.wide[(df.bhv.wide$RT>2000 | df.bhv.wide$RT<300) & df.bhv.wide$group=="ASD" & df.bhv.wide$correct_response!="omitted",])
  num.out.nt<-nrow(df.bhv.wide[(df.bhv.wide$RT>2000 | df.bhv.wide$RT<300) & df.bhv.wide$group=="NT" & df.bhv.wide$correct_response!="omitted",])
    #percentage
    num.out.asd/nrow(df.bhv.wide[df.bhv.wide$group=="ASD",])*100
    num.out.nt/nrow(df.bhv.wide[df.bhv.wide$group=="NT",])*100
  #exclude outliers
    df.bhv.wide<-df.bhv.wide[!df.bhv.wide$correct_response=="omitted",]
    df.bhv.wide<-df.bhv.wide[!(df.bhv.wide$RT>2000 | df.bhv.wide$RT<300),]
  
# exclude invalid trials
  df.bhv.wide<-df.bhv.wide[df.bhv.wide$validity=="valid",]
# exclude incorrect responses for RT
  df.bhv.wide.correct<-df.bhv.wide[df.bhv.wide$correct_response=="correct",]
# merge with demogr
  df.bhv.wide.correct<-merge(df.bhv.wide.correct,demogr, by=c("id", "group"), all.x=T)
# for sensitivity analysis: identify participants with averaged accuracy rates < 0.5 
  # without participants with accuracy < 0.5
  head(df.bhv.wide)
  accuracy.func<- function(x) {
    total_trials<-nrow(x)
    correct.responses<-nrow(x[x$correct_response=="correct",])
    accuracy<-correct.responses/total_trials
    x[,"accuracy"]<-rep(accuracy,nrow(x))
    return(x)
  }
  df.bhv.wide_split<-split(df.bhv.wide, interaction(df.bhv.wide$id, df.bhv.wide$perspective, df.bhv.wide$congruency))
  df.bhv.wide.accuracy<-lapply(df.bhv.wide_split, accuracy.func)
  df.bhv.wide.accuracy<-dplyr::bind_rows(df.bhv.wide.accuracy)
  accuracy.agg<-aggregate(accuracy ~ group + id + perspective + congruency, mean, data=df.bhv.wide.accuracy)
  #identify rows (condition x participant interaction)
  accuracy.agg[accuracy.agg$accuracy<=0.5,]
  # test: save participants
  test<-accuracy.agg[accuracy.agg$accuracy<=0.5,]
  test.id<-test$id
  
  
#### GROUP DIFFERENCES: REACTION TIMES ####
  m1.bhv<-lmer(scale(RT)~group*perspective*congruency + (1|id),data=df.bhv.wide.correct)
  anova(m1.bhv)
  # main effects
  emmeans(m1.bhv, list(pairwise ~ congruency))
  emmeans(m1.bhv, list(pairwise ~ perspective))
  # interaction perspective * congruency
    emm_pc <- emmeans(m1.bhv, ~ perspective * congruency)
    pc_consec <- contrast(emm_pc, "consec", simple = "each", combine = TRUE)
    # CIs (unadjusted)
    pc_ci <- as.data.frame(confint(pc_consec, level = 0.95, adjust = "none"))
    # p-values (FDR)
    pc_p  <- as.data.frame(test(pc_consec, adjust = "fdr"))
    # join
    stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                   "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
    join_cols <- intersect(
      setdiff(names(pc_ci), stat_cols),
      setdiff(names(pc_p),  stat_cols)
    )
    pc_tab <- pc_ci %>%
      left_join(pc_p %>% select(all_of(join_cols), p.value), by = join_cols)
    pc_tab
  # interaction congruency * group
    emm_gc <- emmeans(m1.bhv, ~ group * congruency)
    gc_consec <- contrast(emm_gc, "consec", simple = "each", combine = TRUE)
    # CIs (unadjusted)
    gc_ci <- as.data.frame(confint(gc_consec, level = 0.95, adjust = "none"))
    # p-values (FDR)
    gc_p  <- as.data.frame(test(gc_consec, adjust = "fdr"))
    # join columns: compute fresh for THIS table
    stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                   "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
    join_cols_gc <- intersect(
      setdiff(names(gc_ci), stat_cols),
      setdiff(names(gc_p),  stat_cols)
    )
    gc_tab <- gc_ci %>%
      left_join(
        gc_p %>% select(all_of(join_cols_gc), p.value),
        by = join_cols_gc
      )
    gc_tab
  # contrast of contrasts to compare whether congruency effect is larger in ASD compared to NT
    # 1) EMMs for group × congruency
    emm_gc <- emmeans(m1.bhv, ~ congruency | group)
    # 2) Within-group congruency effect: incongruent - congruent
    cong_within_group <- contrast(emm_gc, method = "revpairwise")  
    # 3) Between-group difference of that effect: ASD - NT
    cong_effect_groupdiff <- contrast(cong_within_group, method = "revpairwise", by = NULL)
    # 4) CI (unadjusted)
    ci_tab <- as.data.frame(confint(cong_effect_groupdiff, level = 0.95, adjust = "none"))
    # 5) p-values (FDR; usually 1 test here, but fine)
    p_tab  <- as.data.frame(test(cong_effect_groupdiff, adjust = "fdr"))
    # 6) Combine (simple, robust join on 'contrast')
    out_tab <- ci_tab %>%
      left_join(p_tab %>% select(contrast, p.value), by = "contrast")
    out_tab
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.bhv)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../RT_table.docx")
  
  
  ### Sensitivity analysis: RT without participants who fulfilled ADHD criteria according to FBB-ADHS
  ids_ASD_ADHD <- c(
    "AUT_F_007",
    "AUT_F_012",
    "SOKO_AUT_005",
    "SOKO_AUT_008",
    "SOKO_AUT_010",
    "SOKO_AUT_017",
    "SOKO_AUT_041"
  )
  
  df.bhv.noADHD <- df.bhv.wide.correct %>%
    filter(!id %in% ids_ASD_ADHD)
  
  m1.bhv<-lmer(scale(RT)~group*perspective*congruency + (1|id),data=df.bhv.noADHD)
  anova(m1.bhv)
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1.bhv)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../RT_table_noADHD.docx")
  
  
#### 2.2 ACCURACY (trial-level) ################################################################################################################################################
  # add variable to behavior data frame that indicates correct vs. incorrect responses
  df.bhv.wide
  df.bhv.wide$response.bin<-0
  df.bhv.wide$response.bin[df.bhv.wide$correct_response=="correct"]<-1
  m2.bhv<-glmer(response.bin~group*perspective*congruency+(1|id), family=binomial, data=df.bhv.wide)
  car::Anova(m2.bhv) #calculates Wald statistics
  # main effects
  emmeans(m2.bhv, pairwise ~ perspective)
  emmeans(m2.bhv, pairwise ~ congruency)
  # interaction effect
    m2.emm <- emmeans(m2.bhv, ~ group * perspective * congruency)
    acc_consec <- contrast(
      m2.emm,
      "consec",
      simple  = "each",
      combine = TRUE
    )
    # 1) Unadjusted 95% CIs (log-odds scale)
    acc_ci <- as.data.frame(confint(acc_consec, level = 0.95, adjust = "none"))
    # 2) FDR-adjusted p-values
    acc_p  <- as.data.frame(test(acc_consec, adjust = "fdr"))
    # 3) Join
    stat_cols <- c("estimate","SE","df","z.ratio","statistic",
                   "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
    join_cols <- intersect(
      setdiff(names(acc_ci), stat_cols),
      setdiff(names(acc_p),  stat_cols)
    )
    acc_consec_tab <- acc_ci %>%
      left_join(acc_p %>% select(all_of(join_cols), p.value), by = join_cols)
    acc_consec_tab
  # contrast of contrasts to compare whether congruency effect is larger in ASD compared to NT
    # first-level contrasts
    diffs <- contrast(
      m2.emm,
      method = "revpairwise",
      by     = c("group", "perspective")
    )
    interaction_contrasts <- contrast(
      diffs,
      method = "pairwise",
      by     = "perspective"
    )
    # 1) Unadjusted 95% CIs
    ic_ci <- as.data.frame(confint(interaction_contrasts, level = 0.95, adjust = "none"))
    # 2) FDR-adjusted p-values
    ic_p  <- as.data.frame(test(interaction_contrasts, adjust = "fdr"))
    # 3) Join
    join_cols_ic <- intersect(
      setdiff(names(ic_ci), stat_cols),
      setdiff(names(ic_p),  stat_cols)
    )
    interaction_contrasts_tab <- ic_ci %>%
      left_join(ic_p %>% select(all_of(join_cols_ic), p.value), by = join_cols_ic)
    interaction_contrasts_tab
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(car::Anova(m2.bhv)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../accuracy_table.docx")
  
  
  ### Sensitivity analysis: Accuracy without participants who fulfilled ADHD criteria according to FBB-ADHS
  ids_ASD_ADHD <- c(
    "AUT_F_007",
    "AUT_F_012",
    "SOKO_AUT_005",
    "SOKO_AUT_008",
    "SOKO_AUT_010",
    "SOKO_AUT_017",
    "SOKO_AUT_041"
  )
  
  df.bhv.noADHD <- df.bhv.wide %>%
    filter(!id %in% ids_ASD_ADHD)
  
  m2.bhv<-glmer(response.bin~group*perspective*congruency+(1|id), family=binomial, data=df.bhv.noADHD)
  car::Anova(m2.bhv) 
  # save anova table
  doc_1 <- read_docx()
  table <- round(as.data.frame(car::Anova(m2.bhv)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../accuracy_table_noADHD.docx")
  
  
#### 2.3 BEHAVIORAL DATA predicted by ERP #########################################################################################################
## bring data formats together
# latency
  df.p200.lat<-df.p200.lat[ , ! colnames(df.p200.lat) %in% c("validity","condition") ]
  df.p3.lat<-df.p3.lat[ , ! colnames(df.p3.lat) %in% c("validity","condition") ]
# amplitude
  df.p200.valid.amp
  df.p200.valid.amp.agg<-aggregate(mean_amp ~ group + id + perspective + congruency, mean, data=df.p200.valid.amp)
  names(df.p200.valid.amp.agg)[5]<-"p200.amp"
  df.p3.valid.amp
  df.p3.valid.amp.agg<-aggregate(mean_amp ~ group + id + perspective + congruency, mean, data=df.p3.valid.amp)
  names(df.p3.valid.amp.agg)[5]<-"p3.amp"
# behavioral
  df.bhv.wide.correct
  df.bhv.wide.correct.agg<-aggregate(RT ~ group + id + perspective + congruency, mean, data=df.bhv.wide.correct)
# merge datasets
  df_list <- list(df.p200.lat, df.p3.lat, 
                  df.p200.valid.amp.agg, df.p3.valid.amp.agg, 
                  df.bhv.wide.correct.agg)
# merge all data frames in list
  bhv.erp<-df_list %>% reduce(full_join, by=c("id","group","perspective","congruency"))
  bhv.erp<-bhv.erp[!is.na(bhv.erp$RT),]
  
# long format for LFSW
  df.LFSW.lat<-df.LFSW.lat[ , ! colnames(df.LFSW.lat) %in% c("validity","condition") ]
  levels(df.LFSW.lat$hemisphere)<-c("left", "right")
# amplitude
  df.LFSW.valid.amp
  df.LFSW.valid.amp.agg<-aggregate(mean_amp ~ group + id + perspective + congruency + hemisphere, mean, data=df.LFSW.valid.amp)
  names(df.LFSW.valid.amp.agg)[6]<-"LFSW.amp"
# merge first LFSW
  df.LFSW<-merge(df.LFSW.lat, df.LFSW.valid.amp.agg, by=c("id","group", "perspective","congruency", "hemisphere"))
  df.LFSW<-merge(df.LFSW, df.bhv.wide.correct.agg, by=c("id","group", "perspective","congruency"))

## Associative models by using combined predictors of amplitude and latency per ERP
# p200
  m1<-lm(scale(RT)~scale(p200.amp)+scale(p200.lat), data = bhv.erp[bhv.erp$perspective=="self" & bhv.erp$congruency=="congruent",])
  summary(m1)
  confint(m1)
  m2<-lm(scale(RT)~scale(p200.amp)+scale(p200.lat), data = bhv.erp[bhv.erp$perspective=="other" & bhv.erp$congruency=="congruent",])
  summary(m2)
  confint(m2)
  m3<-lm(scale(RT)~scale(p200.amp)+scale(p200.lat), data = bhv.erp[bhv.erp$perspective=="self" & bhv.erp$congruency=="incongruent",])
  summary(m3)
  confint(m3)
  m4<-lm(scale(RT)~scale(p200.amp)+scale(p200.lat), data = bhv.erp[bhv.erp$perspective=="other" & bhv.erp$congruency=="incongruent",])
  summary(m4)
  confint(m4)
# p3
  m1<-lm(scale(RT)~scale(p3.amp)+scale(p3.lat), data = bhv.erp[bhv.erp$perspective=="self" & bhv.erp$congruency=="congruent",])
  summary(m1)
  confint(m1)
  m2<-lm(scale(RT)~scale(p3.amp)+scale(p3.lat), data = bhv.erp[bhv.erp$perspective=="other" & bhv.erp$congruency=="congruent",])
  summary(m2)
  confint(m2)
  m3<-lm(scale(RT)~scale(p3.amp)+scale(p3.lat), data = bhv.erp[bhv.erp$perspective=="self" & bhv.erp$congruency=="incongruent",])
  summary(m3)
  confint(m3)
  m4<-lm(scale(RT)~scale(p3.amp)+scale(p3.lat), data = bhv.erp[bhv.erp$perspective=="other" & bhv.erp$congruency=="incongruent",])
  summary(m4)
  confint(m4)
# LFSW
#use same method as for pupil response: neweywest-correction to control for autocorrelations
require(sandwich)
library(lmtest)
  m1<-lm(scale(RT)~scale(LFSW.amp):hemisphere+scale(LFSW.lat):hemisphere, data = df.LFSW[df.LFSW$perspective=="self" & df.LFSW$congruency=="congruent",])
  summary(m1)
  coeftest(m1, vcov = NeweyWest(m1))
    # Newey–West–corrected 95% CIs
    nw_vcov <- NeweyWest(m1)
    coefci(m1, vcov. = nw_vcov, level = 0.95)
  m2<-lm(scale(RT)~scale(LFSW.amp):hemisphere+scale(LFSW.lat):hemisphere, data = df.LFSW[df.LFSW$perspective=="other" & df.LFSW$congruency=="congruent",])
  summary(m2)
  coeftest(m2, vcov = NeweyWest(m2))
    # Newey–West–corrected 95% CIs
    nw_vcov <- NeweyWest(m2)
    coefci(m2, vcov. = nw_vcov, level = 0.95)
  m3<-lm(scale(RT)~scale(LFSW.amp):hemisphere+scale(LFSW.lat):hemisphere, data = df.LFSW[df.LFSW$perspective=="self" & df.LFSW$congruency=="incongruent",])
  summary(m3)
  coeftest(m3, vcov = NeweyWest(m3))
    # Newey–West–corrected 95% CIs
    nw_vcov <- NeweyWest(m3)
    coefci(m3, vcov. = nw_vcov, level = 0.95)
  m4<-lm(scale(RT)~scale(LFSW.amp):hemisphere+scale(LFSW.lat):hemisphere, data = df.LFSW[df.LFSW$perspective=="other" & df.LFSW$congruency=="incongruent",])
  summary(m4)
  coeftest(m4, vcov = NeweyWest(m4))
    # Newey–West–corrected 95% CIs
    nw_vcov <- NeweyWest(m4)
    coefci(m4, vcov. = nw_vcov, level = 0.95)

## FIGURE 2: Show aggregated behavioral data (non-stardardized, on a subject-level) ####
# Reaction Times
  RT.agg<-do.call(data.frame, aggregate(RT ~ group + perspective + congruency, function(x) c(mean = mean(x), sd = sd(x)), data=bhv.erp))
  RT.plot<-ggplot(RT.agg, aes(x = perspective, y = RT.mean, fill = congruency)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha=0.7) +  # Barplot
    geom_jitter(data = bhv.erp, aes(x = perspective, y = RT, color = congruency, alpha = 0.4),
                position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0), 
                show.legend = FALSE) +  # Jitter-Datenpunkte direkt über den Balken
    geom_errorbar(aes(ymin = RT.mean - RT.sd, ymax = RT.mean + RT.sd), 
                  position = position_dodge(width = 0.7), width = 0.25, alpha=0.8) +  # Errorbars
    scale_colour_manual(values = c( "#11A57D",   "#D6A633")) +
    scale_fill_manual(values = c("#11A57D",   "#D6A633" )) + theme_classic() +
    facet_wrap(~ group) +  theme(strip.background = element_rect(fill = "lightgrey", color = NA),  #ändert anzeige von facet_wrap
                                 text = element_text(family = "sans")) + # benutzt arial als schriftart
    labs(x="perspective", y="RT (ms)")
# Accuracy
  acc.agg<-do.call(data.frame, aggregate(accuracy ~ group + perspective + congruency, function(x) c(mean = mean(x), sd = sd(x)), data=bhv.erp))
  accuracy.plot<-ggplot(acc.agg, aes(x = perspective, y = accuracy.mean, fill = congruency)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha=0.7) +  # Barplot
    geom_jitter(data = bhv.erp, aes(x = perspective, y = accuracy, color = congruency, alpha = 0.4),
                position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0), 
                show.legend = FALSE) +  # Jitter-Datenpunkte direkt über den Balken
    geom_errorbar(aes(ymin = accuracy.mean - accuracy.sd, ymax = accuracy.mean + accuracy.sd), 
                  position = position_dodge(width = 0.7), width = 0.25, alpha=0.8) +  # Errorbars
    scale_colour_manual(values = c( "#11A57D",   "#D6A633")) +
    scale_fill_manual(values = c("#11A57D",   "#D6A633" )) + theme_classic() +
    facet_wrap(~ group) +  theme(strip.background = element_rect(fill = "lightgrey", color = NA),  #ändert anzeige von facet_wrap
                                 text = element_text(family = "sans")) + # benutzt arial als schriftart
    labs(x="perspective", y="accuracy")
# arrange plots together for Figure
  plot.behav <- ggarrange(RT.plot, accuracy.plot,
                     labels = c("A", "B"), 
                     ncol = 2, nrow = 1,common.legend = T, legend="bottom") 
  plot.behav
  setwd(".../figures")
  ggsave("plot.behav.pdf",width=950, height=381, dpi=600, units="px", scale=6)


####################################################################################################################################################################    
#### 3. EYE TRACKING DATA ##########################################################################################################################################
require(data.table)
require(zoo)

##  read data
  datapath<-".../Eye Tracking/"
  setwd(datapath)
  # read data from datapath and store in according objects
  data.files<-list.files(path=datapath,full.names=T)

# remove empty files
  empty.files<-c("SoKo-HC-022")
  data.files<-data.files[!grepl(empty.files,data.files)] #remove empty files
  empty.files<-c("AUT_F_018")
  data.files<-data.files[!grepl(empty.files,data.files)]
# remove duplicated data sets
  empty.files<-c("SoKo-AUT-017")
  data.files<-data.files[!grepl(empty.files,data.files)] #remove empty files
  empty.files<-c("SoKo-AUT-018")
  data.files<-data.files[!grepl(empty.files,data.files)]
  
  data.events<-data.files[grep('_events',data.files)]
  data.files_ETdata<-data.files[grep('_ETdata',data.files)]

# read in function
  df.list.data<-list(0)
  for(i in 1:length(data.files_ETdata)){
    df.list.data[[i]]<-fread(data.files_ETdata[i])
    print(paste0('read: ',i))
  }
  
  df.list.events<-lapply(data.events,read.csv,header = F, sep=",", dec=".", stringsAsFactors = T)
# rename variables
  data.labels<-c('device_time_stamp',
                 'left_gaze_origin_validity','right_gaze_origin_validity',
                 'left_gaze_origin_in_user_coordinate_system_x','left_gaze_origin_in_user_coordinate_system_y','left_gaze_origin_in_user_coordinate_system_z',
                 'right_gaze_origin_in_user_coordinate_system_x','right_gaze_origin_in_user_coordinate_system_y','right_gaze_origin_in_user_coordinate_system_z',
                 'left_gaze_origin_in_trackbox_coordinate_system_x','left_gaze_origin_in_trackbox_coordinate_system_y','left_gaze_origin_in_trackbox_coordinate_system_z',
                 'right_gaze_origin_in_trackbox_coordinate_system_x','right_gaze_origin_in_trackbox_coordinate_system_y','right_gaze_origin_in_trackbox_coordinate_system_z',
                 'left_gaze_point_validity','right_gaze_point_validity',
                 'left_gaze_point_in_user_coordinate_system_x','left_gaze_point_in_user_coordinate_system_y','left_gaze_point_in_user_coordinate_system_z',
                 'right_gaze_point_in_user_coordinate_system_x','right_gaze_point_in_user_coordinate_system_y','right_gaze_point_in_user_coordinate_system_z',
                 'left_gaze_point_on_display_area_x','left_gaze_point_on_display_area_y',
                 'right_gaze_point_on_display_area_x','right_gaze_point_on_display_area_y',
                 'left_pupil_validity','right_pupil_validity',
                 'left_pupil_diameter','right_pupil_diameter','timestamp')
# label variables
  for(i in 1:length(df.list.data)){names(df.list.data[[i]])<-data.labels}
  df.list.data<-pblapply(df.list.data,setNames,data.labels)
  df.list.events<-pblapply(df.list.events,setNames,c('event','timestamp'))
# id names
  id.names<-substr(data.files_ETdata,nchar(datapath)+1,nchar(datapath)+20) #delete data path, keep file names
  id.names<-gsub("\\_VP.*","",id.names) # keep only names
  id.names<-gsub("\\_Posner.*","",id.names)
  id.names<-gsub("\\_T2.*","",id.names)
  id.names<-gsub("\\_ETdata.*","",id.names)
  id.names<-gsub("\\_2505202.*","",id.names)
  id.names<-gsub("\\_09062023_.*","",id.names)
  id.names<-gsub("/","",id.names)
  names(df.list.data)<-id.names
  names(df.list.events)<-id.names

## Define trial variable (and add to event data)
  df.list.events<-pblapply(df.list.events,function(x){
    # all trials share keywoprd 'ITI' in level label -->
    trial_beginnings<-grep('ITI',x$event)
    trial_components<-diff(trial_beginnings) #line of beginning of every trial
    last_trial_components<-nrow(x)+1-trial_beginnings[length(trial_beginnings)] # last trial needs to be added manually
    trial_components<-c(trial_components,last_trial_components)
    trial_id<-rep(1:length(trial_beginnings),times=trial_components)
    diff.val<-trial_beginnings[1]-1
    trial_id<-c(rep(999, times=diff.val),trial_id) #label trial events before ITI 999
    x<-data.frame(x,trial_id)
    return(x)
  })

## Merge events to eye tracking data (df.list.data) - new merging function ####
  # function to map df_events to df
  eventfunc_moredata<-function(x,y,z,i){
    event<-which(x[,'timestamp']>=y[i,'timestamp'] & x[,'timestamp']<y[i+1,'timestamp']) #which timestamp in data matches timestamp in i-line events
    z[event,]<-y[i,c(1,3)] #allvariables but the timestamp
    return(z)}
  # apply function 
  start.time <- Sys.time()
  df.list<-list()
  for(i in 1:length(df.list.data)){
    add_data<-data.frame(matrix(data=NA, nrow=nrow(df.list.data[[i]]), ncol=2))
    for(j in 1:nrow(df.list.events[[i]])){add_data<-eventfunc_moredata(df.list.data[[i]],df.list.events[[i]],add_data,j)}
    names(add_data)<-names(df.list.events[[i]][c(1,3)])
    add_data$event<-as.factor(add_data$event)
    levels(add_data$event)<-levels(df.list.events[[i]]$event)
    df.list[[i]]<-data.frame(df.list.data[[i]],add_data)
    print(paste0('processed: ',i))
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken

## Define trial timestamp (consecutive samples within trials, starts at ITI)
  # delete NA and events before first ITI
  df.list<-pblapply(df.list, function(x) {
    x<-x[!is.na(x$event),]
    x<-x[!x$trial_id==999,]
  })
  # ts.trial function
  df.list<-pblapply(df.list,function(x){
    ts.trial<-as.numeric(do.call(c,by(x$trial_id,x$trial_id,seq_along))) #trial.ts
    x<-data.frame(x,ts.trial)
  })

## PUPIL DILATION PREPROCESSING ####
# blink correction function
  fun_blink_cor <- function(signal,lower_threshold=23,upper_threshold=75,samples_before=8,samples_after=8) {
    #change NA to 999 for rle()-function
    findna <- ifelse(is.na(signal),999,signal)
    #find blinks:
    #output of rle(): how many times values (NA) are repeated
    repets <- rle(findna)
    #stretch to length of PD vector for indexing
    repets <- rep(repets[["lengths"]], times=repets[["lengths"]])
    #difference between two timestamps~3.33ms -> 75/3.333=22.5 -> wenn 23 Reihen PD=NA, then blink gap
    #if more than 150ms (45 rows) of NA, missing data due to blink unlikely
    #dummy coding of variables (1=at least 23 consecutive repetitions, 0=less than 23 repetitions)
    repets <- ifelse(repets>=lower_threshold & repets<=upper_threshold, 1, 0)
    #exclude cases where other values than NA (999) are repeated >=23 times by changing dummy value to 0:
    repets[findna!=999 & repets==1] <- 0
    #gives out where changes from 0 to 1 or 1 to 0 appear
    changes <- c(diff(repets),0)
    #define start (interval before blink/missing data)
    changes.start<-which(changes==1) #where NA-sequence starts
    #gives out row numbers of NA (blink) and previous 8 frames
    start.seq<-unlist(lapply(changes.start, function(x) {seq(max(x-(samples_before-1),1), x)}))
    repets[start.seq]<-1
    #define end (interval after blink/missing data)
    changes.end<-which(changes==-1)+1 #where NA.sequence ends
    #gives out row numbers of NA (blink) and subsequent 8 frames
    end.seq<-unlist(lapply(changes.end, function(x) {seq(x, min(x+(samples_before-1),length(repets)))}))
    repets[end.seq]<-1
    #replace PD data in blink interval (start to end) with NA
    signal[repets==1]<-NA
    return(signal)
  }

# pd preprocessing function
  pd.preprocess.func<-function(x){

    #define variables
    Left_Diameter<-x$left_pupil_diameter
    Right_Diameter<-x$right_pupil_diameter
    Left_Validity<-x$left_pupil_validity
    Right_Validity<-x$right_pupil_validity
    RemoteTime<-x$timestamp
    
    #contstant for MAD caluclation
    constant<-3

    # STEP 1 - exclude invalid data ####
    pl <- ifelse((Left_Diameter<2|Left_Diameter>8), NA, Left_Diameter)
    pl <- ifelse(Left_Validity<=2,pl,NA)
    pr <- ifelse((Right_Diameter<2|Right_Diameter>8), NA, Right_Diameter)
    pr <- ifelse(Right_Validity<=2,pr,NA)
    # STEP 2 - filtering ####
    ## A) normalized dilation speed, take into account time jumps with Remote timestamps: ####
    #maximum change in pd compared to last and next pd measurement
    #Left
    pl.speed1<-diff(pl)/diff(RemoteTime) #compared to last
    pl.speed2<-diff(rev(pl))/diff(rev(RemoteTime)) #compared to next
    pl.speed1<-c(NA,pl.speed1)
    pl.speed2<-c(rev(pl.speed2),NA)
    pl.speed<-pmax(pl.speed1,pl.speed2,na.rm=T)
    rm(pl.speed1,pl.speed2)
    #Right
    pr.speed1<-diff(pr)/diff(RemoteTime)
    pr.speed2<-diff(rev(pr))/diff(rev(RemoteTime))
    pr.speed1<-c(NA,pr.speed1)
    pr.speed2<-c(rev(pr.speed2),NA)
    pr.speed<-pmax(pr.speed1,pr.speed2,na.rm=T)
    rm(pr.speed1,pr.speed2)
    #median absolute deviation -SPEED
    #constant<-3
    pl.speed.med<-median(pl.speed,na.rm=T)
    pl.mad<-median(abs(pl.speed-pl.speed.med),na.rm = T)
    pl.treshold.speed<-pl.speed.med+constant*pl.mad 
    pr.speed.med<-median(pr.speed,na.rm=T)
    pr.mad<-median(abs(pr.speed-pr.speed.med),na.rm = T)
    pr.treshold.speed<-pr.speed.med+constant*pr.mad 
    #correct pupil dilation for speed outliers
    pl<-ifelse(abs(pl.speed)>pl.treshold.speed,NA,pl)
    pr<-ifelse(abs(pr.speed)>pr.treshold.speed,NA,pr)
    ## B) delete data around blinks 
    pl<-fun_blink_cor(pl)
    pr<-fun_blink_cor(pr)
    
    ## C) normalized dilation size - median absolute deviation -SIZE ####
    #applies a two pass approach
    #first pass: exclude deviation from trend line derived from all samples
    #second pass: exclude deviation from trend line derived from samples passing first pass
    #-> reintroduction of sample that might have been falsely excluded due to outliers
    #estimate smooth size based on sampling rate
    smooth.length<-100 #measured in ms
    #take sampling rate into account (300 vs. 120):
    #smooth.size<-round(smooth.length/mean(diff(RemoteTime)/1000)) #timestamp resolution in microseconds
    smooth.size<-round(smooth.length/median(diff(RemoteTime),na.rm=T)) #timestamp resolution in milliseconds
    is.even<-function(x){x%%2==0}
    smooth.size<-ifelse(is.even(smooth.size)==T,smooth.size+1,smooth.size) #make sure to be odd value (see runmed)
    #Left
    pl.smooth<-na.approx(pl,na.rm=F,rule=2) #impute missing values with interpolation
    #pl.smooth<-runmed(pl.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
    if(sum(!is.na(pl.smooth))!=0){pl.smooth<-runmed(pl.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
    pl.mad<-median(abs(pl-pl.smooth),na.rm=T)
    #Right
    pr.smooth<-na.approx(pr,na.rm=F,rule=2) #impute missing values with interpolation
    #pr.smooth<-runmed(pr.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
    if(sum(!is.na(pr.smooth))!=0){pr.smooth<-runmed(pr.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
    pr.mad<-median(abs(pr-pr.smooth),na.rm=T)
    #correct pupil dilation for size outliers - FIRST pass
    pl.pass1<-ifelse((pl>pl.smooth+constant*pl.mad)|(pl<pl.smooth-constant*pl.mad),NA,pl)
    pr.pass1<-ifelse((pr>pr.smooth+constant*pr.mad)|(pr<pr.smooth-constant*pr.mad),NA,pr)
    #Left
    pl.smooth<-na.approx(pl.pass1,na.rm=F,rule=2) #impute missing values with interpolation
    #pl.smooth<-runmed(pl.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
    if(sum(!is.na(pl.smooth))!=0){pl.smooth<-runmed(pl.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
    pl.mad<-median(abs(pl-pl.smooth),na.rm=T)
    #Right
    pr.smooth<-na.approx(pr.pass1,na.rm=F,rule=2) #impute missing values with interpolation
    #pr.smooth<-runmed(pr.smooth,k=smooth.size) #smooth algorithm by running median of 15 * 3.3ms
    if(sum(!is.na(pr.smooth))!=0){pr.smooth<-runmed(pr.smooth,k=smooth.size)} #run smooth algo only if not all elements == NA
    pr.mad<-median(abs(pr-pr.smooth),na.rm=T)
    #correct pupil dilation for size outliers - SECOND pass
    pl.pass2<-ifelse((pl>pl.smooth+constant*pl.mad)|(pl<pl.smooth-constant*pl.mad),NA,pl)
    pr.pass2<-ifelse((pr>pr.smooth+constant*pr.mad)|(pr<pr.smooth-constant*pr.mad),NA,pr)
    pl<-pl.pass2
    pr<-pr.pass2
    
    # STEP 3 - processing valid samples  ####
    #take offset between left and right into account
    pd.offset<-pl-pr
    pd.offset<-na.approx(pd.offset,rule=2)
    #mean pupil dilation across both eyes
    pl <- ifelse(is.na(pl)==FALSE, pl, pr+pd.offset)
    pr <- ifelse(is.na(pr)==FALSE, pr, pl-pd.offset)
    #smooth data with moving average algorithm
    pl<-frollmean(pl,n=100/3.333,align="center",na.rm=T)
    pr<-frollmean(pr,n=100/3.333,align="center",na.rm=T)
    #interpolation of NA (for <=150ms)
    pl<-na.approx(pl, na.rm=F, maxgap=45, rule=2)
    pr<-na.approx(pr, na.rm=F, maxgap=45, rule=2)
    pd <- (pl+pr)/2
    # end of function --> return ####
    x[,'pd']<-pd
    return(x)
  }
  # apply function
  df.list<-pblapply(df.list,pd.preprocess.func)

## Create id variable
  names(df.list)<-id.names
  #as own variable
  fun_retrieve_id<-function(x,id){
    k<-nrow(x)
    id<-rep(id,k)
    x[,'id']<-id
    return(x)
  }
  df.list<-pbmapply(fun_retrieve_id,x=df.list,id=names(df.list),SIMPLIFY=F)
  #bind as data frame
  df.pd<-dplyr::bind_rows(df.list)
  # correct ID names
  df.pd$id<-toupper(df.pd$id)
  df.pd$id<-gsub("-", "_", df.pd$id)
  df.pd$id<-gsub("ASS", "AUT", df.pd$id)
  df.pd$id<-gsub("F_AUT", "AUT_F", df.pd$id)
  df.pd$id<-gsub("F_AUT", "AUT_F", df.pd$id)
  df.pd$id[df.pd$id=="QS_A_001"]<-"SOKO_AUT_001"

## Define conditions
# function for perspective, congruency, target and cue
  cond.function<-function(x) {
    # perspective
    perspective<-rep(NA, nrow(x))
    perspective<-ifelse(length(which(x$event=="S"))>0, "self", "other")
    # congruency
    congruency<-rep(NA, nrow(x))
    congruency<-ifelse(length(grep("IL", x$event))>0 | length(grep("IR", x$event))>0, "incongruent", "congruent")
    # target variable
    #save events to use in "grep" function
    evts<-c("CL","CR","IL","IR")
    target<-rep(NA, nrow(x))
    #check if there's first (second, third ...) element of evts in data frame, if so, fill target variable with event name derived from first row of this event
    target<-ifelse(test = length(grep(evts[1], x$event))>0,
                   yes = levels(droplevels(x$event[head(grep(evts[1], x$event), 1)])),
                   # if not "CL" (element 1 of evts), check "CR"
                   no = ifelse(test = length(grep(evts[2], x$event))>0,
                               yes = levels(droplevels(x$event[head(grep(evts[2], x$event), 1)])),
                               # if not "CR" (element 2 of evts), check "IL"
                               no = ifelse(test = length(grep(evts[3], x$event))>0,
                                           yes = levels(droplevels(x$event[head(grep(evts[3], x$event), 1)])),
                                           # if not "IL", check "IR"
                                           no = ifelse(test = length(grep(evts[4], x$event))>0,
                                                       yes = levels(droplevels(x$event[head(grep(evts[4], x$event), 1)])),
                                                       #if neither CL, CR, IL, nor IR can be found in a trial (should not happen if there's no error) -> NA
                                                       no = NA))))
    # cue variable
    cue<-rep(NA, nrow(x))
    #check if there's 1,2 or 3 in events in data frame, if so, fill target variable with 1,2,3, respectively
    cue<-ifelse(test = length(which(x$event=="1"))>0,
                yes = "1",
                no = ifelse(test = length(which(x$event=="2"))>0,
                            yes = "2",
                            no = ifelse(test = length(which(x$event=="3"))>0,
                                        yes = "3",
                                        no = NA)))
    # add to data frame
    new_cols <- data.frame(
      perspective = perspective,
      congruency = congruency,
      target = target,
      cue = cue
    )
    x<-cbind(x,new_cols)
    return(x)
  }
  # use cond function to define perspective, congruency, target and cue (latter both needed for validity definition)
  df.pd_split<-split(df.pd, droplevels(interaction(df.pd$id,df.pd$trial_id)))
  df.pd<-pblapply(df.pd_split, cond.function)
  df.pd<-as.data.frame(dplyr::bind_rows(df.pd))

# remove trials without target (e.g. due to system restart)
  df.pd<-df.pd[!is.na(df.pd$target),]

## define validity
  df.pd$validity<-"invalid"
# congruent trials: no need to split for perspective, because validity same for both perspectives
  #congruent left
  df.pd$validity[df.pd$target=="CL1" & df.pd$cue=="1"]<-"valid"
  df.pd$validity[df.pd$target=="CL2" & df.pd$cue=="2"]<-"valid"
  df.pd$validity[df.pd$target=="CL3" & df.pd$cue=="3"]<-"valid"
  #congruent right
  df.pd$validity[df.pd$target=="CR1" & df.pd$cue=="1"]<-"valid"
  df.pd$validity[df.pd$target=="CR2" & df.pd$cue=="2"]<-"valid"
  df.pd$validity[df.pd$target=="CR3" & df.pd$cue=="3"]<-"valid"
# incongruent trials: IL1, IL2, IL3, IR1, IR2, IR3 always incongruent for avatar perspective, because avatar sees 0 points
# (all points on one wall, max points needs to be seen by participant, but incongruent persp.)
  #incongruent left
  df.pd$validity[df.pd$target=="IL1" & df.pd$cue=="1" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL2" & df.pd$cue=="2" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL3" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL11" & df.pd$cue=="2" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL11" & df.pd$cue=="1" & df.pd$perspective=="other"]<-"valid"
  df.pd$validity[df.pd$target=="IL12" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL12" & df.pd$cue=="1" & df.pd$perspective=="other"]<-"valid"
  df.pd$validity[df.pd$target=="IL21" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IL21" & df.pd$cue=="2" & df.pd$perspective=="other"]<-"valid"
  #incongruent right
  df.pd$validity[df.pd$target=="IR1" & df.pd$cue=="1" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR2" & df.pd$cue=="2" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR3" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR11" & df.pd$cue=="2" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR11" & df.pd$cue=="1" & df.pd$perspective=="other"]<-"valid"
  df.pd$validity[df.pd$target=="IR12" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR12" & df.pd$cue=="2" & df.pd$perspective=="other"]<-"valid"
  df.pd$validity[df.pd$target=="IR21" & df.pd$cue=="3" & df.pd$perspective=="self"]<-"valid"
  df.pd$validity[df.pd$target=="IR21" & df.pd$cue=="1" & df.pd$perspective=="other"]<-"valid"

## ADD TARGET TIMESTAMP (0 at target onset, negative timestamps before that)
  tgt.ts.func<-function(x) {
    start.target<-head(which(x$event==x$target),1)
    start.seq<-1-start.target
    end.seq<-nrow(x)-start.target
    ts.target<-seq(from=start.seq, to=end.seq, by=1)
    x[,'ts.target']<-ts.target
    return(x)
  }
  # apply function for each trial of each participant
  df.pd_split<-split(df.pd, droplevels(interaction(df.pd$id,df.pd$trial_id)))
  df.pd<-pblapply(df.pd_split, tgt.ts.func)
  df.pd<-as.data.frame(dplyr::bind_rows(df.pd))

## Define relative pupil dilation (rpd)
  rpd.function<-function(x) {
    #baseline PD size
    bps.mean<-mean(x$pd[(x$ts.target>(-150/3.3333)) & (x$ts.target<0)], na.rm = T) #first 150ms (during fix cross before target)
    bps<-rep(bps.mean, times=nrow(x)) #repeat baseline size by length of trial
    #relative PD variable
    rpd<-x$pd-bps
    x[,'bps']<-bps
    x[,'rpd']<-rpd
    return(x)
  }
  # apply function for each trial of each participant
  df.pd_split<-split(df.pd, droplevels(interaction(df.pd$id,df.pd$trial_id)))
  df.pd<-pblapply(df.pd_split, rpd.function)
  df.pd<-as.data.frame(dplyr::bind_rows(df.pd))
  # exclude SOKO_AUT_018 (timestamps weren't correctly matched due to gaps in data, also gets excluded because of too low ADOS score)
  df.pd<-df.pd[!df.pd$id=="SOKO_AUT_018",] # gets anyway excluded because ADOS score

# define gaze deviation (from the center of the screen)
  gazedev.func<-function(x) {
    xl<-x$left_gaze_point_on_display_area_x
    xr<-x$right_gaze_point_on_display_area_x
    yl<-x$left_gaze_point_on_display_area_y
    yr<-x$right_gaze_point_on_display_area_y
    x.offset<-xl-xr
    x.offset<-na.approx(x.offset,rule=2)
    y.offset<-yl-yr
    y.offset<-na.approx(y.offset,rule=2)
    xl <- ifelse(is.na(xl)==FALSE, xl, xr+x.offset)
    xr <- ifelse(is.na(xr)==FALSE, xr, xl-x.offset)
    yl <- ifelse(is.na(yl)==FALSE, yl, yr+y.offset)
    yr <- ifelse(is.na(yr)==FALSE, yr, yl-y.offset)
    gazepos.x<-(xl+xr)/2
    gazepos.y<-(yl+yr)/2
  
    #remove outside screen
    gazepos.x<-ifelse(gazepos.x>1 | gazepos.x<0,NA,gazepos.x)
    gazepos.y<-ifelse(gazepos.y>1 | gazepos.y<0,NA,gazepos.y)
  
    #estimate center deviation
    center_deviation<-sqrt((gazepos.x-0.5)^2 + (gazepos.y-0.5)^2)
  
    x[,'gazepos.x']<-gazepos.x
    x[,'gazepos.y']<-gazepos.y
    x[,'center_dev']<-center_deviation
  
    return(x)
  }
  # apply function
  df.pd<-gazedev.func(df.pd)

# delete unneccessary variables
  df.pd <- df.pd[c("timestamp", "event", "trial_id", "ts.trial","pd","id","perspective","congruency",
                    "target","cue","validity","ts.target","bps","rpd","gazepos.x","gazepos.y","center_dev")]

# exclude trials that were too long -> lagging might have occurred during the cues or afterwards
  # maximal length of trial if everything went smoothly
  max.length<-650+750+300+750+350+2000
  max.length<-max.length+200 #add 200ms window in case of just brief lag
  # find length of trials
  df.pd_split<-split(df.pd, droplevels(interaction(df.pd$id, df.pd$trial_id)))
  max.trial<-lapply(df.pd_split, function(x) {max(x$ts.trial)})
  # find which trials are longer than defined max length should be
  excl<-max.trial[max.trial>(max.length/3.333)]
  # exclude trials that are too long by finding the rows via interaction terms
  df.pd$interaction<-droplevels(interaction(df.pd$id, df.pd$trial_id))
  df.pd<-df.pd[!df.pd$interaction  %in% names(excl),]
  # check for ts.target
  min(df.pd$ts.target)*3.333 
  # optimally, cut should be at 2800 (according to stimulus presentation maximum of ITI+cues --> only less than 100ms drüber -> pass
  
# define group variable
  df.pd$group<-"ASD"
  df.pd$group[grep("HC",df.pd$id)]<-"NT"
# turn character values into factor values
  df.pd[sapply(df.pd, is.character)] <- lapply(df.pd[sapply(df.pd, is.character)], as.factor) #turn character-vectors into factors
# delete participants according to demogr -> delete all participants that were not matched for EEG data
  df.pd<-df.pd[df.pd$id %in% demogr$id,]
  df.pd$id<-droplevels(df.pd$id)
  
## get variable for correct vs. incorrect responses
  resp.func<-function(x) {
    # get index of whether the specific event has occurred or not
    response.miss<-ifelse(length(which(x$event=="miss"))>0,1,0)
    response.incorrect<-ifelse(length(which(x$event=="incorrect"))>0,1,0)
    response.correct<-ifelse(length(which(x$event=="correct"))>0,1,0)
    response.seven<-ifelse(length(which(x$event=="7"))>0,1,0)
    response.eight<-ifelse(length(which(x$event=="8"))>0,1,0)
    all.responses<-c(response.miss, response.incorrect, response.correct, response.seven, response.eight)
    # get time points for when the specific events started, but only if after 300ms (excludes responses <300ms from being counted later)
    time.miss<-x$ts.trial[x$event=="miss"][1] # get first entry of timestamps of rows that have response
    time.incorrect<-ifelse(x$ts.trial[x$event=="incorrect"][1]>300/3.333, x$ts.trial[x$event=="incorrect"][1], NA)  
    time.correct<-ifelse(x$ts.trial[x$event=="correct"][1]>300/3.333, x$ts.trial[x$event=="correct"][1], NA)  
    time.seven<-ifelse(x$ts.trial[x$event=="7"][1]>300/3.333, x$ts.trial[x$event=="7"][1], NA) 
    time.eight<-ifelse(x$ts.trial[x$event=="8"][1]>300/3.333, x$ts.trial[x$event=="8"][1], NA) 
    all.responses<-rbind(all.responses, c(time.miss, time.incorrect, time.correct, time.seven, time.eight))
    colnames(all.responses)<-c("miss", "incorrect", "correct", "7", "8")
    rownames(all.responses)<-c("available","time")
    # TRUE/FALSE index for which events occurred and translate it to event name
    index.trigger.available<-all.responses["available",]>0
    index.trigger.available<-which(index.trigger.available==T)
    # get number of response events that occurred (should optimally be 1 (7 or 8)
    # but might be >1 if participant pressed keyboard more than once
    # if 7+7 or 8+8, correctness doesn't change, but if both have been pressed, take only the initial response
    # might be 0 if no response was given
    number.response.triggers<-sum(all.responses["available",]>0)
    # print name of response when only 1 was given, name of initial response if >1 was given and NA if 0 was given
    response<-ifelse(number.response.triggers==1, names(index.trigger.available), 
                     ifelse(number.response.triggers==0, NA, names(which(all.responses["time",]==min(all.responses["time",],na.rm=T)))))
    # add to data frame
    x[,'Stimulus_response']<-rep(response,nrow(x))
    return(x)
  }
  df.pd_split<-split(df.pd, droplevels(interaction(df.pd$id, df.pd$trial_id)))
  # function will give out warning for cases in which responses were given, but too early, so time == NA to exclude (e.g. SOKO_AUT_041, trial 133 --> warning can be ignored
  df.pd<-dplyr::bind_rows(df.pd)
  # check assigned stimulus responses: since only 7 and 8 is included in df.pd, we only have to handle 7 and 8 in the following steps
  # (as long trials were excluded and correct/incorrect did only appear after 2000ms target display)
  table(df.pd$Stimulus_response)
  
# sign correct vs. incorrect responses
  df.pd$correct_response<-"incorrect"
  df.pd$correct_response[df.pd$validity=="valid" & df.pd$Stimulus_response=="7"]<-"correct"
  df.pd$correct_response[df.pd$validity=="invalid" & df.pd$Stimulus_response=="8"]<-"correct"
# delete invalid trials
  df.pd<-df.pd[df.pd$validity=="valid",]  
# create only target data set
  # delete time window before baseline period (cues etc., only last 500ms of fixation cross + target included)
  df.pd.target<-df.pd[df.pd$ts.target>(-150/3.333),]

## calculate constriction amplitude and latency (to residualize out of pupil response during target presentation)
  lat.func<-function(x){
    min_start<-min(x$pd[which(x$ts.target>(100/3.333) & x$ts.target<(1000/3.333))],na.rm=T) #min value after start value but within first second
    constric_lat<-x$ts.target[which(x$pd==min_start)] #pupil constriction latency
    constric_lat<-ifelse(length(constric_lat)==0,NaN,constric_lat) #set value to NA if no constric_lat can be returned
    min_start_long<-rep(min_start, times=nrow(x)) #repeat baseline size by length of trial
    dil_amp<-x$pd-min_start
    cons_amp<-x$bps-min_start_long
    x[,"constric_min"]<-min_start_long
    x[,"constrict_lat"]<-constric_lat
    x[,"dil_amp"]<-dil_amp
    x[,"cons_amp"]<-cons_amp
    return(x)
  }
  # apply function for each trial of each participant
  df.pd.target_split<-split(df.pd.target, droplevels(interaction(df.pd.target$id,df.pd.target$trial_id)))
  df.pd.target<-pblapply(df.pd.target_split, lat.func)
  df.pd.target<-as.data.frame(dplyr::bind_rows(df.pd.target))

#### Demographics for PUPIL SAMPLE ####
  demogr.pd<-demogr[demogr$id %in% df.pd$id,]
  # number participants
  table(demogr.pd$group)
  # sex
  table(demogr.pd$group, demogr.pd$Geschlecht_Index)
  chisq.test(table(demogr.pd$group, demogr.pd$Geschlecht_Index))
  #get statistics for dimensional variables
  by(demogr.pd, demogr.pd$group, psych::describe)
  # age
  t.test(demogr.pd$age[demogr.pd$group=="ASD"], demogr.pd$age[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  # IQ
  t.test(demogr.pd$IQ[demogr.pd$group=="ASD"], demogr.pd$IQ[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  # CBCL
  t.test(demogr.pd$CBCL_T_INT[demogr.pd$group=="ASD"], demogr.pd$CBCL_T_INT[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  t.test(demogr.pd$CBCL_T_EXT[demogr.pd$group=="ASD"], demogr.pd$CBCL_T_EXT[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  t.test(demogr.pd$CBCL_T_GES[demogr.pd$group=="ASD"], demogr.pd$CBCL_T_GES[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  # handedness
  t.test(demogr.pd$LQ[demogr.pd$group=="ASD"], demogr.pd$LQ[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
  # SRS
  t.test(demogr.pd$Gesamt_TW_AB[demogr.pd$group=="ASD"], demogr.pd$Gesamt_TW_AB[demogr.pd$group=="NT"], paired=F, alternative = "two.sided")
# merge demogr and df.pd
  df.pd<-merge(df.pd, demogr.pd, by=c("id", "group"), all.x=T, all.y=T)
  df.pd.target<-merge(df.pd.target, demogr.pd, by=c("id", "group"), all.x=T, all.y=T)

## Calculate separate time windows for PD target 
  df.pd.target$bin<-NA
  df.pd.target$bin[df.pd.target$ts.target*3.333<500]<-"0"
  df.pd.target$bin[df.pd.target$ts.target*3.333>=500 & df.pd.target$ts.target*3.333<1000]<-"1"
  df.pd.target$bin[df.pd.target$ts.target*3.333>=1000 & df.pd.target$ts.target*3.333<1500]<-"2"
  df.pd.target$bin[df.pd.target$ts.target*3.333>=1500]<-"3"
# filter for correct trials
  df.pd.target.correct<-df.pd.target[df.pd.target$correct_response=="correct",]

## calculate onscreentime
  # exclude na
  df.pd.target.correct_clean<-df.pd.target.correct[!is.na(df.pd.target.correct$rpd),]
  df.pd.target.correct_clean<-df.pd.target.correct_clean[df.pd.target.correct_clean$ts.target>=0,] #exclude time points before target
  df.pd.target.correct_clean<-df.pd.target.correct_clean[df.pd.target.correct_clean$ts.target*3.333<=2000,] #exclude "too long" timepoints
  # define onscreen function
  onscreen.func<-function(x){
    max.dur<-2000
    onscreentime<-((nrow(x)*3.333)/max.dur)*100
    id<-x$id[1]
    trial_id<-x$trial_id[1]
    group<-x$group[1]
    df.onscreen<-data.frame(id,trial_id,onscreentime,group)
    return(df.onscreen)
  }
  # apply function
  df.pd.target.correct_clean_split<-split(df.pd.target.correct_clean, droplevels(interaction(df.pd.target.correct_clean$id,df.pd.target.correct_clean$trial_id)))
  onscreen_df<-pblapply(df.pd.target.correct_clean_split, onscreen.func)
  onscreen_df<-as.data.frame(dplyr::bind_rows(onscreen_df))
  onscreen_df<-aggregate(onscreentime ~ id + group, mean, na.rm=T, data=onscreen_df)
  # compare onscreen time between groups
  by(onscreen_df, onscreen_df$group, psych::describe)
  t.test(onscreen_df$onscreentime[onscreen_df$group=="ASD"], onscreen_df$onscreentime[onscreen_df$group=="NT"])
  
## aggregate rpd for bins
  agg.pd.target<-aggregate(rpd~id + trial_id + group + congruency + perspective + age + bin + cons_amp + constrict_lat, mean, na.rm=T, data=df.pd.target.correct[!df.pd.target.correct$bin=="0",])
## add center deviation
  center_deviation<-aggregate(center_dev~id + trial_id + group + congruency + perspective + age + bin + cons_amp + constrict_lat, mean, na.rm=T, data=df.pd.target.correct[!df.pd.target.correct$bin=="0",])
  agg.pd.target<-merge(agg.pd.target, center_deviation, by=c("id","trial_id","group","congruency","perspective","age","bin","cons_amp","constrict_lat"), all.x=T)

## residuals controlling for constriction amplitude + latency
  lm.rpd.res<-lm(rpd ~ cons_amp + constrict_lat, data=agg.pd.target) # extract residuals to correct for constriction
  agg.pd.target$rpd.res<-lm.rpd.res$residuals
  
############################### 3.1 Power analysis for the pupillometry sub-sample (estimates based on pupil data) #######################################################################################
  ## Start from observed design 
  design <- agg.pd.target %>%
    transmute(
      id = factor(id),
      group = factor(group),
      perspective = factor(perspective),
      congruency = factor(congruency),
      bin = as.factor(bin)
    )
  
  ## check trial counts 
  design %>% count(id) %>% summarise(median = median(n), min = min(n), max = max(n))
  
  # Set assumptions
  icc <- 0.3
  total_sd <- 1
  rand_sd  <- sqrt(icc) * total_sd # random intercept SD
  resid_sd <- sqrt(1 - icc) * total_sd  # residual SD
  
  # build the model matrix
  m_template <- makeLmer(
    y ~ group * perspective * congruency * bin + (1|id),
    fixef = rep(0, 1 + (ncol(model.matrix(~ group*congruency*perspective*bin, design)) - 1)),
    VarCorr = list(id = rand_sd^2),
    sigma = resid_sd,
    data = design
  )
  
  ## Power for two-way interaction effect (group x perspective)
  # set effect size 
  names(fixef(m_template))
  coef_name <- "groupNT:perspectiveself"  
  target_beta <- 0.18   # standardized effect size for the interaction effect
  fixef(m_template)[coef_name] <- target_beta
  
  # Power for detecting a two-way interaction effect with small effect size
  powerSim(
    m_template,
    fixed(coef_name, "t"),
    nsim = 500,
    progress = TRUE
  )
  
  ## three-way interaction effect
  # set effect size
  names(fixef(m_template))
  coef_name <- "groupNT:perspectiveself:congruencyincongruent"  
  target_beta <- 0.20  
  fixef(m_template)[coef_name] <- target_beta
  
  # run power analysis
  powerSim(
    m_template,
    fixed(coef_name, "t"),
    nsim = 500,
    progress = TRUE
  )
  
  # set higher effect size
  target_beta <- 0.27 
  fixef(m_template)[coef_name] <- target_beta
  
  powerSim(
    m_template,
    fixed(coef_name, "t"),
    nsim = 500,
    progress = TRUE
  )
  
  
############################### 3.2 Group differences in pupil response (SEPR) #######################################################################################
# run model
  m2<-lmer(scale(rpd.res)~group*congruency*perspective*as.factor(bin) + scale(age) + (1|id), data=agg.pd.target)
  anova(m2)
  # main effects
  contrast(emmeans(m2, ~congruency),"pairwise")
  contrast(emmeans(m2, ~bin),"pairwise")
  # post-hoc effects:
    # helper function
    emm_ci_fdr <- function(contrast_obj) {
      
      ci  <- as.data.frame(confint(contrast_obj, level = 0.95, adjust = "none"))
      p   <- as.data.frame(test(contrast_obj, adjust = "fdr"))
      
      stat_cols <- c("estimate","SE","df","t.ratio","z.ratio","statistic",
                     "lower.CL","upper.CL","asymp.LCL","asymp.UCL","p.value")
      
      join_cols <- intersect(
        setdiff(names(ci), stat_cols),
        setdiff(names(p),  stat_cols)
      )
      
      ci %>%
        left_join(p %>% select(all_of(join_cols), p.value), by = join_cols)
    }
  # interaction effects
  # congruency * bin
  emm_cb <- emmeans(m2, ~ congruency * bin)
  cb_consec <- contrast(emm_cb, "consec", simple = "each", combine = TRUE)
  cb_tab <- emm_ci_fdr(cb_consec)
  cb_tab
  # group*bin
  emm_gb <- emmeans(m2, ~ group * bin)
  gb_consec <- contrast(emm_gb, "consec", simple = "each", combine = TRUE)
  gb_tab <- emm_ci_fdr(gb_consec)
  gb_tab
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../SEPR_table.docx")

#### SUPPLEMENTARY ANALYSES ###
# Model with additional covariate of gaze deviation 
  m2<-lmer(scale(rpd.res)~group*congruency*perspective*as.factor(bin) + scale(age) + scale(center_dev) + (1|id), data=agg.pd.target)
  anova(m2)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../SEPR_gazedeviation_table.docx")

# Model with constriction and latency as covariates instead of residuals 
  m2<-lmer(scale(rpd)~group*congruency*perspective*as.factor(bin) + scale(age) + scale(cons_amp) + scale(constrict_lat) + (1|id), data=agg.pd.target)
  anova(m2)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m2)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../PD_withoutresiduals_table.docx")

#### Figure 4: plot SEPR progression and gaze deviation ####
  # SEPR progression
  df.plot<-aggregate(rpd~id+group+ts.target+bin, mean, na.rm=T, data=df.pd.target[df.pd.target$correct_response=="correct",])
  pd.plot<-ggplot(df.plot[df.plot$ts.target*3.333<2000,], aes(x=ts.target*3.333, y=rpd)) +
    geom_vline(xintercept=0, color="black")+
    geom_vline(xintercept=500, color="darkgrey", linetype="dashed")+
    geom_vline(xintercept=1000, color="darkgrey", linetype="dashed")+
    geom_vline(xintercept=1500, color="darkgrey", linetype="dashed")+
    annotate(geom = "rect", xmin = 500, xmax = 2000, ymin = -Inf, ymax = Inf, fill = "lightgrey", alpha = 0.3) + 
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha=0.45, aes(fill=group)) + 
    stat_summary(fun.data = mean_se, geom = "smooth", aes(color=group)) + 
    scale_colour_manual(values = c("#C96215",   "#AD3B76" )) +
    scale_fill_manual(values = c("#C96215",   "#AD3B76"  )) +
    theme(text=element_text(size=16), strip.text.x = element_text(size=20)) + 
    theme_classic() +  theme(text = element_text(family = "sans")) + # benutzt arial als schriftart
    annotate("text", x=750, y=-0.02, label= "TW-1", size=4) + 
    annotate("text", x=1250, y=-0.02, label= "TW-2", size=4) + 
    annotate("text", x=1750, y=-0.02, label= "TW-3", size=4) + 
    theme(legend.background = element_rect(fill="white",  size=0.5, linetype="solid"))+
    labs(x="time of target presentation (ms)", y="baseline corrected pupil size") + theme(legend.key.size = unit(0.3, 'cm'))
  # gaze plot during target presentation
  gaze.plot<-ggplot(df.pd.target,aes(x=gazepos.x, y=gazepos.y)) + 
    ylim(1,0) + xlim(0,1) +
    geom_hex(aes(color=..count..),bins=200) + 
    scale_fill_viridis(name='density', breaks=c(1000,25500),
                       labels=c("low","high"))+
    scale_color_viridis(name='density', breaks=c(1000,25500),
                        labels=c("low","high"))+
    labs(x="screen x-axis", y="screen y-axis") +   theme_classic() +   #theme(text=element_text(size=16), strip.text.x = element_text(size=20)) + 
    facet_wrap(~group) +  theme(strip.background = element_rect(fill = "lightgrey", color = NA),  #ändert anzeige von facet_wrap
                                 text = element_text(family = "sans")) + # benutzt arial als schriftart 
    theme(panel.spacing = unit(1.25, "lines")) + theme(legend.key.size = unit(0.3, 'cm'))
  # combine plots for Figure
  plot.ET <- ggarrange(pd.plot, gaze.plot,
                          labels = c("A", "B"), 
                          ncol = 2, nrow = 1) 
  plot.ET
  setwd(".../figures")
  ggsave("plot.ET.pdf",width=950, height=381, dpi=600, units="px", scale=6)
  



############################### 3.3 Behavioral data (RT) predicted by pupil response #######################################################################################
# aggregate pupil data on condition level
  agg.pd.target.condition.long<-aggregate(rpd.res ~ id + group + congruency + perspective + bin, mean, na.rm=T, data=agg.pd.target)
  agg.pd.target.condition<-as.data.frame(pivot_wider(agg.pd.target.condition.long, names_from = "bin", names_prefix="rpd_bin", values_from = rpd.res))
  bhv.erp.pd<-merge(agg.pd.target.condition, bhv.erp, by=c("id", "group","congruency", "perspective"), all.x=T)
  
  # add LFSW in wide format
  df.LFSW.wide <- df.LFSW %>%
    pivot_wider(
      id_cols = c(id, group, perspective, congruency),
      names_from = hemisphere,
      values_from = c(LFSW.lat, LFSW.amp),
      names_glue = "{.value}_{hemisphere}"
    )
  bhv.erp.pd <- bhv.erp.pd %>%
    left_join(
      df.LFSW.wide,
      by = c("id", "group", "perspective", "congruency")
    )
  
# calculate predictions 
  #use newey-west correction to account for autocorrelations -> gets robust standard errors
  #use because assumption testing shows autocorrelation of errors
  require(sandwich)
  library(lmtest)
  # NeweyWest method corrects for autocorrelation and heteroscedasticity
  agg.pd.target.condition.long<-merge(agg.pd.target.condition.long, bhv.erp, by=c("id", "group","congruency", "perspective"), all.x=T)
  #congruent-self
  m1<-lm(scale(RT)~scale(rpd.res):bin, data = agg.pd.target.condition.long[agg.pd.target.condition.long$perspective=="self" & agg.pd.target.condition.long$congruency=="congruent",])
  summary(m1)
  coeftest(m1, vcov = NeweyWest(m1))
    # robust 95% CIs
    ci1 <- coefci(m1, vcov. = NeweyWest(m1), level = 0.95)
    ci1
  #congruent-other
  m2<-lm(scale(RT)~scale(rpd.res):bin, data = agg.pd.target.condition.long[agg.pd.target.condition.long$perspective=="other" & agg.pd.target.condition.long$congruency=="congruent",])
  summary(m2)
  coeftest(m2, vcov = NeweyWest(m2))
    # robust 95% CIs
    ci1 <- coefci(m1, vcov. = NeweyWest(m1), level = 0.95)
    ci1
  #incongruent-self
  m3<-lm(scale(RT)~scale(rpd.res):bin, data = agg.pd.target.condition.long[agg.pd.target.condition.long$perspective=="self" & agg.pd.target.condition.long$congruency=="incongruent",])
  summary(m3)
  coeftest(m3, vcov = NeweyWest(m3))
    # robust 95% CIs
    ci1 <- coefci(m3, vcov. = NeweyWest(m3), level = 0.95)
    ci1
  #incongruent-other
  m4<-lm(scale(RT)~scale(rpd.res):bin, data = agg.pd.target.condition.long[agg.pd.target.condition.long$perspective=="other" & agg.pd.target.condition.long$congruency=="incongruent",])
  summary(m4)
  coeftest(m4, vcov = NeweyWest(m4))
    # robust 95% CIs
    ci1 <- coefci(m4, vcov. = NeweyWest(m4), level = 0.95)
    ci1


############################### 3.4 Prediction of RT by combined pupil response and ERPs #######################################################################################
## self-incongruent condition = only condition with significant prediction of RT by SEPR and ERPs indices (as identified by models testing prediction of RT)
## in this condition, significant predictors were P3 amplitude, P3 latency, and SEPR during TW-3

# show that P3 ERP associations are also observable in subsample 
  m1<-lm(scale(RT) ~ scale(p3.amp) + scale(p3.lat), 
         data=bhv.erp.pd[bhv.erp.pd$congruency=="incongruent" & bhv.erp.pd$perspective=="self",])
  summary(m1)
  confint(m1)

# add SEPR
  m2<-lm(scale(RT) ~ scale(rpd_bin3)*scale(p3.amp) + scale(rpd_bin3)*scale(p3.lat), 
         data=bhv.erp.pd[bhv.erp.pd$congruency=="incongruent" & bhv.erp.pd$perspective=="self",])
  summary(m2)
  confint(m2)
    
    
############################### 3.5 Associations of pupil response and ERPs #######################################################################################

#### Table 2: Correlation table between neurophysiological measures ####    
## Cells show: r + uncorrected stars; bold if FDR-significant

  # ------------------ define SEPR (rows) and ERP (cols) ------------------
  vars_sepr <- c("rpd_bin1", "rpd_bin2", "rpd_bin3")
  vars_erp  <- c(
    "p200.amp", "p200.lat",
    "p3.amp", "p3.lat",
    "LFSW.amp_left", "LFSW.lat_left",
    "LFSW.amp_right", "LFSW.lat_right"
  )
  
  pretty_names <- c(
    "rpd_bin1" = "SEPR TW-1",
    "rpd_bin2" = "SEPR TW-2",
    "rpd_bin3" = "SEPR TW-3",
    "p200.amp" = "P200 amp",
    "p200.lat" = "P200 lat",
    "p3.amp"   = "P3 amp",
    "p3.lat"   = "P3 lat",
    "LFSW.amp_left"  = "LFSW amp left",
    "LFSW.lat_left"  = "LFSW lat left",
    "LFSW.amp_right" = "LFSW amp right",
    "LFSW.lat_right" = "LFSW lat right"
  )
  
  # ------------------ helper: correlation table long ------------------
  make_corr_table <- function(df,
                              sepr = vars_sepr,
                              erp  = vars_erp,
                              method = "pearson") {
    
    sepr <- sepr[sepr %in% names(df)] %>% unique()
    erp  <- erp[erp  %in% names(df)] %>% unique()
    
    X <- df %>%
      dplyr::select(all_of(c(sepr, erp))) %>%
      tidyr::drop_na() %>%
      as.data.frame()
    
    tidyr::expand_grid(sepr = sepr, erp = erp) %>%
      mutate(
        r = purrr::map2_dbl(sepr, erp, ~ suppressWarnings(cor(X[[.x]], X[[.y]], method = method))),
        p = purrr::map2_dbl(sepr, erp, ~ suppressWarnings(cor.test(X[[.x]], X[[.y]], method = method))$p.value)
      )
  }
  
  # ------------------ helper: insert condition header rows ------------------
  make_corr_table_with_headers <- function(df_wide) {
    out <- list()
    
    for (cnd in unique(df_wide$cond)) {
      # header row: same columns, empty cells, condition label in sepr
      header <- df_wide[1, ]
      header[,] <- ""
      header$sepr <- paste0("**", as.character(cnd), "**")
      header$cond <- cnd
      
      rows <- df_wide[df_wide$cond == cnd, ]
      out[[as.character(cnd)]] <- rbind(header, rows)
    }
    
    final <- do.call(rbind, out)
    rownames(final) <- NULL
    final$cond <- NULL
    final
  }
  
  # ------------------ 1) create condition variable ------------------
  bhv.erp.pd <- bhv.erp.pd %>%
    mutate(cond = paste(congruency, perspective, sep = " – "))
  
  # (optional) set desired order of conditions
  bhv.erp.pd$cond <- factor(bhv.erp.pd$cond, levels = c(
    "congruent – self",
    "congruent – other",
    "incongruent – self",
    "incongruent – other"
  ))
  
  # ------------------ 2) correlations (long) ------------------
  res_all <- bhv.erp.pd %>%
    group_by(cond) %>%
    group_modify(~ make_corr_table(.x)) %>%
    ungroup()
  
  # ------------------ 3) FDR within each condition ------------------
  alpha <- 0.05
  res_all_fdr <- res_all %>%
    group_by(cond) %>%
    mutate(p_fdr = p.adjust(p, method = "fdr")) %>%
    ungroup()
  
  # ------------------ 4) format cells: r + stars (raw p), bold if FDR ------------------
  corr_table <- res_all_fdr %>%
    mutate(
      stars = case_when(
        p < .001 ~ "***",
        p < .01  ~ "**",
        p < .05  ~ "*",
        TRUE     ~ ""
      ),
      r_txt = sprintf("%.2f", r),
      # bold r if FDR-significant
      r_fmt = ifelse(p_fdr < alpha, paste0("**", r_txt, "**"), r_txt),
      # final cell: r + uncorrected stars
      cell = paste0(r_fmt, stars)
    ) %>%
    select(cond, sepr, erp, cell)
  
  # ------------------ 5) pivot to wide table ------------------
  corr_table_wide <- corr_table %>%
    mutate(
      sepr = dplyr::coalesce(pretty_names[sepr], sepr),
      erp  = dplyr::coalesce(pretty_names[erp],  erp)
    ) %>%
    pivot_wider(
      id_cols    = c(cond, sepr),
      names_from = erp,
      values_from = cell
    ) %>%
    arrange(cond, sepr)
  
  # ------------------ 6) add condition header rows and prep for Word ------------------
  final_corr_table <- make_corr_table_with_headers(corr_table_wide)
  final_corr_table <- as.data.frame(final_corr_table)
  colnames(final_corr_table)[1] <- ""  # empty first column header like your other tables
  
  # ------------------ 7) export to Word using officer ------------------
  doc_1 <- read_docx()
  
  doc_1 <- body_add_table(doc_1, final_corr_table, style = "table_template")
  
  print(
    doc_1,
    target = ".../SEPR_ERP_correlations.docx"
  )
  

##### 3.5.1 Model: Effect of SEPR on group differences in LFSW? #######################################################################################
# add aggregated SEPR to LFSW df
LFSW.amp_sepr<-merge(df.LFSW.valid.amp, bhv.erp.pd, by=c("id", "group", "perspective", "congruency"))
# run model with RPD TW-3 and interaction with group included
m1<-lmer(scale(mean_amp)~group*perspective*congruency*hemisphere  + rpd_bin3 + group:rpd_bin3 + (1|id), data=LFSW.amp_sepr)
anova(m1)
  # save table
  doc_1 <- read_docx()
  table <- round(as.data.frame(anova(m1)),3)
  colnames(table)[ncol(table)]<-"p"
  table <- tibble::rownames_to_column(table,var = " ")
  doc_1 <- body_add_table(doc_1, table, style = "table_template")
  print(doc_1, target = ".../LFSW_group_SEPR.docx")