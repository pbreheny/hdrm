#' World Health Organization study on acute respiratory illnesses
#' 
#' @description
#' 
#' The presentation of an acutely ill young infant presents health workers,
#' especially those in developing countries, with a very difficult problem. Serious
#' infections are the main cause of morbidity and mortality in infants under 3
#' months of age in these countries, and diagnosing the severity of the illness is
#' rather difficult.
#' 
#' To study this problem, the World Health Organization (WHO) collected data on a
#' number of readily accessible variables such as vital signs, family history, and
#' clinical observations resulting from physical examination. The patients' disease
#' status was later determined based on the course of the disease and various
#' laboratory tests. The goal of the study was to develop a early prediction rule
#' for grading the severity of the disease so that timely treatment could be
#' delivered (and costly but unnecessary treatments avoided).
#' 
#' The WHO study looked at several acute respiratory illnesses in several
#' countries. This data set contains data on pneumonia from the country Ethiopia.
#' 
#' @format
#' 
#' * `y`: A pneumonia score, measured on the following scale: 1=No disease,
#'   2=Cold/cough, 3=Pneumonia, 4=Severe pneumonia, 5=Life-threatening illness.
#' 
#' * `X`: A matrix with 816 rows and 66 columns. Each row of `X` represents an
#' individual. The columns are described below.
#'   * The following columns are objective clinical measurements:
#'     * `biwt`: Birth weight
#'     * `hcir`: Head circumference
#'     * `wght`: Weight (in grams)
#'     * `lgth`: Length (in centimeters)
#'     * `temp`: Temperature (in Celsius)
#'     * `hrat`: Heart rate
#'     * `age`: Age (in days)
#'     * `rr`: Adjusted respiratory rate
#'     * `waz`: Weight-for-age (z-score)
#'   * There are also a number of clinical observations, judged on a severity scale
#'     with higher numbers denoting increased severity. For example, 0-3 would
#'     represent absent/mild/moderate/severe:
#'     * `omph`: omphalitis (0-1)
#'     * `conj`: conjunctivitis (0-1)
#'     * `slpm`: sleeping more (0-1)
#'     * `slpl`: sleeping less (0-1)
#'     * `wake`: wakes less easy (0-1)
#'     * `convul`: history of convulsion (0-1)
#'     * `hfa`: history of abnormal feeding (0-3)
#'     * `hfb`: fast breathing (0-1)
#'     * `hfe`: history of fever (0-1)
#'     * `hap`: history of stop breathing (0-1)
#'     * `hcl`: less activity (0-1)
#'     * `hcm`: crying more (0-1)
#'     * `hcs`: crying less (0-1)
#'     * `hdi`: history of diarrhoea (0-1)
#'     * `hvo`: vomit more (0-1)
#'     * `bat`: birth preterm (0-1)
#'     * `fde`: fever at delivery (0-1)
#'     * `chi`: previous child died (0-1)
#'     * `hbr`: engorged breasts (0-1)
#'     * `twb`: water broke (0-1)
#'     * `ldy`: days in labour (1-9)
#'     * `inc`: respiratory distress (0-2)
#'     * `sr1`: respiratory state (0-3)
#'     * `sr2`: respiratory state (0-3); I do not know why this is measured twice
#'     * `apn`: apnea (0-1)
#'     * `lcw`: lower chest in-drawing (0-3)
#'     * `nfl`: nasal flaring (0-3)
#'     * `str`: stridor (0-2)
#'     * `gru`: grunting (0-2)
#'     * `coh`: cough heard (0-1)
#'     * `ccy`: central cyanosis (0-1)
#'     * `jau`: jaundice (0-1)
#'     * `csd`: drowsy state (0-2)
#'     * `csa`: agitated state (0-1)
#'     * `aro`: arousal (0-2)
#'     * `qcr`: quality of crying (0-2)
#'     * `con`: consolability (0-2)
#'     * `att`: attentive (0-2)
#'     * `mvm`: amount of movement (0-2)
#'     * `afe`: drinking ability (0-2)
#'     * `absu`: sucking ability (0-2)
#'     * `stu`: skin turgor (0-2)
#'     * `deh`: dehydrated (0-2)
#'     * `dcp`: digital capillary refill (0-2)
#'     * `crs`: crepitation (0-2)
#'     * `skr`: skin rash (0-2)
#'     * `abb`: bulging fontanelle (0-1)
#'     * `abk`: sunken fontanelle (0-1)
#'     * `hyp`: hypotonia (0-3)
#'     * `whz`: wheezing (0-1)
#'     * `hdb`: difficulty breathing (0-1)
#'     * `smi2`: smiling ability
#'     * `puskin`: pustular skin rash (0-1)
#'     * `abd`: abdominal distension (0-4)
#'     * `nut`: nutritional status score (0-3)
#'     * `oto`: otoscopy impression (0-2)
#'     * `oab`: sclerema (0-1)
#' 
#' @source
#' 
#' I obtained the data from
#' [http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets](http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets). The
#' original reference is:
#' 
#' Harrell F, Margolis P, Gove S, Mason K, Mulholland E, Lehmann D, Muhe L,
#' Gatchalian S and Eichenwald H (1998). Development of a clinical prediction
#' model for an ordinal outcome: the World Health Organization multicentre study
#' of clinical signs and etiological agents of pneumonia, sepsis and meningitis
#' in young infants. *Statistics in Medicine*, **17**: 909-944.
#' 
#' @name whoari
NULL
