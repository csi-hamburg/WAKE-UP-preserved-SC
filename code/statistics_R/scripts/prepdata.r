ROIlobe.86 <- list(Frontal = c('Superiorfrontal'
                               , 'Rostralmiddlefrontal', 'Caudalmiddlefrontal'
                               , 'Parsopercularis', 'Parstriangularis', 'Parsorbitalis'
                               , 'Lateralorbitofrontal', 'Medialorbitofrontal'
                               , 'Precentral', 'Paracentral', 'Frontalpole')
                   , Parietal = c('Superiorparietal', 'Inferiorparietal'
                                  , 'Supramarginal', 'Postcentral', 'Precuneus')
                   , Temporal = c('Superiortemporal', 'Middletemporal', 'Inferiortemporal'
                                  , 'Bankssts', 'Fusiform', 'Transversetemporal'
                                  ,  'Temporalpole')
                   , Occipital = c('Lateraloccipital', 'Lingual'
                                   , 'Cuneus', 'Pericalcarine')
                   , Limbic = c('Rostralanteriorcingulate'
                                , 'Caudalanteriorcingulate', 'Posteriorcingulate', 'Isthmuscingulate'
                                , 'Hippocampus', 'Parahippocampal', 'Amygdala', 'Insula', 'Entorhinal')
                   , Subcortical = c('Accumbens_area', 'Caudate', 'Hypothalamus'
                                     , 'Cerebellum_Cortex'
                                     , 'Pallidum', 'Putamen', 'Thalamus_Proper')
)

ROIlobe.116 <- list(Frontal = c('Precentral'
                                , 'Frontal_Sup', 'Frontal_Sup_Orb', 'Frontal_Sup_Medial'
                                , 'Frontal_Mid', 'Frontal_Mid_Orb910', 'Frontal_Mid_Orb2526'
                                , 'Frontal_Inf_Oper', 'Frontal_Inf_Tri', 'Frontal_Inf_Orb', 'Rolandic_Oper'
                                , 'Supp_Motor_Area', 'Olfactory', 'Rectus')
                    , Parietal = c('Postcentral'
                                   , 'Parietal_Sup', 'Parietal_Inf'
                                   , 'SupraMarginal', 'Angular'
                                   , 'Precuneus', 'Paracentral_Lobule')
                    , Temporal = c('Fusiform', 'Heschl', 'Temporal_Sup', 'Temporal_Inf'
                                   , 'Temporal_Pole_Sup', 'Temporal_Mid', 'Temporal_Pole_Mid'
                    )
                    , Occipital = c('Calcarine', 'Cuneus', 'Lingual'
                                    , 'Occipital_Sup', 'Occipital_Mid', 'Occipital_Inf'
                                    ,'Cerebelum_Crus1', 'Cerebelum_Crus2'
                                    , 'Cerebelum_3', 'Cerebelum_4_5'
                                    , 'Cerebelum_6', 'Cerebelum_7b'
                                    , 'Cerebelum_8', 'Cerebelum_9'
                                    , 'Cerebelum_10'
                                    , 'Vermis_1_2', 'Vermis_3'
                                    , 'Vermis_4_5', 'Vermis_6'
                                    , 'Vermis_7', 'Vermis_8'
                                    , 'Vermis_9', 'Vermis_10')
                    , Limbic = c('Cingulum_Ant', 'Cingulum_Mid', 'Cingulum_Post','Insula'
                                 ,'ParaHippocampal',  'Hippocampus',  'Amygdala')
                    , Subcortical = c('Caudate', 'Putamen', 'Pallidum', 'Thalamus')
)

require(tidyverse)
require(magrittr)                    

splitlab <- function(s){
  t <- strsplit(as.character(s),"=")[[1]][[2]]
  ROIside<-c(ROI = substr(t,1,nchar(t)-2)
             , hemisphere = substr(t,nchar(t),nchar(t)))
  return(ROIside)
}

splitlab116 <- function(s){
  t <- strsplit(as.character(s),"_")[[1]]
  side <- t[[length(t)]]
  ROI <- strsplit(t[1:(length(t)-1)],'=') %>% unlist() %>% as.list() %$% do.call(paste, c(.[2:length(.)], sep='_'))
  ROIside <- c(ROI = ROI, hemisphere = side)
  return(ROIside)
}



lobe.fcn <- function(ROI){
 if (asz == 86) ROIlobe = ROIlobe.86
 if (asz == 116) ROIlobe = ROIlobe.116
 lobe <- lapply(ROIlobe, function(s){ROI %in% s}) %>%
    unlist() %>% 
    which(arr.ind = TRUE) %>% 
    names()
 if (length(lobe) == 0) lobe <- NA
 return(lobe)
}

if (asz == 86){
  nemo <- read.csv('../../derivatives/NeMo_output/nemo86.csv', header = TRUE)
  nemo <- nemo %>% 
    filter(!stringr::str_detect(lab,'Cerebellum_Cortex'))
  
  nemo <- nemo %>% 
    filter(!ID %in% c('1-14-006-v03','2-01-070-v03', '5-04-010-v03')) %>% ## unsatisfactory stroke lesion segmentation
    filter(lesionvolumeV0 > 0 & lesionvolumeV3 > 0)
  
  df <- do.call(rbind,lapply(nemo$lab,splitlab)) %>% data.frame() 

} else if (asz == 116){
  nemo<-read.csv('../../derivatives/NeMo_output/nemo116.csv', header = TRUE)
  
  nemo <- nemo %>% 
    filter(!ID %in% c('1-14-006-v03','2-01-070-v03', '5-04-010-v03')) %>% ## unsatisfactory stroke lesion segmentation
    filter(lesionvolumeV0 > 0 & lesionvolumeV3 > 0)
  
  nemo <- nemo %>% 
    filter(!stringr::str_detect(lab,'Vermis') & !stringr::str_detect(lab,'Cerebelum')) %>% 
    mutate(lab = forcats::fct_recode(lab, `10=Frontal_Mid_Orb910_R` = '10=Frontal_Mid_Orb_R'
                                     , `26=Frontal_Mid_Orb2526_R` = '26=Frontal_Mid_Orb_R'
                                     , `9=Frontal_Mid_Orb910_L` = '9=Frontal_Mid_Orb_L'
                                     , `25=Frontal_Mid_Orb2526_L` = '25=Frontal_Mid_Orb_L'
    )
    )  
  df <- do.call(rbind,lapply(nemo$lab,splitlab116)) %>% data.frame()

} else {
  stop()
}

df$lobe <- lapply(df$ROI,lobe.fcn) %>% unlist() %>% as.factor()
df$lobe <- factor(df$lobe, levels = c('Frontal','Parietal','Temporal','Occipital','Limbic','Subcortical'))
df$hemisphere<-c('right','left')[as.factor(df$hemisphere)]


nemo <- cbind(nemo,df) %>%
  mutate(vol0 = lesionvolumeV0) %>% 
  gather(key, value, -lab, -ID, -treatment, -lesion_side, -lesion_location, -lesion_supratentorial, -ROI, -hemisphere, -vol0, -lobe) %>% 
  separate(key, c("prefix","visit"),-2) %>%   
  spread(prefix, value) %>% 
  mutate(ID = as.factor(substr(ID,1,nchar(as.character(ID))-4)))
nemo$visit<-as.factor(nemo$visit)
nemo$llv<-log(nemo$lesionvolume)
nemo$ROI <- as.factor(nemo$ROI)

d <- subset(nemo, lesion_side == hemisphere & lesion_location == 'mcaOrAca') %>% 
  droplevels() %>% 
  dplyr::select(ID, treatment, lesion_side, visit, ROI, lobe, vol0, lesionvolume, llv, nemoscore)

contrasts(d$ROI) <- contr.sum

d$treatment <- as.factor(d$treatment)

data.clinical <- gdata::read.xls('./../../clinical/KeyStudyData_21-Nov-2017_short_version.xls', sheet = 1)

dd <- data.clinical %>% 
  filter(Subject.ID %in% levels(d$ID)) %>% droplevels() %>% 
  rename('ID' = Subject.ID
         , 'mRS' = Visit.5.MRS
         , 'age' = Demographic.data..age
         , 'NIHSS' = Visit.0.NIHSS.sum.score) %>% 
  mutate(goodOutcome = mRS <= 1) %>% 
  dplyr::select(ID, goodOutcome,age, NIHSS) %>%  
  merge(d)

dd$ID <- seq(1,length(unique(dd$ID)))[as.factor(dd$ID)]
rm(list=setdiff(ls(), c('dd', 'asz')))
write.csv(dd, file = paste0('data_', asz, '.dat'), row.names = FALSE)

