library(bnlearn)

#Function to append each row in a vector form a 2 column CSv with headings.
veccreate <- function(filecsv){
  resvec<-c()
  dat <- read.csv(filecsv,header=TRUE)
  for (i in 1:length(dat[,1])){
    resvec<-append(resvec,dat[i,1])
    resvec<-append(resvec,dat[i,2])
    # print(dat[i,1])
    # print(dat[i,2])
  }
  return(resvec)
}

#defining graph nodes and arcs
UEVBS<-empty.graph(nodes = c("Outage_probability_good_low_enough",
                             "In_proximity_of_other_UE_VBS_devices",
                             "UE_VBS_Device",
                             "In_proximity_of_other_UE_clients_non_UE_VBS_enabled",
                             "Energy_efficiency_HIGH",
                             "Energy_efficiency_MEDIUM",
                             "Energy_efficiency_LOW",
                             "CQI_LOW",
                             "CQI_MEDIUM",
                             "CQI_HIGH",
                             "Spectral_efficiency_HIGH",
                             "Spectral_efficiency_MEDIUM",
                             "Spectral_efficiency_LOW",
                             "Overall_resources_HIGH",
                             "Overall_resources_MEDIUM",
                             "Overall_resources_LOW",
                             "Backhaul_data_enough",
                             "In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area",
                             "In_proximity_of_UE_VBS_SC_devices",
                             "Cellular_regular_operation",
                             "Relay_operation",
                             "Cluster_head_operation",
                             "Cluster_member_operation"))
arc.set<-matrix(c("Outage_probability_good_low_enough","CQI_LOW",
                  "Outage_probability_good_low_enough","CQI_MEDIUM",
                  "Outage_probability_good_low_enough","CQI_HIGH",
                  "CQI_HIGH","Spectral_efficiency_HIGH",
                  "CQI_HIGH","Spectral_efficiency_MEDIUM",
                  "CQI_HIGH","Spectral_efficiency_LOW",
                  "CQI_MEDIUM","Spectral_efficiency_HIGH",
                  "CQI_MEDIUM","Spectral_efficiency_MEDIUM",
                  "CQI_MEDIUM","Spectral_efficiency_LOW",
                  "CQI_LOW","Spectral_efficiency_HIGH",
                  "CQI_LOW","Spectral_efficiency_MEDIUM",
                  "CQI_LOW","Spectral_efficiency_LOW",
                  "Energy_efficiency_LOW","Overall_resources_HIGH",
                  "Energy_efficiency_LOW","Overall_resources_MEDIUM",
                  "Energy_efficiency_LOW","Overall_resources_LOW",
                  "Energy_efficiency_MEDIUM","Overall_resources_HIGH",
                  "Energy_efficiency_MEDIUM","Overall_resources_MEDIUM",
                  "Energy_efficiency_MEDIUM","Overall_resources_LOW",
                  "Energy_efficiency_HIGH","Overall_resources_HIGH",
                  "Energy_efficiency_HIGH","Overall_resources_MEDIUM",
                  "Energy_efficiency_HIGH","Overall_resources_LOW",
                  "Spectral_efficiency_LOW","Overall_resources_HIGH",
                  "Spectral_efficiency_LOW","Overall_resources_MEDIUM",
                  "Spectral_efficiency_LOW","Overall_resources_LOW",
                  "Spectral_efficiency_MEDIUM","Overall_resources_HIGH",
                  "Spectral_efficiency_MEDIUM","Overall_resources_MEDIUM",
                  "Spectral_efficiency_MEDIUM","Overall_resources_LOW",
                  "Spectral_efficiency_HIGH","Overall_resources_HIGH",
                  "Spectral_efficiency_HIGH","Overall_resources_MEDIUM",
                  "Spectral_efficiency_HIGH","Overall_resources_LOW",
                  "Overall_resources_HIGH","Backhaul_data_enough",
                  "Overall_resources_MEDIUM","Backhaul_data_enough",
                  "Overall_resources_LOW","Backhaul_data_enough",
                  "Outage_probability_good_low_enough","In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area",
                  "In_proximity_of_other_UE_VBS_devices","In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area",
                  "In_proximity_of_other_UE_VBS_devices","In_proximity_of_UE_VBS_SC_devices",
                  "CQI_HIGH","Cellular_regular_operation",
                  "CQI_MEDIUM","Cellular_regular_operation",
                  "CQI_LOW","Cellular_regular_operation",
                  "CQI_HIGH","Cluster_member_operation",
                  "CQI_MEDIUM","Cluster_member_operation",
                  "CQI_LOW","Cluster_member_operation",
                  "In_proximity_of_UE_VBS_SC_devices","Cluster_member_operation",
                  "In_proximity_of_UE_VBS_SC_devices","Relay_operation",
                  "In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area","Relay_operation",
                  "Backhaul_data_enough","Relay_operation",
                  "UE_VBS_Device","Relay_operation",
                  "UE_VBS_Device","Cluster_head_operation",
                  "Backhaul_data_enough","Cluster_head_operation",
                  "In_proximity_of_other_UE_clients_non_UE_VBS_enabled","Cluster_head_operation"),
                byrow = TRUE, ncol = 2,
                dimnames = list(NULL,c("from","to")))

arcs(UEVBS) <- arc.set

#importing/defining the CPTs for the BN
Outage_probability_good_low_enough.lv <- c("T","F")
Outage_probability_good_low_enough.prob <- array(c(0.95,0.05),dim = 2,
                                                 dimnames = list(Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
In_proximity_of_other_UE_VBS_devices.lv <- c("T","F")
In_proximity_of_other_UE_VBS_devices.prob <- array(c(0.01,0.99),dim = 2,
                                                   dimnames = list(Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
UE_VBS_Device.lv <- c("T","F")
UE_VBS_Device.prob <- array(c(0.05,0.95),dim = 2,
                            dimnames = list(UE_VBS_Device = UE_VBS_Device.lv))
In_proximity_of_other_UE_clients_non_UE_VBS_enabled.lv <- c("T","F")
In_proximity_of_other_UE_clients_non_UE_VBS_enabled.prob <- array(c(0.9,0.1),dim = 2,
                                                                  dimnames = list(In_proximity_of_other_UE_clients_non_UE_VBS_enabled = In_proximity_of_other_UE_clients_non_UE_VBS_enabled.lv))
Energy_efficiency_HIGH.lv <- c("T","F")
Energy_efficiency_HIGH.prob <- array(c(0.33,0.67),dim = 2,
                                     dimnames = list(Energy_efficiency_HIGH =Energy_efficiency_HIGH.lv))
Energy_efficiency_MEDIUM.lv <- c("T","F")
Energy_efficiency_MEDIUM.prob <- array(c(0.33,0.67),dim = 2,
                                       dimnames = list(Energy_efficiency_MEDIUM =Energy_efficiency_MEDIUM.lv))
Energy_efficiency_LOW.lv <- c("T","F")
Energy_efficiency_LOW.prob <- array(c(0.33,0.67),dim = 2,
                                    dimnames = list(Energy_efficiency_LOW =Energy_efficiency_LOW.lv))
CQI_LOW.lv <- c("T","F")
CQI_LOW.prob <- array(c(0.01,0.99,0.9,0.1),dim = c(2,2),
                      dimnames = list(CQI_LOW =CQI_LOW.lv,Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
CQI_MEDIUM.lv <- c("T","F")
CQI_MEDIUM.prob <- array(c(0.7,0.3,0.2,0.8),dim = c(2,2),
                         dimnames = list(CQI_MEDIUM =CQI_MEDIUM.lv,Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
CQI_HIGH.lv <- c("T","F")
CQI_HIGH.prob <- array(c(0.9,0.1,0.01,0.99),dim = c(2,2),
                       dimnames = list(CQI_HIGH =CQI_HIGH.lv,Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
Spectral_efficiency_HIGH.lv <- c("T","F")
Spectral_efficiency_HIGH.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Spectral efficiency HIGH_ CQI HIGH CQI MEDIUM CQI LOW.csv")),dim = c(2,2,2,2),
                                       dimnames = list(Spectral_efficiency_HIGH =Spectral_efficiency_HIGH.lv,CQI_LOW =CQI_LOW.lv,CQI_MEDIUM =CQI_MEDIUM.lv,CQI_HIGH =CQI_HIGH.lv))
Spectral_efficiency_MEDIUM.lv <- c("T","F")
Spectral_efficiency_MEDIUM.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Spectral efficiency MEDIUM _CQI HIGH CQI MEDIUM CQI LOW.csv")),dim = c(2,2,2,2),
                                         dimnames = list(Spectral_efficiency_MEDIUM =Spectral_efficiency_MEDIUM.lv,CQI_LOW =CQI_LOW.lv,CQI_MEDIUM =CQI_MEDIUM.lv,CQI_HIGH =CQI_HIGH.lv))
Spectral_efficiency_LOW.lv <- c("T","F")
Spectral_efficiency_LOW.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Spectral efficiency LOW_CQI HIGH CQI MEDIUM CQI LOW.csv")),dim = c(2,2,2,2),
                                      dimnames = list(Spectral_efficiency_LOW =Spectral_efficiency_LOW.lv,CQI_LOW =CQI_LOW.lv,CQI_MEDIUM =CQI_MEDIUM.lv,CQI_HIGH =CQI_HIGH.lv))
Overall_resources_HIGH.lv <- c("T","F")
Overall_resources_HIGH.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Overall resources HIGH_SE HIGH SE MEDIUM SE LOW EE HIGH EE MEDIUM EE LOW.csv")),dim = c(2,2,2,2,2,2,2),
                                     dimnames = list(Overall_resources_HIGH =Overall_resources_HIGH.lv,
                                                     Energy_efficiency_LOW=Energy_efficiency_LOW.lv,
                                                     Energy_efficiency_MEDIUM=Energy_efficiency_MEDIUM.lv,
                                                     Energy_efficiency_HIGH=Energy_efficiency_HIGH.lv,
                                                     Spectral_efficiency_LOW = Spectral_efficiency_LOW.lv,
                                                     Spectral_efficiency_MEDIUM=Spectral_efficiency_MEDIUM.lv,
                                                     Spectral_efficiency_HIGH=Spectral_efficiency_HIGH.lv))
Overall_resources_MEDIUM.lv <- c("T","F")
Overall_resources_MEDIUM.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Overall resources MEDIUM_SE HIGH SE MEDIUM SE LOW EE HIGH EE MEDIUM EE LOW.csv")),dim = c(2,2,2,2,2,2,2),
                                       dimnames = list(Overall_resources_MEDIUM =Overall_resources_MEDIUM.lv,
                                                       Energy_efficiency_LOW=Energy_efficiency_LOW.lv,
                                                       Energy_efficiency_MEDIUM=Energy_efficiency_MEDIUM.lv,
                                                       Energy_efficiency_HIGH=Energy_efficiency_HIGH.lv,
                                                       Spectral_efficiency_LOW = Spectral_efficiency_LOW.lv,
                                                       Spectral_efficiency_MEDIUM=Spectral_efficiency_MEDIUM.lv,
                                                       Spectral_efficiency_HIGH=Spectral_efficiency_HIGH.lv))
Overall_resources_LOW.lv <- c("T","F")
Overall_resources_LOW.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Overall resources LOW_SE HIGH SE MEDIUM SE LOW EE HIGH EE MEDIUM EE LOW.csv")),dim = c(2,2,2,2,2,2,2),
                                    dimnames = list(Overall_resources_LOW =Overall_resources_LOW.lv,
                                                    Energy_efficiency_LOW=Energy_efficiency_LOW.lv,
                                                    Energy_efficiency_MEDIUM=Energy_efficiency_MEDIUM.lv,
                                                    Energy_efficiency_HIGH=Energy_efficiency_HIGH.lv,
                                                    Spectral_efficiency_LOW = Spectral_efficiency_LOW.lv,
                                                    Spectral_efficiency_MEDIUM=Spectral_efficiency_MEDIUM.lv,
                                                    Spectral_efficiency_HIGH=Spectral_efficiency_HIGH.lv))
Backhaul_data_enough.lv <- c("T","F")
Backhaul_data_enough.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Backhaul data enough_Overall resources HIGH Overall resources MEDIUM Overall resources LOW.csv")),dim = c(2,2,2,2),
                                   dimnames = list(Backhaul_data_enough =Backhaul_data_enough.lv,
                                                   Overall_resources_LOW =Overall_resources_LOW.lv,
                                                   Overall_resources_MEDIUM =Overall_resources_MEDIUM.lv,
                                                   Overall_resources_HIGH =Overall_resources_HIGH.lv))
In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area.lv <- c("T","F")
In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/In proximity of a UE-VBS in unreachable are_Outage probability good In proximity of other UE-VBSs.csv")),dim = c(2,2,2),
                                                                                        dimnames = list(In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area =In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area.lv,
                                                                                                        In_proximity_of_other_UE_VBS_devices=In_proximity_of_other_UE_VBS_devices.lv,
                                                                                                        Outage_probability_good_low_enough = Outage_probability_good_low_enough.lv))
In_proximity_of_UE_VBS_SC_devices.lv <- c("T","F")
In_proximity_of_UE_VBS_SC_devices.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/In proximity of UE-VBS-SC device_In proximity of other UE-VBSs.csv")),dim = c(2,2),
                                                dimnames = list(In_proximity_of_UE_VBS_SC_devices = In_proximity_of_UE_VBS_SC_devices.lv,
                                                                In_proximity_of_other_UE_VBS_devices=In_proximity_of_other_UE_VBS_devices.lv))
Cellular_regular_operation.lv <- c("T","F")
Cellular_regular_operation.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Cellular regular operation_CQI HIGH CQI MEDIUM CQI LOW.csv")),dim = c(2,2,2,2),
                                         dimnames = list(Cellular_regular_operation=Cellular_regular_operation.lv,
                                                         CQI_LOW =CQI_LOW.lv,CQI_MEDIUM =CQI_MEDIUM.lv,CQI_HIGH =CQI_HIGH.lv))
Relay_operation.lv <- c("T","F")
Relay_operation.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Relay operation_In proximity of UE-VBS+unreachable In proximity of other UE-VBS-CS UE-VBS device backhaul data enough.csv")),dim = c(2,2,2,2,2),
                              dimnames = list(Relay_operation = Relay_operation.lv,
                                              Backhaul_data_enough = Backhaul_data_enough.lv,
                                              UE_VBS_Device = UE_VBS_Device.lv,
                                              In_proximity_of_UE_VBS_SC_devices= In_proximity_of_UE_VBS_SC_devices.lv,
                                              In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area=In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area.lv))
Cluster_head_operation.lv <- c("T","F")
Cluster_head_operation.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Cluster head operation_Backhaul data enough EU-VBS device in proximity of other UE-clients.csv")),dim = c(2,2,2,2),
                                     dimnames = list(Cluster_head_operation =Cluster_head_operation.lv,
                                                     In_proximity_of_other_UE_clients_non_UE_VBS_enabled = In_proximity_of_other_UE_clients_non_UE_VBS_enabled.lv,
                                                     UE_VBS_Device = UE_VBS_Device.lv,
                                                     Backhaul_data_enough = Backhaul_data_enough.lv))
Cluster_member_operation.lv <- c("T","F")
Cluster_member_operation.prob <- array(veccreate(paste(dirname(rstudioapi::getSourceEditorContext()$path),sep="","/CPT tables/Cluster member operation_CQI HIGH CQI MEDIUM CQI LOW in proximity of UE-VBS-SC device.csv")),dim = c(2,2,2,2,2),
                                       dimnames = list(Cluster_member_operation = Cluster_member_operation.lv,
                                                       In_proximity_of_UE_VBS_SC_devices= In_proximity_of_UE_VBS_SC_devices.lv,
                                                       CQI_LOW =CQI_LOW.lv,CQI_MEDIUM =CQI_MEDIUM.lv,CQI_HIGH =CQI_HIGH.lv))

CPT <- list(Outage_probability_good_low_enough = Outage_probability_good_low_enough.prob,
            In_proximity_of_other_UE_VBS_devices = In_proximity_of_other_UE_VBS_devices.prob,
            UE_VBS_Device = UE_VBS_Device.prob,
            In_proximity_of_other_UE_clients_non_UE_VBS_enabled = In_proximity_of_other_UE_clients_non_UE_VBS_enabled.prob,
            Energy_efficiency_HIGH = Energy_efficiency_HIGH.prob,
            Energy_efficiency_MEDIUM = Energy_efficiency_MEDIUM.prob,
            Energy_efficiency_LOW = Energy_efficiency_LOW.prob,
            CQI_LOW = CQI_LOW.prob,
            CQI_MEDIUM = CQI_MEDIUM.prob,
            CQI_HIGH = CQI_HIGH.prob,
            Spectral_efficiency_HIGH = Spectral_efficiency_HIGH.prob,
            Spectral_efficiency_MEDIUM = Spectral_efficiency_MEDIUM.prob,
            Spectral_efficiency_LOW = Spectral_efficiency_LOW.prob,
            Overall_resources_HIGH = Overall_resources_HIGH.prob,
            Overall_resources_MEDIUM = Overall_resources_MEDIUM.prob,
            Overall_resources_LOW = Overall_resources_LOW.prob,
            Backhaul_data_enough = Backhaul_data_enough.prob,
            In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area = In_proximity_of_a_UE_VBS_device_located_in_a_high_outage_probability_area.prob,
            In_proximity_of_UE_VBS_SC_devices = In_proximity_of_UE_VBS_SC_devices.prob,
            Cellular_regular_operation = Cellular_regular_operation.prob,
            Relay_operation = Relay_operation.prob,
            Cluster_head_operation = Cluster_head_operation.prob,
            Cluster_member_operation = Cluster_member_operation.prob)
#fitting the BN
BN <- custom.fit(UEVBS,CPT)

#libraries used to extract usefull values
library(gRbase)
library(gRain)
# junction <- compile(as.grain(BN))

#Examples

fnodes=c("Cellular_regular_operation",
         "Relay_operation",
         "Cluster_head_operation",
         "Cluster_member_operation")

# for (i in fnodes){
#   print(querygrain(junction,node = i))
# }

resvec <- c()
for (i in c("T","F")){
  for (j in c("T","F")){
    for(k in c("T","F")){
      for (q in c("T","F")){
        res<-cpquery(BN,
                     event = (Cluster_member_operation=="T"),
                     evidence = list(
                       UE_VBS_Device = i,
                       In_proximity_of_other_UE_clients_non_UE_VBS_enabled=j,
                       In_proximity_of_other_UE_VBS_devices=k,
                       Outage_probability_good_low_enough=q
                     ),
                     method= "lw")
        resvec <- append(resvec,res)
      }
    }
  }
}
print(resvec)