# -*- coding: utf-8 -*-
"""
Created on Wed Mar 09 18:00:05 2016

@author: cs08
"""

import pandas as pd
import numpy as np
import glob
                                


'''This Script combines bottom and surface data for a range of variables over 20+ years. 
The variables have been averaged, std values calculated and the resulting values appended.
1 output is combined means(for pca analysis, the other has means and std for EDA)'''


#data_dir="C:\Projects\WGINOSE\Data\wgi_bot_sur\\"
#output_dir=r"C:\Projects\WGINOSE\Bottom_Surface_outputs\\"

data_dir="C:\Users\cs08\Documents\Projects\WGINOSE\WG_2017\data\\"
output_dir=r"C:\Users\cs08\Documents\Projects\WGINOSE\WG_2017\output_data\\"

bottom_data=glob.glob(data_dir+"*bottom.csv")
surface_data=glob.glob(data_dir+"*surface.csv")
#finds files ending with the strings bottom and surface.csv / can be altered

for i, j in zip(bottom_data,surface_data):
    #each surface and bottom csv is paired
    name=i.split("\\")[-1]
    name=name.replace("_bottom.csv","")
   
    bottom = pd.read_csv(i,header=1,na_values=["-9","0"])                       
    surface = pd.read_csv(j,header=1,na_values=["-9","0"])
    #no data value -9 and 0's are replaced with NA's                     
    
    variables=['TEMP', 'PSAL', 'DOXY(umol/l)','PHOS(umol/l)', 'TPHS(umol/l)',
               'SLCA(umol/l)', 'NTRA(umol/l)', 'NTRI(umol/l)', 'AMON(umol/l)', 
               'NTOT(umol/l)', 'PHPH', 'ALKY(mmol/l)', 'CPHL(mg/m^3)']
    #key variables for anlyis, can be reduced or expanded depending on data availability
     
    pivot=pd.pivot_table(bottom,index=["Year"],values=variables,aggfunc='count')
    #pivot table of counts for each variable. If a variable only has 1 count,
    #it is removed as there will be no std value.                   
                         
    pivot=pivot.replace([0], [np.nan])
    #pivot=pivot.loc[pivot.index != 2015] 
    #removes selected year from analysis, can change depending on variable availability        
    pivot=pivot.replace([1], [np.nan])
    #makes counts with one NA, then varaible gets removed
    pivot=pivot.dropna(axis=1,how='any')         
    pivot.to_csv(output_dir+name+"_count_test_bot.csv")
    
    bottom_mean=pd.pivot_table(bottom,index=["Year"],
                                values=list(pivot.columns.values),
                                aggfunc=np.mean)
                                #pivot table run for means of all key variables
                                #over the years also run on std's. Uses variables from the pivot count table
                                  
    bottom_mean.columns=["Bottom_mean_" + s for s in bottom_mean.columns]
    #pivot table cols renamed to reflect aggfunc.                                        
                                           
    bottom_std=pd.pivot_table(bottom,index=["Year"],
                                values=variables,
                                aggfunc=np.std)
                                        
    bottom_std.columns=["Bottom_std_" + s for s in bottom_std.columns] 
    
    
    pivot=pd.pivot_table(surface,index=["Year"],
                         values=variables,
                         aggfunc='count')
                         
    pivot=pivot.replace([0], [np.nan])
    #pivot=pivot.loc[pivot.index != 2015]         
    pivot=pivot.replace([1], [np.nan])  
    pivot=pivot.dropna(axis=1,how='any')         
    pivot.to_csv(output_dir+name+"_count_test_surf.csv")
         
    surface_mean=pd.pivot_table(surface,index=["Year"],
                                values=list(pivot.columns.values),
                                aggfunc=np.mean)
                                 
    surface_mean.columns=["Surface_mean_" + s for s in surface_mean.columns] 
                      
    surface_std=pd.pivot_table(surface,index=["Year"],
                               values=variables,
                               aggfunc=np.std)
    
    surface_std.columns=["Surface_std_" + s for s in surface_std.columns]                       
                                    
    r_model = pd.concat([bottom_mean,  surface_mean], axis=1)        
    #r_model=r_model.loc[r_model.index != 2015]
    #removes all 2015 data(pretty much all NA's)                  
    r_model=r_model.dropna(axis=1,how='any')
    #removes columns with NA values
    
    analysis = pd.concat([bottom_mean, bottom_std, surface_mean,surface_std], axis=1)
    #analysis=analysis.loc[analysis.index != 2008]
    #analysis=analysis.loc[analysis.index != 2015]                   
    analysis=analysis.dropna(axis=1,how='any')                 
    
    r_model.to_csv(output_dir+name+"_surface_bottom.csv")
    analysis.to_csv(output_dir+name+"_mean_std.csv")  
    
    #surface_mean=surface_mean.loc[surface_mean.index != 2015]
    #surface_mean=surface_mean.dropna(axis=1,how='any')
    #surface_mean.to_csv(output_dir+name+"_surfaceMEAN.csv")
    
    #surface_std.to_csv(output_dir+name+"_surfaceSTD.csv")
    #bottom_std.to_csv(output_dir+name+"_bottomSTD.csv")     
       
#should integrate al lcode, have two scripts one for means / means+std's
       
#And should clean outputs at the end so people can choose stage of filtering (i.e years being removed etc)



















            