import itertools, os, sys
import matplotlib.pyplot as plt
import numpy as np
import visca_funcs as visca
#from pathlib import Path
#home = str(Path.home())

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
try:
    work_dir = 'Working_Directory_'+settings['name']+'/select'
except KeyError:
    print('Error in input file -- Aborting')
    sys.exit()
dcd_dir = './%s/dcds'%work_dir
os.system('mkdir -p %s'%work_dir)
os.system('mkdir -p %s'%dcd_dir)
pdbfilename = f"{settings['pdb_location']}{settings['name']}.pdb"

#Sending output to file
out_log = '%s/log.txt'%work_dir
print('Work Directories is setup piping output to %s'%out_log)
sys.stdout = open(out_log,'w')
print('### Running Standalone Deriver ###\nScript by Kris Strunge\n')
print('Read the following User Input from "%s":'%settings_file)
for key in settings.keys():
    print('%s = %s'%(key,settings[key]))
print()

#Checking PDBfile before copying to dcddir
if visca.Check_pdbfile(pdbfilename):
    print('PDB file checked - Correct format')
elif visca.Fix_pdbfile_TER(pdbfilename):
    print('PDB file succesfully corrected - returning to calculation')
else:
    print('Unable to correct PDB file - Terminating program')
    sys.exit()
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], dcd_dir))

#Split source tracjectory into smallest valid trajectories enumerated
print('## Starting the splitting of %s/%s.dcd ##'%(settings['dcd_location'],settings['name']))
print("Entering ./%s/dcds"%work_dir)
os.chdir(dcd_dir)
print("################################")
print("# Calling splitter.tcl via VMD #")
print("################################")
VMD_log = 'VMD_log.txt'
os.system(settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'splitter.tcl" -args "%s%s.dcd" %s %s > ../%s'%(settings['dcd_location'],settings['name'], settings['steps'], settings['FPSplit'],VMD_log))
print("Done with splitting via VMD into pro-dcd files\nOutput can be found in %s/%s"%(work_dir,VMD_log))
print('Leaving %s'%dcd_dir)
os.chdir('../../../')
print()

fortran_script = settings['fortran_script_location']+settings['fortran_script']

#Finding all small .dcd files (pro dcds)
files = os.listdir(path=dcd_dir)
#filter out non-dcd files and sort according to initial frame
pro_dcds = [files[i] for i,name in enumerate(files) if name[-4:]==".dcd"]
pro_dcds = visca.Sort_dcd_names(pro_dcds)
#print("pro_dcds: ",pro_dcds)

combs = []
N = 0
l_pro_dcds = len(pro_dcds)
l_pro_dcds = 1
##Generate all continous combinations of dcd files
#for size in range(1,l_pro_dcds+1):
#    for i in range(l_pro_dcds-size+1):
#        comb = pro_dcds[i:i+size]
#        combs.append(comb)
#        N += 1

#Generate all possible combinations of dcd files
for e in range(1,l_pro_dcds+1):
    for comb in list(itertools.combinations(pro_dcds,e)):
        combs += [comb]
        N += 1

##Generate continous combination of a certain lenght (size is the number of pro-dcd per calculation)
#size = 1
#for i in range(len(pro_dcds)-size+1):
#    comb = pro_dcds[i:i+size]
#    combs.append(comb)
#    N += 1

#print(combs)
print("Number of calculations to be done: ",N,flush=True)

#Detecting polarization combinations
potential_pol_combs = ['SSP',
                       'PPP',
                       'SPS',
                       'PSS',
                       'PSP',
                       'PPS',
                       'SPP',
                       'SSS']
pol_combs = [pol_comb for pol_comb in potential_pol_combs if pol_comb in settings.keys()]
print('Polarization Combinations found in input file:')
print(pol_combs,'\n')

#Experimental data import
xvals_file = settings['exp_data_location']+settings['xvals']
exp_files = [settings['exp_data_location']+settings[pol_comb] for pol_comb in pol_combs]
print('Reading experimental data from:')
print('xvals:',xvals_file)

for i,exp_file in enumerate(exp_files):
    print(pol_combs[i],':',exp_file)
print()
#raw_intensities = [visca.Read_exp_SFG(f)[0] for f in exp_files]
#raw_xvals = visca.Read_exp_SFG(xvals_file)[0]
exp_data_raw = {pol_comb: visca.Read_exp_SFG(f)[0] for pol_comb,f in zip(pol_combs,exp_files)}
exp_data_raw['xvals'] = visca.Read_exp_SFG(xvals_file)[0]

RSS_range_i = int(settings['RSS_range_i'])
RSS_range_f = int(settings['RSS_range_f'])
exp_data_reduced_range = {pol_comb: visca.Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw[pol_comb]) for pol_comb in pol_combs}
exp_data_reduced_range['xvals'] = visca.Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw['xvals'])
exp_data_reduced_range_plot = {pol_comb: visca.Reduce_range(1500, 1800, exp_data_raw['xvals'],exp_data_raw[pol_comb]) for pol_comb in pol_combs}
exp_data_reduced_range_plot['xvals'] = visca.Reduce_range(1500, 1800, exp_data_raw['xvals'], exp_data_raw['xvals'])

#Experimental Normalization constant
N = max(exp_data_reduced_range[settings['Normalize_wrt']]) #Normalization constant
print('Experimental Normalization Constant = %3.6f'%N)

#Experimental Baseline correction
for pol_comb in pol_combs: #Baseline correction loop
    if '%s_bg'%pol_comb in settings.keys():
        print('baseline correction of %s with %s from user input'%(pol_comb,settings['%s_bg'%pol_comb]))
        correction = float(settings['%s_bg'%pol_comb])*N
        exp_data_reduced_range[pol_comb] = [val-correction for val in exp_data_reduced_range[pol_comb]]
        exp_data_reduced_range_plot[pol_comb] = [val-correction for val in exp_data_reduced_range_plot[pol_comb]]

#Correction factor for imperfect half-wave plate
for pol_comb in pol_combs: #Baseline correction loop
    if '%s_factor'%pol_comb in settings.keys():
        print('scaling %s with %s from user input'%(pol_comb,settings['%s_factor'%pol_comb]))
        factor = float(settings['%s_factor'%pol_comb])
        exp_data_reduced_range[pol_comb] = [val*factor for val in exp_data_reduced_range[pol_comb]]
        exp_data_reduced_range_plot[pol_comb] = [val*factor for val in exp_data_reduced_range_plot[pol_comb]]
#Normalization
N = max(exp_data_reduced_range[settings['Normalize_wrt']])/1.0 #Normalization constant
exp_data = {pol_comb: visca.Normalize(exp_data_reduced_range[pol_comb],N) for pol_comb in pol_combs}
exp_data_plot = {pol_comb: visca.Normalize(exp_data_reduced_range_plot[pol_comb],N) for pol_comb in pol_combs}

#Change working directory
os.chdir(dcd_dir)
#Calculating response from pro_dcds
pro_calc_data = {}
all_RSS = []
print('\nFortran calculation on all pro-dcd files')
M = len(pro_dcds)
for i,pro_dcd in enumerate(pro_dcds):
    sfg_file_name = pro_dcd[:-4]+"_SFG.txt"
    if sfg_file_name not in files:
        command = 'time "'+fortran_script+'" -pdb '+settings['name']+'.pdb -spec_min 1500 -spec_max 1800 -dcd '+pro_dcd+' -SFG -inhom 0 -width %s -charge 1 -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH %s -Omega0 %s > fortran_log.txt 2> /dev/null'%(settings['width'],settings['mOH'],settings['Omega0'])
        print(f'Frame {i+1} out of {M}. Calculating {sfg_file_name}', file=sys.__stdout__)
        os.system(command)
    try:
        pro_calc_data[pro_dcd] = visca.Read_calc_SFG(sfg_file_name)
    except FileNotFoundError:
        print(f'\nError - {sfg_file_name} was not found.\nThis is very likely because the ViSCa Fortran script did not run correctly.\nCheck your .pdb and .dcd files are correctly formatted, the correct number of frames are specified and the filepaths are correct in the input file.\nCheck that your input file correctly specifies the path to the fortran script ("visca.out").\nOtherwise consider crying a bit.',file=sys.__stdout__)
        os.system('rm '+pro_dcd)
        sys.exit()
    length_of_data = len(pro_calc_data[pro_dcd][list(pro_calc_data[pro_dcd].keys())[0]])
print('All Fortran calculations - Done')
print('Leaving dcd directory')
os.chdir('../../..')
print('Calculating Fresnel factors...')
#Calculate Fresnel factors
#Calculate Fresnel factors
fresnel = visca.fresnel_factors(float(settings.get('lambda_vis')), 
				float(settings.get('omega_ir')), 
				complex(settings.get('n1_sfg')), 
				complex(settings.get('n1_vis')), 
				complex(settings.get('n1_ir')), 
				complex(settings.get('n2_sfg')), 
				complex(settings.get('n2_vis')), 
				complex(settings.get('n2_ir')), 
				complex(settings.get('ni_sfg')), 
				complex(settings.get('ni_vis')), 
				complex(settings.get('ni_ir')), 
				np.radians(float(settings.get('theta1_vis'))), 
				np.radians(float(settings.get('theta1_ir'))))
print('Fresnel factors:')
fresnel_pol_combs = ['SSP', 'SPS', 'PSS', 'PPP(XXZ)', 'PPP(XZX)', 'PPP(ZXX)', 'PPP(ZZZ)', 'PSP(ZYX)', 'PSP(XYZ)', 'SPP(YZX)', 'SPP(YXZ)', 'PPS(ZXY)', 'PPS(XZY)',]
for i,p in enumerate(fresnel_pol_combs):
    print(f'{p}: {fresnel[i]:2.4f}')

print('\nCombining different pro-dcd results and deriving trajectory with lowest RSS:')
lowest_RSS = {'value': np.infty, 'comb': 'none'}
#Calculating SFG data in terms of polarisation combinations from lab frame components
sfg_pol_comb_path = work_dir+'/sfg_pol_combs'
if not os.path.exists(sfg_pol_comb_path):
    os.mkdir(sfg_pol_comb_path)
#sfg_file = pro_calc_data.keys()
for sfg_file in pro_calc_data:
    out_string = sfg_file[:-4]
    sfg_data = pro_calc_data.get(sfg_file)
    if 'pol_comb_sfg'+sfg_file[:-4]+'.txt' not in os.listdir(path=sfg_pol_comb_path):
        print(f'Combining tensor elements for {sfg_file}',file=sys.__stdout__)
        sfg_data = {key:np.array(sfg_data[key]) for key in sfg_data} #Put data into numpy array for easy vectorized math
    
        SSP = np.square(np.abs( fresnel[0] * (sfg_data["Re(YYZ)"] + 1.0j*sfg_data["Im(YYZ)"]) )).real
        
        SPS = np.square(np.abs( fresnel[1] * (sfg_data["Re(YZY)"] + 1.0j*sfg_data["Im(YZY)"]) )).real
        
        PSS = np.square(np.abs( fresnel[2] * (sfg_data["Re(ZYY)"] + 1.0j*sfg_data["Im(ZYY)"]) )).real
        
        X_XXZ = -fresnel[3] * (sfg_data["Re(YYZ)"] + 1.0j*sfg_data["Im(YYZ)"])
        X_XZX = -fresnel[4] * (sfg_data["Re(YZY)"] + 1.0j*sfg_data["Im(YZY)"])
        X_ZXX = +fresnel[5] * (sfg_data["Re(ZYY)"] + 1.0j*sfg_data["Im(ZYY)"])
        X_ZZZ = +fresnel[6] * (sfg_data["Re(ZZZ)"] + 1.0j*sfg_data["Im(ZZZ)"])
        PPP = np.square(np.abs( X_XXZ + X_XZX + X_ZXX + X_ZZZ )).real
        
        X_ZYX = +fresnel[7] * (sfg_data["Re(ZYX)"] + 1.0j*sfg_data["Im(ZYX)"])
        X_XYZ = -fresnel[8] * (sfg_data["Re(XYZ)"] + 1.0j*sfg_data["Im(XYZ)"])
        PSP = np.square(np.abs( X_ZYX + X_XYZ )).real
        
        X_YZX = +fresnel[9]  * (sfg_data["Re(YZX)"] + 1.0j*sfg_data["Im(YZX)"])
        X_YXZ = -fresnel[10] * (sfg_data["Re(YXZ)"] + 1.0j*sfg_data["Im(YXZ)"])
        SPP = np.square(np.abs( X_YZX + X_YXZ )).real
        
        X_ZXY = +fresnel[11] * (sfg_data["Re(ZXY)"] + 1.0j*sfg_data["Im(ZXY)"])
        X_XZY = -fresnel[12] * (sfg_data["Re(XZY)"] + 1.0j*sfg_data["Im(XZY)"])
        PPS = np.square(np.abs( X_ZXY + X_XZY )).real
    ##Khezar's old code - Did not support complex fresnel factors and has a bug in the PPP linear combination
    #    Im_YYZ = np.array(sfg_data["Im(YYZ)"])
    #    Re_YYZ = np.array([sfg_data["Re(YYZ)"]])
    #    SSP_fresnel2 = (float(fresnel[0]) ** 2)
    #    SSP = SSP_fresnel2 * (np.square(Im_YYZ) + np.square(Re_YYZ))
    #
    #    Im_YZY = np.array(sfg_data["Im(YZY)"])
    #    Re_YZY = np.array(sfg_data["Re(YZY)"])
    #    SPS_fresnel2 = (float(fresnel[1]) ** 2)
    #    SPS = SPS_fresnel2 * (np.square(Im_YZY) + np.square(Re_YZY))
    #
    #    Im_ZYY = np.array(sfg_data["Im(ZYY)"])
    #    Re_ZYY = np.array(sfg_data["Re(ZYY)"])
    #    PSS_fresnel2 = (float(fresnel[2]) ** 2)
    #    PSS = PSS_fresnel2 * (np.square(Im_ZYY) + np.square(Re_ZYY))
    #
    #    Im_YYZ = np.array(sfg_data["Im(YYZ)"])
    #    Re_YYZ = np.array(sfg_data["Re(YYZ)"])
    #    Im_YZY = np.array(sfg_data["Im(YZY)"])
    #    Re_YZY = np.array(sfg_data["Re(YZY)"])
    #    Im_ZYY= np.array(sfg_data["Im(ZYY)"])
    #    Re_ZYY = np.array(sfg_data["Re(ZYY)"])
    #    Im_ZZZ = np.array(sfg_data["Im(ZZZ)"])
    #    Re_ZZZ = np.array(sfg_data["Re(ZZZ)"])
    #    XXZ_fresnel = (float(fresnel[3]))
    #    XZX_fresnel = (float(fresnel[4]))
    #    ZXX_fresnel = (float(fresnel[5]))
    #    ZZZ_fresnel = (float(fresnel[6]))
    #    PPP_re = -XXZ_fresnel * Re_YYZ - XZX_fresnel * Re_YZY + ZXX_fresnel * Re_ZYY + ZZZ_fresnel * Re_ZZZ
    #    PPP_im = XXZ_fresnel * Im_YYZ - XZX_fresnel * Im_YZY - ZXX_fresnel * Im_ZYY - ZZZ_fresnel * Im_ZZZ
    #    PPP = np.square(PPP_re) + np.square(PPP_im)
    #    Im_ZYX = np.array(sfg_data["Im(ZYX)"])
    #    Re_ZYX = np.array(sfg_data["Re(ZYX)"])
    #    Im_XYZ = np.array(sfg_data["Im(XYZ)"])
    #    Re_XYZ = np.array(sfg_data["Re(XYZ)"])
    #    ZYX_fresnel = (float(fresnel[7]))
    #    XYZ_fresnel = (float(fresnel[8]))
    #    PSP_re = ZYX_fresnel * Re_ZYX - XYZ_fresnel * Re_XYZ
    #    PSP_im = ZYX_fresnel * Im_ZYX - XYZ_fresnel * Im_XYZ
    #    PSP = np.square(PPP_re) + np.square(PPP_im)
    #
    #    Im_YZX = np.array(sfg_data["Im(YZX)"])
    #    Re_YZX = np.array(sfg_data["Re(YZX)"])
    #    Im_YXZ = np.array(sfg_data["Im(YXZ)"])
    #    Re_YXZ = np.array(sfg_data["Re(YXZ)"])
    #    YZX_fresnel = (float(fresnel[9]))
    #    YXZ_fresnel = (float(fresnel[10]))
    #    SPP_re = YZX_fresnel * Re_YZX - YXZ_fresnel * Re_YXZ
    #    SPP_im = YZX_fresnel * Im_YZX - YXZ_fresnel * Im_YXZ
    #    SPP = np.square(SPP_re) + np.square(SPP_im)
    #
    #    Im_ZXY = np.array(sfg_data["Im(ZXY)"])
    #    Re_ZXY = np.array(sfg_data["Re(ZXY)"])
    #    Im_XZY = np.array(sfg_data["Im(XZY)"])
    #    Re_XZY = np.array(sfg_data["Re(XZY)"])
    #    ZXY_fresnel = (float(fresnel[11]))
    #    XZY_fresnel = (float(fresnel[12]))
    #    PPS_re = ZXY_fresnel * Re_ZXY - XZY_fresnel * Re_XZY
    #    PPS_im = ZXY_fresnel * Im_ZXY - XZY_fresnel * Im_XZY
    #    PPS = np.square(PPS_re) + np.square(PPS_im)
    
        xvals = sfg_data["#Frequency"]
        sfg_pol_comb = np.vstack((xvals, SSP, PPP, SPS, PSS, PSP, SPP, PPS)).T
        np.savetxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file[:-4]+'.txt', sfg_pol_comb, fmt='%1.9f')
    else:
        sfg_pol_comb = np.loadtxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file[:-4]+'.txt')


#Normalization and reduce range to only include Amide-1 peak    
    calc_data = {'xvals':sfg_pol_comb[:,0], 
		 'SSP': sfg_pol_comb[:,1],
		 'PPP': sfg_pol_comb[:,2],
		 'SPS': sfg_pol_comb[:,3],
		 'PSS': sfg_pol_comb[:,4],
		 'PSP': sfg_pol_comb[:,5],
		 'SPP': sfg_pol_comb[:,6],
		 'PPS': sfg_pol_comb[:,7]}
    N_calc = max(calc_data[settings['Normalize_wrt']])
    calcs = {pol_comb: visca.Normalize(calc_data[pol_comb],N_calc) for pol_comb in pol_combs}
    calcs['xvals'] = calc_data['xvals']
    calc_data_reduced_range = {pol_comb: visca.Reduce_range(RSS_range_i, RSS_range_f, calcs['xvals'], calcs[pol_comb]) for pol_comb in pol_combs}
    calc_data_reduced_range['xvals'] = visca.Reduce_range(RSS_range_i, RSS_range_f, calcs['xvals'], calcs['xvals'])
#calculate RSS    
    RSSs = {pol_comb: visca.RSS(calc_data_reduced_range['xvals'], calc_data_reduced_range[pol_comb], exp_data_reduced_range['xvals'], exp_data[pol_comb]) for pol_comb in pol_combs}
    RSS_total = 0
    for pol_comb in pol_combs:
        RSS_total += RSSs[pol_comb]
        

    if 'RSS_file' in settings.keys():
        all_RSS.append(RSS_total)

#Save and Print new lowest RSS if found
    if RSS_total < lowest_RSS['value']:
        lowest_RSS['value'] = RSS_total
        try:
            lowest_RSS['out_string'] = out_string
        except NameError:
            out_string = ''
            lowest_RSS['out_string'] = out_string
        lowest_RSS['RSSs'] = RSSs
        print("New lowest combination: ",out_string)
        s = ''
        for pol_comb in pol_combs:
            s += 'RSS_%s = %3.3f '%(pol_comb,RSSs[pol_comb])
        print(s,flush=True)
        print("RSS_total = %3.3f"%RSS_total,flush=True)
        print()
    elif RSS_total < lowest_RSS['value']+0.1:
        print("A quite good combination: ",out_string)
        s = ''
        for pol_comb in pol_combs:
            s += 'RSS_%s = %3.3f '%(pol_comb,RSSs[pol_comb])
        print(s,flush=True)
        print("RSS_total = %3.3f"%RSS_total,flush=True)
        print()
#Print lowest RSS result
print()
print('##################################')
print('# Final lowest RSS_total = %3.3f #'%lowest_RSS['value'], flush=True)
print('##################################')
print('It was obtained by combining the following trajectories: ')
print(lowest_RSS['out_string'])
print()
print("The RSS separated on different pol. combs.:")
s = ''
for pol_comb in pol_combs:
    s += 'RSS_%s = %3.3f '%(pol_comb,lowest_RSS['RSSs'][pol_comb])
print(s,flush=True)

#Print all RSS value to RSS file if specified
if 'RSS_file' in settings.keys():
    RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
    print('\nprinting all RSS values to %s'%RSS_file)
    with open(RSS_file,'w') as f:
        first_write_flag = True
        f.write('initial_frame   final_frame  RSS  lowest_RSS\n')
        for comb,RSS in zip(combs,all_RSS):
            frame_i,_ = visca.Get_dcd_i_and_f(comb[0])
            _,frame_f = visca.Get_dcd_i_and_f(comb[-1])
            if first_write_flag:
                f.write('%i             %i             %3.5f           %3.5f\n'%(frame_i,frame_f,RSS,lowest_RSS['value']))
                first_write_flag = False
            else:
                f.write('%i             %i             %3.5f\n'%(frame_i,frame_f,RSS))






