import matplotlib.pyplot as plt
import numpy as np
import os, sys
import visca_funcs as visca
#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
try:
    work_dir = 'Working_Directory_'+settings['name']+'/select'
except KeyError:
    print('Error in input file -- Aborting')
    sys.exit()

#Handle command line input arguments
input_args = sys.argv[2:]

noplot_flag = False
if '-noplot' in sys.argv:
    noplot_flag = True
    input_args.remove('-noplot')
    #Piping output to plot_log in the plot dir
    out_log = '%s/plotter_log.txt'%work_dir
    sys.stdout = open(out_log,'w')

N_inputs = len(input_args)
frameaveraging_flag = False #if more than 2 frame indexes are given it calculates the average of these
if N_inputs==0:
    print('Additional input arguments for frames (initial and final) where not found -- Aborting')
    sys.exit()
elif N_inputs==1:
    fi = input_args[0]
    ff = str(int(fi)+1)
elif N_inputs==2:
    fi = input_args[0]
    ff = input_args[1]
elif N_inputs>2:
    frameaveraging_flag = True

#Setting up work directories
plot_dir = './Working_Directory_'+settings['name']+'/select/plot'
os.system('mkdir -p %s'%plot_dir)
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], plot_dir))
work_dir = './Working_Directory_'+settings['name']+'/select'

fortran_script = settings['fortran_script_location']+settings['fortran_script']

os.chdir(plot_dir)
if frameaveraging_flag:
    #Make concaternated dcd file with the correct frames
    averaging_dcds = []
    for fi in input_args:
        ff = str(int(fi)+1)
        averaging_dcds.append('f'+fi+'to'+ff+'.dcd')
        print(f'Creating {averaging_dcds[-1]}')
        command = settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'splitter_i_to_f.tcl" -args "%s%s.dcd" %s %s > ../VMD_log.txt 2> ../VMD_log.txt'%(settings['dcd_location'],settings['name'],str(fi),str(ff))
        os.system(command)
    split_dcd = f'f{"to".join(input_args)}'
    command = f'catdcd -o {split_dcd}.dcd {" ".join(averaging_dcds)}'
    print(command)
    os.system(command)

else:
    command = settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'splitter_i_to_f.tcl" -args "%s%s.dcd" %s %s > ../VMD_log.txt 2> ../VMD_log.txt'%(settings['dcd_location'],settings['name'],str(fi),str(ff))
    os.system(command)
    split_dcd = 'f'+fi+'to'+ff

command = 'time "'+fortran_script+'" -pdb '+settings['name']+'.pdb -spec_min 1500 -spec_max 1800 -dcd '+split_dcd+'.dcd -SFG -inhom 0 -width %s -charge 1 -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH %s -Omega0 %s 1> fortran_log.txt 2> /dev/null'%(settings['width'],settings['mOH'],settings['Omega0'])
os.system(command)
os.chdir('../../../')

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

#Calculating SFG data in terms of polarisation combinations from lab frame components
sfg_pol_comb_path = work_dir+'/sfg_pol_combs'
if not os.path.exists(sfg_pol_comb_path):
    os.mkdir(sfg_pol_comb_path)
sfg_file_name = split_dcd+'_SFG.txt'
sfg_data = visca.Read_calc_SFG(plot_dir+'/'+sfg_file_name)
sfg_data = {key:np.array(sfg_data[key]) for key in sfg_data}

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

xvals = sfg_data["#Frequency"]
sfg_pol_comb = np.vstack((xvals, SSP, PPP, SPS, PSS, PSP, SPP, PPS)).T
np.savetxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file_name[:-8]+'.txt', sfg_pol_comb, fmt='%1.9f')

#sfg_pol_comb = np.loadtxt(work_dir+'/sfg_pol_combs/pol_comb_sfg'+split_dcd+'.txt')
#calc_data = {'xvals':sfg_pol_comb[:,0], 
#		'SSP': sfg_pol_comb[:,1], 
#		'PPP': sfg_pol_comb[:,2], 
#		'SPS': sfg_pol_comb[:,3], 
#		'PSS': sfg_pol_comb[:,4]}
calc_data = visca.Read_pol_comb_file(work_dir+'/sfg_pol_combs/pol_comb_sfg'+split_dcd+'.txt',False)

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

#Correction factor for imperfect half-wave plate THIS IS PROBABLY BS
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

#Calculation Data Import
colours_calc = ["darkgreen", "black", "darkblue", "red"]
colours_exp = ["lime", "darkgrey", "royalblue", "orange"]

#Normalization
N_calc = max(calc_data[settings['Normalize_wrt']])
calcs = {pol_comb: visca.Normalize(calc_data[pol_comb],N_calc) for pol_comb in pol_combs}

RSSs = [visca.RSS(calc_data['xvals'], calcs[pol_comb], exp_data_reduced_range['xvals'], exp_data[pol_comb]) for calc,pol_comb in zip(calcs,pol_combs)]
RSS_total = sum(RSSs)
for i,pol_comb in enumerate(pol_combs):
    print('RSS_%s = %2.4f'%(pol_comb,RSSs[i]))
print('RSS_TOTAL = %2.4f'%RSS_total)
#print("RSS_SSP = "+str(RSSs[0])+" RSS_PPP = "+str(RSSs[1])+" RSS_SPS = "+str(RSSs[2])+" RSS_PSP = "+str(RSSs[3])+"\nRSS_total = "+str(RSS_total),flush=True)
print()


i=-1
#for calc,I in zip(calcs[::-1],intensities_plot[::-1]):
for pol_comb in pol_combs[::-1]:
    plt.plot(exp_data_reduced_range_plot['xvals'], exp_data_plot[pol_comb], colours_exp[i], label="experimental "+pol_comb, lw=1)
    plt.plot(calc_data['xvals'], calcs[pol_comb], colours_calc[i], label="calculated "+pol_comb, lw=3)
    i -= 1
#Plot RSS range
plt.plot(2*[RSS_range_i],[0,1],'--r')
plt.plot(2*[RSS_range_f],[0,1],'--r')
fs = 16
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}
#plt.title("Trajectory "+settings['name']+" frame "+fi+"-"+ff+" with RSS = %2.2f"%RSS_total)
plt.title(f"Trajectory {settings['name']}. frame {'-'.join(input_args)} with RSS = %2.2f"%RSS_total)
plt.xticks(fontsize=fs*0.8)
plt.yticks(fontsize=fs*0.8)
plt.xlabel("Frequency $[\mathrm{cm}^{-1}]$", font=font)
plt.ylabel("Normalized VSFG Intensity", font=font)
plt.legend(fontsize=fs)
plt.savefig(plot_dir+'/'+split_dcd+'.pdf',dpi=400)
if not noplot_flag:
    plt.show()
