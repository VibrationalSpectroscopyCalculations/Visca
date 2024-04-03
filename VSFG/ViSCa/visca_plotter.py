import matplotlib.pyplot as plt
import numpy as np
import os, sys
import visca_funcs as visca
#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)
fi = sys.argv[2]
ff = sys.argv[3]

#filename=settings['name']

#Setting up work directories
plot_dir = './Working_Directory_'+settings['name']+'/Plots'
os.system('mkdir -p %s'%plot_dir)
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], plot_dir))

fortran_script = settings['fortran_script_location']+settings['fortran_script']

os.chdir(plot_dir)
command = 'vmd -dispdev text -e "'+settings['tcl_scripts_location']+'splitter_i_to_f.tcl" -args "%s%s.dcd" %s %s > ../VMD_log.txt'%(settings['dcd_location'],settings['name'],str(fi),str(ff))
os.system(command)

split_dcd = 'f'+fi+'to'+ff

sfg_pol_comb = np.loadtxt('../sfg_pol_combs/pol_comb_sfg'+split_dcd+'.txt')
calc_data = {'xvals':sfg_pol_comb[:,0], 'SSP': sfg_pol_comb[:,1], 'PPP': sfg_pol_comb[:,2], 'SPS': sfg_pol_comb[:,3], 'PSS': sfg_pol_comb[:,4]}

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

plt.title("Trajectory "+settings['name']+" frame "+fi+"-"+ff+" with RSS = %2.2f"%RSS_total)
plt.xlabel("Frequency")
plt.ylabel("Normalized SFG Intensity")
plt.legend()
plt.savefig(split_dcd+'.pdf',dpi=400)
plt.show()
