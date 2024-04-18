import visca_funcs as visca
import matplotlib.pyplot as plt
import matplotlib.font_manager as font_manager
import numpy as np
import sys,os,copy
import statistics

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)
main_dir = os.getcwd()
work_dir = 'Working_Directory_'+settings['name']+'/select'
plot_dir = main_dir+'/'+work_dir+'/plot'   
RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
os.system('mkdir %s -p'%plot_dir)
RSS_data = visca.Read_RSS_file(RSS_file)

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


#sort structures after their RSS
i_s = [i for _,i in sorted(zip(RSS_data['RSS'],RSS_data['initial_frame']))]
f_s = [f for _,f in sorted(zip(RSS_data['RSS'],RSS_data['final_frame']))]
RSSs = sorted(RSS_data['RSS'])

#print(i_s[:314])
#print(f_s[:314])

#Initialize standard deviations
calc_std = 0
exp_std = float(settings['exp_std'])
i = 0 #structes in ensemble

#Expand ensemble until std_calc is higher than std_exp
spec_files = []
while calc_std < exp_std:
    #Calculate spectra for new structure
    plot_dir_files = os.listdir(path=plot_dir)
    spec_file = 'f%ito%i.txt'%(int(i_s[i]),int(f_s[i]))
    spec_files.append(spec_file)
    if spec_file in plot_dir_files:
        print(spec_file,'already exist')
    else:
        print(spec_file,'Calculating')
        visca.Write_plot(settings_file, str(int(i_s[i])), str(int(f_s[i])))
    
    #Read Calculated spectra for processing
    specs = {}
    for spec_file in spec_files:
        spec_data = visca.Read_RSS_file('%s/%s'%(plot_dir,spec_file)) #dict with wavenumber, SSP, PPP, SPS, PSP
    #Make list of spectra with correct range between RSS_range_i and RSS_range_f
        ranged_spec_data = {pol_comb: [] for pol_comb in pol_combs}
        ranged_spec_data['wavenumber'] = []
        for j,wavenumber in enumerate(spec_data['wavenumber']):
            if wavenumber>float(settings['RSS_range_i']) and wavenumber<float(settings['RSS_range_f']):
                ranged_spec_data['wavenumber'].append(spec_data['wavenumber'][j])
                for pol_comb in pol_combs:
                    ranged_spec_data[pol_comb].append(spec_data[pol_comb][j])
        specs[spec_file] = ranged_spec_data
        N_points = len(specs[spec_file]['wavenumber'])

    #Calculate list of Means for for each point
    N = len(specs) #number of spectra
    mean = {pol_comb: np.zeros(N_points) for pol_comb in pol_combs}
    for j in range(N_points):
        for spec_file in specs:
            for pol_comb in pol_combs:
                mean[pol_comb][j] += specs[spec_file][pol_comb][j]/N 


#    #Calculate average deviation from best spectrum
#    calc_std = 0
#    for pol_comb in pol_combs:
#        pol_comb_calc_std = 0
#        for spec_file in specs:
#            point_calc_std = 0
#            for j in range(len(mean)):
#                point_calc_std += abs(specs[spec_files[0]][pol_comb][j]-specs[spec_file][pol_comb][j])
#            pol_comb_calc_std += point_calc_std/N_points
#        calc_std += pol_comb_calc_std/len(pol_combs)
     
    #Calculate average std. dev.
    calc_std = 0
    for pol_comb in pol_combs:
        pol_comb_calc_std = 0
        for spec_file in specs:
            point_calc_std = 0
            for j in range(len(mean)):
                point_calc_std += (mean[pol_comb][j]-specs[spec_file][pol_comb][j])**2
            pol_comb_calc_std += np.sqrt(point_calc_std/N_points)
        calc_std += pol_comb_calc_std/len(pol_combs)
    print('Ensemble of %i structures - standard deviation = %2.5f'%(i,calc_std))
    i += 1
    if i > 1000:
        print('ERROR: Ensemble limit reached - expand limit in code')
        break
#report RSS cutoff (RSS of the last structure)
print('\nCalculated standard deviation from calculation: %2.5f\nResulting RSS cutoff: %s of ensemble with %i structures'%(calc_std,RSSs[i],i))

#Write file with ensemble structures and their RSS
ensemble_file = 'results/ensemble_select_%s.dat'%settings['name']
os.system('mkdir -p results')
with open(ensemble_file,'w') as f:
    for j in range(i):
        f.write('{:>6}   {:<2.4f}\n'.format(int(i_s[j]),RSSs[j]))

###########################
### PROGRAM STOPS HERE  ###
###                     ###
###########################
