import visca_orient_funcs as visca
import matplotlib.pyplot as plt
import matplotlib.font_manager as font_manager
import numpy as np
import sys,os,copy
import statistics

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)
main_dir = os.getcwd()
work_dir = 'Working_Directory_'+settings['name']+'/orient'
plot_dir = main_dir+'/'+work_dir+'/plot'
sfg_pol_comb_dir = main_dir+'/'+work_dir+'/sfg_pol_combs'
RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
os.system('mkdir %s -p'%plot_dir)
RSS_data = visca.Read_RSS_file(RSS_file)
if 'RSS_cutoff' in settings.keys():
    RSS_cutoff = float(settings['RSS_cutoff'])
    print('RSS_cutoff read from inputfile: %2.3f'%RSS_cutoff)
else:
    RSS_cutoff = 2*RSS_data['lowest_RSS'][0]
    print('Using default RSS_cutoff: %2.2f'%RSS_cutoff)

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

#Calculating spectra files associated with all RSS-values below cutoff
spec_files = []
lowest_RSS = RSS_data['lowest_RSS'][0]
sfg_pol_comb_dir_files = os.listdir(path=sfg_pol_comb_dir)
RSS_list = []
for i,f,RSS in zip(RSS_data['theta'],RSS_data['psi'],RSS_data['RSS']):
    if RSS>RSS_cutoff:
        continue
    spec_file = 'pol_comb_sfgtheta%i_phi0_psi%i.txt'%(int(i),int(f))
    spec_files.append(spec_file)
    RSS_list.append(RSS)
    if spec_file in sfg_pol_comb_dir_files:
        print(spec_file,'already exist')
    else:
         print(f'Error - {spec_file} not found. Run "orient" to generate these files. -- Aborting')
         sys.exit()
#        print(spec_file,'Calculating')
#        visca.Write_plot(settings_file, str(int(i)), str(int(f)))
#        os.system('python3 Make_plotter_write.py %s %i %i'%(settings_file,int(i),int(f)))
    if RSS==lowest_RSS:
       print('Found best spectrum')
       best_spec = visca.Read_pol_comb_file(sfg_pol_comb_dir+'/'+spec_file, settings['Normalize_wrt'])

N_tot = len(RSS_data['RSS'])
N = len(RSS_list)
RSS_mean = np.mean(RSS_list)
#RSS_std = np.sqrt(sum([(RSS-RSS_mean)**2 for RSS in RSS_list])/len(RSS_list)) #This is right but slow
RSS_std = statistics.stdev(RSS_list)
print('Number of structures in ensemble = %i corresponds to %2.2f%% of all structures'%(N, 100*N/N_tot))
print('Average of RSS =',RSS_mean)
print('Standard deviation of RSSs =',RSS_std)

exp_file = 'exp_spec.txt'
plot_dir_files = os.listdir(path=plot_dir)
if exp_file not in plot_dir_files:
        print(exp_file,'Calculating')
        visca.Write_exp(settings_file)
#        os.system('python3 Make_plotter_write_exp.py %s %i %i'%(settings_file,int(i),int(f)))
exp_spec = visca.Read_RSS_file((plot_dir+'/'+exp_file))


first_flag = True
for spec_file in spec_files:
    spec_data = visca.Read_pol_comb_file(sfg_pol_comb_dir+'/'+spec_file, settings['Normalize_wrt'])
    if first_flag:
        low_spec = copy.deepcopy(spec_data)
        high_spec = copy.deepcopy(spec_data)
        first_flag  = False
    for pol_comb in pol_combs:
        for i in range(len(spec_data['wavenumber'])):
            if spec_data[pol_comb][i] < low_spec[pol_comb][i]:
               low_spec[pol_comb][i] = spec_data[pol_comb][i]     
            
            elif spec_data[pol_comb][i] > high_spec[pol_comb][i]:
               high_spec[pol_comb][i] = spec_data[pol_comb][i]     

colors1 = ["orange", "blue", "green", "darkred"]
#colors2 = ["lightskyblue", "deepskyblue", "blue", "darkblue"]

for i,pol_comb in enumerate(pol_combs):
    plt.fill_between(best_spec['wavenumber'],low_spec[pol_comb],high_spec[pol_comb],color=colors1[i],alpha=0.3)

for i,pol_comb in enumerate(pol_combs):
    plt.plot(best_spec['wavenumber'],best_spec[pol_comb],colors1[i],label=pol_comb,lw=2)

for i,pol_comb in enumerate(pol_combs):
    plt.plot(exp_spec['wavenumber'],exp_spec[pol_comb],colors1[i],lw=0.75)

font = 'Arial'
fs = 16
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}

plt.xticks(fontsize=fs*0.8)
plt.yticks(fontsize=fs*0.8)
plt.xlabel(r'Frequency $[\mathrm{cm}^{-1}]$',font=font)
plt.ylabel('Normalized VSFG intensity',font=font)
plt.legend(fontsize=fs)
plt.title('%s RSS-cutoff = %1.3f'%(work_dir,RSS_cutoff),font=font)
plt.xlim([1500,1800])
#plt.xlim([1575,1680])
plt.ylim([-0.1,1.05])
plt.savefig('./'+work_dir+'/plot/shadedplot.pdf')
plt.show()



