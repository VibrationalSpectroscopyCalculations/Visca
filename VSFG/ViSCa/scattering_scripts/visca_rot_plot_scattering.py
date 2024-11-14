import matplotlib.pyplot as plt
import os, sys
import visca_orient_funcs as visca
import numpy as np
from numpy import cos, sin, exp

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
try:
    work_dir = 'Working_Directory_'+settings['name']+'/orient'
except KeyError:
    print('Error in input file -- Aborting')
    sys.exit()

#Handle command line input arguments
input_args = sys.argv[2:]

noplot_flag = False
if '-noplot' in sys.argv:
    noplot_flag = True
    input_args.remove('-noplot')

multichain_flag = False
if '-multichain' in sys.argv:
    multichain_flag = True
    input_args.remove('-multichain')

if len(input_args)==2:
    theta = input_args[0]
    phi = '0'
    psi = input_args[1]
elif len(input_args)<2:
    print('Additional input arguments for orientation angles (theta and psi) where not found -- Aborting')
    sys.exit()
else:
    print(f'Too many additional input arguments for orientation angles (two needed but received {input_args} -- Aborting')
    sys.exit()

#Setting up work directories
work_dir = './Working_Directory_'+settings['name']+'/orient'
plot_dir = work_dir+'/plot'
#dcd_dir = work_dir+'/dcds'
os.system('mkdir -p %s'%plot_dir)
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], plot_dir))

fortran_script = settings['fortran_script_location']+settings['fortran_script']

dcdname = f'theta{theta}_phi{phi}_psi{psi}'
sfg_file = f'pol_comb_sfgtheta{theta}_phi{phi}_psi{psi}.txt'

os.chdir(plot_dir)
print("################################")
print("# Calling rotator.tcl via VMD  #")
print("################################")
VMD_log = 'VMD_log.txt'
print(f'Constructing .dcd with orientation theta={theta}, phi={phi} and psi={psi}')
os.system(settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'rotator_theta_phi_psi.tcl" -args "%s.pdb" %i %i %i > ../%s 2> ../%s'%(settings['name'], int(theta), int(phi), int(psi), VMD_log, VMD_log))

if multichain_flag:
   sfg_file = f'../dcds/theta{theta}_phi{phi}_psi{psi}_SFG.txt' #relative path from plot_dir to SFG.txt in dcd_dir
   if os.path.isfile(sfg_file):
       print(f'Copying {sfg_file[8:]} from dcd directory')
       command = f'cp {sfg_file} .'
       os.system(command)
   else:
       print(f'{sfg_file} was not found.\nFor multichain plotter the SFG.txt need to already exist in the dcds directory\nMake this file by running "visca orient_multichain <Your_inputfile>.inp" and setting the following parameters in the input file:\n   theta_i = {theta}\n   theta_f = {theta}\n   psi_i   = {psi}\n   psi_f   = {psi}\n   steps   = 1\n')
       sys.exit()
else:
   print('Running the ViSCa fortran script')
   command = 'time "'+fortran_script+'" -pdb '+settings['name']+'.pdb -spec_min 1500 -spec_max 1800 -dcd '+dcdname+'.dcd -SFG -inhom 0 -width %s -charge 1 -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH %s -Omega0 %s 1> fortran_log.txt 2> /dev/null'%(settings['width'],settings['mOH'],settings['Omega0'])
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
sfg_file_name = dcdname+'_SFG.txt'
sfg_data = visca.Read_calc_SFG(plot_dir+'/'+sfg_file_name)
sfg_data = {key:np.array(sfg_data[key]) for key in sfg_data}
R = float(settings.get('R'))
#scat = float(settings.get('scat_angle'))*np.pi/180 
scat_min = float(settings.get('scat_min'))
scat_max = float(settings.get('scat_max'))
scat_range = int(scat_max-scat_min+1)
beta = float(settings.get('beta'))*np.pi/180 
alpha = float(settings.get('alpha'))*np.pi/180 

k0 = 2*np.pi*(1/float(settings.get('lambda_vis'))+1e7/(float(settings.get('omega_ir')))) #3448 is used for the theory paper
#q=visca.q(k0, scat)
w = (k0**-1)*2*np.pi
Grrr = np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Gppr = np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Gprp = np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Grpp = np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Xrrr = np.array(sfg_data["Re(ZZZ)"]) + 1j*np.array(sfg_data["Im(ZZZ)"], dtype = "complex")
Xrpp = np.array(sfg_data["Re(ZYY)"]) + 1j*np.array(sfg_data["Im(ZYY)"], dtype = "complex")
Xprp = np.array(sfg_data["Re(YZY)"]) + 1j*np.array(sfg_data["Im(YZY)"], dtype = "complex")
Xppr = np.array(sfg_data["Re(YYZ)"]) + 1j*np.array(sfg_data["Im(YYZ)"], dtype = "complex")
xvals = np.array(sfg_data["#Frequency"])
Eppp =np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Essp =np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Esps =np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
Epss =np.zeros(len(sfg_data["#Frequency"]), dtype = "complex")
for a in range(scat_range):
    for n in range(len(sfg_data["#Frequency"])):
        Grrr[n] = (visca.Grrr(Xrrr[n], Xrpp[n], Xprp[n], Xppr[n],R,k0,np.radians(scat_min+a)))
        Gppr[n] = (visca.Gppr(Xrrr[n], Xrpp[n], Xprp[n], Xppr[n],R,k0,np.radians(scat_min+a)))
        Gprp[n] = (visca.Gprp(Xrrr[n], Xrpp[n], Xprp[n], Xppr[n],R,k0,np.radians(scat_min+a)))
        Grpp[n] = (visca.Grpp(Xrrr[n], Xrpp[n], Xprp[n], Xppr[n],R,k0,np.radians(scat_min+a)))
    
    coef = (w**2*exp(1j*k0*R))/(2*3e8**2*R)

    Eppp += -coef*(cos(np.radians(scat_min+a)/2)*((Grrr+Grpp)*cos(beta)+(Grrr-Grpp)*cos(np.radians(scat_min+a)-beta + 2*alpha))- sin(np.radians(scat_min+a)/2)*((Gppr-Gprp)*sin(beta)+(Gprp+Gppr)*sin(np.radians(scat_min+a)-beta + 2*alpha)))

    Essp += -coef*Gppr*(cos(beta) * (np.radians(scat_min+a)/2+alpha) + sin(beta) * sin(np.radians(scat_min+a)/2+alpha))

    Esps += -coef*Gprp*cos(np.radians(scat_min+a)/2+alpha)

    Epss += -coef*Grpp*cos(np.radians(scat_min+a)/2)

Eppp_avg = np.divide(Eppp,scat_range)
Essp_avg = np.divide(Essp,scat_range)
Esps_avg = np.divide(Esps,scat_range)
Epss_avg = np.divide(Epss,scat_range)

PPP = (abs(Eppp_avg)**2)
SSP = (abs(Essp_avg)**2)
SPS = (abs(Esps_avg)**2)
PSS = (abs(Epss_avg)**2)
sfg_pol_comb = np.vstack((xvals, SSP, PPP, SPS, PSS)).T
np.savetxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file_name[:-8]+'.txt', sfg_pol_comb, fmt='%1.9g')


#sfg_pol_comb = np.loadtxt(work_dir+'/sfg_pol_combs/pol_comb_sfg'+dcdname+'.txt')
calc_data = {'xvals':sfg_pol_comb[:,0],
                'SSP': sfg_pol_comb[:,1],
                'PPP': sfg_pol_comb[:,2],
                'SPS': sfg_pol_comb[:,3],
                'PSS': sfg_pol_comb[:,4]}

os.chdir(plot_dir)
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
N = max(exp_data_reduced_range[settings['Normalize_wrt']]) #Normalization constant
exp_data = {pol_comb: visca.Normalize(exp_data_reduced_range[pol_comb],N) for pol_comb in pol_combs}
exp_data_plot = {pol_comb: visca.Normalize(exp_data_reduced_range_plot[pol_comb],N) for pol_comb in pol_combs}

#Calculation Data Import
colours_calc = ["darkgreen", "black", "darkblue", "red"]
colours_exp = ["lime", "darkgrey", "royalblue", "orange"]

#Normalization
N_calc = max(calc_data[settings['Normalize_wrt']])
calcs = {pol_comb: visca.Normalize(calc_data[pol_comb],N_calc) for pol_comb in pol_combs}
calc_data_reduced_range = {pol_comb: visca.Reduce_range(RSS_range_i, RSS_range_f, calc_data['xvals'], calcs[pol_comb]) for pol_comb in pol_combs}
calc_data_reduced_range['xvals'] = visca.Reduce_range(RSS_range_i, RSS_range_f, calc_data['xvals'], calc_data['xvals'])
RSSs = [visca.RSS(calc_data_reduced_range['xvals'], calc_data_reduced_range[pol_comb], exp_data_reduced_range['xvals'], exp_data[pol_comb]) for calc,pol_comb in zip(calcs,pol_combs)]
RSS_total = sum(RSSs)
print('RSS total = %2.4f'%RSS_total)
for i,pol_comb in enumerate(pol_combs):
    print('RSS %s: %2.4f'%(pol_comb,RSSs[i]))
print()


i=-1
#for calc,I in zip(calcs[::-1],intensities_plot[::-1]):
for pol_comb in pol_combs[::-1]:
    plt.plot(exp_data_reduced_range_plot['xvals'], exp_data_plot[pol_comb], colours_exp[i], lw=1)# label="experimental "+pol_comb)
    plt.plot(calc_data['xvals'], calcs[pol_comb], colours_calc[i], lw=3, label=pol_comb)#label="calculated "+pol_comb)
    i -= 1
##Plot RSS range
#plt.plot(2*[RSS_range_i],[0,1],'--r')
#plt.plot(2*[RSS_range_f],[0,1],'--r')
fs = 12
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}

plt.rc('font', **font)
plt.xticks(fontsize=fs)
plt.yticks(fontsize=fs)

plt.title(settings['name'][0:4]+f" theta: {theta} degrees psi: {psi} degrees with RSS = {RSS_total:2.4f}")
plt.xlabel("Frequency",fontsize=fs)
plt.ylabel("Normalized SFG Intensity",fontsize=fs)
plt.xlim([1500, 1800])
plt.ylim([-0.1, 1.1])
plt.legend()
plt.tight_layout(pad=1)
plt.savefig(dcdname+'.pdf',dpi=400)
if not noplot_flag:
    plt.show()
