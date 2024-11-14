import itertools, os, sys
import matplotlib.pyplot as plt
import numpy as np
from numpy import cos, sin, exp
import visca_orient_funcs as visca
#from pathlib import Path
#home = str(Path.home())

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
try:
    work_dir = 'Working_Directory_'+settings['name']+'/orient'
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
print('### Running Orientation RSS ###\nScript by Kris Strunge\n')
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
    print('Unable to correct PDB file -- Aborting')
    sys.exit()
#Check for hydrogen atoms in pdb file
if visca.Check_pdbfile_hydrogen(pdbfilename, out_log):
    print('PDB file checked - Contained hydrogen atoms\n')
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], dcd_dir))

#Reading input limits
theta_i,theta_f = int(settings['theta_i']),int(settings['theta_f'])
#phi_i,phi_f = int(settings['phi_i']),int(settings['phi_f']) # Azimuthal angle is already averaged over in the analytical equations in the fortran script
psi_i,psi_f = int(settings['psi_i']),int(settings['psi_f'])

#Setting up rotations
thetas = np.linspace(theta_i,theta_f,int(settings['steps'])+1)
#phis = np.linspace(phi_i,phi_f,int(settings['steps']))
psis = np.linspace(psi_i,psi_f,int(settings['steps'])+1)

#Saves if the limits are equal to reduce running cost by a factor of "steps"
if theta_i==theta_f:
    thetas = np.linspace(theta_i,theta_f,1)
#if phi_i==phi_f:
#    phis = np.linspace(phi_i,phi_f,1)
if psi_i==psi_f:
    psis = np.linspace(psi_i,psi_f,1)

#Making rotated .dcds
print("Entering ./%s/dcds"%work_dir)
os.chdir(dcd_dir)
print("################################")
print("# Calling rotator.tcl via VMD  #")
print("################################")
VMD_log = 'VMD_log.txt'

pro_dcds = []
files = os.listdir(path='.')
M = len(thetas)*len(psis)
i = 1
for theta in thetas:
    for psi in psis:
        phi = 0
        pro_dcds.append('theta%i_phi%i_psi%i.dcd'%(theta,phi,psi))
        if 'theta%i_phi%i_psi%i.dcd'%(theta,phi,psi) in files:
            print('Found .dcd for theta= %i, phi= %i and psi= %i'%(theta,phi,psi))
            continue 
        else:
            print('Making .dcd for theta= %i, phi= %i and psi= %i'%(theta,phi,psi))
            print(f'VMD orienting {i} out of {M} orientations. Calculating theta= {theta}, phi= {phi} and psi= {psi}', file=sys.__stdout__)
            i += 1
            os.system(settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'rotator_theta_phi_psi.tcl" -args "%s.pdb" %i %i %i > ../%s 2> ../%s'%(settings['name'], theta, phi, psi, VMD_log, VMD_log))
print("Done with making rotating dcds via VMD into pro-dcd files\nOutput can be found in %s/%s"%(work_dir,VMD_log))
print('Leaving %s'%dcd_dir)
os.chdir('../../../')
print()

fortran_script = settings['fortran_script_location']+settings['fortran_script']

#Finding all small .dcd files (pro dcds)
#files = os.listdir(path=dcd_dir)
#filter out non-dcd files and sort according to initial frame
#pro_dcds = [files[i] for i,name in enumerate(files) if name[-4:]==".dcd"]
print("pro_dcds: ",pro_dcds)

combs = [[pro_dcd] for pro_dcd in pro_dcds]
N=len(combs)
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
        print(f'Orientation {i+1} out of {M}. Calculating {sfg_file_name}', file=sys.__stdout__)
        print('Fortran command:',command)
        os.system(command)
    try:
        pro_calc_data[pro_dcd] = visca.Read_calc_SFG(sfg_file_name)
    except FileNotFoundError:
        print(f'\nError - {sfg_file_name} was not found.\nThis is very likely because the ViSCa Fortran script did not run correctly.\nCheck your .pdb (formatting issues in .pdb files are common!) and .dcd files are correctly formatted and their path is correct in the input file.\nCheck that your input file correctly specifies the correct path to the fortran script ("visca.out").\nOtherwise consider seeking technical (or emotional) support.',file=sys.__stdout__)
        sys.exit()
    length_of_data = len(pro_calc_data[pro_dcd][list(pro_calc_data[pro_dcd].keys())[0]])
print('All Fortran calculations - Done but might still have errors (check if *_SFG.txt are correct')
print('Leaving dcd directory\n')
os.chdir('../../../')
print('Calculating Fresnel factors...')
#Calculate Fresnel factors
fresnel = visca.fresnel_factors(float(settings.get('lambda_vis')), float(settings.get('omega_ir')), complex(settings.get('n1_sfg')), complex(settings.get('n1_vis')), complex(settings.get('n1_ir')), complex(settings.get('n2_sfg')), complex(settings.get('n2_vis')), complex(settings.get('n2_ir')), complex(settings.get('ni_sfg')), complex(settings.get('ni_vis')), complex(settings.get('ni_ir')), np.radians(float(settings.get('theta1_vis'))), np.radians(float(settings.get('theta1_ir'))))
print('Fresnel factors calculated')
print(fresnel)

print('\nCombining different pro-dcd results and deriving trajectory with lowest RSS:')
lowest_RSS = {'value': np.infty, 'comb': 'none', 'out_string':'', 'RSSs':{pol_comb:np.infty for pol_comb in pol_combs}}
#Calculating SFG data in terms of polarisation combinations from lab frame components using scattering theory
sfg_pol_comb_path = work_dir+'/sfg_pol_combs'
if not os.path.exists(sfg_pol_comb_path):
    os.mkdir(sfg_pol_comb_path)
sfg_file = pro_calc_data.keys()
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
for sfg_file in pro_calc_data:
    out_string = sfg_file[:-4]
    sfg_data = pro_calc_data.get(sfg_file)
    if 'pol_comb_sfg'+sfg_file[:-4]+'.txt' not in os.listdir(path=sfg_pol_comb_path):
        print(f'Combining tensor elements for {sfg_file}',file=sys.__stdout__)
        sfg_data = {key:np.array(sfg_data[key]) for key in sfg_data} #Put data into numpy array for easy vectorized math

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
        np.savetxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file[:-4]+'.txt', sfg_pol_comb, fmt='%1.9f')
    else:
        sfg_pol_comb = np.loadtxt(sfg_pol_comb_path+'/pol_comb_sfg'+sfg_file[:-4]+'.txt')


#Normalization and reduce range to only include Amide-1 peak    
    calc_data = {'xvals':sfg_pol_comb[:,0], 
            'SSP': sfg_pol_comb[:,1],
            'PPP': sfg_pol_comb[:,2],
            'SPS': sfg_pol_comb[:,3],
            'PSS': sfg_pol_comb[:,4]}
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
        f.write('theta             psi          RSS    lowest_RSS\n')
        for comb,RSS in zip(combs,all_RSS):
            theta,phi,psi = visca.Get_dcd_theta_phi_psi(comb[0])
            if first_write_flag:
                f.write('%i             %i             %3.5f           %3.5f\n'%(theta,psi,RSS,lowest_RSS['value']))
                first_write_flag = False
            else:
                f.write('%i             %i             %3.5f\n'%(theta,psi,RSS))


#Cleaning procedure - deleting tracing dcd files and SFG files
if 'post_clean' not in settings.keys() or settings['post_clean']=='True':
    print('Post Cleaning Initialized:')

    print(f'Cleaning {dcd_dir}')
    os.system(f'rm {dcd_dir}/theta*.dcd')
    os.system(f'rm {dcd_dir}/theta*_SFG.txt')
    os.system(f'touch {dcd_dir}/this_folder_was_cleaned.txt')
    os.system(f'date >> {dcd_dir}/this_folder_was_cleaned.txt')
    os.system(f'printf "All .dcd and .txt files was removed from this folder after running {sys.argv[0]}" >> {dcd_dir}/this_folder_was_cleaned.txt')
    
    print(f'Cleaning {sfg_pol_comb_path}')
    os.system(f'rm {sfg_pol_comb_path}/pol_comb_sfg*.txt')
    os.system(f'touch {sfg_pol_comb_path}/this_folder_was_cleaned.txt')
    os.system(f'date >> {sfg_pol_comb_path}/this_folder_was_cleaned.txt')
    os.system(f'printf "All .txt files was removed from this folder after running {sys.argv[0]}" >> {sfg_pol_comb_path}/this_folder_was_cleaned.txt')




