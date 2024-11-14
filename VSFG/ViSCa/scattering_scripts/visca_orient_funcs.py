import matplotlib.pyplot as plt
import numpy as np
from numpy import cos, sin, exp
from scipy import interpolate
from pathlib import Path
import os, sys

def Read_calc_SFG(filename):
    data = {}
    first_line = True
    titles = []
    with open(filename) as f:
        for line in f:
            line_list = line.split()

            if first_line: #Setting titles
                for title in line_list:
                    titles.append(title)
                    data[title] = []
                first_line = False

            else: #Reading data into dictionary
               for i,string_val in enumerate(line_list):
                    data[titles[i]].append(float(string_val))
    return data

def Plot_data(x_key,y_key,data):
    x_list = data[x_key]
    y_list = data[y_key]
    plt.plot(x_list, y_list,label=y_key)
    plt.xlabel(x_key)
##Testing calc reader and plotter
#data = Read_calc_SFG("comb_SFG.dat")
#Plot_data("Frequency","Abs(PSP)",data)
#Plot_data("Frequency","Abs(SSP)",data)
#Plot_data("Frequency","Abs(PPP)",data)
#Plot_data("Frequency","Abs(SPS)",data)
#plt.legend()
#plt.show()

def Read_exp_SFG(filename):
    data = []
    first_line = True
    with open(filename) as f:
        for line in f:
            line_list = line.split()
            if first_line:
                for string_val in line_list:
                    data.append([float(string_val)])
                first_line = False
            else:
                for i,string_val in enumerate(line_list):
                    data[i].append(float(string_val))
    return data

def Normalize(l,N=False):
    if not N:
        N = max(l)
    return [e/N for e in l]

def Reduce_range(x_i, x_f, xvals, l):
    assert len(xvals)==len(l), "Risky to Reduce range of list using xvals that is not same length - you are likely mismatcing x and y data"
    reduced_l = [val for i,val in enumerate(l) if x_i<xvals[i]<x_f]
    return reduced_l

def RSS(x_calc, y_calc, x_exp, y_exp, plot=False):
    spline_calc = interpolate.splrep(x_calc,y_calc,s=0) #Spline representation of calculated data. s=0 => no smoothing
    N = len(x_exp)
    if plot:
        x_interpol = np.linspace(x_calc[0],x_calc[-1],2000)
        y_interpol = interpolate.splev(x_interpol,spline_calc, der=0)
        plt.plot(x_interpol, y_interpol,label="spline",lw=4)
        plt.plot(x_calc, y_calc,label="calc")
        plt.legend()
        plt.show()
    def RS(x_e, y_e):
        return (y_e - interpolate.splev(x_e, spline_calc, der=0) )**2
    return sum([RS(x_e, y_e) for x_e,y_e in zip(x_exp,y_exp)]) /N

def Sort_dcd_names(l):
    keys = []
    for name in l:
        read_flag = False
        s = ''
        for c in name:
            if c=='f':
                read_flag = True
            elif c=='t':
                read_flag = False
            elif read_flag:
               s += c
        keys.append(int(s))
    return [x for _,x in sorted(zip(keys,l))]


def Get_dcd_i_and_f(name):
   s_i = ''
   s_f = ''
   for c in name:
       if c=='f':
           read_flag_i = True
       elif c=='t':
           read_flag_i = False
       elif read_flag_i:
          s_i += c
       elif c=='o':
           read_flag_f = True
       elif c=='.':
           read_flag_f = False
       elif read_flag_f:
          s_f += c
   return (int(s_i), int(s_f))

def Get_dcd_ax_and_ay(name):
   s_i = ''
   s_f = ''
   for c in name:
       if c=='x':
           read_flag_i = True
       elif c=='_':
           read_flag_i = False
       elif read_flag_i:
          s_i += c
       elif c=='y':
           read_flag_f = True
       elif c=='.':
           read_flag_f = False
       elif c=='S':
           read_flag_f = False
       elif read_flag_f:
          s_f += c
   return (int(s_i), int(s_f))

def Get_dcd_theta_phi_psi(name):
   name = name.replace('theta','').replace('phi','').replace('psi','').replace('.dcd','')
   vals = name.split('_')
   out = [int(val) for val in vals]
   return out

def Abs_squared_list(a,b):
    return [aa**2 + bb**2 for aa,bb in zip(a,b)]

def Read_settings(filename):
    settings = {'vmd_command':'vmd'}
    with open(filename) as f:
        for line in f:
            line = line.replace(" ","")
            line = line.replace("\n","")
            if line:
                try:
                    key,s = line.split("=")
                except:
                    print("Non-Read line found in input file")
                    continue
                if key[-8:]=="location":
                    s = Location_checker(s)
                settings[key] = s
    return settings

def Location_checker(s):
    home = str(Path.home())
    s = s.replace("Dropbox(AAU-Dropbox)","Dropbox (AAU-Dropbox)") #ugly save for the stupid space in dropbox dir name
    s = s.replace("~", home)
    s = s.replace("$HOME", home)
    if s[-1] != '/': #add the optional "/" at the end of pathnames
        s += '/'
    return s

def Read_RSS_file(filename):
    data = {}
    first_line = True
    titles = []
    with open(filename) as f:
        for line in f:
            line_list = line.split()

            if first_line: #Setting titles
                for title in line_list:
                    titles.append(title)
                    data[title] = []
                first_line = False

            else: #Reading data into dictionary
               for i,string_val in enumerate(line_list):
                   data[titles[i]].append(float(string_val))
    return data

def Read_pol_comb_file(filename, Normalize_wrt, pol_combs=['SSP','PPP','SPS','PSS','PSP','SPP','PPS']):
    sfg_pol_comb = np.loadtxt(filename).T
    calc_data = {'xvals':sfg_pol_comb[0],
		 'wavenumber':sfg_pol_comb[0],
                 'SSP': sfg_pol_comb[1],
                 'PPP': sfg_pol_comb[2],
                 'SPS': sfg_pol_comb[3],
                 'PSS': sfg_pol_comb[4],
                 'PSP': sfg_pol_comb[5],
                 'SPP': sfg_pol_comb[6],
                 'PPS': sfg_pol_comb[7]}
    #Normalizing spectra
    if Normalize_wrt:
        N_calc = max(calc_data[Normalize_wrt])
        calcs = {pol_comb: Normalize(calc_data[pol_comb],N_calc) for pol_comb in pol_combs}
        calcs['xvals'] = calc_data['xvals']
        calcs['wavenumber'] = calc_data['xvals']
        return calcs
    else:
        return calc_data


### Functions added 24th Jan 2022     ###
### To do rotation i theta,pih angles ###
### Tested in: "rotation_script.py"   ###

def unit_vector(vector):
    """ Returns the unit vector of the vector.  """
    return vector / np.linalg.norm(vector)

def angle_between(v1, v2):
    """ Returns the angle in radians between vectors 'v1' and 'v2'::

            >>> angle_between((1, 0, 0), (0, 1, 0))
            1.5707963267948966
            >>> angle_between((1, 0, 0), (1, 0, 0))
            0.0
            >>> angle_between((1, 0, 0), (-1, 0, 0))
            3.141592653589793
    """
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
#    return np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))
    return (np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))

def rad2degree(l):
    if isinstance(l,list) or isinstance(l,tuple):
        return [a*180/np.pi for a in l]
    else:
        return l*180/np.pi

def degree2rad(l):
    if isinstance(l,list) or isinstance(l,tuple):
        return [a*np.pi/180 for a in l]
    else: 
        return l*np.pi/180

def rot_around_y(v,a):
    return [v[0]*np.cos(a) + v[2]*np.sin(a), v[1], -v[0]*np.sin(a) + v[2]*np.cos(a)] #rotated vector v angle a around y-axis

def rot_around_x(v,a):
    return [v[0], v[1]*np.cos(a) - v[2]*np.sin(a), v[1]*np.sin(a) + v[2]*np.cos(a)] #rotated around x-axis

def rot_around_z(v,a):
    return [v[0]*np.cos(a) - v[1]*np.sin(a), v[0]*np.sin(a) + v[1]*np.cos(a), v[2]] #rotated vector v a radians around z-axis

def z_to_xy(z):
    zx_hat = [0, z[1], z[2]] #projection of z onto the yz-plane
    zy_hat = [z[0], 0, z[2]] #projection of z onto the xz-plane
    e_Z = [0,0,1]
    y = -np.arccos(angle_between(e_Z, zy_hat)) #rotation angle around y-axis
    z_p = rot_around_y(z,y)
    x = np.arccos(angle_between(z_p, e_Z))
    return rad2degree((x,y))

def thetaphi_to_z(theta, phi): #takes angles in degrees
    theta,phi = degree2rad(theta),degree2rad(phi)
    z = [0,0,1]
    zp = rot_around_y(z,theta)
    return rot_around_z(zp,phi)

def plot_vector(v):
    plt.plot([0,v[0]],[0,v[1]],[0,v[2]])
    plt.plot([v[0]],[v[1]],[v[2]],'.k')

def thetaphi_to_xy(theta,phi):
    return z_to_xy(thetaphi_to_z(theta, phi))


def fresnel_factors(lambda_vis, omega_ir, n1_sfg, n1_vis, n1_ir, n2_sfg, n2_vis, n2_ir, ni_sfg, ni_vis, ni_ir, theta1_vis, theta1_ir):
      #calculate interfacial refractive indices
      #convert visible wavelength to frequency in cm-1
      omega_vis = 1e7 / lambda_vis
      #calculate sfg frequency in cm-1
      omega_sfg = omega_vis + omega_ir
      #calculate theta1 for sfg light
      theta1_sfg = np.arcsin((n1_vis * omega_vis * np.sin(theta1_vis) + n1_ir * omega_ir * np.sin(theta1_ir))/(n1_sfg * omega_sfg))
      #calculate theta2 for all wavelengths based on Snell's law
      theta2_sfg = np.arcsin((n1_sfg * np.sin(theta1_sfg))/n2_sfg)
      theta2_vis = np.arcsin((n1_vis * np.sin(theta1_vis))/n2_vis)
      theta2_ir = np.arcsin((n1_ir * np.sin(theta1_ir))/n2_ir)

      #calculate fresnel factors
      Lxx_sfg = (2 * n1_sfg * np.cos(theta2_sfg)) / (n1_sfg * np.cos(theta2_sfg) + n2_sfg * np.cos(theta1_sfg))
      Lxx_vis = (2 * n1_vis * np.cos(theta2_vis)) / (n1_vis * np.cos(theta2_vis) + n2_vis * np.cos(theta1_vis))
      Lxx_ir = (2 * n1_ir * np.cos(theta2_ir)) / (n1_ir * np.cos(theta2_ir) + n2_ir * np.cos(theta1_ir))

      Lyy_sfg = (2 * n1_sfg * np.cos(theta1_sfg)) / (n1_sfg * np.cos(theta1_sfg) + n2_sfg * np.cos(theta2_sfg))
      Lyy_vis = (2 * n1_vis * np.cos(theta1_vis)) / (n1_vis * np.cos(theta1_vis) + n2_vis * np.cos(theta2_vis))
      Lyy_ir = (2 * n1_ir * np.cos(theta1_ir)) / (n1_ir * np.cos(theta1_ir) + n2_ir * np.cos(theta2_ir))

      Lzz_sfg = ((2 * n2_sfg * np.cos(theta1_sfg)) / (n1_sfg * np.cos(theta2_sfg) + n2_sfg * np.cos(theta1_sfg))) * (n1_sfg / ni_sfg)**2
      Lzz_vis = ((2 * n2_vis * np.cos(theta1_vis)) / (n1_vis * np.cos(theta2_vis) + n2_vis * np.cos(theta1_vis))) * (n1_vis / ni_vis)**2
      Lzz_ir = ((2 * n2_ir * np.cos(theta1_ir)) / (n1_ir * np.cos(theta2_ir) + n2_ir * np.cos(theta1_ir))) * (n1_ir / ni_ir)**2

      #calculate local field coefficients (note some of these have will have a negative sign, but this is implemented when the functions are called later with the data)
      F_YYZ = (Lyy_sfg * Lyy_vis * Lzz_ir * np.sin(theta1_ir))
      F_YZY = (Lyy_sfg * Lzz_vis * Lyy_ir * np.sin(theta1_vis))
      F_ZYY = (Lzz_sfg * Lyy_vis * Lyy_ir * np.sin(theta1_sfg))
      F_XXZ = (Lxx_sfg * Lxx_vis * Lzz_ir * np.cos(theta1_sfg) * np.cos(theta1_vis) * np.sin(theta1_ir))
      F_XZX = (Lxx_sfg * Lzz_vis * Lxx_ir * np.cos(theta1_sfg) * np.sin(theta1_vis) * np.cos(theta1_ir))
      F_ZXX = (Lzz_sfg * Lxx_vis * Lxx_ir * np.sin(theta1_sfg) * np.cos(theta1_vis) * np.cos(theta1_ir))
      F_ZZZ = (Lzz_sfg * Lzz_vis * Lzz_ir * np.sin(theta1_sfg) * np.sin(theta1_vis) * np.sin(theta1_ir))
      F_ZYX = (Lzz_sfg * Lyy_vis * Lxx_ir * np.sin(theta1_sfg) * np.cos(theta1_ir))
      F_XYZ = (Lxx_sfg * Lyy_vis * Lzz_ir * np.cos(theta1_sfg) * np.sin(theta1_ir))
      F_YZX = (Lyy_sfg * Lzz_vis * Lxx_ir * np.sin(theta1_vis) * np.cos(theta1_ir))
      F_YXZ = (Lyy_sfg * Lxx_vis * Lzz_ir * np.cos(theta1_vis) * np.sin(theta1_ir))
      F_ZXY = (Lzz_sfg * Lxx_vis * Lyy_ir * np.sin(theta1_sfg) * np.cos(theta1_vis))
      F_XZY = (Lxx_sfg * Lzz_vis * Lyy_ir * np.cos(theta1_sfg) * np.sin(theta1_vis))
      return(F_YYZ, F_YZY, F_ZYY, F_XXZ, F_XZX, F_ZXX, F_ZZZ, F_ZYX, F_XYZ, F_YZX, F_YXZ, F_ZXY, F_XZY)

### Functions added 16th Apr 2024        ###
### Copied from select visca_funcs.py    ###
### To do plotting with separate scripts ###
### by Kris Strunge                      ###

def Check_pdbfile(pdb_filename):
    '''Function to check pdb formatting. Speciffically if atom coordinates end with TER flag'''
    TER_flag = False
    ATOM_flag = False
    with open(pdb_filename) as pdbfile:
        for line in pdbfile:
            line_list = line.split()
            if len(line_list)<1: #skip empty lines
                continue
            if line_list[0] == 'ATOM': # Identify ATOM section
                ATOM_flag = True
            if ATOM_flag and line_list[0]!='ATOM': #Check that ATOM section ends with TER
                if line_list[0]=='TER':
                    return True
                else:
                    print(f'Warning: Missing "TER" flag in pdbfile {pdb_filename} - trying to autocorrect')
                    return False
        if len(line_list)>0 and line_list[0]=='ATOM': #file ended with unfinished ATOM section: adding TER
            print(f'Warning: Missing "TER" line at the end of pdbfile {pdb_filename} - trying to autocorrect')
            return False
    print(f'Warning: Major error in pdb format - Check your pdbfile {pdb_filename}')
    return False
                
def Fix_pdbfile_TER(pdb_filename):
    print('Fixing pdbfile by adding TER flag to the end of coordinates')
    pdb_backup_filename = pdb_filename+'.backup'
    print(f'Moving pdbfile to {pdb_backup_filename}')
    os.system(f'mv {pdb_filename} {pdb_backup_filename}')
    
    ATOM_flag = False
    TER_flag = False
    with open(pdb_backup_filename) as pdbfile_old:
        with open(pdb_filename,'w') as pdbfile:
            for line in pdbfile_old:
                line_list = line.split()
                pdbfile.write(line)
                if line_list[0]=='ATOM':
                    ATOM_flag = True
                if ATOM_flag and line_list[0]!='ATOM':
                    ATOM_flag = False
                    pdbfile.write('TER\n')
                    TER_flag =  True
            if len(line_list)>0 and line_list[0]=='ATOM': #file ended with unfinished ATOM section: adding TER
                ATOM_flag = False
                pdbfile.write('TER\n')
                TER_flag = True
    return TER_flag

def Check_pdbfile_hydrogen(pdb_filename, out_log):
    N_atoms = 0
    N_hydrogens = 0
    N_carbons = 0
    N_oxygens = 0
    N_nitrogens = 0
    with open(pdb_filename) as pdbfile:
        for line in pdbfile:
            line_list = line.split()
            if len(line_list)<1: #skip empty lines
                continue
            if line_list[0]=='ATOM':
                N_atoms += 1
                atom_label = line_list[2][0]
                if atom_label=='H':
                    N_hydrogens += 1
                if atom_label=='C':
                    N_carbons += 1
                if atom_label=='O':
                    N_oxygens += 1
                if atom_label=='N':
                    N_nitrogens += 1
    N_unknown = N_atoms - N_hydrogens - N_carbons - N_oxygens - N_nitrogens
#    with open(out_log, 'a') as logfile:
#        logfile.write(f'\nPDB file check found:\n   {N_carbons} carbon atoms\n   {N_hydrogens} hydrogen atoms\n   {N_oxygens} oxygen atoms\n   {N_nitrogens} nitrogen atoms\n   {N_unknown} other atoms\n')   
    print(f'\nPDB file check found:\n   {N_carbons} carbon atoms\n   {N_hydrogens} hydrogen atoms\n   {N_oxygens} oxygen atoms\n   {N_nitrogens} nitrogen atoms\n   {N_unknown} other atoms\n')   
    if N_hydrogens/N_atoms < 0.05 or N_hydrogens < 10:
        print(f'Error - Too few hydrogens found in pdb file: "{pdb_filename}"',file=sys.__stdout__)
        print('Continue simulation despite .pdb file having suspiciously few hydrogen atoms? (y/n)',file=sys.__stdout__)
        if '-autoyes' in sys.argv or input('Continue simulation despite .pdb file having suspiciously few hydrogen atoms? (y/n)y\n')!='y':
            sys.exit()
    return True



def Write_plot(settings_file, theta, psi):
    phi = 0
    settings = Read_settings(settings_file)
    #Setting up work directories
    main_dir = os.getcwd()
    working_dir = main_dir+'/Working_Directory_'+settings['name']+'/orient'
    plot_dir = working_dir +'/plot'
    
    os.system('mkdir -p %s'%plot_dir)
    os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], plot_dir))
    
    fortran_script = settings['fortran_script_location']+settings['fortran_script']
    
    os.chdir(plot_dir)
    command = settings['vmd_command']+' -dispdev text -e "'+settings['tcl_scripts_location']+'rotator_theta_phi_psi.tcl" -args "%s.pdb" %i %i %i > ../VMD_log.txt 2> ../VMD_log.txt'%(settings['name'], int(theta), int(phi), int(psi))
    os.system(command)
    
    split_dcd = 'f'+theta+'to'+psi
    command = 'time "'+fortran_script+'" -pdb '+settings['name']+'.pdb -spec_min 1500 -spec_max 1800 -dcd '+split_dcd+'.dcd -Ham -IR -Raman -SFG -inhom 0 -width %s -charge 1 -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH %s -Omega0 %s > fortran_log.txt'%(settings['width'],settings['mOH'],settings['Omega0'])
    os.system(command)
#    sfg_data = np.loadtxt(working_dir+f'/sfg_pol_combs/pol_comb_sfgtheta{theta}_phi0_psi{psi}.txt')
#    split_dcd = f'pol_comb_sfgtheta{theta}_phi0_psi{psi}'
    calc_data = Read_pol_comb_file(working_dir+f'/sfg_pol_combs/pol_comb_sfgtheta{theta}_phi0_psi{psi}.txt', False) # {'xvals':sfg_data[:,0], 'SSP': sfg_data[:,1], 'PPP': sfg_data[:,2], 'SPS': sfg_data[:,3], 'PSS': sfg_data[:,4]}
    os.chdir('../../../')
    print('Calculating Fresnel factors...')
    
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
#    print('Polarization Combinations found in input file:')
#    print(pol_combs,'\n')
    
    #Experimental data import
    xvals_file = settings['exp_data_location']+settings['xvals']
    exp_files = [settings['exp_data_location']+settings[pol_comb] for pol_comb in pol_combs]
    print('Reading experimental data from:')
    print('xvals:',xvals_file)
    
    for i,exp_file in enumerate(exp_files):
        print(pol_combs[i],':',exp_file)
    print()
    #raw_intensities = [Read_exp_SFG(f)[0] for f in exp_files]
    #raw_xvals = Read_exp_SFG(xvals_file)[0]
    exp_data_raw = {pol_comb: Read_exp_SFG(f)[0] for pol_comb,f in zip(pol_combs,exp_files)}
    exp_data_raw['xvals'] = Read_exp_SFG(xvals_file)[0]
    
    RSS_range_i = int(settings['RSS_range_i'])
    RSS_range_f = int(settings['RSS_range_f'])
    exp_data_reduced_range = {pol_comb: Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw[pol_comb]) for pol_comb in pol_combs}
    exp_data_reduced_range['xvals'] = Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw['xvals'])
    exp_data_reduced_range_plot = {pol_comb: Reduce_range(1500, 1800, exp_data_raw['xvals'],exp_data_raw[pol_comb]) for pol_comb in pol_combs}
    exp_data_reduced_range_plot['xvals'] = Reduce_range(1500, 1800, exp_data_raw['xvals'], exp_data_raw['xvals'])
    
    #Experimental Normalization constant
    N = max(exp_data_reduced_range[settings['Normalize_wrt']]) #Normalization constant
#    print('Experimental Normalization Constant = %3.6f'%N)
    
    #Experimental Baseline correction
    for pol_comb in pol_combs: #Baseline correction loop
        if '%s_bg'%pol_comb in settings.keys():
 #           print('baseline correction of %s with %s from user input'%(pol_comb,settings['%s_bg'%pol_comb]))
            correction = float(settings['%s_bg'%pol_comb])*N
            exp_data_reduced_range[pol_comb] = [val-correction for val in exp_data_reduced_range[pol_comb]]
            exp_data_reduced_range_plot[pol_comb] = [val-correction for val in exp_data_reduced_range_plot[pol_comb]]
    
    #Correction factor for imperfect half-wave plate
    for pol_comb in pol_combs: #Baseline correction loop
        if '%s_factor'%pol_comb in settings.keys():
#            print('scaling %s with %s from user input'%(pol_comb,settings['%s_factor'%pol_comb]))
            factor = float(settings['%s_factor'%pol_comb])
            exp_data_reduced_range[pol_comb] = [val*factor for val in exp_data_reduced_range[pol_comb]]
            exp_data_reduced_range_plot[pol_comb] = [val*factor for val in exp_data_reduced_range_plot[pol_comb]]
    
    #Normalization
    N = max(exp_data_reduced_range[settings['Normalize_wrt']])/1.0 #Normalization constant
    exp_data = {pol_comb: Normalize(exp_data_reduced_range[pol_comb],N) for pol_comb in pol_combs}
    exp_data_plot = {pol_comb: Normalize(exp_data_reduced_range_plot[pol_comb],N) for pol_comb in pol_combs}
    
    #Normalization
    N_calc = max(calc_data[settings['Normalize_wrt']])
    calcs = {pol_comb: Normalize(calc_data[pol_comb],N_calc) for pol_comb in pol_combs}
    
    
    #Print all RSS value to RSS file if specified
    spec_file = plot_dir+f'/f{theta}to{psi}.txt'
    print('\nprinting spectrum to %s'%spec_file)
    with open(spec_file,'w') as f:
        header = ['wavenumber']+pol_combs
        header = '         '.join(header)
        f.write(header+'\n')
        for i in range(len(calc_data['xvals'])):
            s = '   '.join(['%1.3e'%(calcs[pol_comb][i]) for pol_comb in pol_combs])
            f.write('%7.0f      '%(calc_data['xvals'][i]) + s +'\n' )

def Write_exp(settings_file):
    settings  = Read_settings(settings_file)
    main_dir = os.getcwd()
    work_dir = 'Working_Directory_'+settings['name']+'/orient'
    plot_dir = main_dir+'/'+work_dir+'/plot'
    RSS_file = '%s/%s'%(work_dir,settings['RSS_file'])
    os.system('mkdir %s -p'%plot_dir)
    RSS_data = Read_RSS_file(RSS_file)
    if 'RSS_cutoff' in settings.keys():
        RSS_cutoff = float(settings['RSS_cutoff'])
        print('RSS_cutoff read from inputfile: %2.3f'%RSS_cutoff)
    else:
        RSS_cutoff = 2*RSS_data['lowest_RSS'][0]
        print('Using default RSS_cutoff: %2.2f'%RSS_cutoff)
    
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
    #raw_intensities = [Read_exp_SFG(f)[0] for f in exp_files]
    #raw_xvals = Read_exp_SFG(xvals_file)[0]
    exp_data_raw = {pol_comb: Read_exp_SFG(f)[0] for pol_comb,f in zip(pol_combs,exp_files)}
    exp_data_raw['xvals'] = Read_exp_SFG(xvals_file)[0]
    
    RSS_range_i = int(settings['RSS_range_i'])
    RSS_range_f = int(settings['RSS_range_f'])
    exp_data_reduced_range = {pol_comb: Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw[pol_comb]) for pol_comb in pol_combs}
    exp_data_reduced_range['xvals'] = Reduce_range(RSS_range_i, RSS_range_f, exp_data_raw['xvals'], exp_data_raw['xvals'])
    exp_data_reduced_range_plot = {pol_comb: Reduce_range(1500, 1800, exp_data_raw['xvals'],exp_data_raw[pol_comb]) for pol_comb in pol_combs}
    exp_data_reduced_range_plot['xvals'] = Reduce_range(1500, 1800, exp_data_raw['xvals'], exp_data_raw['xvals'])
    
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
    exp_data = {pol_comb: Normalize(exp_data_reduced_range[pol_comb],N) for pol_comb in pol_combs}
    exp_data_plot = {pol_comb: Normalize(exp_data_reduced_range_plot[pol_comb],N) for pol_comb in pol_combs}
    
    
    #Print all exp data to "exp_spec.txt"
    spec_file = plot_dir+'/exp_spec.txt'
    print('\nprinting spectrum to %s'%spec_file)
    with open(spec_file,'w') as f:
        header = ['wavenumber']+pol_combs
        header = '         '.join(header)
        f.write(header+'\n')
        for i in range(len(exp_data_reduced_range_plot['xvals'])):
            s = '   '.join(['%1.3e'%(exp_data_plot[pol_comb][i]) for pol_comb in pol_combs])
            f.write('%7.2f      '%(exp_data_reduced_range_plot['xvals'][i]) + s +'\n' )
            f.write('%7.2f      '%(exp_data_reduced_range_plot['xvals'][i]) + s + '\n')#, exp_data_plot[pol_combs[0]][i], exp_data_plot[pol_combs[1]][i], exp_data_plot[pol_combs[2]][i], exp_data_plot[pol_combs[3]][i]))

### Functions added 1st Jul 2024            ###
### For splitting pdbs into multiple chains ###
### by Kris Strunge                         ###

def first_chain_name(pdbfile):
    with open(pdbfile) as f:
        for line in f:
            line_list = line.split()
            if line_list[0] == 'ATOM':
                return line_list[4]
    print('Error - could not find first chain name - Terminating')
    sys.exit()

def pdb_chain_splitter(pdb_location, name):
    pdbfile = pdb_location + name + '.pdb'

#    #Read full pdb and define splits at every "TER"
#    chains = [[]]
#    chainid = 0
#    with open(pdbfile) as f:
#        for line in f:
#            line_list = line.split()
#            if line_list[0] == 'TER':
#                chains[chainid].append(line)
#                chainid = chainid + 1
#                chains.append([])
#            elif line_list[0] =='ATOM':
#                chains[chainid].append(line)
#    chains = chains[:-1] #removes empty array at the end of chains so it doesn't get its own .pdb file

    #Read full pdb and define splits at every change in chain name
    chains = [[]]
    chainid = 0
    chain_name = first_chain_name(pdbfile)
    with open(pdbfile) as f:
        for line in f:
            line_list = line.split()
            if line_list[0] == 'ATOM' and line_list[4]!=chain_name:
                chain_name = line_list[4]
                #print('new chain name',chain_name)
                chains[chainid].append('TER')
                chainid = chainid + 1
                chains.append([])
            if line_list[0] =='ATOM':
                chains[chainid].append(line)
    # Finish last pdb file
    chains[chainid].append('TER')
    chainid = chainid + 1
    
    #Save atom coordinates of each chain into numbered pdb files ("1.pdb", "2.pdb", "3.pdb", etc.)
    for i in range(chainid):
        print(f'Writing "{str(i+1)}.pdb" single chain pdb with {len(chains[i])} atoms')
        with open(pdb_location+str(i+1)+'.pdb','w') as f:
            for l in range(len(chains[i])):
                    f.write(chains[i][l])
    return chainid


### Functions added 4th Jul 2024                 ###
### For Writing combined SFG file for multichain ###
### by Kris Strunge                              ###

def Write_SFG_file(data_dict, filename):
    headers_l = list(data_dict.keys())
    headers_s = ' '.join(headers_l)
    with open(filename, 'w') as f:
        f.write(headers_s+'\n')
        for i in range(len(data_dict[headers_l[0]])): # for each index in data arrays
            for header in headers_l:
                f.write(f'{data_dict[header][i]:2.6f}  ')
            f.write('\n')
#    data_np = np.array([np.array(data_dict[header]) for header in headers_l])
#    np.savetxt(filename, data_np, header=headers_s)
### Functions added 31st Jul 2024        ###
### Copied from Make_my_funcs.py (old)   ###
### To use scattering field factors      ###
### by Khezar Saeed                      ###
def foward_interface(n1, n2, n3, theta1):
    theta2 = np.arcsin(n1*np.sin(theta1)/n2)
    return np.arcsin(n2*np.sin(theta2)/n3)

def q(k0,scat):
    return 4*k0*sin(scat)

def A(R,k0, scat):
    return ((6*1j/(q(k0,scat)**4*R**2))*(
        2*(1-(q(k0,scat)**2*R**2)/3)*sin(q(k0,scat)*R)-2*q(k0,scat)*R*
        cos(q(k0,scat)*R)))


def B(R,k0,scat):
    return (6*1j/(q(k0,scat)**4*R**2))*(
        (q(k0,scat)**2*R**2-2)*sin(q(k0,scat)*R)-
        q(k0,scat)*R*((q(k0,scat)**2*R**2)/3-2)*cos(q(k0,scat)*R))


# Here r is perpendicular and p is parallel
def Grrr(Xrrr, Xrpp, Xprp, Xppr,R,k0,scat):
    return 2*np.pi*(B(R,k0,scat)*Xrrr+A(R,k0, scat)*
                    (Xrpp+Xprp+Xppr))

def Grpp(Xrrr, Xrpp, Xprp, Xppr,R,k0,scat):
    return np.pi*(A(R,k0, scat)*Xrrr+
                  (A(R,k0, scat)+2*B(R,k0, scat))*Xrpp-
                  A(R,k0, scat)*(Xprp+Xppr))

def Gprp(Xrrr, Xrpp, Xprp, Xppr,R,k0,scat):
    return np.pi*(A(R,k0, scat)*(Xrrr-Xrpp)+
                  (A(R,k0, scat)+2*B(R,k0, scat))*Xprp-A(R,k0, scat)*Xppr)

def Gppr(Xrrr, Xrpp, Xprp, Xppr,R,k0,scat):
    return np.pi*(A(R,k0, scat)*(Xrrr-Xrpp)-
                  A(R,k0, scat)*Xprp+
                  (A(R,k0, scat)+2*B(R,k0, scat))*Xppr)

