<<<<<<< HEAD

=======
>>>>>>> b9fc894055b4175331f1a6449e41ca215e87f359
import os, sys
import matplotlib.pyplot as plt
import numpy as np
import visca_orient_funcs as visca

#Read user inputs
settings_file = sys.argv[1]
settings = visca.Read_settings(settings_file)

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
try:
    work_dir = 'Working_Directory_'+settings['name']+'/Raman'
except KeyError:
    print('Error in input file -- Aborting')
    sys.exit()
os.system('mkdir -p %s'%work_dir)
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
os.system('cp "%s%s.pdb" %s'%(settings['pdb_location'], settings['name'], work_dir))
os.chdir(work_dir)

fortran_script = settings['fortran_script_location']+settings['fortran_script']

command = 'time "'+fortran_script+'" -pdb '+settings['name']+'.pdb -spec_min 1500 -spec_max 1800 -Raman -inhom 0 -width %s -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH %s -Omega0 %s > fortran_log.txt 2> /dev/null'%(settings['width'],settings['mOH'],settings['Omega0'])
print('Fortran command:',command)
os.system(command)

Raman = np.loadtxt(settings['name']+'_Raman.txt')
wavenumber = Raman[:,0]
raman_intensity = Raman[:,1]
norm_raman_intensity = raman_intensity / max(raman_intensity)
plt.plot(wavenumber,norm_raman_intensity)
fs = 12
font = {'family' : 'Arial',
        'weight' : 'normal',
        'size'   : fs}

plt.rc('font', **font)
plt.xticks(fontsize=fs)
plt.yticks(fontsize=fs)

plt.title(settings['name'][0:4]+" Raman spectrum")
plt.xlabel("Frequency",fontsize=fs)
plt.ylabel("Normalised Raman Intensity",fontsize=fs)
plt.xlim([1500, 1800])
plt.ylim([-0.1, 1.1])
plt.tight_layout(pad=1)
plt.savefig(settings['name']+'_Raman.pdf',dpi=400)
<<<<<<< HEAD
plt.show()
=======
plt.show()
>>>>>>> b9fc894055b4175331f1a6449e41ca215e87f359
