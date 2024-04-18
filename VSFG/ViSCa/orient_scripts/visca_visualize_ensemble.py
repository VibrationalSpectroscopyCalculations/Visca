import sys, os
import visca_orient_funcs as visca

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
cwd = os.getcwd()
work_dir = cwd+'/Working_Directory_'+settings['name']+'/orient'
#plot_dir = '%s/plot/'%work_dir
dcd_dir = '%s/dcds/'%work_dir

ensemble_dcd_filenames = []
with open('./results/ensemble_orient_%s.dat'%settings['name']) as ensemble_file:
    for i,line in enumerate(ensemble_file):
        theta = int(line.split()[0])
        psi = int(line.split()[1])
        phi = 0
        dcd_filename = f'theta{theta}_phi{phi}_psi{psi}.dcd'
        ensemble_dcd_filenames.append(dcd_filename)

#adding empty spaces and filepath
dcd_list = [' '+dcd_dir+filename for filename in ensemble_dcd_filenames]
dcd_string = ''
for s in dcd_list:
    dcd_string += s

pdb_filename = dcd_dir+settings['name']+'.pdb'
os.system(settings['vmd_command']+' '+pdb_filename+dcd_string)

print('VMD visualization:', dcd_string)

