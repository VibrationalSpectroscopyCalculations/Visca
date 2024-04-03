import sys, os
import visca_funcs as visca

#Read user inputs
settings_file = sys.argv[1]
settings  = visca.Read_settings(settings_file)

#Setting up work directories
cwd = os.getcwd()
work_dir = cwd+'/Working_Directory_'+settings['name']
plot_dir = '%s/plot/'%work_dir


ensemble_dcd_filenames = []
with open('./results/ensemble_Working_Directory_%s.dat'%settings['name']) as ensemble_file:
    for i,line in enumerate(ensemble_file):
        frame_i = int(line.split()[0])
        dcd_filename = 'f%ito%i.dcd'%(frame_i,frame_i+1)
        ensemble_dcd_filenames.append(dcd_filename)

#adding empty spaces and filepath
dcd_list = [' '+plot_dir+filename for filename in ensemble_dcd_filenames]
dcd_string = ''
for s in dcd_list:
    dcd_string += s

pdb_filename = plot_dir+settings['name']+'.pdb'
os.system(settings['vmd_command']+' '+pdb_filename+dcd_string)

print(dcd_string)

