import argparse
import subprocess
import shlex # for quoting filenames before passing them to the shell
import os.path # for checking whether output file already exists

parser = argparse.ArgumentParser()
parser.add_argument('file', help='A .doc or .docx file to open')
parser.add_argument('-o', '--output',
                    help='Specify output file (must end with .txt)')
args = parser.parse_args()

class DocError(Exception):
    '''Errors for non-Word type files'''
    pass

class Doc:
    '''Class for Word documents'''
    def __init__(self, filename):
        self.name = shlex.quote(filename)
        if filename.endswith('.doc'):
            self.doctype = 'doc'
        elif filename.endswith('.docx'):
            self.doctype = 'docx'
        else:
            raise DocError

    def display_in_less(self):
        if self.doctype == 'doc':
            subprocess.run(['bash', '-c', 'antiword ' + self.name + ' | less'])
        else:
            subprocess.run(['bash', '-c', 'docx2txt ' + self.name + ' - | less'])

    def output_to_txt(self, output_file):
        '''Creates a .txt file with the contents of the Word document'''
        if not output_file.endswith('.txt'):
            print('Please give the output file a .txt extension')
        else:
            if os.path.isfile(output_file):
                s = input('File {} exists. Overwrite? (y/N): '.format(output_file))
                if not s or s.lower()[0] != 'y':
                    raise FileExistsError

            # print('Writing to file {}...'.format(output_file))
            if self.doctype == 'doc':
                subprocess.run(['bash', '-c', 'antiword ' + self.name \
                                + ' > ' + output_file])
            else:
                subprocess.run(['bash', '-c', 'docx2txt ' + self.name \
                                + ' ' + output_file])

if __name__ == '__main__':
    try:
        f = Doc(args.file)
        if args.output:
            try:
                f.output_to_txt(args.output)
            except FileExistsError:
                print('File already exists. Exiting...')
        else:
            f.display_in_less()
    except DocError:
        print('Please give only .doc or .docx files as arguments.')

