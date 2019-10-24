#!/usr/bin/env python

import openpyxl as x
from os.path import basename

print('''
HOW TO USE:
(under construction)
''')

def get_sheetname(input_file, sheet_num=0):
    '''Load xlsx file and return pointer to
       appropriate sheet (first one by default)'''
    wb = x.load_workbook(filename=input_file)
    return wb[wb.sheetnames[sheet_num]]


def transform_values(sh, FROM=1, TO=783, ata='d', chi='c'):
    '''Take pointer to sheet and a range of cells
       and return a list of strings in the format

       atayal_chinese

       The ranges have to be given manually,
       but I have coded in defaults based on the
       files that I use at the time of writing.
    '''

    l = []

    for i in range(FROM,TO):
        a = sh[ata+str(i)].value
        c = sh[chi+str(i)].value
        if a is None or c is None:
            l.append('')
        else:
            a = a.replace('?','Q') \
                 .replace('!','E') \
                 .replace("'",'ʔ') \
                 .replace(';',',')
            l.append(a+'_'+c)

    return l

def write_to_file(output_file, l):

    f = open(output_file,'w')

    for item in l:
        f.write(item)
        f.write('\n')

    f.close()

def im_lazy(input_file):
    output_file = basename(input_file).replace('xlsx','txt')
    sheet = get_sheetname(input_file)
    data = transform_values(sheet)
    write_to_file(output_file, data)
    print('All operations finished!')

