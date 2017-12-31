import xlsxwriter
import re
import sys

file_path = sys.argv[1]

f = open(file_path, encoding='utf-16')

# Create a .xlsx file in same directory and with same name as original
workbook = xlsxwriter.Workbook(file_path[:-9] + '.xlsx')
worksheet = workbook.add_worksheet()

# Python magic
for line in f:
    tierRegex = re.search(r'item\s?\[(\d+)\]', line)
    textRegex = re.search(r'text\s?=\s?"(.*)"', line)
    if tierRegex:
        col = int(tierRegex.group(1)) - 1
        row = 0
    elif textRegex:
        worksheet.write(row, col, textRegex.group(1).replace('""', '"'))
        row += 1

f.close()
workbook.close()
