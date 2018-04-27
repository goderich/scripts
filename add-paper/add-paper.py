import sys
import os
import subprocess

PAPIS_DIR = '/home/iwaka/Documents/papers/'


def main(filename):
    author_name = input('Enter author name'
                        '(e.g. "van der Smith, John A."):\n> ')
    year = input('Enter year of publication:\n> ')
    title = input('Enter title of book or article:\n> ')
    name_year_title = (author_name, year, title)

    new_dir_name = create_dir_name(name_year_title)
    new_file_name = new_dir_name + '.pdf'
    rename_pdf(filename, new_file_name)

    papis_add(new_file_name, name_year_title)

    add_year_to_yaml(new_dir_name, year)
    print('Added ' + new_file_name + ' to papis library.')

    remove_original_file(new_file_name)


def create_dir_name(name_year_title):
    '''
    last_name and formatted_year are either empty strings,
    or end with an underscore for name formatting
    '''
    author_name, year, title = name_year_title

    last_name = get_author_last_name(author_name)
    formatted_year = get_formatted_year(year)
    formatted_title = get_formatted_title(title)

    return last_name + formatted_year + formatted_title


def get_author_last_name(author_name):
    if author_name != '':
        last_name = author_name.split(',')[0]
        last_name = last_name.replace(' ', '_')
        return last_name + '_'
    else:
        return ''


def get_formatted_year(year):
    if year != '':
        return year + '_'
    else:
        return ''


def get_formatted_title(title):
    if len(title) > 50:
        primary_title = title.split(':')[0]
        return strip_punctuation_and_spaces(primary_title)
    else:
        return strip_punctuation_and_spaces(title)


def strip_punctuation_and_spaces(string):
    return ''.join(x for x in string
                   if x.isalnum() or x.isspace
                   ).replace(' ', '_')


def rename_pdf(filename, new_name):
    os.rename(filename, new_name)


def papis_add(filename, name_year_title):
    author_name, year, title = name_year_title
    dir_name = filename[:-4]
    subprocess.run(['papis', 'add', filename,
                    '--author', author_name,
                    '--title', title,
                    '--name', dir_name])


def add_year_to_yaml(dir_name, year):
    info_file = open(PAPIS_DIR + dir_name + '/info.yaml', 'a')
    info_file.write("year: '" + year + "'")
    info_file.close()


def remove_original_file(filename):
    os.remove(filename)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Please provide one file as argument.')
    elif not sys.argv[1].lower().endswith('.pdf'):
        print('Please only use pdf files.')
    else:
        main(sys.argv[1])
