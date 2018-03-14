import sys
import os
import subprocess

PAPIS_DIR = '/home/iwaka/Documents/papers/'


def main(filename):
    author_name = input('Enter author name'
                        '(e.g. "van der Smith, John A."):\n')
    year = input('Enter year of publication:\n')
    title = input('Enter title of book or article:\n')
    name_year_title = (author_name, year, title)

    new_name = create_file_name(name_year_title)
    rename_pdf(filename, new_name)

    papis_add(new_name, name_year_title)

    add_year(new_name, year)
    print('Added ' + new_name + '.pdf to papis library.')

    remove_original_file(new_name)


def create_file_name(name_year_title):
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
        return primary_title[:50].replace(' ', '_')
    else:
        return title.replace(' ', '_')


def rename_pdf(filename, new_name):
    os.rename(filename, new_name + '.pdf')


def papis_add(filename, name_year_title):
    author_name, year, title = name_year_title
    subprocess.run(['papis', 'add', filename + '.pdf',
                    '--author', author_name,
                    '--title', title,
                    '--name', filename])


def add_year(filename, year):
    info_file = open(PAPIS_DIR + filename + '/info.yaml', 'a')
    info_file.write("year: '" + year + "'")
    info_file.close()


def remove_original_file(filename):
    os.remove(filename + '.pdf')


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Please provide one file as argument.')
    elif not sys.argv[1].lower().endswith('.pdf'):
        print('Please only use pdf files.')
    else:
        main(sys.argv[1])
