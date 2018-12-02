import sys
import os
import subprocess
import yaml

PAPIS_DIR = '/home/iwaka/Documents/papers/'


def main(filename):
    author_name = input('Enter author name '
                        '(e.g. "van der Smith, John A."):\n> ')
    year = input('Enter year of publication:\n> ')
    title = input('Enter title of document:\n> ')
    name_year_title = (author_name, year, title)

    type_abbrev = input('Is it an (a)rticle, (b)ook, book (c)hapter, '
                        '(p)roceedings paper, ab(s)tract, (m)aster thesis, '
                        'PhD (d)issertation, p(r)esentation, or (o)ther? '
                        '(default article)\n> ')

    bibtex_type = get_bibtex_type(type_abbrev)

    new_dir_name = create_dir_name(name_year_title)
    new_file_name = new_dir_name + '.pdf'
    rename_pdf(filename, new_file_name)

    papis_add(new_file_name, name_year_title)

    info_yaml = open(PAPIS_DIR + new_dir_name + '/info.yaml', 'r')
    yaml_data = yaml.safe_load(info_yaml)
    info_yaml.close()

    # do stuff with YAML file
    yaml_data['year'] = year
    yaml_data['type'] = bibtex_type

    info_yaml_write = open(PAPIS_DIR + new_dir_name + '/info.yaml', 'w')
    yaml.dump(yaml_data, info_yaml_write)
    info_yaml_write.close()

    print('Added ' + new_file_name + ' to papis library.')

    remove_original_file(new_file_name)


def create_dir_name(name_year_title: (str, str, str)) -> str:
    '''
    last_name and formatted_year are either empty strings,
    or end with an underscore for name formatting
    '''
    author_name, year, title = name_year_title

    last_name = get_author_last_name(author_name)
    formatted_year = get_formatted_year(year)
    formatted_title = get_formatted_title(title)

    return last_name + formatted_year + formatted_title


def get_author_last_name(author_name: str) -> str:
    '''Any spaces are replaced with _ for directory naming'''
    if author_name != '':
        last_name = author_name.split(',')[0]
        last_name = last_name.replace(' ', '_')
        return last_name + '_'
    else:
        return ''


def get_formatted_year(year: str) -> str:
    if year != '':
        return year + '_'
    else:
        return ''


def get_formatted_title(title: str) -> str:
    if len(title) > 50:
        primary_title = title.split(':')[0]
        return strip_punctuation_and_spaces(primary_title)
    else:
        return strip_punctuation_and_spaces(title)


def get_bibtex_type(letter: str) -> str:
    lowercase_letter = letter.lower()
    bibtex_types = {
        'a': 'article',
        '':  'article',
        'b': 'book',
        'c': 'incollection',
        'p': 'inproceedings',
        's': 'abstract',
        'r': 'presentation',
        'm': 'mastersthesis',
        'd': 'phdthesis',
        }

    try:
        return bibtex_types[lowercase_letter]
    except KeyError:
        return ''


def strip_punctuation_and_spaces(string: str) -> str:
    return ''.join(x for x in string
                   if x.isalnum() or x.isspace
                   ).replace(' ', '_')


def rename_pdf(filename, new_name) -> None:
    os.rename(filename, new_name)


def papis_add(filename, name_year_title) -> None:
    author_name, year, title = name_year_title
    dir_name = filename[:-4]
    subprocess.run(['papis', 'add', filename,
                    '--author', author_name,
                    '--title', title,
                    '--name', dir_name])


def remove_original_file(filename) -> None:
    os.remove(filename)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Please provide one file as argument.')
    elif not sys.argv[1].lower().endswith('.pdf'):
        print('Please only use pdf files.')
    else:
        main(sys.argv[1])
