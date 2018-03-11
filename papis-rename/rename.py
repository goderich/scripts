import os
import yaml


def main():
    papis_dir = '/home/iwaka/Documents/papers/'
    for root, dirs, files in os.walk(papis_dir):
        if 'info.yaml' in files:
            info_file = open(root + '/info.yaml', 'r')
            metadata = yaml.load(info_file)
            new_name = create_new_name(metadata)

            dirpath = root[:root.rindex('/') + 1]

            try:
                os.rename(root, dirpath + new_name)
            except OSError as err:
                print(err)
                newname = input("Input new name: ")
                os.rename(root, dirpath + newname)


def create_new_name(metadata):
    author = get_authorname(metadata['author'])
    title = get_title(metadata['title'])

    try:
        year = get_year(metadata['year'], author)
    except KeyError:
        year = ''

    return author + year + title


def get_authorname(yaml_author):
    if yaml_author not in ['???????', 'Unknown']:
        author = yaml_author.split(',')[0]
        for char in ['{', '}', '_', '\\']:
            author = author.replace(char, '')
        author = author.replace(' ', '_') + '_'
        return author
    else:
        return ''


def get_title(yaml_title):
    title = yaml_title.replace('/', '-')
    for char in ['{', '}', '_', '\\']:
        title = title.replace(char, '')
    title = title.replace(' ', '_')
    if title.lower().endswith('.pdf'):
        title = title[:-4]
    return title


def get_year(yaml_year, author):
        if author != '':
            return yaml_year + '_'
        else:
            return ''


if __name__ == '__main__':
    main()
