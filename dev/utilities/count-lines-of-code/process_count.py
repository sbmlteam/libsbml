#!/usr/bin/env python

from xml.dom.minidom import *
import sys, os


def get_value(node, name):
    temp = node.getAttributeNode(name)
    if temp is None:
        return None
    return temp.nodeValue


def get_adjusted_name(node):
    name = get_value(node, 'name')
    if name == 'Bourne Shell':
        adjusted_name = '.sh/.bat'
    elif name == 'HTML':
        adjusted_name = 'Doc scripts'
    elif name == 'make':
        adjusted_name = 'GNU make'
    elif name == 'SWIG interface':
        adjusted_name = 'SWIG definitions'
    elif name == 'm4':
        adjusted_name = 'M4'
    else:
        adjusted_name = name
    return adjusted_name


def parse_cloc_xml(filename):

    dom = parse(filename)

    all_languages = []

    for node in dom.getElementsByTagName('language'):
        name = get_adjusted_name(node)
        no_files = get_value(node, 'files_count')
        comment = get_value(node, 'comment')
        code = get_value(node, 'code')

        language = dict({'name': name,
                         'files': no_files,
                         'comment': comment,
                         'code': code})

        all_languages.append(language)

    for node in dom.getElementsByTagName('total'):
        no_files = get_value(node, 'sum_files')
        comment = get_value(node, 'comment')
        code = get_value(node, 'code')

        total = dict({'num_files': no_files,
                      'comment': comment,
                      'code': code})

    data = dict({'languages': all_languages,
                 'totals': total})
    return data


def get_index(languages, language):
    index = 0
    while (index < len(languages) and languages[index]['name'] != language):
        index += 1
    if index >= len(languages):
        return -1
    else:
        return index



def get_line(languages, language):
    i = get_index(languages, language)
    if i == -1:
        return 'ERROR ENCOUNTERED'
    else:
        numbers = languages[i]
        if language == '.sh/.bat':
            language = '\\texttt{.sh}/\\texttt{.bat}'
        return '{:16s} & {:10s} & {:6s} & {:6s}'.format(language, numbers['files'], numbers['code'], numbers['comment'])




def main(args):
    if not os.path.isfile('results_cloc.xml'):
        print ('The cloc program needs to be run before the output can be processed.'
               ' Please run count.bat/count.sh to create the appropriate file.')
        return
    data = parse_cloc_xml('results_cloc.xml')

    file_out = open('loc_table.tex', 'w')

    file_out.write('  \\begin{tabular}[t]{lrrrr}\n')
    file_out.write('    \\toprule\n')
    file_out.write('                       &                   & \\textbf{Code}  & \\textls[-15]{\\textbf{Comment}}\\\\[-2pt]\n')
    file_out.write('    \\textbf{Language} & \\textbf{Files}   & \\textbf{Lines} & \\textbf{Lines}\\\\\n')
    file_out.write('    \\midrule\n')
    # okay not going to be clever and try and sort it
    # I know what order I want them in

    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'C')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'C++')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'C/C++ Header')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'C\\#')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'CMake')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'GNU make')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'Java')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'Javascript')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'M4')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'MATLAB')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'Perl')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'PHP')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'Python')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'R')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'Ruby')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], '.sh/.bat')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'SWIG definitions')))
    file_out.write('     {}\\\\\n'.format(get_line(data['languages'], 'XML')))
    file_out.write('    \\midrule\n')
    file_out.write('    \\emph{:16s}  & {:10s} & {:10s} & {:10s} \\\\\n'.format('{Total}', data['totals']['num_files'], data['totals']['code'], data['totals']['comment']))
    file_out.write('    \\bottomrule\n')
    file_out.write('  \\end{tabular}\n')

    file_out.close()

    print('loc_table.tex has been successfully written')

if __name__ == '__main__':
    main(sys.argv)