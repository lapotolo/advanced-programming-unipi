import subprocess
import sys
import urllib.request
import re


def read_stdin():
    try:
        return sys.argv[1], sys.argv[2]
        # url = sys.argv[1]
        # snippet_path = sys.argv[2]
    except Exception:
        print('Two arguments are required to run this script (url, snippet_path).\nSCRIPT EXECUTION ABORTED.')
        sys.exit()

# source: https://stackoverflow.com/questions/7243750/download-file-from-web-in-python-3
def download_and_save(url, file_name):
    try:
        with urllib.request.urlopen(url) as response, open(file_name, 'wb') as out_file:
            data = response.read()  # a `bytes` object
            out_file.write(data)
            return data.decode('utf-8')
    except Exception:
        print('The url passed is not valid or no answer from the server.\nSCRIPT EXECUTION ABORTED.')
        sys.exit()


# add snippet not found exception
def read_snippet(path):
    text_read = ''
    try:
        with open(path, 'r') as a_java_function:
            for line in a_java_function:
                text_read += line # fastest way to concatenate strings
        return text_read
    except Exception:
        print('The path given for the local java snippet does not exist.\nSCRIPT EXECUTION ABORTED.')
        sys.exit()


def add_snippet(java_class_src, snipp_src):
    # the function is added at the beginning of the class definition
    # no need to have a perfect indentation. Java is not Python
    return re.sub(r'(\.*{\s*)', r'\1' + snipp_src + '\n\t', java_class_src, count=1)
    # BONUS (maybe bugged)
    # the function is added at the end of the class definition
    # return re.sub(r'(}\s*)}$', r'\1' + snipp_src + '\n}', java_class_src)


def get_funct_name(first_line):
    return re.search(r'(\w*\s*)\(', first_line).group(1)


def refactor_java_prints(java_class_src, funct_name):
    return re.sub(r'System.out.println\((.+)\);', r'System.out.println(' + funct_name + r'(\1));', java_class_src)


def save_refactored_java_class(data, file_name):
    with open(file_name, 'w') as out_file:
        out_file.write(data)


def compile_java_class(file_name, err_filepath):
    p = subprocess.Popen('javac ' + file_name, shell=True, stderr=subprocess.PIPE)
    err = p.stderr.readlines()
    if len(err) > 0:  # if there's something in stderr
        print('Compilation error occurred. Check COMP_ERR.txt for details')
        with open(err_filepath, 'wb') as err_file:
            for line in err:
                err_file.write(line)
        sys.exit()
    print('Compilation succeded!')


def run_java_file(file_name, out_filepath, err_filepath):
    p = subprocess.Popen('java ' + file_name, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out = p.stdout.readlines()
    err = p.stderr.readlines()
    if len(out) > 0:  # if there's something in stdout
        print('Check OUTPUT.txt for {0}.class output.'.format(file_name))
        with open(out_filepath, 'wb') as out_file:
            for line in out:
                out_file.write(line)
    if len(err) > 0:  # if there's something in stderr
        print('Runtime error occurred. Check ERROR.txt for details')
        with open(err_filepath, 'wb') as err_file:
            for line in err:
                err_file.write(line)


# Setting up
url, snippet_path = read_stdin()
file_name = url.split('/')[-1]
file_name_original_src = '_original_' + file_name
compilation_error_filepath = 'COMP_ERR.txt'
runtime_error_filepath = 'ERROR.txt'
output_filepath = 'OUTPUT.txt'

# Execution
class_src = download_and_save(url, file_name_original_src)
snippet_src = read_snippet(snippet_path)
class_src = add_snippet(class_src, snippet_src)
new_function_name = get_funct_name(snippet_src.split('\n')[0])
class_src = refactor_java_prints(class_src, new_function_name)
save_refactored_java_class(class_src, file_name)
compile_java_class(file_name, compilation_error_filepath)
run_java_file(file_name.split('.java')[0], output_filepath, runtime_error_filepath)
