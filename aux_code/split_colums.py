import argparse

def split_columns(input_file):
    # Read the input file
    with open(input_file, 'r') as f:
        lines = f.readlines()

    # Get the column headers (excluding the first column)
    headers = lines[0].strip().split('\t')[1:]

    # Create a dictionary to hold file handles for each column
    column_files = {header: open(f'{header}.txt', 'w') for header in headers}

    # Iterate through the lines and split columns into respective files
    for line in lines[1:]:
        values = line.strip().split('\t')[1:]
        for header, value in zip(headers, values):
            column_files[header].write(value + '\n')

    # Close all file handles
    for file in column_files.values():
        file.close()

def main():
    parser = argparse.ArgumentParser(description='Split tab-separated columns into separate files.')
    parser.add_argument('-f', '--filename', help='Path to the input tab-separated text file')
    args = parser.parse_args()

    split_columns(args.filename)

if __name__ == '__main__':
    main()
