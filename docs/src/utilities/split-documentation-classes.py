"""This helper splits doxygen documentation into separate files for each class.
"""
import sys
import os

def main(args):
    if len(args) != 3:
        print ("Must be given two argument: the path to the documentation file to be split, and a target directory, where to place the documentation files")
        sys.exit(1)

    # Read the documentation file
    documenation_file = args[1]
    target_dir = args[2]

    if not os.path.exists(documenation_file):
        print ("The given documentation file does not exist")
        sys.exit(1)

    # read the file line by line
    with open(documenation_file, 'r') as f:
        current_file = None
        for line in f:
            if ' <!--' in line:
                continue

            if '@class' in line and not '@@class' in line:
                # close the current file
                if current_file:
                    current_file.write(' */\n')
                    current_file.close()

                # we found a new class documentation
                class_name = line.strip().split(" ")[-1]
                # create a new file
                new_file = os.path.join(target_dir, "temp_" + class_name + ".txt")
                current_file = open(new_file, 'w')
                current_file.write('/**\n' + line)
            else:
                # append to the current file
                if current_file:
                    current_file.write(line)

        # close the last file
        if current_file:
            current_file.write(' */\n')
            current_file.close()
if __name__ == '__main__':    
    main(sys.argv)