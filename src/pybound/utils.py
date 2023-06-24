import pickle
from os import makedirs
from os.path import isdir


def serialize(data, file_path):
    initialiseFile(file_path)
    if file_path.endswith(".obj"):
        filehandler = open(file_path, "wb")
        pickle.dump(data, filehandler)
        filehandler.close()


def deserialize(file_path):
    if file_path.endswith(".obj"):
        filehandler = open(file_path, "rb")
        data = pickle.load(filehandler)
        filehandler.close()
        return data


def initialiseFile(file_path):
    """helper function that ensures a suitable file exists"""
    try:
        # if overwrite: #TODO: remove
        f = open(file_path, "w")
        f.close()
        # else:
        # f = open(file_path, "r")
        # f.close()
        # raise Exception("The {file} already exists and no premission has been given to overwrite.".format(file=file_path))
    except FileNotFoundError:
        file_path_parts = file_path.split("/")
        prefix = "/".join(file_path_parts[:-1])
        if not isdir(prefix):
            makedirs(prefix)
        f = open(file_path, "x")
        f.close()


def get_problems(problem_list):
    """helper function reading in cases by giving case names in the form initial_goal"""
    index = []
    with open(problem_list) as cases:
        for case in cases:
            case = case.split()
            initialState = case[0].strip()
            goalState = case[1].strip()
            caseName = initialState + "_" + goalState
            index.append(caseName)
    return index
