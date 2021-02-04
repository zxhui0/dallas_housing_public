"""
Useful, generic functions
"""

from pathlib import Path
import os
import sys

source_path = str(Path(os.path.abspath(__file__)).parent.parent)
if source_path not in sys.path:
    sys.path.insert(0, source_path)

import logging
import yaml


def load_config(env='dev'):
    return open_yaml(Path(os.path.abspath(__file__)).parent.parent.parent / 'config.yaml')[env]


def open_yaml(path):
    """
    Load yaml file

    Parameters
    ----------
    path: pathlib.PosixPath
        Path to yaml file
    Return
    ------
    Dictionary
        Dictionary with yaml file content
    """

    with open(path) as stream:
        try:
            yaml_dict = yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            logging.error('Error when opening YAML file.', exc_info=1)

    return yaml_dict
