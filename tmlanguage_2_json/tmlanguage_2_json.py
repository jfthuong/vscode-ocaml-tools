import json
from pathlib import Path
from pprint import pprint
import logging
from typing import Any, Dict, Generator, List, Tuple, Union
from xml.etree import ElementTree

try:
    import yaml
except ImportError:
    print("WARNING: 'yaml' module not found. Cannot export to YAML")

# TODO: improve typing for Grammar (find out how to do cyclic types)
Value_Dict = Dict[str, Any]
Value = Union[str, List[Value_Dict], Value_Dict]
Grammar = Dict[str, Any]
Element = ElementTree.Element


class GrammarError(Exception):
    """Error in Grammar"""

    pass


class Parser:
    def __init__(self, tml_path: Path):
        self.tml = tml_path
        self.grammar: Grammar = dict()

    @classmethod
    def parse_string(cls, string_r: Element, location: str = None) -> str:
        """Parse a string"""
        loc = f" (in {location})" if location else ""
        if not string_r.text:
            raise GrammarError(f"Tag '{string_r.tag}' has no text{loc}")
        return string_r.text

    @classmethod
    def parse_array(cls, array_r: Element, location: str = None) -> List[Any]:
        """Parse an array"""
        loc = f" (in {location})" if location else ""
        list_values: List[Value] = list()
        for child in array_r:
            if child.tag == "string":
                list_values.append(cls.parse_string(child))
            elif child.tag == "dict":
                list_values.append(cls.parse_dict(child))
            elif child.tag == "array":
                list_values.append(cls.parse_array(child))
            else:
                raise GrammarError(f"Tag '{child.tag}' not supported in arrays{loc}")

        return list_values

    @classmethod
    def parse_dict(cls, dict_r: Element, location: str = None) -> Dict[str, Any]:
        """Parse a dictionary"""
        loc = f" (in {location})" if location else ""

        def get_key_val(e: Element) -> Generator[Tuple[str, Value], None, None]:
            children = iter(e)
            while True:
                # We will check that we have a key with text
                # and either a value, an array, or (maybe) a dict
                key_xml = next(children)
                k_tag = key_xml.tag
                if k_tag != "key":
                    raise GrammarError(f"Key {k_tag} shall be 'key'")
                key = cls.parse_string(key_xml)

                value_xml = next(children)
                v_tag = value_xml.tag
                value: Value
                if v_tag == "string":
                    value = cls.parse_string(value_xml, location=f"key {key}")
                elif v_tag == "array":
                    value = cls.parse_array(value_xml, location=f"key {key}")
                elif v_tag == "dict":
                    value = cls.parse_dict(value_xml, location=f"key {key}")
                else:
                    raise GrammarError(f"Incorrect value type ({v_tag}) in {key}")

                yield key, value

        grammar: Grammar = dict()
        try:
            for key, value in get_key_val(dict_r):
                grammar[key] = value
        except (RuntimeError, StopIteration):
            pass

        return grammar

    def parse(self) -> "Parser":
        """Convert a tmLanguage using XML syntax to a dictionary"""
        with self.tml.open() as f:
            tree = ElementTree.parse(f)

        root_dict = tree.getroot().find("./dict")
        if root_dict is None:
            raise GrammarError(f"No Top-Level Dictionary found in {self.tml}")

        try:
            self.grammar = self.parse_dict(root_dict)
        except GrammarError as e:
            logging.error(f"Error while parsing TextMate Grammar {self.tml}: {e}")

        print(f"Finished parsing TextMate Grammar {self.tml}")
        return self

    def to_json(self, path: Path) -> "Parser":
        """Export to JSON"""
        with path.open("w") as f:
            json.dump(self.grammar, f)
        print(f"Exported TextMate Grammar to JSON in {path}")

        return self

    def to_yaml(self, path: Path) -> "Parser":
        """Export to YAML"""
        with path.open("w") as f:
            yaml.dump(self.grammar, f)
        print(f"Exported TextMate Grammar to YAML in {path}")

        return self


def main(tml_path: Union[str, Path], out_path: str = None, json=True, yaml=False):
    """Export a TextMate to the desired format"""
    xml_path : Path = Path(__file__).parent.absolute() / tml_path  # to CHDIR
    # xml_path = Path(tml_path)

    parser = Parser(xml_path).parse()

    if out_path is None:
        json_path = xml_path.with_suffix(".tmLanguage.json")
    else:
        json_path = Path(out_path).with_suffix(".json")
    if json:
        parser.to_json(json_path)
    if yaml:
        parser.to_yaml(json_path.with_suffix(".yaml"))


if __name__ == "__main__":
    xml_path = Path(__file__).absolute().with_name("small.tmLanguage")
    main(xml_path, yaml=True)

    main("../syntaxes/menhir.tmLanguage", yaml=True)