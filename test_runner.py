
import os

from typing import Optional

from enum import Enum
from serde import serde
from serde.yaml import from_yaml

import subprocess


class TestClass(Enum):
    INITVARS  = "initvars"

INITVARS_VALID_FLAGS = ["strict"]

@serde
class TestScenario :
    name : str
    classname : TestClass
    flags : Optional[list[str]]


@serde
class TestConfig: 
    executable_path : str  
    test_dir : str
    out_dir : str
    scenarios : list[TestScenario]

@serde
class TestResult : 
    filepath : str 
    output_stdout : str
    output_stderr : str

class Test :
    file : str 
    method : str
    parameters : list[str]

    def __init__(self):
        self.parameters = []

class CommandBuilder:

    test: Optional[Test] = None

    def _check_initialized(self):
        if self.test is None:
            raise ValueError("Test not initialized")

    def _add_flag(self, flag):
        if len(flag) == 0 :
            raise ValueError("Flag cannot be empty")
        if len(flag) == 1 : 
            self.test.parameters.append(f"-{flag}")
        else:
            self.test.parameters.append(f"--{flag}")

    def _set_method(self, testclass : TestClass):
        match testclass:
            case TestClass.INITVARS:
                self.test.method = "initvars"

    def create(self): 
        self.test = Test()
        return self
    
    def _get_valid_flag_list(self, testclass : TestClass) -> str:
        match testclass:
            case TestClass.INITVARS:
                return INITVARS_VALID_FLAGS


    def with_scenario(self, scenario : TestScenario, file:str) :
        self._check_initialized()
        self._set_method(scenario.classname)
        valid_flags = self._get_valid_flag_list(scenario.classname)

        self.test.file = file
        match scenario.flags:
            case None:
                pass
            case flags:
                for flag in flags:
                    if flag not in valid_flags:
                        raise ValueError(f"Flag {flag} not valid for scenario {scenario.classname}")

        return self
    def build(self) -> Test:
        self._check_initialized()
        test = self.test 
        self.test = None
        return test

def find_all_testable_files(test_dir : str) -> list[str] :
    testable_files = []
    for root, dirs, files in os.walk(test_dir):
        for file in files:
            if file.endswith(".jazz") or file.endswith(".jinc"):
                testable_files.append(os.path.join(root, file))
        for dir in dirs:
            testable_files += find_all_testable_files(dir)
    return testable_files

def make_tests(config : TestConfig):
    scenarios = {}
    builder = CommandBuilder()
    testable_files = find_all_testable_files(config.test_dir)
    for scenario in config.scenarios:
        tests = []
        for file in testable_files:
            test = builder.create().with_scenario(scenario,file).build()
            tests.append(test)
        scenarios[scenario.name] = tests
    return scenarios

def run_test(test: Test, config: TestConfig):
    print(f"Running test {test.file}")
    result = subprocess.run(
        [config.executable_path]+[test.method]+test.parameters + [test.file],
        stdout = subprocess.PIPE,
        text=True,
        env= dict(os.environ) | {"JASMINPATH": "Jade=../libjade/oldsrc-should-delete/"}
    )
    print(result.stdout)


with open ("tests.yaml") as file:
    data = file.read()
    test_config = from_yaml(TestConfig,data)

print(test_config)
scenarios = make_tests(test_config)
for scenario in scenarios:
    for test in scenarios[scenario]:
        run_test(test, test_config)

