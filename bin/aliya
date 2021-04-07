#!/usr/bin/env python

import os
from pathlib import Path

try:
    import fire
except ImportError:
    from pip._internal.main import main as pip

    pip(["install", "fire"])
    import fire


class Aliya:
    def __init__(self):
        self._root = Path(__file__).parent.parent.absolute()

    def update(self):
        os.system("cd $ALIYA &&  git pull --ff-only && cd $OLDPWD")

    def hello(self):
        self.lisp('(aliya:hello)')

    def lisp(self, form):
        asd = "/".join(str(self._root / "aliya.asd").split("\\"))
        os.system("ros -S {} -s aliya -e {}".format(asd, form))


if __name__ == "__main__":
    fire.Fire(Aliya)
