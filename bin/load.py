import sys
from pathlib import Path

HOME = Path(__file__).resolve().parent.parent
sys.path.append(str(HOME / "src"))
import aliya

if __name__ == "__main__":
    print(dir(aliya))
